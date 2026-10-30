"""Render the narration beats of vo_script.md with ElevenLabs (D282).

Each beat goes to the endpoint "with-timestamps", which returns the audio and
the start and end time of each character. The narration strip uses these
times. Output, next to this script: <beat>.mp3, <beat>.align.json,
vo_durations.json and vo_beats.json (the shown and the spoken text).

Paid calls: each request is a row in the call log of the project record
(calls/elevenlabs-calls.csv of fine_tuned_jev). The script refuses a request
that takes the characters of the log over CHAR_CAP. Beats whose text, voice
and settings did not change are not sent again (cache by hash).

The key comes from ELEVENLABS_API_KEY or from the .Renviron of fine_tuned_jev.
The script never prints or writes the key.

Usage (from video/voiceover/):
    python make_vo.py                    # all beats with VOICE
    python make_vo.py --only v00_intro   # some beats (vo_durations.json then holds only these)
    python make_vo.py --sample v00_intro # one beat with each voice of SAMPLE_VOICES
"""
import argparse
import base64
import csv
import hashlib
import json
import os
import re
import subprocess
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

HERE = Path(__file__).resolve().parent
PROJECT = HERE.parents[2] / "fine_tuned_jev"
LOG = PROJECT / "replication_materials" / "calls" / "elevenlabs-calls.csv"
CACHE = HERE / ".vo_cache.json"

MODEL = "eleven_v4"                     # Eleven v4, chosen by fmm
VOICE = "1FmDfZG0Nx2dCk793S1a"          # the voice of fmm
SETTINGS = {"stability": 0.5, "similarity_boost": 0.75}   # the two settings of v4
SAMPLE_VOICES = {"fmm": VOICE}
CHAR_CAP = 20000                        # D282
PRICE_PER_CHAR = 0.08 / 1000            # USD, API price of Eleven v4 (elevenlabs.io/pricing/api, 2026-10-29)
PURPOSE = "Space video (D282)"

BEAT_RE = re.compile(r"^\*\*(v\d+\w*)\*\*\s+[-—]+\s+(.*\S)\s*$")
MARKUP_RE = re.compile(r"\{([^|}]*)\|([^}]*)\}")


def shown(text):
    return MARKUP_RE.sub(r"\1", text)


def spoken(text):
    return MARKUP_RE.sub(r"\2", text)


def parse_script():
    beats = []
    for line in (HERE / "vo_script.md").read_text(encoding="utf-8").splitlines():
        m = BEAT_RE.match(line.strip())
        if m:
            beats.append((m.group(1), m.group(2)))
    return beats


def api_key():
    key = os.environ.get("ELEVENLABS_API_KEY")
    if not key:
        for line in (PROJECT / ".Renviron").read_text(encoding="utf-8").splitlines():
            m = re.match(r'\s*ELEVENLABS_API_KEY\s*=\s*"?([^"\s]+)"?', line)
            if m:
                key = m.group(1)
    if not key:
        raise SystemExit("ELEVENLABS_API_KEY not found")
    return key


def chars_used():
    if not LOG.exists():
        return 0
    with open(LOG, newline="", encoding="utf-8") as f:
        return sum(int(r["input_tokens"] or 0) for r in csv.DictReader(f))


def log_call(call_id, request_id, status, latency_ms, chars):
    new = not LOG.exists()
    with open(LOG, "a", newline="", encoding="utf-8") as f:
        w = csv.writer(f, lineterminator="\n")
        if new:
            w.writerow(["time_utc", "call_id", "purpose", "model", "request_id", "status",
                        "latency_ms", "input_tokens", "output_tokens", "cost_usd"])
        w.writerow([datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"), call_id, PURPOSE,
                    MODEL, request_id, status, latency_ms, chars, 0,
                    f"{chars * PRICE_PER_CHAR:.4f}"])


def tts(key, voice, text, call_id):
    """One request; returns (mp3 bytes, alignment). Logs the call."""
    used = chars_used()
    if used + len(text) > CHAR_CAP:
        raise SystemExit(f"stop: {used} + {len(text)} characters is over the cap of {CHAR_CAP}")
    body = json.dumps({"text": text, "model_id": MODEL, "voice_settings": SETTINGS}).encode()
    req = urllib.request.Request(
        f"https://api.elevenlabs.io/v1/text-to-speech/{voice}/with-timestamps"
        "?output_format=mp3_44100_128",
        data=body, headers={"xi-api-key": key, "Content-Type": "application/json"})
    t0 = time.time()
    try:
        with urllib.request.urlopen(req, timeout=120) as r:
            data = json.load(r)
            rid = r.headers.get("request-id", "")
            log_call(call_id, rid, 200, int((time.time() - t0) * 1000), len(text))
    except urllib.error.HTTPError as e:
        detail = e.read().decode(errors="replace")[:300]
        log_call(call_id, e.headers.get("request-id", ""), e.code,
                 int((time.time() - t0) * 1000), 0)
        raise SystemExit(f"ElevenLabs error {e.code}: {detail}")
    return base64.b64decode(data["audio_base64"]), data.get("alignment")


def duration(path):
    r = subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                        "-of", "default=noprint_wrappers=1:nokey=1", str(path)],
                       capture_output=True, text=True)
    return round(float(r.stdout.strip()), 2)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", help="render this beat with each voice of SAMPLE_VOICES")
    ap.add_argument("--only", help="comma-separated beats to render (a first test)")
    args = ap.parse_args()
    beats = parse_script()
    key = api_key()

    if args.sample:
        text = spoken(dict(beats)[args.sample])
        out = HERE / "samples"
        out.mkdir(exist_ok=True)
        for name, voice in SAMPLE_VOICES.items():
            mp3, _ = tts(key, voice, text, f"sample-{name}-{args.sample}")
            (out / f"{args.sample}_{name}.mp3").write_bytes(mp3)
            print(f"sample {name}: {duration(out / f'{args.sample}_{name}.mp3')}s")
        print(f"characters in the log: {chars_used()} of {CHAR_CAP}")
        return

    cache = json.loads(CACHE.read_text()) if CACHE.exists() else {}
    durs, info = {}, {}
    if args.only:
        beats = [b for b in beats if b[0] in args.only.split(",")]
    for name, text in beats:
        say = spoken(text)
        h = hashlib.sha1(json.dumps([VOICE, MODEL, SETTINGS, say]).encode()).hexdigest()
        mp3 = HERE / f"{name}.mp3"
        if cache.get(name) != h or not mp3.exists():
            audio, align = tts(key, VOICE, say, name)
            mp3.write_bytes(audio)
            (HERE / f"{name}.align.json").write_text(json.dumps(align), encoding="utf-8")
            cache[name] = h
            CACHE.write_text(json.dumps(cache, indent=1))
            print(f"rendered {name} ({len(say)} characters)")
        durs[name] = duration(mp3)
        info[name] = {"shown": shown(text), "spoken": say}
    (HERE / "vo_durations.json").write_text(json.dumps(durs, indent=1))
    (HERE / "vo_beats.json").write_text(json.dumps(info, indent=1), encoding="utf-8")
    total = sum(durs.values())
    print(f"{len(durs)} beats, {total:.1f}s of speech; "
          f"characters in the log: {chars_used()} of {CHAR_CAP}")


if __name__ == "__main__":
    main()
