"""Fit the picture to the narration and build the narration strip.

The frame is 1920 x 1080: the app or a card in the top 1920 x 864, and the
narration strip in the bottom 1920 x 216 (D281). The strip shows the text of
the narration, timed from the character times that ElevenLabs returns.

For each segment, target = beat + LEAD + TAIL + extra. A card is held for
the target. A video window is cut from its capture:
  rate None   : 1x and trimmed when the window is long enough; else slowed
                to MIN_RATE at most, then the last frame is held
  rate "fit"  : sped up or slowed so that the window fills the target
                (never below MIN_RATE; the rest is a held frame)
  rate number : that speed; a held frame fills the rest, a trim cuts it

Emits: filter.txt, inputs.json, vo_delays.json, strip/ (PNG and strip.txt),
captions/demo.srt, and the QC table.

Run from video/ after make_vo.py and make_cards.py, before assemble.py.
"""
import json
import re
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).resolve().parent
VOD = HERE / "voiceover"
STRIP = HERE / "strip"
CAPD = HERE / "captions"
FONTS = HERE / "assets" / "fonts"

VO = json.loads((VOD / "vo_durations.json").read_text())
BEATS = json.loads((VOD / "vo_beats.json").read_text(encoding="utf-8"))
SCRIPT = {m.group(1): m.group(2) for m in (
    re.match(r"^\*\*(v\d+\w*)\*\*\s+[-—]+\s+(.*\S)\s*$", line.strip())
    for line in (VOD / "vo_script.md").read_text(encoding="utf-8").splitlines()) if m}

LEAD, TAIL = 0.4, 0.4
MIN_RATE = 0.85
FPS = 30
TOP_H, STRIP_H, W = 864, 216, 1920
FADE = 0.3

# Chapters of the strip: a label and a short name
CHAPTERS = {
    "intro": ("DECISIONS", "Intro"),
    "p1": ("PART 1", "Images · sample"),
    "p2": ("PART 2", "Images · your data"),
    "p3": ("PART 3", "Text"),
    "end": ("SUMMARY", "Recap"),
}

CARD_FILES = ["intro", "types", "part1", "part2", "part3", "recap", "outro"]
VID_FILES = ["home", "sample", "upload", "text_sample", "text_score"]
CARD_IN = {n: i for i, n in enumerate(CARD_FILES)}
VID_IN = {n: len(CARD_FILES) + i for i, n in enumerate(VID_FILES)}
IN_STRIP = len(CARD_FILES) + len(VID_FILES)
IN_VOTRK = IN_STRIP + 1

# (name, kind, source, window, beat, rate, extra, chapter)
# A window is (start, end) in seconds, or two names of the marks that
# capture_demo.py wrote (captures/<source>_marks.json); "end" is the end.
SEGS = [
    ("intro", "card", "intro", None, "v00_intro", None, 0.6, "intro"),
    ("types", "card", "types", None, "v01_types", None, 0.6, "intro"),
    ("p1card", "card", "part1", None, "v10_card", None, 0.4, "p1"),
    ("layout", "vid", "home", (0.6, 16.2), "v11_layout", "fit", 0, "p1"),
    ("pick", "vid", "sample", (0.0, "question"), "v12_sample", None, 0, "p1"),
    ("question", "vid", "sample", ("question", "buttons"), "v13_question", "fit", 0, "p1"),
    ("run", "vid", "sample", ("buttons", "answers"), "v14_run", "fit", 0, "p1"),
    ("answers", "vid", "sample", ("answers", "table"), "v15_answers", "fit", 0, "p1"),
    ("table", "vid", "sample", ("table", "code"), "v16_table", None, 0, "p1"),
    ("code", "vid", "sample", ("code", "import"), "v17_code", "fit", 0, "p1"),
    ("import", "vid", "sample", ("import", "end"), "v18_import", None, 0, "p1"),
    ("p2card", "card", "part2", None, "v20_card", None, 0.4, "p2"),
    ("noul", "vid", "upload", ("noul", "desc"), "v21_noul", "fit", 0, "p2"),
    ("desc", "vid", "upload", ("desc", "zip"), "v22_desc", "fit", 0, "p2"),
    ("zip", "vid", "upload", ("zip", "try5"), "v23_zip", "fit", 0, "p2"),
    ("try5", "vid", "upload", ("try5", "run_all"), "v24_try", "fit", 0, "p2"),
    ("runall", "vid", "upload", ("run_all", "calib"), "v25_runall", "fit", 0, "p2"),
    ("calib", "vid", "upload", ("calib", "code"), "v26_calib", "fit", 0, "p2"),
    ("upcode", "vid", "upload", ("code", "end"), "v27_code", None, 0, "p2"),
    ("p3card", "card", "part3", None, "v30_card", None, 0.4, "p3"),
    ("textsample", "vid", "text_sample", (0.3, 18.0), "v31_sample", "fit", 0, "p3"),
    ("paste", "vid", "text_score", (0.3, "score"), "v32_paste", None, 0, "p3"),
    ("score", "vid", "text_score", ("score", "run"), "v33_score", "fit", 0, "p3"),
    ("scorerun", "vid", "text_score", ("run", "end"), "v34_run", None, 0, "p3"),
    ("recap", "card", "recap", None, "v50_recap", None, 0.6, "end"),
    ("outro", "card", "outro", None, "v51_outro", None, 1.8, "end"),
]

# --- Strip ---------------------------------------------------------------

TAN = (237, 236, 226)        # Light Tan #EDECE2
RED = (196, 18, 48)          # Miami Red #C41230 (5.2:1 on Light Tan)
INK = (17, 17, 17)           # text (16:1 on Light Tan)
SUB = (112, 104, 92)         # Dark Tan #70685C, chapter name (4.6:1 on Light Tan)
RULE = (204, 201, 184)       # Medium Tan #CCC9B8
F_TEXT = ImageFont.truetype(str(FONTS / "source-sans-3-latin-600-normal.woff2"), 44)
F_LABEL = ImageFont.truetype(str(FONTS / "ibm-plex-mono-latin-600-normal.woff2"), 24)
F_NAME = ImageFont.truetype(str(FONTS / "source-sans-3-latin-600-normal.woff2"), 28)
TEXT_X, TEXT_W, LINE_H = 420, 1444, 58
HOLD_MAX = 2.5               # a caption stays at most this long after its words
MARKUP_RE = re.compile(r"\{([^|}]*)\|([^}]*)\}")


def wrap(text, max_w=TEXT_W):
    d = ImageDraw.Draw(Image.new("RGB", (1, 1)))
    lines, cur = [], ""
    for word in text.split():
        test = f"{cur} {word}".strip()
        if d.textlength(test, font=F_TEXT) <= max_w:
            cur = test
        else:
            lines.append(cur)
            cur = word
    lines.append(cur)
    return lines


def split_raw(raw):
    """Split the raw text of a beat (with markup) into chunks of at most two lines."""
    keep = {}

    def protect(m):
        key = f"\u0000{len(keep)}\u0000"
        keep[key] = m.group(0)
        return key
    safe = MARKUP_RE.sub(protect, raw)
    restore = lambda s: re.sub("\u0000\\d+\u0000", lambda m: keep[m.group(0)], s)
    shown = lambda s: MARKUP_RE.sub(r"\1", restore(s))

    def split(piece):
        if len(wrap(shown(piece))) <= 2:
            return [piece]
        cuts = [m.end() for m in re.finditer(r"[:,;] ", piece)]
        if not cuts:
            words = piece.split(" ")
            half = len(words) // 2
            return split(" ".join(words[:half])) + split(" ".join(words[half:]))
        mid = min(cuts, key=lambda c: abs(c - len(piece) / 2))
        return split(piece[:mid].strip()) + split(piece[mid:].strip())

    out = []
    for sentence in re.split(r"(?<=[.!?])\s+", safe):
        out += split(sentence)
    return [restore(c) for c in out if c.strip()]


def chunk_times(beat):
    """[(shown, start, end)] for the chunks of a beat, in seconds from the beat start."""
    align = json.loads((VOD / f"{beat}.align.json").read_text())
    chars = "".join(align["characters"])
    starts, ends = align["character_start_times_seconds"], align["character_end_times_seconds"]
    out, pos = [], 0
    for raw in split_raw(SCRIPT[beat]):
        say = MARKUP_RE.sub(r"\2", raw).strip()
        a = chars.find(say, pos)
        if a < 0:
            raise SystemExit(f"{beat}: chunk not in the alignment: {say!r}")
        b = a + len(say)
        out.append((MARKUP_RE.sub(r"\1", raw).strip(), starts[a], ends[b - 1]))
        pos = b
    return out


def render_strip(path, chapter, text):
    img = Image.new("RGB", (W, STRIP_H), TAN)
    d = ImageDraw.Draw(img)
    d.rectangle((0, 0, W, 5), fill=RED)
    label, name = CHAPTERS[chapter]
    x, y = 60, 76
    for c in label:                       # letter-spaced label
        d.text((x, y), c, font=F_LABEL, fill=RED)
        x += d.textlength(c, font=F_LABEL) + 3
    d.text((60, y + 38), name, font=F_NAME, fill=SUB)
    d.line((TEXT_X - 36, 44, TEXT_X - 36, STRIP_H - 38), fill=RULE, width=2)
    if text:
        lines = wrap(text)
        y0 = 5 + (STRIP_H - 5 - LINE_H * len(lines)) / 2 + 4
        for i, line in enumerate(lines):
            d.text((TEXT_X, y0 + i * LINE_H), line, font=F_TEXT, fill=INK)
    img.save(path)


def srt_time(t):
    ms = round(t * 1000)
    return f"{ms // 3600000:02d}:{ms // 60000 % 60:02d}:{ms // 1000 % 60:02d},{ms % 1000:03d}"


# --- Timeline ------------------------------------------------------------

def capture_end(src):
    import subprocess
    r = subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                        "-of", "default=noprint_wrappers=1:nokey=1",
                        str(HERE / "captures" / f"{src}.mp4")], capture_output=True, text=True)
    return float(r.stdout.strip()) - 0.1


def resolve(src, win):
    """A window of seconds; mark names come from captures/<src>_marks.json."""
    marks = json.loads((HERE / "captures" / f"{src}_marks.json").read_text())
    if "end" not in marks:                      # captures made before the "end" mark
        marks["end"] = capture_end(src)
    pick = lambda v: marks[v] if isinstance(v, str) else v
    return round(pick(win[0]), 2), round(pick(win[1]), 2)


def main():
    branches, labels, delays, rows, card_durs = [], [], {}, [], {}
    caps = []                                   # (start, end, shown, chapter)
    seg_chapter = []                            # (start, end, chapter)
    t = 0.0
    for i, (name, kind, src, win, beat, rate, extra, chapter) in enumerate(SEGS):
        vod = VO[beat]
        target = round(vod + LEAD + TAIL + extra, 2)
        prev_src = SEGS[i - 1][2] if i else None
        next_src = SEGS[i + 1][2] if i + 1 < len(SEGS) else None
        if kind == "card":
            dur = target
            card_durs[src] = dur
            chain = (f"[{CARD_IN[src]}:v]scale={W}:{TOP_H},setsar=1,fps={FPS},"
                     f"trim=0:{dur},setpts=PTS-STARTPTS")
            shown_rate = "card"
        else:
            win = resolve(src, win)
            w0, w1 = win
            wlen = w1 - w0
            if rate is None:
                rate = 1.0 if wlen >= target else max(MIN_RATE, wlen / target)
            elif rate == "fit":
                rate = max(MIN_RATE, wlen / target)
            dur = target
            hold = max(0.0, target - wlen / rate)
            chain = (f"[{VID_IN[src]}:v]trim={w0}:{w1},setpts=(PTS-STARTPTS)/{rate:.4f},"
                     f"fps={FPS},setsar=1")
            if hold > 0:
                chain += f",tpad=stop_mode=clone:stop_duration={hold + 0.1:.2f}"
            chain += f",trim=0:{dur},setpts=PTS-STARTPTS"
            shown_rate = f"{rate:.2f}" + (f"+hold {hold:.1f}" if hold > 0.05 else "")
        if src != prev_src:
            chain += f",fade=t=in:st=0:d={0.8 if i == 0 else FADE}"
        if src != next_src:
            chain += f",fade=t=out:st={round(dur - (1.2 if next_src is None else FADE), 2)}:d={1.2 if next_src is None else FADE}"
        chain += ",format=yuv420p"
        branches.append(chain + f"[s{i}]")
        labels.append(f"[s{i}]")
        delays[beat] = round(t + LEAD, 2)
        for shown, a, b in chunk_times(beat):
            caps.append([t + LEAD + a, t + LEAD + b, shown, chapter])
        seg_chapter.append((t, t + dur, chapter))
        rows.append((name, round(t, 2), dur, beat, vod, shown_rate, win))
        t = round(t + dur, 2)
    total = t

    # Caption ends: until the next caption, at most HOLD_MAX after the words
    for k, c in enumerate(caps):
        nxt = caps[k + 1][0] if k + 1 < len(caps) else total
        c[1] = min(nxt, c[1] + HOLD_MAX)

    # Strip: one PNG for each caption and each blank chapter, in an ffconcat list
    STRIP.mkdir(exist_ok=True)
    for f in STRIP.glob("*.png"):
        f.unlink()
    pieces = []                                 # (start, end, png)
    chapter_at = lambda x: next(ch for a, b, ch in seg_chapter if a <= x < b + 1e-6)
    cursor = 0.0
    for k, (a, b, shown, chapter) in enumerate(caps):
        if a > cursor + 0.01:
            png = STRIP / f"blank_{chapter_at(cursor)}.png"
            if not png.exists():
                render_strip(png, chapter_at(cursor), "")
            pieces.append((cursor, a, png))
        png = STRIP / f"c{k:03d}.png"
        render_strip(png, chapter, shown)
        pieces.append((a, b, png))
        cursor = b
    if cursor < total:
        png = STRIP / "blank_end.png"
        render_strip(png, "end", "")
        pieces.append((cursor, total, png))
    with open(STRIP / "strip.txt", "w") as f:
        f.write("ffconcat version 1.0\n")
        for a, b, png in pieces:
            f.write(f"file '{png.name}'\nduration {b - a:.3f}\n")
        f.write(f"file '{pieces[-1][2].name}'\n")

    # Captions for YouTube, from the same text
    CAPD.mkdir(exist_ok=True)
    with open(CAPD / "demo.srt", "w", encoding="utf-8") as f:
        for k, (a, b, shown, _) in enumerate(caps, start=1):
            f.write(f"{k}\n{srt_time(a)} --> {srt_time(b)}\n{shown}\n\n")

    lines = [";\n".join(branches)]
    lines.append(f"{''.join(labels)}concat=n={len(SEGS)}:v=1:a=0[top]")
    lines.append(f"[{IN_STRIP}:v]fps={FPS},scale={W}:{STRIP_H},setsar=1,format=yuv420p,"
                 f"trim=0:{total},setpts=PTS-STARTPTS[strip]")
    lines.append("[top][strip]vstack=inputs=2[vout]")
    lines.append(f"[{IN_VOTRK}:a]atrim=0:{total},loudnorm=I=-16:LRA=7:TP=-1.5,"
                 f"aresample=48000[aout]")
    (HERE / "filter.txt").write_text(";\n".join(lines) + "\n", encoding="utf-8")

    (HERE / "vo_delays.json").write_text(json.dumps({"total": total, "delays": delays, "aux": {}}, indent=1))
    manifest = [{"kind": "card", "file": f"cards/{n}.png", "t": card_durs[n]} for n in CARD_FILES]
    manifest += [{"kind": "vid", "file": f"captures/{n}.mp4"} for n in VID_FILES]
    manifest += [{"kind": "concat", "file": "strip/strip.txt"},
                 {"kind": "audio", "file": "voiceover/vo_track.wav"}]
    (HERE / "inputs.json").write_text(json.dumps(manifest, indent=1))

    print(f"{'segment':<11} {'start':>7} {'dur':>6} {'beat':<13} {'vod':>5}  rate        window")
    for name, st, dur, beat, vod, rt, win in rows:
        print(f"{name:<11} {st:>7.2f} {dur:>6.2f} {beat:<13} {vod:>5.2f}  {rt:<11} {win}")
    print(f"{len(caps)} captions; TOTAL {total}s = {int(total // 60)}:{total % 60:05.2f}")


if __name__ == "__main__":
    main()
