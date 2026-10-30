"""Two-pass assembly of the demo video (after gen_timeline.py).

Pass 1 (premix): place each narration beat at its delay and write
voiceover/vo_track.wav (there is no music, D288).
Pass 2 (final): the cards, the captures, the strip (an ffconcat of PNG
files) and vo_track go through filter.txt. Output: decisions-demo.mp4,
1920 x 1080, 30 fps, H.264 and AAC, with +faststart.

Run from video/:  python assemble.py
"""
import json
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent
VOD = HERE / "voiceover"
OUT = HERE / "decisions-demo.mp4"


def run(cmd):
    print(" ".join(str(c) for c in cmd)[:200], "...")
    subprocess.run([str(c) for c in cmd], check=True)


def premix(meta):
    total = meta["total"]
    inputs, chains, labels = [], [], []
    for idx, (name, start) in enumerate(meta["delays"].items()):
        inputs += ["-i", VOD / f"{name}.mp3"]
        chains.append(f"[{idx}:a]aresample=48000,adelay={int(round(start * 1000))}:all=1[b{idx}]")
        labels.append(f"[b{idx}]")
    graph = (";".join(chains) + ";" + "".join(labels) +
             f"amix=inputs={len(labels)}:duration=longest:normalize=0,"
             f"apad=whole_dur={total},atrim=0:{total}[vo]")
    run(["ffmpeg", "-y", "-v", "error", *inputs, "-filter_complex", graph, "-map", "[vo]",
         "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", VOD / "vo_track.wav"])


def final():
    manifest = json.loads((HERE / "inputs.json").read_text())
    args = ["ffmpeg", "-y", "-v", "error"]
    for item in manifest:
        path = HERE / item["file"]
        if item["kind"] == "card":
            args += ["-loop", "1", "-framerate", "30", "-t", str(item["t"]), "-i", path]
        elif item["kind"] == "concat":
            args += ["-f", "concat", "-safe", "0", "-i", path]
        else:
            args += ["-i", path]
    args += ["-/filter_complex", HERE / "filter.txt",   # ffmpeg 7+: read the graph from a file
             "-map", "[vout]", "-map", "[aout]",
             "-c:v", "libx264", "-crf", "18", "-preset", "medium", "-pix_fmt", "yuv420p",
             "-r", "30", "-c:a", "aac", "-b:a", "192k", "-movflags", "+faststart", OUT]
    run(args)
    print("wrote", OUT.name)


if __name__ == "__main__":
    meta = json.loads((HERE / "vo_delays.json").read_text())
    premix(meta)
    final()
