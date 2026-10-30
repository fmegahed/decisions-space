"""Capture the footage of the demo video from the live Space with Playwright.

Each scenario runs in its own browser context (a new session of the app) and
records captures/<name>.mp4 at 1920 x 864: the top part of the frame. The
narration strip below it is added in gen_timeline.py.

Recording: a CDP screencast (JPEG frames at device scale 1.2), not the
recorder of Playwright, whose VP8 video makes the text of the app soft. The
helpers wait with page.wait_for_timeout(), never time.sleep(): the screencast
needs an acknowledgement for each frame, and only a Playwright call lets the
event handler run.

Paid calls (D285): only "Try on 5" and "Run all" of the scenarios upload and
text_score call Jev. --dry skips these clicks. The samples show their saved
runs at no cost.

Usage (from video/):
    python capture_demo.py                     # all scenarios
    python capture_demo.py --only upload       # one scenario
    python capture_demo.py --only upload --dry # no paid click
"""

import argparse
import base64
import json
import os
import shutil
import subprocess
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE = "https://fmegahed-decisions.hf.space/"
HERE = Path(__file__).resolve().parent
CAPTURES = HERE / "captures"
DATA = HERE / "assets" / "data"
VIEW = {"width": 1600, "height": 720}
SCALE = 1.2                     # 1600 x 720 CSS px -> 1920 x 864 frames
FRAME = (1920, 864)
DRY = False
RUNS = []                       # the paid runs of this capture, for the call record

# The question of the upload (D283): Yes / no, with the descriptions of the paper
PIPE_INSTRUCTIONS = "Close-up of a car brake area. Is the metal staple that holds the thin black pipe missing?"
PIPE_TRUE = ("The thin black pipe is not held by its metal staple: the small, bright, square clip "
             "near the center of the image is absent, and the pipe passes the bracket with nothing holding it.")
PIPE_FALSE = ("A small, bright, square metal staple (a white or silver clip) holds the thin black pipe "
              "near the center of the image, where the pipe meets the bracket.")

# The question of the Score example on text
SCORE_INSTRUCTIONS = "How severe is the injury in this report of a work injury?"
SCORE_LEVELS = [
    ("low", "The worker will likely recover fully, for example from a sprain, a simple fracture or a concussion."),
    ("medium", "The worker will likely have some lasting harm, for example from a severe burn or a crush injury that needs surgery."),
    ("high", "The worker has a permanent loss, for example an amputation."),
]

# A visible cursor in Miami Red, so that viewers can follow the clicks
CURSOR_JS = """
(() => {
  const attach = () => {
    if (!document.body || document.getElementById('__pw_cursor')) return;
    const c = document.createElement('div');
    c.id = '__pw_cursor';
    Object.assign(c.style, {
      position: 'fixed', left: '-50px', top: '-50px', width: '22px', height: '22px',
      border: '3px solid #C41230', background: 'rgba(196,18,48,0.28)',
      borderRadius: '50%', pointerEvents: 'none', zIndex: '2147483647',
      transform: 'translate(-50%,-50%)', boxShadow: '0 1px 6px rgba(0,0,0,0.35)',
      transition: 'width 0.12s, height 0.12s, background 0.12s'
    });
    document.body.appendChild(c);
    document.addEventListener('mousemove', e => {
      c.style.left = e.clientX + 'px'; c.style.top = e.clientY + 'px';
    }, true);
    document.addEventListener('mousedown', () => {
      c.style.background = 'rgba(196,18,48,0.65)';
      c.style.width = '30px'; c.style.height = '30px';
    }, true);
    document.addEventListener('mouseup', () => {
      c.style.background = 'rgba(196,18,48,0.28)';
      c.style.width = '22px'; c.style.height = '22px';
    }, true);
  };
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', attach);
  } else { attach(); }
})();
"""


# --------------------------------------------------------------------------
# Recorder: CDP screencast -> JPEG frames with time stamps -> 30 fps H.264
# --------------------------------------------------------------------------

class Recorder:
    def __init__(self, page, name):
        self.page = page
        self.dir = CAPTURES / f"{name}_frames"
        shutil.rmtree(self.dir, ignore_errors=True)
        self.dir.mkdir(parents=True)
        self.out = CAPTURES / f"{name}.mp4"
        self.frames = []
        self.marks = {}
        self.t0 = None
        self.cdp = page.context.new_cdp_session(page)
        self.cdp.on("Page.screencastFrame", self._frame)

    def _frame(self, params):
        path = self.dir / f"{len(self.frames):06d}.jpg"
        path.write_bytes(base64.b64decode(params["data"]))
        # The stamp is on the clock of the browser; keep the arrival time too
        self.frames.append((params["metadata"]["timestamp"], path, time.time()))
        self.cdp.send("Page.screencastFrameAck", {"sessionId": params["sessionId"]})

    def start(self):
        self.cdp.send("Page.startScreencast", {
            "format": "jpeg", "quality": 95, "everyNthFrame": 1,
            "maxWidth": FRAME[0], "maxHeight": FRAME[1]})
        self.t0 = time.time()
        self.page.wait_for_timeout(300)

    def mark(self, label):
        """Note the time of an event (seconds from the start), for the cut table."""
        self.marks[label] = round(time.time() - self.t0, 2)
        print(f"    mark {label}: {self.marks[label]}s")

    def stop(self):
        if self.t0 is None:
            return
        self.page.wait_for_timeout(500)
        self.cdp.send("Page.stopScreencast")
        end = time.time()
        if not self.frames:
            raise RuntimeError("no screencast frame")
        # The browser clock differs from time.time(): estimate the offset from
        # the arrival times (the smallest gap is the frame with no queue), so
        # that the video time matches the marks
        offset = min(ts - got for ts, _, got in self.frames)
        stamps = ([self.t0 + offset] + [t for t, _, _ in self.frames[1:]] + [end + offset])
        lst = self.dir / "frames.txt"
        with open(lst, "w") as f:
            for (_, p, _), cur, nxt in zip(self.frames, stamps[:-1], stamps[1:]):
                f.write(f"file '{p.name}'\nduration {max(nxt - cur, 0.001):.4f}\n")
            # ffmpeg 9 drops the duration of the last entry, and with a repeat of
            # the last file it holds the frame too long: repeat it, then cut at -t
            f.write(f"file '{self.frames[-1][1].name}'\nduration {1 / 30:.4f}\n")
        subprocess.run([
            "ffmpeg", "-y", "-v", "error", "-f", "concat", "-safe", "0", "-i", str(lst),
            "-vf", f"scale={FRAME[0]}:{FRAME[1]}:flags=lanczos,fps=30,format=yuv420p",
            "-t", f"{end - self.t0:.3f}",
            "-c:v", "libx264", "-crf", "16", "-preset", "medium", str(self.out)], check=True)
        self.marks["end"] = round(end - self.t0, 2)
        (CAPTURES / f"{self.out.stem}_marks.json").write_text(json.dumps(self.marks, indent=1))
        if not os.environ.get("KEEP_FRAMES"):
            shutil.rmtree(self.dir, ignore_errors=True)
        print(f"    saved {self.out.name} ({end - self.t0:.1f}s, {len(self.frames)} frames, "
              f"clock offset {offset:+.2f}s)")


# --------------------------------------------------------------------------
# Helpers (human pace, so that the footage narrates well)
# --------------------------------------------------------------------------

def settle(page, seconds):
    page.wait_for_timeout(int(seconds * 1000))


def idle(page, timeout=90):
    """Wait until Shiny is not busy."""
    deadline = time.time() + timeout
    settle(page, 0.5)
    while time.time() < deadline:
        busy = page.evaluate("document.documentElement.classList.contains('shiny-busy')")
        if not busy:
            settle(page, 0.4)
            return
        settle(page, 0.25)
    raise RuntimeError("Shiny stayed busy")


def glide_to(page, locator, pause=0.35):
    if DRY and locator.count() == 0:   # a dry run has no answers to point at
        settle(page, pause)
        return
    locator.scroll_into_view_if_needed()
    settle(page, 0.15)
    box = locator.bounding_box()
    if box:
        page.mouse.move(box["x"] + box["width"] / 2, box["y"] + box["height"] / 2, steps=28)
    settle(page, pause)


def point(page, locator, hold=1.2):
    """Rest the cursor on an element while the narration names it."""
    glide_to(page, locator, pause=hold)


def click_slow(page, locator, pause_after=0.8):
    glide_to(page, locator)
    locator.click(delay=90)
    settle(page, pause_after)


def type_slow(page, locator, text, delay=38):
    click_slow(page, locator, pause_after=0.3)
    page.keyboard.press("Control+A")
    page.keyboard.type(text, delay=delay)
    settle(page, 0.5)


def smooth_scroll(page, total, step=60, tick=0.03):
    remaining = abs(total)
    sign = 1 if total > 0 else -1
    while remaining > 0:
        delta = min(step, remaining)
        page.mouse.wheel(0, sign * delta)
        remaining -= delta
        settle(page, tick)


def scroll_top(page):
    page.evaluate("window.scrollTo({top: 0, behavior: 'smooth'})")
    settle(page, 0.9)


def file_button(page, input_id):
    """The visible button (Browse, Add) of a Shiny file input."""
    return page.locator(f"#{input_id}").locator("xpath=..")


def goto_home(page):
    page.goto(BASE, wait_until="networkidle", timeout=180000)
    page.locator("#sample_textiles").wait_for(state="attached", timeout=180000)
    idle(page)
    page.mouse.move(800, 300, steps=10)


def pick_radio(page, name, value):
    click_slow(page, page.locator(f"label:has(input[name='{name}'][value='{value}'])"))
    idle(page)


def wait_run(page, n, timeout=240):
    """Wait for the end of a run: the page is not running and shows n answers."""
    deadline = time.time() + timeout
    settle(page, 1.0)
    while time.time() < deadline:
        running = page.evaluate("document.body.classList.contains('is-running')")
        tiles = page.locator(".gallery .tile").count()
        rows = page.locator("#table tbody tr").count()
        if not running and max(tiles, rows) >= n:
            idle(page)
            return
        settle(page, 0.5)
    raise RuntimeError("the run never finished")


def paid_click(page, rec, locator, n, label):
    """Click Try on 5 or Run all (a paid run), then note its stats for the record."""
    before = page.locator(".spend .mono").inner_text()
    if DRY:
        point(page, locator, hold=1.0)
        print(f"    DRY: skipped {label}")
        return
    rec.mark(f"{label}_click")
    click_slow(page, locator, pause_after=0.3)
    wait_run(page, n)
    rec.mark(f"{label}_done")
    stats = page.locator(".stats").inner_text().replace("\n", " ")
    after = page.locator(".spend .mono").inner_text()
    RUNS.append({"time_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                 "scenario": rec.out.stem, "run": label, "inputs": n, "stats": stats,
                 "visit_before": before, "visit_after": after})
    print(f"    {label}: {stats} | visit {before} -> {after}")


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------

def sc_home(page, rec):
    """The empty page: the three groups, the two cards, the Help switch."""
    goto_home(page)
    rec.start()
    settle(page, 0.8)
    groups = page.locator(".sidebar .group")
    point(page, groups.nth(0).locator("h2"), 1.0)
    point(page, groups.nth(1).locator("h2"), 1.0)
    point(page, groups.nth(2).locator("h2"), 1.0)
    scroll_top(page)
    point(page, page.locator("#answers_title"), 1.1)
    point(page, page.locator(".code-summary"), 1.1)
    rec.mark("help")
    help_sw = page.locator("label.switch")
    click_slow(page, help_sw, pause_after=1.4)
    click_slow(page, help_sw, pause_after=1.2)
    settle(page, 1.0)


def sc_sample(page, rec):
    """The textile sample: every button, on its saved run (no cost)."""
    goto_home(page)
    rec.start()
    settle(page, 0.8)
    click_slow(page, page.locator("#samples_menu summary"), pause_after=1.0)
    rec.mark("pick_textiles")
    click_slow(page, page.locator("#sample_textiles"), pause_after=0.5)
    idle(page)
    settle(page, 1.0)
    rec.mark("question")
    point(page, page.locator("#source"), 1.0)
    point(page, page.locator("label:has(input[name='qtype'][value='choice'])"), 1.2)
    point(page, page.locator("#instructions"), 1.8)
    point(page, page.locator("#answer_desc_1"), 1.4)
    point(page, page.locator("#answer_desc_2"), 1.4)
    rec.mark("references")
    point(page, page.locator(".ref-row").nth(0), 1.3)
    point(page, page.locator(".ref-row").nth(1), 1.3)
    rec.mark("buttons")
    point(page, page.locator("#try5"), 2.6)
    point(page, page.locator("#run_all"), 2.6)
    rec.mark("run_all_click")
    click_slow(page, page.locator("#run_all"), pause_after=0.3)
    wait_run(page, 46)
    settle(page, 3.0)
    rec.mark("answers")
    point(page, page.locator(".stats"), 3.5)
    point(page, page.locator(".gallery .tile").nth(0), 3.5)
    point(page, page.locator("#target"), 2.5)
    smooth_scroll(page, 700)
    settle(page, 3.5)
    smooth_scroll(page, -700)
    settle(page, 0.5)
    rec.mark("table")
    click_slow(page, page.locator("label:has(input[name='view'][value='table'])"), pause_after=1.0)
    idle(page)
    smooth_scroll(page, 420)
    settle(page, 3.5)
    point(page, page.locator("#download_answers"), 1.6)
    rec.mark("code")
    code = page.locator(".code-summary")
    click_slow(page, code, pause_after=0.8)
    idle(page)
    code.scroll_into_view_if_needed()
    settle(page, 0.6)
    smooth_scroll(page, 300)
    settle(page, 2.0)
    smooth_scroll(page, 360)
    settle(page, 1.8)
    rec.mark("code_buttons")
    point(page, page.locator(".copy-btn"), 1.2)
    point(page, page.locator("#download_code"), 1.2)
    point(page, page.locator("#download_bundle"), 1.4)
    smooth_scroll(page, 500)
    settle(page, 2.0)
    rec.mark("import")
    scroll_top(page)
    point(page, page.locator(".import-btn"), 1.8)
    settle(page, 1.0)


def sc_upload(page, rec):
    """Own images: a Yes / no question, the pipe staple zip, references, runs.

    The marks name the windows of gen_timeline.py (beats v21 to v27).
    """
    goto_home(page)
    rec.start()
    settle(page, 0.6)
    rec.mark("noul")
    pick_radio(page, "qtype", "noul")
    settle(page, 0.8)
    type_slow(page, page.locator("#instructions"), PIPE_INSTRUCTIONS, delay=30)
    settle(page, 0.8)
    rec.mark("desc")
    type_slow(page, page.locator("#answer_desc_1"), PIPE_TRUE, delay=14)
    type_slow(page, page.locator("#answer_desc_2"), PIPE_FALSE, delay=14)
    idle(page)
    settle(page, 1.0)
    rec.mark("zip")
    scroll_top(page)
    point(page, file_button(page, "upload_images"), 1.0)
    page.locator("#upload_images").set_input_files(str(DATA / "pipe_staple.zip"))
    idle(page)
    point(page, page.locator("#run_all"), 3.0)          # Run all 16: the labels are read
    for ans in ("true", "false"):
        files = sorted((DATA / "references" / ans).glob("*.png"))
        point(page, file_button(page, f"ref_{ans}"), 0.8)
        page.locator(f"#ref_{ans}").set_input_files([str(f) for f in files])
        idle(page)
        settle(page, 1.6)
    rec.mark("try5")
    point(page, page.locator("#try5"), 0.8)
    paid_click(page, rec, page.locator("#try5"), 5, "try5")
    scroll_top(page)
    point(page, page.locator(".stats"), 3.2)             # 1.00 AUC, 5 images
    point(page, page.locator(".spend"), 2.6)             # the meter of this visit
    rec.mark("run_all")
    point(page, page.locator("#run_all"), 0.8)
    paid_click(page, rec, page.locator("#run_all"), 16, "run_all")
    scroll_top(page)
    point(page, page.locator(".stats"), 3.0)             # 1.00 AUC, 16 images
    rec.mark("calib")
    tiles = page.locator(".gallery .tile")
    point(page, tiles.nth(0).locator(".p"), 2.2)         # 0.69, the top missing staple
    point(page, tiles.nth(7).locator(".p"), 2.2)         # the lowest missing staple
    smooth_scroll(page, 230)
    point(page, tiles.nth(8).locator(".p"), 2.4)         # the good parts: 0.09 and lower
    settle(page, 1.5)
    rec.mark("code")
    code = page.locator(".code-summary")
    click_slow(page, code, pause_after=0.8)
    idle(page)
    code.scroll_into_view_if_needed()
    settle(page, 0.6)
    smooth_scroll(page, 260)
    settle(page, 3.0)
    smooth_scroll(page, 420)
    settle(page, 2.5)


def sc_text_sample(page, rec):
    """Text: the injury reports sample, on its saved run (no cost)."""
    goto_home(page)
    rec.start()
    settle(page, 0.8)
    pick_radio(page, "mode", "text")
    click_slow(page, page.locator("#samples_menu summary"), pause_after=0.6)
    click_slow(page, page.locator("#sample_injuries"), pause_after=0.3)
    idle(page)
    rec.mark("question")
    point(page, page.locator("#instructions"), 1.2)
    point(page, page.locator(".answer-row").nth(0), 0.8)
    rec.mark("run_all_click")
    click_slow(page, page.locator("#run_all"), pause_after=0.3)
    wait_run(page, 75)
    rec.mark("answers")
    point(page, page.locator(".stats"), 2.5)
    smooth_scroll(page, 400)
    settle(page, 2.5)


def sc_text_score(page, rec):
    """Text: own texts (paste) with a Score question, then a run."""
    goto_home(page)
    rec.start()
    settle(page, 0.8)
    pick_radio(page, "mode", "text")
    settle(page, 0.5)
    rec.mark("paste")
    texts = (DATA / "injury_texts.txt").read_text(encoding="utf-8")
    box = page.locator("#paste_text")
    click_slow(page, box, pause_after=0.3)
    box.fill(texts)
    idle(page)
    settle(page, 1.2)
    point(page, file_button(page, "upload_csv"), 1.2)
    rec.mark("score")
    pick_radio(page, "qtype", "score")
    settle(page, 0.6)
    type_slow(page, page.locator("#instructions"), SCORE_INSTRUCTIONS, delay=30)
    click_slow(page, page.locator("#answer_add"), pause_after=0.5)
    idle(page)
    rec.mark("levels")
    for i, (name, desc) in enumerate(SCORE_LEVELS, start=1):
        type_slow(page, page.locator(f"#answer_name_{i}"), name, delay=60)
        idle(page)
        type_slow(page, page.locator(f"#answer_desc_{i}"), desc, delay=14)
        idle(page)
    settle(page, 0.8)
    rec.mark("run")
    paid_click(page, rec, page.locator("#run_all"), len(texts.strip().split("\n\n")), "run_all")
    settle(page, 1.0)
    rec.mark("answers")
    point(page, page.locator(".stats"), 1.6)
    point(page, page.locator("#target"), 1.6)
    smooth_scroll(page, 380)
    settle(page, 3.5)
    smooth_scroll(page, 300)
    settle(page, 3.0)


SCENARIOS = {
    "home": sc_home,
    "sample": sc_sample,
    "upload": sc_upload,
    "text_sample": sc_text_sample,
    "text_score": sc_text_score,
}


def run(names):
    CAPTURES.mkdir(exist_ok=True)
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        for name in names:
            print(f"--- capturing {name}{' (dry)' if DRY else ''}")
            ctx = browser.new_context(viewport=VIEW, device_scale_factor=SCALE, bypass_csp=True)
            ctx.add_init_script(CURSOR_JS)
            page = ctx.new_page()
            rec = Recorder(page, name)
            try:
                SCENARIOS[name](page, rec)
                settle(page, 1.5)  # post-roll
            finally:
                rec.stop()
                ctx.close()
        browser.close()
    if RUNS:
        log = CAPTURES / "paid_runs.jsonl"
        with open(log, "a", encoding="utf-8") as f:
            for r in RUNS:
                f.write(json.dumps(r) + "\n")
        print(f"paid runs appended to {log.name}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", help="comma-separated scenario names")
    ap.add_argument("--dry", action="store_true", help="skip the paid clicks")
    args = ap.parse_args()
    DRY = args.dry
    wanted = args.only.split(",") if args.only else list(SCENARIOS)
    unknown = set(wanted) - set(SCENARIOS)
    if unknown:
        raise SystemExit(f"unknown scenario(s): {', '.join(sorted(unknown))}")
    run(wanted)
