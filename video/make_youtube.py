"""Write youtube.md: the title, the description with timed chapters, and the
tags for the YouTube upload (after gen_timeline.py).

A chapter starts at the segment of its first beat: the start of the beat in
vo_delays.json less LEAD. YouTube shows chapters only when the first is at
0:00, there are at least 3, and each lasts at least 10 seconds; the script
stops if a chapter breaks a rule.

Run from video/:  python make_youtube.py
"""
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
LEAD = 0.4          # as in gen_timeline.py
MIN_CHAPTER = 10    # seconds (YouTube)

TITLE = "decisions: ask a decision model about your images and texts (app demo, R code included)"

CHAPTERS = [  # (first beat, label)
    ("v00_intro", "What decisions does"),
    ("v01_types", "Three question types: Yes / no (noul), Choice, Score"),
    ("v10_card", "Images, a sample: the layout and the woven textiles"),
    ("v13_question", "The question, the answers and the references"),
    ("v14_run", "Try on 5 and Run all"),
    ("v15_answers", "The answers: AUC, accuracy, tiles and table"),
    ("v17_code", "The R code, the bundle and Import"),
    ("v20_card", "Your own images: pipe staples from AutoVI"),
    ("v21_noul", "A Yes / no question and its descriptions"),
    ("v23_zip", "Upload a zip file and the references"),
    ("v24_try", "Try on 5"),
    ("v25_runall", "Run all: AUC and accuracy of 1"),
    ("v26_calib", "Raw probabilities, calibration and the R code"),
    ("v30_card", "Text: the injury reports sample"),
    ("v32_paste", "Paste your own texts"),
    ("v33_score", "A Score question: low, medium, high"),
    ("v34_run", "The Score answers"),
    ("v50_recap", "Summary"),
    ("v51_outro", "Try it, and credits"),
]

INTRO = """\
decisions is a free app on Hugging Face. It asks a decision model, Jev from TypeSafe, one question about each image or text. It gives you the answers, their probabilities and the R code of the run.

This demo shows images first: a sample that explains the buttons, then your own images (the pipe staple images of our paper). Then it shows text. On the way, it explains the three question types: Yes / no (noul), Choice and Score.

Try the app: https://huggingface.co/spaces/fmegahed/decisions
R package: https://github.com/fmegahed/decisions
App source: https://github.com/fmegahed/decisions-space"""

OUTRO = """\
Data
- Woven textiles: simulated with the R package spc4sts (https://github.com/fmegahed/qe_genai, MIT licence).
- Brake assemblies: Carvalho, P., Lafou, M., Durupt, A., Leblanc, A., & Grandvalet, Y. (2024). The Automotive Visual Inspection Dataset (AutoVI): A genuine industrial production dataset for unsupervised anomaly detection [Data set]. Zenodo. https://doi.org/10.5281/zenodo.10459003 (CC BY-NC-SA 4.0)
- Injury reports: OSHA Severe Injury Reports (public domain).

Made by Fadel M. Megahed (Miami University) and Ying-Ju (Tessa) Chen (University of Dayton).
Model: Jev (jev-1.14.0) from TypeSafe. Narration: an AI voice from ElevenLabs. Script, screen capture and edit: Claude Code."""

TAGS = ["decision models", "R", "Shiny", "Hugging Face", "image classification",
        "text classification", "defect detection", "quality control", "calibration",
        "few-shot learning", "AutoVI", "TypeSafe"]


def stamp(t):
    t = int(t)
    return f"{t // 60}:{t % 60:02d}"


def chapters():
    meta = json.loads((HERE / "vo_delays.json").read_text())
    starts = [max(0.0, meta["delays"][beat] - LEAD) for beat, _ in CHAPTERS]
    ends = starts[1:] + [meta["total"]]
    assert int(starts[0]) == 0, "the first chapter must start at 0:00"
    assert len(starts) >= 3, "YouTube needs at least 3 chapters"
    for (beat, label), s, e in zip(CHAPTERS, starts, ends):
        # YouTube reads the stamps in whole seconds
        assert int(e) - int(s) >= MIN_CHAPTER, f"{label} ({beat}) lasts {int(e) - int(s)} s"
    return [f"{stamp(s)} {label}" for (_, label), s in zip(CHAPTERS, starts)]


def main():
    desc = "\n\n".join([INTRO, "Chapters\n" + "\n".join(chapters()), OUTRO])
    assert len(TITLE) <= 100, "YouTube titles hold at most 100 characters"
    assert len(desc) <= 5000 and "<" not in desc and ">" not in desc, "YouTube description rules"
    text = (f"# YouTube upload\n\n## Title\n\n{TITLE}\n\n## Description\n\n```text\n{desc}\n```\n\n"
            f"## Tags\n\n{', '.join(TAGS)}\n\n"
            "## Subtitles\n\nUpload `captions/demo.srt` as English subtitles (with timing).\n")
    (HERE / "youtube.md").write_text(text, encoding="utf-8")
    print(f"wrote youtube.md: title {len(TITLE)} characters, description {len(desc)} characters, "
          f"{len(CHAPTERS)} chapters")


if __name__ == "__main__":
    main()
