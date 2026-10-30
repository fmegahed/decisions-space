"""Prepare the demo data of the video (D283): the pipe staple zip, its
references and the texts of the Score example.

Images come only from the split reference_pool of the paper project
(results/image_split_manifest.csv), so no test image goes to Jev again. The
four references are the references of the paper
(results/reference_manifest_seed202610_by_dataset.csv).

Run from video/:  python prep_data.py
Writes assets/data/ (not tracked) and data_manifest.csv (tracked).
"""
import csv
import random
import shutil
import zipfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
PROJECT = HERE.parents[1] / "fine_tuned_jev"   # the paper project, next to this repository
OUT = HERE / "assets" / "data"
SEED = 2026
PER_CLASS = 8

# Folder of each AutoVI class in the zip: a Yes / no question has the answers
# true (the staple is missing) and false (the staple is there)
FOLDER = {"missing": "true", "good": "false"}

# The texts of the Score example: six short reports of decisions::ohio_injuries
TEXTS = [
    "An employee was walking when she slipped and fell to the ground, fracturing her left wrist.",
    "A temporary employee was walking near a meat production line when they slipped on the wet floor and fell. The employee sustained a concussion and was hospitalized.",
    "An employee fell onto a hot billet and suffered a burn to the left leg. The employee was hospitalized.",
    "An employee was operating a wax injection die. The die's metal lid (which weighed about 125 pounds) fell on his hand. He suffered crush injuries to the index and middle fingers that required surgery.",
    "On March 31, 2025, at 10:45, an employee was working on a conveyor when their right arm was amputated at the elbow.",
    "On February 9, 2025, an employee's hand was caught in an auger conveyor, resulting in the amputation of four fingers.",
]
TEXT_IDS = ["2025010827", "2025032474", "20251010301", "2025010244", "2025033012", "2025021265"]


def main():
    shutil.rmtree(OUT, ignore_errors=True)
    (OUT / "references" / "true").mkdir(parents=True)
    (OUT / "references" / "false").mkdir(parents=True)

    with open(PROJECT / "results" / "reference_manifest_seed202610_by_dataset.csv") as f:
        refs = [r for r in csv.DictReader(f) if r["dataset"] == "pipe_staple"]
    ref_paths = {r["image_path"] for r in refs}

    with open(PROJECT / "results" / "image_split_manifest.csv") as f:
        pool = [r for r in csv.DictReader(f)
                if r["dataset"] == "pipe_staple" and r["split"] == "reference_pool"
                and r["image_path"] not in ref_paths]

    rows = []
    rng = random.Random(SEED)
    for kind in ("missing", "good"):
        cands = sorted((r for r in pool if r["defect_type"] == kind), key=lambda r: r["image_path"])
        for r in rng.sample(cands, PER_CLASS):
            rows.append({"role": "input", "folder": FOLDER[kind],
                         "name": f"{kind}_{r['image_name']}", "source": r["image_path"]})
    for r in refs:
        kind = "missing" if "/missing/" in r["image_path"] else "good"
        name = f"{kind}_{Path(r['image_path']).name}"
        shutil.copy(PROJECT / r["image_path"], OUT / "references" / FOLDER[kind] / name)
        rows.append({"role": "reference", "folder": FOLDER[kind], "name": name,
                     "source": r["image_path"]})

    with zipfile.ZipFile(OUT / "pipe_staple.zip", "w", zipfile.ZIP_DEFLATED) as z:
        for r in rows:
            if r["role"] == "input":
                z.write(PROJECT / r["source"], f"{r['folder']}/{r['name']}")

    (OUT / "injury_texts.txt").write_text("\n\n".join(TEXTS) + "\n", encoding="utf-8")
    for i, t in zip(TEXT_IDS, TEXTS):
        rows.append({"role": "text", "folder": "-", "name": i, "source": "decisions::ohio_injuries"})

    with open(HERE / "data_manifest.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["role", "folder", "name", "source"])
        w.writeheader()
        w.writerows(rows)
    print(f"{sum(r['role'] == 'input' for r in rows)} inputs, "
          f"{sum(r['role'] == 'reference' for r in rows)} references, {len(TEXTS)} texts")
    print("zip:", (OUT / "pipe_staple.zip").stat().st_size // 1024, "KB")


if __name__ == "__main__":
    main()
