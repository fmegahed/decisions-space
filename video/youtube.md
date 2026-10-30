# YouTube upload

## Title

decisions: ask a decision model about your images and texts (app demo, R code included)

## Description

```text
decisions is a free app on Hugging Face. It asks a decision model, Jev from TypeSafe, one question about each image or text. It gives you the answers, their probabilities and the R code of the run.

This demo shows images first: a sample that explains the buttons, then your own images (the pipe staple images of our paper). Then it shows text. On the way, it explains the three question types: Yes / no (noul), Choice and Score.

Try the app: https://huggingface.co/spaces/fmegahed/decisions
R package: https://github.com/fmegahed/decisions
App source: https://github.com/fmegahed/decisions-space

Chapters
0:00 What decisions does
0:15 Three question types: Yes / no (noul), Choice, Score
0:38 Images, a sample: the layout and the woven textiles
1:02 The question, the answers and the references
1:15 Try on 5 and Run all
1:27 The answers: AUC, accuracy, tiles and table
1:53 The R code, the bundle and Import
2:10 Your own images: pipe staples from AutoVI
2:24 A Yes / no question and its descriptions
2:41 Upload a zip file and the references
2:52 Try on 5
3:03 Run all: AUC and accuracy of 1
3:15 Raw probabilities, calibration and the R code
3:36 Text: the injury reports sample
3:53 Paste your own texts
4:05 A Score question: low, medium, high
4:21 The Score answers
4:33 Summary
4:44 Try it, and credits

Data
- Woven textiles: simulated with the R package spc4sts (https://github.com/fmegahed/qe_genai, MIT licence).
- Brake assemblies: Carvalho, P., Lafou, M., Durupt, A., Leblanc, A., & Grandvalet, Y. (2024). The Automotive Visual Inspection Dataset (AutoVI): A genuine industrial production dataset for unsupervised anomaly detection [Data set]. Zenodo. https://doi.org/10.5281/zenodo.10459003 (CC BY-NC-SA 4.0)
- Injury reports: OSHA Severe Injury Reports (public domain).

Made by Fadel M. Megahed (Miami University) and Ying-Ju (Tessa) Chen (University of Dayton).
Model: Jev (jev-1.14.0) from TypeSafe. Narration: an AI voice from ElevenLabs. Script, screen capture and edit: Claude Code.
```

## Tags

decision models, R, Shiny, Hugging Face, image classification, text classification, defect detection, quality control, calibration, few-shot learning, AutoVI, TypeSafe

## Subtitles

Upload `captions/demo.srt` as English subtitles (with timing).
