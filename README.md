---
title: decisions
emoji: 🔎
colorFrom: red
colorTo: yellow
sdk: docker
app_port: 7860
pinned: false
license: mit
short_description: Ask Jev a question about each image or text in a set.
---

# decisions

## App

This app asks Jev a question about each image or text in a set. Jev is a decision model of TypeSafe. For each input, the app shows the answer and the probability of each answer. It also gives the R code of the run and a bundle with the setup and the answers. A visitor needs no key and no account.

The app uses the R package [decisions](https://github.com/fmegahed/decisions) (version 0.1.0). Two samples run at no cost: textile images and injury reports. Their answers are saved.

Open the app: <https://huggingface.co/spaces/fmegahed/decisions>

## Privacy

Your images, texts and references go to TypeSafe (Jev). Do not upload confidential data. The app keeps no input after your session. It stores only the daily totals of calls, tokens and cost. The bundle and the logs hold no key.

## Limits

The demo pays for every call. The limits protect its credit.

- Each session can spend USD 0.25. Each day (UTC) can spend USD 2. Each month (UTC) can spend USD 20.
- A file can have 5 MB. A zip file can have 50 MB.
- A run can have 100 images or 200 texts.
- The app sends 10 inputs in each chunk.
- Jev costs USD 0.042 for each million input tokens.

Edit `R/config.R` to change these values. The help text of the app reads them from this file.

## Docker

The Space runs this Docker image. You can build it and run it yourself.

```sh
docker build -t decisions-space .
docker run -p 7860:7860 -e HF_JEV_API=... decisions-space
```

Then open <http://localhost:7860>. Replace `...` with a TypeSafe key.

- `HF_JEV_API` is the TypeSafe key. Without it, the app opens, but a run shows "The demo is not available right now." A run of an unchanged sample still shows its saved answers.
- `HF_USAGE_TOKEN` is a Hugging Face token that can write to the private dataset `fmegahed/decisions-space-usage`. The app keeps its daily totals there. Without it, the app cannot reach the dataset. It then keeps the totals in memory, writes a warning to the log, and loses the totals when the app restarts.

The app never writes a key to a file or to the log.

## Layout

- `app.R`: starts the app.
- `R/`: the app code. `config.R` holds the model, the limits and the links.
- `www/`: the style, the script, the logo and the fonts.
- `Dockerfile`: the image of the Space.
- `tests/`: the tests. `tests/live_smoke.R` makes two paid calls and is not part of the suite.
- `status/status.csv`: one row for each visit of the status workflow.
- `.github/workflows/`: `tests.yml` runs the tests and builds the image. `deploy.yml` uploads the app files to the Space. `status.yml` visits the app twice a day.

## Tests

Run the tests from the repository root:

```sh
Rscript tests/testthat.R
```

The tests use a mock in place of Jev, so they need no key and make no paid call. Set `SPACE_MOCK=1` to start the app with the same mock, for example `SPACE_MOCK=1 Rscript -e 'shiny::runApp()'`. This mode is for local checks only. The Dockerfile never sets it.

The accessibility test needs Chrome (shinytest2 and axe-core). If chromote does not find Chrome, set the variable `CHROMOTE_CHROME` to the path of Chrome.

## License

MIT. See `LICENSE`.

## Authors

Fadel M. Megahed (Miami University) and Ying-Ju (Tessa) Chen (University of Dayton).

## Citation

If you use the package or the app, cite both:

- Megahed, F. M., & Chen, Y.-J. (2026). *decisions: Typed decisions from decision models* (R package version 0.1.0). https://github.com/fmegahed/decisions
- Megahed, F. M., & Chen, Y.-J. (2026). *decisions Space* (Version 1.0.0) [Web application]. Hugging Face. https://huggingface.co/spaces/fmegahed/decisions
