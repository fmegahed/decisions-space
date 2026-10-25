# Help: the text of the intro, the help lines, the notes, the Uptime tab and
# About. All text is in 80% ASD-STE100: short sentences, active voice, no
# contractions. Numbers and versions come from app_config(), so the text
# cannot drift from the behaviour.

# The keys of help_text()
HELP_KEYS <- c(
  "intro", "data", "question", "references", "references_score", "answers",
  "code", "privacy", "record", "about"
)

# The authors, as the footer and About show them
AUTHORS <- "Fadel M. Megahed (Miami University) \u00b7 Ying-Ju (Tessa) Chen (University of Dayton)"

# Links of the package, the records and the source of the app
LINKS <- list(
  package = "https://github.com/fmegahed/decisions",
  package_record = "https://fmegahed.github.io/decisions/record/",
  project_record = "https://github.com/fmegahed/decision-models-for-industrial-image-qc/tree/main/replication_materials",
  source = "https://github.com/fmegahed/decisions-space",
  license = "https://github.com/fmegahed/decisions-space/blob/main/LICENSE",
  issues = "https://github.com/fmegahed/decisions-space/issues",
  status = "https://github.com/fmegahed/decisions-space/blob/main/status/status.csv"
)

# The installed version of decisions, as "0.1.0"
package_version_text <- function() as.character(utils::packageVersion("decisions"))

# The version of decisions and the short commit, as "0.1.0 @ b25fc9f"
decisions_version <- function() {
  sprintf("%s @ %s", package_version_text(), substr(DECISIONS_SHA, 1, 7))
}

# The help text of `key`, as one string. The text of "about" is Markdown.
help_text <- function(key) {
  key <- rlang::arg_match(key, HELP_KEYS)
  cfg <- app_config()
  switch(key,
    intro = "The app sends your question to Jev, a decision model of TypeSafe. Start from a sample, or use your own images or texts.",
    data = "Your own images: one zip file with one folder for each answer (labelled), or image files (unlabelled). Your own texts: paste them with a blank line between two texts, or use a CSV file.",
    question = sprintf(
      "Give each answer a short name and a description. A choice question has %d to %d answers. A score question has %d to %d levels, from low to high. Yes / no has the answers true and false.",
      cfg$answer_range$choice[1], cfg$answer_range$choice[2],
      cfg$answer_range$score[1], cfg$answer_range$score[2]
    ),
    references = sprintf(
      "Give at most %d example images for each answer. They show the model what to look for.",
      cfg$max_references
    ),
    references_score = "A score question takes no references. References attach only to Yes / no and Choice questions.",
    answers = "The answers show here after a run. A run of an unchanged sample shows its saved run, at no cost.",
    code = "The code repeats this run: the question, the references, the model and one decide() call. Run it in your own R with your TypeSafe key in TYPESAFE_API_KEY. Install the package with pak::pak(\"fmegahed/decisions\"). For your own images, read the comment on paths at the top of the code.",
    privacy = "Your images and texts go to TypeSafe (Jev). Do not upload confidential data.",
    record = "A GitHub workflow opens this app twice a day, as a visitor does. Each visit adds a row to a public file. The visit also keeps the app awake. The table shows the last 60 visits.",
    about = about_text(cfg)
  )
}

# The text of the About tab, as Markdown
about_text <- function(cfg = app_config()) {
  usd <- \(x) paste("USD", format(x, big.mark = ","))
  num <- \(x) format(x, big.mark = ",")
  paste(c(
    "## App",
    "",
    "This app asks Jev, a decision model of TypeSafe, a question about each image or text in a set. For each input, it shows the answer and the probability of each answer. It also gives the R code of the run and a bundle with the setup and the answers. You need no key and no account.",
    "",
    "## Package",
    "",
    sprintf(
      "The app uses the R package [decisions](%s), version %s. The [package record](%s) shows how the package was made and tested. The method comes from the paper \"Beyond CLIP: Calibrated (Few-Shot) Defect Detection With Structured Decision Models\" (in preparation). The [project record](%s) holds its decisions and its data.",
      LINKS$package, package_version_text(), LINKS$package_record, LINKS$project_record
    ),
    "",
    "## Model",
    "",
    sprintf(
      "Each call goes to %s. The app sends each image with a longest side of %d pixels. It shows the raw probabilities of Jev. They are not calibrated. The package can calibrate them.",
      cfg$model, cfg$image_settings$max_size
    ),
    "",
    "## Limits",
    "",
    sprintf("- Each file: %d MB. Each zip file: %d MB.", cfg$max_file_mb, cfg$max_zip_mb),
    sprintf(
      "- Each run: %d images or %d texts. Each text: %s characters.",
      cfg$max_images, cfg$max_texts, num(cfg$max_text_chars)
    ),
    sprintf(
      "- Instructions: %d characters. Each description: %d characters.",
      cfg$max_instructions, cfg$max_description
    ),
    sprintf(
      "- Choice: %d to %d answers. Score: %d to %d levels. References: at most %d for each answer.",
      cfg$answer_range$choice[1], cfg$answer_range$choice[2],
      cfg$answer_range$score[1], cfg$answer_range$score[2], cfg$max_references
    ),
    sprintf(
      "- Cost: %s for each visit, %s each day and %s each month for all visitors together (UTC). When a limit is reached, the samples and the R code still work.",
      usd(cfg$limit_session), usd(cfg$limit_day), usd(cfg$limit_month)
    ),
    sprintf(
      "- The app estimates the cost from the input tokens, at %s for each million tokens.",
      usd(cfg$price_per_million)
    ),
    "",
    "## Privacy",
    "",
    "Your images, texts and references go to TypeSafe (Jev). Do not upload confidential data. The app keeps no input after your session. It stores only the daily totals of calls, tokens and cost. The bundle and the logs hold no key.",
    "",
    "## Versions",
    "",
    sprintf(
      "decisions-space %s (%s) \u00b7 decisions %s \u00b7 %s",
      cfg$version, cfg$date, decisions_version(), cfg$model
    ),
    "",
    "## Cite",
    "",
    sprintf(
      "- Megahed, F. M., & Chen, Y.-J. (2026). *decisions: Typed decisions from decision models* (R package version %s). %s",
      package_version_text(), LINKS$package
    ),
    sprintf(
      "- Megahed, F. M., & Chen, Y.-J. (2026). *decisions Space* (Version %s) [Web application]. Hugging Face. https://huggingface.co/spaces/fmegahed/decisions",
      cfg$version
    ),
    "",
    "## Authors",
    "",
    AUTHORS,
    "",
    "## Source",
    "",
    sprintf(
      "The source code is on [GitHub](%s), under the [MIT license](%s). Report a problem as an [issue](%s).",
      LINKS$source, LINKS$license, LINKS$issues
    )
  ), collapse = "\n")
}
