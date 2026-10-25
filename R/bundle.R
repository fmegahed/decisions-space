# Bundle and import. The bundle is one zip file with the code, the setup of
# decisions, the answers and the references of a run. It holds no input image
# of the visitor and no key. Import reads a setup of this app back, with our
# own parser: it never calls decisions::read_setup() and never unserializes.

# The text of the README of a bundle (80% ASD-STE100)
bundle_readme <- function(spec, refs, name, has_data, date) {
  files <- c(
    sprintf("- `%s.R`: the R code of the run.", name),
    sprintf(
      "- `%s-setup.json`: the setup of the run. It holds the model, the question, the references and the image settings. It holds no key.",
      name
    ),
    sprintf("- `%s-answers.csv`: the answers of the run.", name),
    if (nrow(refs) > 0) "- `references/`: the reference images of the run. The code reads them.",
    if (has_data) "- `data.csv`: your texts. The code reads them."
  )
  run <- c(
    "Put the files in one folder. Make this folder the working directory of R.",
    if (spec$mode == "image" && spec$source != "textiles") {
      'Put your images in the folder "images", as the code lists them. This bundle holds no input image.'
    },
    if (spec$mode == "text" && spec$source == "upload" && !has_data) {
      'Save your file as "data.csv" in the same folder.'
    },
    sprintf("Then run `%s.R`.", name)
  )
  paste(c(
    sprintf("# %s", name),
    "",
    "This zip file holds one run of the decisions Space.",
    "",
    "## Files",
    "",
    files,
    "",
    "## Versions",
    "",
    sprintf("- App: %s", APP_VERSION),
    sprintf("- decisions: commit %s", DECISIONS_SHA),
    sprintf("- Model: %s", MODEL),
    sprintf("- Date: %s", format(date)),
    "",
    "## Install",
    "",
    "Install the package decisions in R:",
    "",
    '    pak::pak("fmegahed/decisions")',
    "",
    "## Key",
    "",
    "The code needs your TypeSafe key. Save the key in the environment variable `TYPESAFE_API_KEY`.",
    "For example, add the line `TYPESAFE_API_KEY=` followed by your key to the file `.Renviron` in your home folder. Then restart R.",
    "Never write the key in the script.",
    "",
    "## Run",
    "",
    run,
    "",
    "## Cite",
    "",
    "If you use this run, cite the package and the Space:",
    "",
    "- Megahed, F. M., & Chen, Y.-J. (2026). *decisions: Typed decisions from decision models* (R package version 0.1.0). https://github.com/fmegahed/decisions",
    sprintf(
      "- Megahed, F. M. (2026). *decisions Space* (Version %s) [Web application]. Hugging Face. https://huggingface.co/spaces/fmegahed/decisions",
      APP_VERSION
    )
  ), collapse = "\n")
}

# Write the bundle of a run to `zipfile` and return its path. `table` is the
# answers table of answers_table().
write_bundle <- function(spec, table, zipfile, date = Sys.Date()) {
  name <- run_name(spec)
  refs <- reference_files(spec)
  dir <- tempfile("bundle")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  readr::write_file(paste0(run_code(spec, date), "\n"), file.path(dir, paste0(name, ".R")))
  readr::write_file(answers_csv(table), file.path(dir, paste0(name, "-answers.csv")))
  # The setup holds the references as the prepared images; the key stays out
  references <- if (nrow(refs) > 0) spec$references[unique(refs$answer)]
  setup <- decisions::decision_setup(
    make_decider(), stats::setNames(list(as_decisions_question(spec$question)), name),
    references = references, max_size = IMAGE_SETTINGS$max_size,
    upscale = IMAGE_SETTINGS$upscale
  )
  decisions::write_setup(setup, file.path(dir, paste0(name, "-setup.json")))
  # The references of a sample stay in the package (system.file())
  if (nrow(refs) > 0 && spec$source != "textiles") {
    dest <- file.path(dir, "references", refs$folder, refs$file)
    purrr::walk(unique(dirname(dest)), dir.create, recursive = TRUE)
    file.copy(refs$path, dest)
  }
  # Pasted text goes in as data.csv; the file of a CSV upload stays with the visitor
  has_data <- spec$mode == "text" && spec$source == "upload" &&
    is.null(spec$text_column) && nrow(spec$inputs) > 0
  if (has_data) {
    readr::write_csv(tibble::tibble(text = spec$inputs$text), file.path(dir, "data.csv"))
  }
  readr::write_file(
    bundle_readme(spec, refs, name, has_data, date), file.path(dir, "README.md")
  )
  zipfile <- normalizePath(zipfile, mustWork = FALSE)
  zip::zip(zipfile, files = list.files(dir), root = dir, mode = "mirror")
  zipfile
}

# --- Import ---------------------------------------------------------------

IMPORT_REFUSED <- "This file was not made by this app."

# One text value: a string that is not NA
is_one_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

# The path of the setup file: the file itself, or the `*-setup.json` of a zip
# file, unpacked below a temporary folder. NULL when there is none.
import_setup_path <- function(path) {
  if (!file.exists(path) || file.size(path) > mb_bytes(MAX_ZIP_MB)) return(NULL)
  if (tolower(tools::file_ext(path)) != "zip") return(path)
  entries <- tryCatch(zip::zip_list(path), error = function(e) NULL)
  if (is.null(entries)) return(NULL)
  found <- entries |>
    dplyr::filter(
      endsWith(filename, "-setup.json"), purrr::map_lgl(filename, safe_entry),
      !purrr::map_lgl(filename, is_junk), uncompressed_size <= mb_bytes(MAX_ZIP_MB)
    )
  if (nrow(found) == 0) return(NULL)
  dest <- tempfile("import")
  zip::unzip(path, files = found$filename[1], exdir = dest)
  file.path(dest, found$filename[1])
}

# TRUE when the parsed JSON has the layout and the settings of this app
is_app_setup <- function(json) {
  question <- if (length(json$questions) == 1) json$questions[[1]]
  answers <- question$answers
  settings <- json$image_settings
  is.list(json) &&
    identical(as.numeric(json$version), 1) &&
    is_one_string(json$decider$model) &&
    is.null(json$calibrator) && is.null(json$calibrator_hash) &&
    is_one_string(question$type) && question$type %in% c("noul", "choice", "score") &&
    is_one_string(question$instructions) &&
    length(answers) > 0 && !is.null(names(answers)) &&
    all(purrr::map_lgl(answers, \(x) is.null(x) || is_one_string(x))) &&
    identical(as.numeric(settings$max_size), as.numeric(IMAGE_SETTINGS$max_size)) &&
    identical(settings$upscale, IMAGE_SETTINGS$upscale) &&
    identical(settings$crop, IMAGE_SETTINGS$crop)
}

# Decode the references of the file below a temporary folder, as
# references/<answer>/ref_<i>.jpg. NULL when there is a bad reference.
import_references <- function(json, answers) {
  encoded <- json$references
  if (length(encoded) == 0) return(list(paths = NULL, problems = character()))
  if (!is.list(encoded) || !all(names(encoded) %in% names(answers))) return(NULL)
  folders <- answer_folders(names(answers))
  dir <- tempfile("import-references")
  problems <- character()
  paths <- purrr::imap(encoded, \(images, answer) {
    if (length(images) > MAX_REFERENCES) {
      problems <<- c(problems, sprintf(
        "You can add at most %d references for each answer. The app used the first %d.",
        MAX_REFERENCES, MAX_REFERENCES
      ))
      images <- images[seq_len(MAX_REFERENCES)]
    }
    purrr::imap_chr(unname(images), \(x, i) {
      bytes <- if (is_one_string(x)) tryCatch(jsonlite::base64_dec(x), error = function(e) raw())
      jpeg <- length(bytes) > 3 && all(bytes[1:3] == as.raw(c(0xff, 0xd8, 0xff)))
      if (!jpeg || length(bytes) > mb_bytes(MAX_FILE_MB)) return(NA_character_)
      path <- file.path(dir, "references", folders[[answer]], sprintf("ref_%s.jpg", i))
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      writeBin(bytes, path)
      path
    })
  })
  if (anyNA(unlist(paths))) return(NULL)
  list(paths = paths, problems = problems)
}

# Read a setup of this app: a bundle zip or the setup JSON. Returns the
# question (in the format of spec.R), the references (answer to file paths) and
# the problems. `question` is NULL when the file is refused.
read_import <- function(path) {
  refuse <- function(problems = IMPORT_REFUSED) {
    list(question = NULL, references = NULL, problems = problems)
  }
  setup_path <- import_setup_path(path)
  if (is.null(setup_path)) return(refuse())
  json <- tryCatch(jsonlite::read_json(setup_path), error = function(e) NULL)
  if (is.null(json) || !isTRUE(tryCatch(is_app_setup(json), error = function(e) FALSE))) {
    return(refuse())
  }
  raw <- json$questions[[1]]
  answers <- purrr::map_chr(raw$answers, \(x) x %||% "")
  if (raw$type == "noul" && !setequal(names(answers), c("true", "false"))) return(refuse())
  question <- list(type = raw$type, instructions = raw$instructions, answers = answers)
  problems <- check_question(question)
  if (length(problems) > 0) return(refuse(problems))
  references <- import_references(json, answers)
  if (is.null(references) || (raw$type == "score" && length(references$paths) > 0)) return(refuse())
  model <- json$decider$model
  if (model != MODEL) {
    problems <- c(problems, sprintf("This file uses %s. The app uses %s.", model, MODEL))
  }
  list(
    question = question, references = references$paths,
    problems = c(problems, references$problems)
  )
}
