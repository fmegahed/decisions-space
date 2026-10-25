unzip_bundle <- function(zipfile, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  zip::unzip(zipfile, exdir = dir)
  dir
}

bundle_of <- function(spec, env = parent.frame()) {
  table <- answers_table(spec, upload_answers(spec), "defective")
  zipfile <- file.path(withr::local_tempdir(.local_envir = env), "run.zip")
  write_bundle(spec, table, zipfile, date = as.Date("2026-11-03"))
}

# The bundle of a text spec: the saved answers are renamed to true and false
bundle_of_text <- function(spec, env = parent.frame()) {
  answers <- upload_answers(spec) |>
    dplyr::rename(.pred_true = .pred_nominal, .pred_false = .pred_defective)
  table <- answers_table(spec, answers, "true")
  zipfile <- file.path(withr::local_tempdir(.local_envir = env), "run.zip")
  write_bundle(spec, table, zipfile)
}

# A zip file with one text file
make_zip_of <- function(rel, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  writeLines("x", file.path(root, rel))
  zf <- file.path(withr::local_tempdir(.local_envir = env), "x.zip")
  zip::zip(zf, files = rel, root = root, mode = "mirror")
  zf
}

# Change the setup file of an unzipped bundle with `change(json)`; returns its path
edit_setup <- function(spec, change, env = parent.frame()) {
  dir <- unzip_bundle(bundle_of(spec, env = env), env = env)
  path <- file.path(dir, "my_parts-setup.json")
  json <- change(jsonlite::read_json(path))
  jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null")
  path
}

NOT_OURS_TEXT <- "This file was not made by this app."

test_that("the bundle of an upload has the files and the references", {
  spec <- upload_spec(labelled = TRUE)
  zipfile <- bundle_of(spec)
  expect_true(file.exists(zipfile))
  entries <- zip::zip_list(zipfile)$filename
  expect_true(all(c("my_parts.R", "my_parts-setup.json", "my_parts-answers.csv", "README.md") %in% entries))
  refs <- stringr::str_subset(entries, "^references/.+\\.jpg$")
  expect_length(refs, 4)
  expect_setequal(dirname(refs), c("references/nominal", "references/defective"))
  expect_false(any(stringr::str_detect(entries, "^images/")))
  dir <- unzip_bundle(zipfile)
  expect_identical(
    readr::read_file(file.path(dir, "my_parts.R")),
    paste0(run_code(spec, date = as.Date("2026-11-03")), "\n")
  )
})

test_that("decisions reads the setup file of the bundle", {
  spec <- upload_spec(labelled = TRUE)
  dir <- unzip_bundle(bundle_of(spec))
  setup <- decisions::read_setup(file.path(dir, "my_parts-setup.json"))
  expect_identical(setup@decider@model, MODEL)
  expect_identical(setup@max_size, 768)
  expect_true(setup@upscale)
  expect_identical(setup@questions[[1]]@instructions, spec$question$instructions)
  expect_identical(unname(lengths(setup@references)), c(2L, 2L))
})

test_that("the bundle holds no key, no Bearer text and no sk- text", {
  withr::local_envvar(HF_JEV_API = "fake-key-1234567890")
  spec <- upload_spec(labelled = TRUE)
  dir <- unzip_bundle(bundle_of(spec))
  texts <- list.files(dir, pattern = "\\.(R|json|csv|md)$", recursive = TRUE, full.names = TRUE)
  expect_gte(length(texts), 4)
  for (f in texts) {
    content <- readr::read_file(f)
    expect_false(stringr::str_detect(content, "HF_JEV_API|Bearer|sk-"), info = f)
    expect_false(stringr::str_detect(content, stringr::fixed("fake-key-1234567890")), info = f)
  }
})

test_that("the README states the versions, the install, the key and the citation", {
  spec <- upload_spec(labelled = TRUE)
  dir <- unzip_bundle(bundle_of(spec))
  readme <- readr::read_file(file.path(dir, "README.md"))
  for (x in c(APP_VERSION, DECISIONS_SHA, MODEL, "2026-11-03", 'pak::pak("fmegahed/decisions")', "TYPESAFE_API_KEY", "cite")) {
    expect_true(stringr::str_detect(readme, stringr::fixed(x)), info = x)
  }
  expect_false(stringr::str_detect(readme, "n't\\b"))
})

test_that("the bundle of a sample has no references folder", {
  spec <- sample_spec("textiles")
  table <- answers_table(spec, sample_answers("textiles"), "defective")
  zipfile <- file.path(withr::local_tempdir(), "t.zip")
  write_bundle(spec, table, zipfile)
  entries <- zip::zip_list(zipfile)$filename
  expect_setequal(entries, c("textiles.R", "textiles-setup.json", "textiles-answers.csv", "README.md"))
  setup <- decisions::read_setup(file.path(unzip_bundle(zipfile), "textiles-setup.json"))
  expect_identical(unname(lengths(setup@references)), c(2L, 2L))
})

test_that("the bundle of pasted text holds data.csv, and a CSV upload does not", {
  paste_zip <- bundle_of_text(text_spec(csv = FALSE))
  expect_true("data.csv" %in% zip::zip_list(paste_zip)$filename)
  data <- utils::read.csv(file.path(unzip_bundle(paste_zip), "data.csv"))
  expect_identical(names(data), "text")
  expect_identical(data$text, c("I want my money back.", "Thanks a lot."))
  csv_zip <- bundle_of_text(text_spec(csv = TRUE))
  expect_false("data.csv" %in% zip::zip_list(csv_zip)$filename)
})

test_that("read_import() restores the question and two references per answer", {
  spec <- upload_spec(labelled = TRUE)
  zipfile <- bundle_of(spec)
  imp <- read_import(zipfile)
  expect_identical(imp$problems, character())
  expect_identical(imp$question$type, "choice")
  expect_identical(imp$question$instructions, spec$question$instructions)
  expect_identical(imp$question$answers, spec$question$answers)
  expect_named(imp$references, c("nominal", "defective"))
  expect_true(all(lengths(imp$references) == 2))
  paths <- unlist(imp$references)
  expect_true(all(file.exists(paths)))
  expect_true(all(stringr::str_detect(paths, "references/(nominal|defective)/ref_[12]\\.jpg$")))
  expect_true(all(magick::image_info(magick::image_read(paths))$format == "JPEG"))
  # the JSON file alone works too
  alone <- read_import(file.path(unzip_bundle(zipfile), "my_parts-setup.json"))
  expect_identical(alone$question$answers, spec$question$answers)
  expect_true(all(lengths(alone$references) == 2))
})

test_that("an import gives a spec that makes valid code", {
  imp <- read_import(bundle_of(upload_spec(labelled = FALSE)))
  spec <- list(
    mode = "image", question = imp$question, references = imp$references,
    inputs = empty_inputs(), source = "import", name = "import"
  )
  expect_no_error(parse(text = run_code(spec)))
})

test_that("a file with a calibrator is refused, and nothing is unserialized", {
  path <- edit_setup(upload_spec(labelled = TRUE), \(json) {
    json$calibrator <- "abcd"
    json
  })
  local_no_unserialize()
  imp <- read_import(path)
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$question)
})

test_that("the six-question setup of decisions is refused", {
  local_no_unserialize()
  path <- system.file("extdata", "rubric_setup.json", package = "decisions")
  imp <- read_import(path)
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$question)
})

test_that("a file that is not a setup is refused", {
  local_no_unserialize()
  path <- withr::local_tempfile(fileext = ".json")
  writeLines("not json {", path)
  expect_identical(read_import(path)$problems, NOT_OURS_TEXT)
  expect_identical(read_import(make_zip_of("a.txt"))$problems, NOT_OURS_TEXT)
})

test_that("other image settings are refused", {
  local_no_unserialize()
  path <- edit_setup(upload_spec(labelled = TRUE), \(json) {
    json$image_settings$max_size <- 512
    json
  })
  expect_identical(read_import(path)$problems, NOT_OURS_TEXT)
})

test_that("another model gives the problem and keeps the question", {
  local_no_unserialize()
  spec <- upload_spec(labelled = TRUE)
  path <- edit_setup(spec, \(json) {
    json$decider$model <- "jev-1.0.0"
    json
  })
  imp <- read_import(path)
  expect_identical(imp$problems, sprintf("This file uses jev-1.0.0. The app uses %s.", MODEL))
  expect_identical(imp$question$instructions, spec$question$instructions)
})

test_that("a reference that is not a JPEG is refused", {
  local_no_unserialize()
  path <- edit_setup(upload_spec(labelled = TRUE), \(json) {
    json$references$nominal[[1]] <- jsonlite::base64_enc(charToRaw("not an image"))
    json
  })
  expect_identical(read_import(path)$problems, NOT_OURS_TEXT)
})

# A setup file of the bundle with a change, for the refusal tests
refused_after <- function(change) {
  path <- edit_setup(upload_spec(labelled = FALSE), change, env = parent.frame())
  read_import(path)
}

test_that("a file with only calibrator_hash set is refused", {
  local_no_unserialize()
  imp <- refused_after(\(json) {
    json$calibrator_hash <- "abcd"
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$question)
})

test_that("unnamed references are refused", {
  local_no_unserialize()
  imp <- refused_after(\(json) {
    json$references <- unname(json$references)
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$references)
})

test_that("references for an answer that does not exist are refused", {
  local_no_unserialize()
  imp <- refused_after(\(json) {
    names(json$references) <- c("nominal", "other")
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
})

test_that("a reference over the file limit is refused", {
  local_no_unserialize()
  big <- c(as.raw(c(0xff, 0xd8, 0xff)), raw(mb_bytes(MAX_FILE_MB) + 1))
  imp <- refused_after(\(json) {
    json$references$nominal[[1]] <- jsonlite::base64_enc(big)
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
})

test_that("more references than the limit for one answer are refused", {
  local_no_unserialize()
  imp <- refused_after(\(json) {
    json$references$nominal <- rep(json$references$nominal, MAX_REFERENCES + 1)
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$question)
})

test_that("a score question with references is refused", {
  local_no_unserialize()
  imp <- refused_after(\(json) {
    json$questions[[1]]$type <- "score"
    json
  })
  expect_identical(imp$problems, NOT_OURS_TEXT)
  expect_null(imp$question)
})

test_that("an entry with ../ in a zip file is skipped and nothing is written outside", {
  local_no_unserialize()
  dir <- unzip_bundle(bundle_of(upload_spec(labelled = FALSE)))
  # Name the entry "xx/evil-setup.json", then turn "xx/" into "../" in the bytes
  file.copy(file.path(dir, "my_parts-setup.json"), file.path(dir, "evil-setup.json"))
  dir.create(file.path(dir, "xx"))
  file.rename(file.path(dir, "evil-setup.json"), file.path(dir, "xx", "evil-setup.json"))
  zf <- file.path(withr::local_tempdir(), "evil.zip")
  zip::zip(zf, files = "xx/evil-setup.json", root = dir, mode = "mirror")
  bytes <- readBin(zf, "raw", file.size(zf))
  from <- charToRaw("xx/evil")
  at <- which(purrr::map_lgl(seq_along(bytes), \(i) identical(bytes[i + seq_along(from) - 1], from)))
  expect_length(at, 2)
  for (i in at) bytes[i:(i + 2)] <- charToRaw("../")
  writeBin(bytes, zf)
  expect_identical(read_import(zf)$problems, NOT_OURS_TEXT)
  expect_false(file.exists(file.path(tempdir(), "evil-setup.json")))
  # a valid setup next to the unsafe entry still imports
  zip::zip_append(zf, files = "my_parts-setup.json", root = dir, mode = "mirror")
  expect_identical(read_import(zf)$problems, character())
  expect_false(file.exists(file.path(tempdir(), "evil-setup.json")))
})

test_that("an answer name with characters that Windows forbids imports and cleans the folder", {
  local_no_unserialize()
  odd <- "a*b?\"c<d>e|f"
  expect_identical(unname(answer_folders(odd)), "a_b__c_d_e_f")
  spec <- upload_spec(labelled = FALSE)
  names(spec$question$answers) <- c("nominal", odd)
  names(spec$references) <- c("nominal", odd)
  table <- answers_table(spec, upload_answers(spec) |> dplyr::rename(".pred_a_b_c_d_e_f" = ".pred_defective"), "nominal")
  zipfile <- file.path(withr::local_tempdir(), "run.zip")
  write_bundle(spec, table, zipfile)
  expect_true(any(stringr::str_detect(zip::zip_list(zipfile)$filename, "^references/a_b__c_d_e_f/")))
  imp <- read_import(zipfile)
  expect_identical(imp$problems, character())
  expect_named(imp$references, c("nominal", odd))
  expect_true(all(file.exists(unlist(imp$references))))
  expect_true(all(stringr::str_detect(unlist(imp$references), "/references/(nominal|a_b__c_d_e_f)/ref_")))
})

test_that("read_import() never stops on a bad file", {
  local_no_unserialize()
  empty <- withr::local_tempfile(fileext = ".json")
  file.create(empty)
  for (path in c(empty, file.path(tempdir(), "nothing.zip"), tempdir())) {
    expect_no_error(imp <- read_import(path))
    expect_identical(imp$problems, NOT_OURS_TEXT)
  }
})
