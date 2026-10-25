# Data of the visitor: images (loose files or zip files), references, pasted
# text and CSV files. Each reader returns a list with `inputs` (the inputs
# tibble of spec.R) and `problems` (short sentences for the visitor). The
# readers never stop on bad data: they skip it and say why.

IMAGE_EXTENSIONS <- c("jpg", "jpeg", "png", "webp")

# Bytes in a limit given in MB
mb_bytes <- function(mb) mb * 1024^2

# TRUE when a file name is an image that Jev can read
is_image_name <- function(x) tolower(tools::file_ext(x)) %in% IMAGE_EXTENSIONS

# TRUE when a zip entry stays inside the folder of the zip: no absolute path,
# no drive letter, no backslash and no `..`
safe_entry <- function(x) {
  !startsWith(x, "/") && !grepl("^[A-Za-z]:", x) && !grepl("\\", x, fixed = TRUE) &&
    !any(strsplit(x, "/", fixed = TRUE)[[1]] == "..")
}

# TRUE for the files that macOS and Windows add to a zip file: a folder
# __MACOSX, a dot file or folder (._a.jpg, .DS_Store), Thumbs.db, desktop.ini
is_junk <- function(x) {
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]
  any(parts == "__MACOSX" | startsWith(parts, ".")) ||
    tolower(utils::tail(parts, 1)) %in% c("thumbs.db", "desktop.ini")
}

# The inputs tibble with no rows
empty_inputs <- function() {
  tibble::tibble(
    id = character(), path = character(), text = character(),
    file = character(), label = character()
  )
}

# Unique ids: a running number joined with the file name, so that the same
# name in two folders gives two ids. Spaces and odd characters become "_".
make_ids <- function(file) {
  clean <- stringr::str_replace_all(file, "[^\\p{L}\\p{N}._-]+", "_")
  sprintf("%03d_%s", seq_along(file), clean)
}

# Unpack the safe image entries of a zip file into the folder `dest`. The
# entries are counted from the listing first: when they are more than
# `budget`, nothing is extracted. An entry over the file limit is never
# extracted. One call to zip::unzip() extracts the rest, below `dest` only.
# Returns the files (`rel`, `path`, `size`), the count of other files, the
# count of images and the problems.
unpack_zip <- function(zipfile, name, dest, budget = MAX_IMAGES) {
  entries <- tryCatch(zip::zip_list(zipfile), error = function(e) NULL)
  if (is.null(entries)) {
    return(list(
      files = NULL, other = 0L, count = 0L,
      problems = sprintf("%s could not be opened. Check that it is a zip file.", name)
    ))
  }
  kept <- entries |>
    dplyr::filter(!endsWith(filename, "/")) |>
    dplyr::filter(purrr::map_lgl(filename, safe_entry)) |>
    dplyr::filter(!purrr::map_lgl(filename, is_junk))
  # A single top folder around the label folders is not a label
  parts <- strsplit(kept$filename, "/", fixed = TRUE)
  tops <- unique(purrr::map_chr(parts, 1))
  rel <- kept$filename
  if (length(tops) == 1 && any(lengths(parts) >= 3)) {
    rel <- stringr::str_remove(rel, stringr::fixed(paste0(tops, "/")))
  }
  kept <- kept |> dplyr::mutate(rel = rel)
  is_img <- is_image_name(kept$filename)
  too_big <- is_img & kept$uncompressed_size > mb_bytes(MAX_FILE_MB)
  use <- kept |> dplyr::filter(is_img, !too_big)
  problems <- sprintf("%s was skipped. A file can be at most %s MB.", basename(kept$rel[too_big]), MAX_FILE_MB)
  out <- list(files = NULL, other = sum(!is_img), count = nrow(use), problems = problems)
  if (nrow(use) == 0 || nrow(use) > budget) return(out)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  zip::unzip(zipfile, files = use$filename, exdir = dest)
  out$files <- use |>
    dplyr::mutate(path = file.path(dest, filename), size = uncompressed_size) |>
    dplyr::select(rel, path, size)
  out
}

# Sentence for the files that are not images
not_image_problem <- function(n) {
  if (n == 0) return(character())
  if (n == 1) return("1 file is not an image and was skipped. Use JPEG, PNG or WebP.")
  sprintf("%d files are not images and were skipped. Use JPEG, PNG or WebP.", n)
}

# Copy a loose upload into `dir` (one subfolder for each upload); returns the new path
keep_upload <- function(path, name, dir, i) {
  dest <- file.path(dir, sprintf("file%03d", i))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(path, file.path(dest, basename(name)))
  file.path(dest, basename(name))
}

# Images from the files of shiny::fileInput() (columns `name`, `datapath`).
# A folder in a zip file gives the label; loose files have no label. Every
# file is written below `dir`, so the caller can delete `dir` at the end of
# the session. The result carries `dir`.
read_images <- function(files, dir = tempfile("upload")) {
  if (is.null(files) || nrow(files) == 0) {
    return(list(inputs = empty_inputs(), problems = character(), dir = dir))
  }
  problems <- character()
  other <- 0L
  loaded <- 0L
  counted <- 0L
  found <- purrr::pmap(list(files$name, files$datapath, seq_len(nrow(files))), \(name, path, i) {
    if (tolower(tools::file_ext(name)) == "zip") {
      if (file.size(path) > mb_bytes(MAX_ZIP_MB)) {
        problems <<- c(problems, sprintf("%s was skipped. A zip file can be at most %s MB.", name, MAX_ZIP_MB))
        return(NULL)
      }
      zipped <- unpack_zip(path, name, file.path(dir, sprintf("zip%03d", i)), MAX_IMAGES - loaded)
      problems <<- c(problems, zipped$problems)
      other <<- other + zipped$other
      counted <<- counted + zipped$count
      loaded <<- loaded + NROW(zipped$files)
      return(zipped$files)
    }
    if (is_junk(name)) return(NULL)
    if (!is_image_name(name)) {
      other <<- other + 1L
      return(NULL)
    }
    if (file.size(path) > mb_bytes(MAX_FILE_MB)) {
      problems <<- c(problems, sprintf("%s was skipped. A file can be at most %s MB.", name, MAX_FILE_MB))
      return(NULL)
    }
    counted <<- counted + 1L
    if (loaded >= MAX_IMAGES) return(NULL)
    loaded <<- loaded + 1L
    tibble::tibble(rel = name, path = keep_upload(path, name, dir, i), size = file.size(path))
  })
  images <- dplyr::bind_rows(found)
  problems <- c(problems, not_image_problem(other))
  if (counted > MAX_IMAGES) {
    problems <- c(problems, sprintf(
      "A run can have at most %d images. This upload has %d. Upload fewer images.",
      MAX_IMAGES, counted
    ))
    images <- images[0, ]
  }
  if (nrow(images) == 0) return(list(inputs = empty_inputs(), problems = problems, dir = dir))
  parts <- strsplit(images$rel, "/", fixed = TRUE)
  file <- purrr::map_chr(parts, utils::tail, 1)
  label <- purrr::map_chr(parts, \(p) if (length(p) >= 2) p[1] else NA_character_)
  inputs <- tibble::tibble(
    id = make_ids(file), path = images$path, text = NA_character_,
    file = file, label = label
  )
  list(inputs = inputs, problems = problems, dir = dir)
}

# Reference images: loose image files, at most MAX_REFERENCES, copied below `dir`
read_references <- function(files, dir = tempfile("upload")) {
  if (is.null(files) || nrow(files) == 0) {
    return(list(paths = character(), problems = character(), dir = dir))
  }
  is_img <- is_image_name(files$name)
  too_big <- is_img & file.size(files$datapath) > mb_bytes(MAX_FILE_MB)
  problems <- c(
    sprintf("%s was skipped. A file can be at most %s MB.", files$name[too_big], MAX_FILE_MB),
    not_image_problem(sum(!is_img))
  )
  use <- which(is_img & !too_big)
  if (length(use) > MAX_REFERENCES) {
    problems <- c(problems, sprintf(
      "You can add at most %d references for each answer. The app used the first %d.",
      MAX_REFERENCES, MAX_REFERENCES
    ))
    use <- use[seq_len(MAX_REFERENCES)]
  }
  paths <- purrr::map_chr(use, \(i) keep_upload(files$datapath[i], files$name[i], dir, i))
  list(paths = paths, problems = problems, dir = dir)
}

# Inputs from texts: skip the texts that are too long, and stop when there
# are too many. `file` is the display name of each text.
texts_to_inputs <- function(text, file, label, problems = character()) {
  long <- stringr::str_length(text) > MAX_TEXT_CHARS
  if (any(long)) {
    problems <- c(problems, sprintf(
      "%s was skipped. A text can have at most %s characters.",
      file[long], format(MAX_TEXT_CHARS, big.mark = ",")
    ))
  }
  keep <- !long
  if (sum(keep) > MAX_TEXTS) {
    problems <- c(problems, sprintf(
      "A run can have at most %d texts. This upload has %d. Use fewer texts.",
      MAX_TEXTS, sum(keep)
    ))
    keep <- rep(FALSE, length(text))
  }
  if (!any(keep)) return(list(inputs = empty_inputs(), problems = problems))
  inputs <- tibble::tibble(
    id = make_ids(file[keep]), path = NA_character_, text = text[keep],
    file = file[keep], label = label[keep]
  )
  list(inputs = inputs, problems = problems)
}

# One input for each paragraph; one or more blank lines separate paragraphs
read_text_paste <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text[1])) text <- ""
  paragraphs <- text[1] |>
    stringr::str_replace_all("\r\n?", "\n") |>
    stringr::str_split("\\n\\s*\\n") |>
    unlist() |>
    stringr::str_trim()
  paragraphs <- paragraphs[paragraphs != ""]
  texts_to_inputs(
    paragraphs, sprintf("Text %d", seq_along(paragraphs)),
    rep(NA_character_, length(paragraphs))
  )
}

# The encoding of a CSV file: UTF-8 when the bytes are valid UTF-8 (a BOM is
# fine), else the best guess of readr, else Latin-1
csv_encoding <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  if (!0x00 %in% bytes && validUTF8(rawToChar(bytes))) return("UTF-8")
  guess <- readr::guess_encoding(path)$encoding
  if (length(guess) == 0 || is.na(guess[1])) "latin1" else guess[1]
}

# Read a CSV file with every column as text; NULL when it cannot be read
read_csv_text <- function(path, n_max = Inf) {
  tryCatch(
    readr::read_csv(
      path, locale = readr::locale(encoding = csv_encoding(path)),
      col_types = readr::cols(.default = "c"), na = character(),
      n_max = n_max, progress = FALSE
    ),
    error = function(e) NULL
  )
}

# The column names of a CSV file
csv_columns <- function(path) {
  if (file.size(path) > mb_bytes(MAX_FILE_MB)) return(character())
  names(read_csv_text(path, n_max = 0))
}

# Inputs from a CSV file: the text column, and optionally the label column.
# Empty texts are dropped with a count; blank labels stay unlabelled.
read_text_csv <- function(path, text_col, label_col = NULL, name = basename(path)) {
  if (file.size(path) > mb_bytes(MAX_FILE_MB)) {
    return(list(
      inputs = empty_inputs(),
      problems = sprintf("%s was skipped. A file can be at most %s MB.", name, MAX_FILE_MB)
    ))
  }
  data <- read_csv_text(path)
  if (is.null(data)) {
    return(list(inputs = empty_inputs(), problems = "The CSV file could not be read."))
  }
  if (!text_col %in% names(data)) {
    return(list(
      inputs = empty_inputs(),
      problems = sprintf("The column %s is not in the file.", text_col)
    ))
  }
  has_label <- !is.null(label_col) && label_col %in% names(data)
  data <- tibble::tibble(
    text = stringr::str_trim(data[[text_col]]),
    label = if (has_label) stringr::str_trim(data[[label_col]]) else NA_character_,
    file = sprintf("Row %d", seq_len(nrow(data)))
  ) |>
    dplyr::mutate(label = dplyr::na_if(label, ""))
  empty <- data$text == ""
  problems <- if (sum(empty) == 1) {
    "1 empty text was dropped"
  } else if (sum(empty) > 1) {
    sprintf("%d empty texts were dropped", sum(empty))
  } else {
    character()
  }
  data <- data[!empty, ]
  texts_to_inputs(data$text, data$file, data$label, problems)
}

# Match the labels to the answer names after str_squish() and str_to_lower().
# A matched label gets the exact answer name; any other label becomes NA (the
# input stays, unlabelled) and is listed once. `answers` is the vector of
# answers (named) or of answer names.
match_labels <- function(inputs, answers) {
  clean <- \(x) stringr::str_to_lower(stringr::str_squish(x))
  answer_names <- if (is.null(names(answers))) answers else names(answers)
  hit <- match(clean(inputs$label), clean(answer_names))
  unmatched <- stringr::str_squish(inputs$label[is.na(hit) & !is.na(inputs$label)])
  unmatched <- unmatched[!duplicated(clean(unmatched))]
  inputs$label <- answer_names[hit]
  list(inputs = inputs, problems = sprintf("%s is not an answer", unmatched))
}
