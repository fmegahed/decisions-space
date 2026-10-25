# Specs of visitors for the tests of code.R and bundle.R, and a stub of
# decide() that records its arguments. No call to Jev is made.

# A tiny JPEG at `path`
tiny_jpg <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  magick::image_blank(8, 8, "red") |> magick::image_write(path, format = "jpeg")
  invisible(path)
}

# The question of the upload tests: quotes and backticks must stay valid R
upload_question <- function() {
  list(
    type = "choice",
    instructions = "Is the part \"ok\"? Look at `inspection_image`.",
    answers = c(nominal = "The part is fine.", defective = "The part is broken.")
  )
}

# A spec of an image upload: loose images, or a labelled zip, with two
# references for each answer (read with read_images() and read_references())
upload_spec <- function(labelled = TRUE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  rel <- c("nominal/a.jpg", "nominal/b.jpg", "defective/c.jpg")
  for (r in rel) tiny_jpg(file.path(dir, r))
  if (labelled) {
    zf <- file.path(withr::local_tempdir(.local_envir = env), "set.zip")
    zip::zip(zf, files = rel, root = dir, mode = "mirror")
    files <- data.frame(name = "set.zip", datapath = zf)
  } else {
    files <- data.frame(name = basename(rel), datapath = file.path(dir, rel))
  }
  inputs <- read_images(files, dir = withr::local_tempdir(.local_envir = env))$inputs
  folder <- textiles_folder()
  refs <- list(
    nominal = c("nominal_textile_0731.jpg", "nominal_textile_0630.jpg"),
    defective = c("local_textile_0471.jpg", "global_textile_0330.jpg")
  ) |>
    purrr::map(\(f) {
      read_references(
        data.frame(name = f, datapath = file.path(folder, f)),
        dir = withr::local_tempdir(.local_envir = env)
      )$paths
    })
  list(
    mode = "image", question = upload_question(), references = refs,
    inputs = inputs, source = "upload", name = "my parts"
  )
}

# A spec of pasted text, or of a CSV file with the text column "review"
text_spec <- function(csv = FALSE, env = parent.frame()) {
  question <- list(
    type = "noul", instructions = "Does the text ask for a refund?",
    answers = c(true = "", false = "")
  )
  if (csv) {
    path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
    writeLines(c("review,tag", "I want my money back,a", "Thanks a lot,b"), path)
    inputs <- read_text_csv(path, "review")$inputs
  } else {
    inputs <- read_text_paste("I want my money back.\n\nThanks a lot.")$inputs
  }
  list(
    mode = "text", question = question, references = NULL, inputs = inputs,
    source = "upload", name = "refunds",
    text_column = if (csv) "review"
  )
}

# Answers for an upload spec: rows of the saved textile run, with the ids of the spec
upload_answers <- function(spec) {
  sample_answers("textiles")[seq_len(nrow(spec$inputs)), ] |>
    dplyr::mutate(id = spec$inputs$id)
}

# Run the code with a stub of decide(); returns the arguments of the call.
# The code runs in the current folder.
run_stub <- function(code) {
  env <- new.env(parent = globalenv())
  env$seen <- NULL
  env$decide <- function(...) {
    env$seen <- list(...)
    tibble::tibble()
  }
  eval(parse(text = code), env)
  env$seen
}
