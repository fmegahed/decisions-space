# Runs: calls to Jev in chunks, the classes of errors, and the cost.
# The raw .error text of decide() can hold a part of a key. It must never be
# shown or logged: the app uses only the category of classify_error().

# The decider of TypeSafe; the key is read only when decide() asks for it
make_decider <- function() {
  decisions::decider_typesafe(MODEL, credentials = \() Sys.getenv(KEY_VAR))
}

# Row numbers 1..n in chunks of `size`, as a list of integer vectors
chunk_ids <- function(n, size = CHUNK_SIZE) {
  split(seq_len(n), ceiling(seq_len(n) / size)) |> unname()
}

# Names of the probability columns of decide(), one for each answer name
pred_column <- function(answers) {
  paste0(".pred_", janitor::make_clean_names(answers, case = "none"))
}

# Columns of a decide() output with NA answers, one row for each input.
# The column names follow decide(): .pred_<cleaned answer name>.
empty_answers <- function(question, n, error = "config") {
  answers <- names(question$answers)
  cols <- stats::setNames(
    rep(list(rep(NA_real_, n)), length(answers)),
    pred_column(answers)
  )
  out <- tibble::tibble(
    .pred_class = factor(rep(NA_character_, n), levels = answers),
    !!!cols
  )
  if (question$type == "score") out$.score <- rep(NA_real_, n)
  out |>
    dplyr::mutate(
      .model = MODEL, .input_tokens = NA_real_, .cost = NA_real_,
      .seconds = NA_real_, .error = error
    )
}

# Run the inputs `rows` of a spec through decide(); one row for each input.
# When decide() aborts (for example with no key), every row gets
# .error = "config" and NA answers, so that the chunks can be bound.
run_chunk <- function(spec, rows, decide_fn = decisions::decide) {
  inputs <- spec$inputs[rows, ]
  data_arg <- if (spec$mode == "image") {
    list(image = inputs$path)
  } else {
    list(text = inputs$text)
  }
  answers <- tryCatch(
    rlang::exec(
      decide_fn, make_decider(), as_decisions_question(spec$question),
      !!!data_arg, references = spec$references,
      max_size = IMAGE_SETTINGS$max_size, upscale = IMAGE_SETTINGS$upscale,
      crop = IMAGE_SETTINGS$crop
    ),
    error = \(e) empty_answers(spec$question, nrow(inputs))
  )
  tibble::as_tibble(answers) |>
    dplyr::mutate(id = inputs$id, .before = 1)
}

# Category of an error text (vectorised): NA when there is no error
classify_error <- function(text) {
  dplyr::case_when(
    is.na(text) ~ NA_character_,
    text == "config" ~ "config",
    stringr::str_detect(text, "^HTTP 402") |
      stringr::str_detect(text, stringr::regex(
        "credit|balance|insufficient|payment", ignore_case = TRUE
      )) ~ "credit",
    stringr::str_detect(text, "^HTTP 40[13]") ~ "auth",
    stringr::str_detect(text, "^HTTP 429") ~ "rate",
    stringr::str_detect(text, "^HTTP 5") |
      stringr::str_detect(text, stringr::regex(
        "timeout|timed out", ignore_case = TRUE
      )) ~ "server",
    stringr::str_detect(text, "^HTTP 4") ~ "input",
    .default = "answer"
  )
}

# The sentence that the visitor reads for a category
error_message <- function(category) {
  unname(c(
    config = "The demo is not available right now.",
    auth = "The demo is not available right now.",
    credit = "The credit of this demo is used up. Try again later.",
    rate = "Too many requests. Try again in a minute.",
    server = "TypeSafe did not answer. Try again.",
    input = "TypeSafe refused this input.",
    answer = "The answer could not be read."
  )[category])
}

# These categories stop all runs for the rest of the day
stops_the_day <- function(category) {
  category %in% c("config", "auth", "credit")
}

# Calls, input tokens and cost (USD) of the answers of a chunk
chunk_usage <- function(answers) {
  tokens <- sum(answers$.input_tokens, na.rm = TRUE)
  list(calls = nrow(answers), tokens = tokens, cost = tokens * PRICE_PER_TOKEN)
}

# Cost (USD) of n inputs, with the guess or with the measured mean of tokens
estimate_cost <- function(spec, n, mean_tokens = NULL) {
  n * (mean_tokens %||% TOKENS_GUESS[[spec$mode]]) * PRICE_PER_TOKEN
}

# The files that a chunk of a spec reads: the images of its rows and every
# reference. decide() aborts when one is missing, so the server checks them first.
chunk_files <- function(spec, rows) {
  c(character(), if (spec$mode == "image") spec$inputs$path[rows], unlist(spec$references, use.names = FALSE))
}

# The reasons not to start a run (empty when it can start). decisions checks the
# question here, in this process: an error inside a chunk counts as a key
# problem, and it stops the runs of the day.
run_problems <- function(spec) {
  if (nrow(spec$inputs) == 0) {
    return(if (spec$mode == "image") "Add images first." else "Add texts first.")
  }
  problems <- check_question(spec$question)
  if (length(problems) > 0) return(problems)
  valid <- tryCatch(!is.null(as_decisions_question(spec$question)), error = \(e) FALSE)
  if (!valid) {
    return(paste(
      "The app cannot use this question.",
      "Write the instructions, and give each answer a different name."
    ))
  }
  character()
}

# One log line for each chunk: the time, the counts, the tokens, the cost and
# the error categories. Never an input, a file name, the question or an
# .error text.
log_chunk <- function(now, n, use, categories) {
  counts <- table(categories[!is.na(categories)])
  errors <- if (length(counts) == 0) "none" else paste(names(counts), counts, collapse = ", ")
  message(sprintf(
    "%s chunk: %d inputs, %d calls, %.0f tokens, USD %.6f, errors: %s",
    format(now, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"), n, use$calls, use$tokens,
    use$cost, errors
  ))
}
