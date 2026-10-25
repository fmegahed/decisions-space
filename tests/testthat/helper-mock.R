# Mock of decisions::decide() for the tests and for the local mock mode of the
# app (SPACE_MOCK=1). It makes no call. It uses only functions of R/ and
# packages, and it has no testthat call at top level.

# The saved answers of a sample with the input (path or text) of each row
sample_lookup <- function(id) {
  spec <- sample_spec(id)
  spec$inputs |>
    dplyr::transmute(id, input = dplyr::coalesce(path, text)) |>
    dplyr::left_join(sample_answers(id), by = "id") |>
    dplyr::select(-id)
}

# A function with the signature of decide(). `lookup` has the column `input`
# and the columns of a decide() output. `environment(f)$calls` counts the calls.
mock_decide <- function(lookup = NULL, error = NULL, abort = FALSE) {
  calls <- 0L
  function(decider, questions, text = NULL, image = NULL, references = NULL,
           max_size = 768, upscale = FALSE, crop = "none", max_active = 10,
           rpm = NULL, path = NULL, ignore_hash = FALSE) {
    calls <<- calls + 1L
    if (abort) cli::cli_abort("The key is empty.")
    inputs <- if (is.null(image)) text else image
    question <- list(
      type = if (inherits(questions, "decisions::question_score")) "score" else "choice",
      answers = questions@answers
    )
    blank <- empty_answers(question, length(inputs), error = error %||% NA_character_)
    if (!is.null(error)) return(blank)
    first <- paste0(".pred_", janitor::make_clean_names(names(question$answers)[1], case = "none"))
    fallback <- blank |>
      dplyr::mutate(
        dplyr::across(dplyr::starts_with(".pred_") & !".pred_class", \(x) 0),
        .pred_class = factor(names(question$answers)[1], levels = names(question$answers)),
        .input_tokens = 0, .cost = 0, .seconds = 0
      )
    fallback[[first]] <- 1
    if (".score" %in% names(fallback)) fallback$.score <- 1
    if (is.null(lookup)) return(fallback)
    found <- match(inputs, lookup$input)
    rows <- lookup[found[!is.na(found)], names(fallback)[names(fallback) %in% names(lookup)]]
    out <- fallback
    for (nm in names(rows)) out[[nm]][!is.na(found)] <- rows[[nm]]
    out
  }
}
