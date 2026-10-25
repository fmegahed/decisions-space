# Answers: the table of a run, its stats, the notes and the CSV.
# The table holds only the category message of an error, never the raw .error.

# The table of answers, sorted by the probability of the target, highest first
answers_table <- function(spec, answers, target) {
  names_ans <- names(spec$question$answers)
  probs <- answers[pred_column(names_ans)] |>
    stats::setNames(paste0("p_", janitor::make_clean_names(names_ans, case = "none")))
  error <- answers[[".error"]] %||% rep(NA_character_, nrow(answers))
  tibble::tibble(
    id = answers$id,
    answer = as.character(answers$.pred_class),
    p_target = answers[[pred_column(target)]],
    probs,
    tokens = answers$.input_tokens,
    seconds = answers$.seconds,
    problem = error_message(classify_error(error))
  ) |>
    dplyr::left_join(
      spec$inputs |> dplyr::select(id, file, text, label),
      by = "id"
    ) |>
    dplyr::mutate(text = stringr::str_trunc(text, 150, ellipsis = "...")) |>
    dplyr::select(
      id, file, text, answer, p_target, dplyr::starts_with("p_"),
      label, tokens, seconds, problem
    ) |>
    dplyr::arrange(dplyr::desc(p_target))
}

# Number of inputs, cost (USD), accuracy and AUC; NA when they do not apply
answer_stats <- function(table, spec, target) {
  names_ans <- names(spec$question$answers)
  labelled <- table |> dplyr::filter(!is.na(label), !is.na(answer))
  accuracy <- if (nrow(labelled) == 0) {
    NA_real_
  } else {
    yardstick::accuracy_vec(
      factor(labelled$label, levels = names_ans),
      factor(labelled$answer, levels = names_ans)
    )
  }
  scored <- labelled |> dplyr::filter(!is.na(p_target))
  auc <- if (length(names_ans) == 2 && dplyr::n_distinct(scored$label) == 2) {
    yardstick::roc_auc_vec(
      factor(scored$label, levels = c(target, setdiff(names_ans, target))),
      scored$p_target, event_level = "first"
    )
  } else {
    NA_real_
  }
  list(
    n = nrow(table),
    cost = sum(table$tokens, na.rm = TRUE) * PRICE_PER_TOKEN,
    accuracy = accuracy, auc = auc
  )
}

# The text for the image of a row, for screen readers
alt_text <- function(row, target) {
  sprintf(
    "%s: answer %s, probability of %s %s",
    row$file, row$answer, target, format(round(row$p_target, 2), nsmall = 2)
  )
}

# What the probability is. The facts of a sample follow only for its saved
# answers and its default target.
help_note <- function(spec, stats, target, saved = FALSE) {
  generic <- sprintf(
    "The probability is the raw probability of Jev for %s. It is not calibrated.",
    target
  )
  facts <- spec$source %in% names(SAMPLE_FACTS_TEXT) && saved &&
    identical(target, target_answer(spec$question))
  if (!facts) return(generic)
  paste(c(generic, SAMPLE_FACTS_TEXT[[spec$source]]), collapse = " ")
}

# The CSV text of the table for the download
answers_csv <- function(table) {
  readr::format_csv(table, na = "")
}
