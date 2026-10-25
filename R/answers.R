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

# What the probability is. A saved run of a sample adds that the app reuses it.
# The facts of a sample describe its full saved run: they follow only for all
# its saved answers (`full`, not Try on 5) and its default target.
help_note <- function(spec, stats, target, saved = FALSE, full = TRUE) {
  generic <- sprintf(
    "The probability is the raw probability of Jev for %s. It is not calibrated.",
    target
  )
  saved <- spec$source %in% names(SAMPLE_FACTS_TEXT) && saved
  if (!saved) return(generic)
  facts <- if (full && identical(target, target_answer(spec$question))) {
    SAMPLE_FACTS_TEXT[[spec$source]]
  }
  paste(c(generic, facts, SAVED_RUN_TEXT), collapse = " ")
}

# The CSV text of the table for the download
answers_csv <- function(table) {
  readr::format_csv(table, na = "")
}

# The answers belong to one setup and one set of inputs. The ids alone repeat
# ("Text 1", "Row 1", "001_a.jpg"), so the key holds the paths and the texts too.
answers_key <- function(spec) {
  content <- spec$inputs[intersect(c("id", "path", "text"), names(spec$inputs))]
  rlang::hash(list(spec_fingerprint(spec), content))
}

# The answers table as the page shows it: the input, the answer, the
# probability of each answer, the label, the tokens, the seconds, the problem
table_view <- function(table, spec) {
  answers <- names(spec$question$answers)
  probs <- table[paste0("p_", janitor::make_clean_names(answers, case = "none"))] |>
    stats::setNames(paste("p", answers))
  shown <- tibble::tibble(Input = dplyr::coalesce(table$text, table$file), Answer = table$answer) |>
    dplyr::bind_cols(probs) |>
    dplyr::mutate(
      Label = table$label, Tokens = as.integer(table$tokens), Seconds = table$seconds,
      Problem = table$problem
    )
  empty <- c(Label = all(is.na(table$label)), Problem = all(is.na(table$problem)))
  shown |> dplyr::select(-dplyr::any_of(names(empty)[empty]))
}

# The answers table of a spec with no answers (for a download before a run)
empty_table <- function(spec) {
  answers <- empty_answers(spec$question, 0) |> dplyr::mutate(id = character(), .before = 1)
  answers_table(spec, answers, target_answer(spec$question))
}
