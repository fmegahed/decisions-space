# Run spec: the question, the references and the inputs of one run.
#
# A question is list(type, instructions, answers). The type is "noul",
# "choice" or "score". The names of answers are the answer names. The values
# are the descriptions ("" for none). For noul the names are "true" and "false".
#
# A spec is list(mode, question, references, inputs, source, name). The inputs
# are a tibble with id, path, text, file and label.

# The smallest and the largest number of answers of a type. Yes / no has the
# two fixed answers true and false.
answer_range <- function(type) ANSWER_RANGE[[type]] %||% c(2, 2)

# Problems of a question, as a character vector (empty when it is valid)
check_question <- function(question) {
  answers <- question$answers
  problems <- character()
  if (question$type %in% names(ANSWER_RANGE)) {
    range <- ANSWER_RANGE[[question$type]]
    if (length(answers) < range[1] || length(answers) > range[2]) {
      problems <- c(problems, stringr::str_glue(
        "A {question$type} question needs {range[1]} to {range[2]} answers."
      ))
    }
  }
  if (nchar(question$instructions) > MAX_INSTRUCTIONS) {
    problems <- c(problems, stringr::str_glue(
      "The instructions can have {MAX_INSTRUCTIONS} characters at most."
    ))
  }
  if (any(nchar(answers) > MAX_DESCRIPTION)) {
    problems <- c(problems, stringr::str_glue(
      "A description can have {MAX_DESCRIPTION} characters at most."
    ))
  }
  keys <- stringr::str_to_lower(stringr::str_squish(names(answers)))
  if (anyDuplicated(keys) > 0 || any(keys == "")) {
    problems <- c(problems, "The answer names must be unique and not empty.")
  }
  as.character(problems)
}

# The question of the decisions package that decide() needs
as_decisions_question <- function(question) {
  answers <- as.list(question$answers)
  switch(question$type,
    noul = decisions::question_noul(
      question$instructions,
      true = if (nzchar(answers$true)) answers$true,
      false = if (nzchar(answers$false)) answers$false
    ),
    choice = decisions::question_choice(question$instructions, unlist(answers)),
    score = decisions::question_score(question$instructions, unlist(answers))
  )
}

# The answer that counts as the target of the metrics
target_answer <- function(question, pick = NULL) {
  answers <- names(question$answers)
  if (question$type == "noul") {
    "true"
  } else if (!is.null(pick) && pick %in% answers) {
    pick
  } else {
    dplyr::last(answers)
  }
}

# One hash for what decides the answers, so that a saved run can be matched
spec_fingerprint <- function(spec) {
  rlang::hash(list(
    spec$mode, spec$question$type, spec$question$instructions,
    spec$question$answers, MODEL,
    purrr::map(spec$references, \(paths) list(basename(paths), file.size(paths)))
  ))
}
