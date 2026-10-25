# Output builders: the dynamic inputs of the question (answer_rows,
# reference_rows) and the parts of the outputs that the server renders
# (spending meter, stats line, gallery, bundle files). The look is in
# www/theme.css. They use sr(), upload_input() and IMAGE_ACCEPT of R/ui.R,
# which R finds when a builder runs.

# The answers of a question: a name and a description for each answer, with
# buttons to add and remove answers. Yes / no has the fixed names true and
# false, so it has no name inputs and no buttons.
answer_rows <- function(question) {
  answers <- question$answers
  fixed <- question$type == "noul"
  range <- ANSWER_RANGE[[question$type]] %||% c(2, 2)
  rows <- purrr::map(seq_along(answers), \(i) {
    shiny::div(
      class = "answer-row",
      if (fixed) {
        shiny::span(class = "k", names(answers)[i])
      } else {
        shiny::textInput(paste0("answer_name_", i), sr(paste("Name of answer", i)), names(answers)[i])
      },
      shiny::textAreaInput(
        paste0("answer_desc_", i), sr(paste("Description of answer", i)), answers[[i]], rows = 3
      ),
      if (!fixed && length(answers) > range[1]) {
        shiny::actionButton(
          paste0("answer_remove_", i), shiny::span(`aria-hidden` = "true", "\u00d7"),
          class = "btn-quiet btn-icon", `aria-label` = paste("Remove answer", i)
        )
      }
    )
  })
  shiny::tagList(
    shiny::div(
      class = "answer-head", `aria-hidden` = "true",
      shiny::span(if (fixed) "Answer" else "Name"), shiny::span("Description")
    ),
    rows,
    if (!fixed && length(answers) < range[2]) {
      shiny::actionButton("answer_add", "Add answer", class = "btn-quiet sm")
    }
  )
}

# The id of the reference input of each answer: "ref_" and the clean name
ref_input_id <- function(answers) {
  paste0("ref_", janitor::make_clean_names(answers, case = "none"))
}

# One row for each answer: its reference images and a file input. `srcs` is a
# named list of image addresses. A score question takes no references.
reference_rows <- function(question, srcs = list()) {
  if (question$type == "score") return(shiny::tagList())
  answers <- names(question$answers)
  ids <- ref_input_id(answers)
  purrr::map(seq_along(answers), \(i) {
    thumbs <- purrr::imap(srcs[[answers[i]]] %||% character(), \(src, j) {
      shiny::img(src = src, alt = sprintf("Reference %d of %s", j, answers[i]))
    })
    shiny::div(
      class = "ref-row",
      shiny::span(class = "k-plain", answers[i]),
      shiny::div(
        shiny::div(class = "thumbs", thumbs),
        upload_input(
          ids[i], sr(paste("References of", answers[i])), IMAGE_ACCEPT,
          multiple = TRUE, button = "Add"
        )
      )
    )
  }) |>
    shiny::tagList()
}

# The spending meter of the visit
spend_meter_ui <- function(spent, limit = LIMIT_SESSION) {
  shiny::div(
    class = "spend",
    shiny::div(
      class = "spend-row", shiny::span("This visit"),
      shiny::span(class = "mono", sprintf("$%.2f of $%.2f", spent, limit))
    ),
    shiny::div(
      class = "meter", `aria-hidden` = "true",
      shiny::tags$i(style = sprintf("width:%.0f%%", min(100, 100 * spent / limit)))
    )
  )
}

# The stats line of answer_stats(); `note` is a short tag such as "Saved run"
stats_ui <- function(stats, mode, note = NULL) {
  item <- \(value, label) {
    shiny::span(class = "stat", shiny::span(class = "v", value), shiny::span(class = "l", label))
  }
  shiny::div(
    class = "stats",
    if (!is.na(stats$auc)) item(sprintf("%.2f", stats$auc), "AUC"),
    if (!is.na(stats$accuracy)) item(sprintf("%.2f", stats$accuracy), "accuracy"),
    item(stats$n, if (mode == "image") "images" else "texts"),
    item(sprintf("$%.3f", stats$cost), "cost"),
    if (!is.null(note)) shiny::span(class = "chip", note)
  )
}

# The gallery: one tile for each row of answers_table(), with the probability
# of the target as a number and a bar, the answer and the label. `src` holds
# the address of the image of each row.
gallery_ui <- function(table, target, src) {
  tiles <- purrr::map(seq_len(nrow(table)), \(i) {
    row <- table[i, ]
    p <- row$p_target
    caption <- c(
      if (!is.na(row$answer)) paste("answer", row$answer),
      if (!is.na(row$label)) paste("label", row$label)
    )
    shiny::tags$figure(
      class = "tile",
      shiny::img(src = src[i], alt = alt_text(row, target), loading = "lazy"),
      shiny::tags$figcaption(
        if (is.na(p)) {
          shiny::div(class = "t", if (is.na(row$problem)) "No answer" else row$problem)
        } else {
          shiny::div(
            class = "p", sprintf("%.2f", p),
            shiny::span(
              class = "bar", `aria-hidden` = "true",
              shiny::tags$i(style = sprintf("width:%.0f%%", 100 * p))
            )
          )
        },
        shiny::div(class = "t", paste(caption, collapse = " \u00b7 "))
      )
    )
  })
  shiny::div(class = "gallery", tiles)
}

# The files of the bundle, with one line on each. `files` are the names in the
# zip file (a folder ends in "/").
BUNDLE_FILE_TEXT <- c(
  "\\.R$" = "The code above.",
  "-setup\\.json$" = "Model, question, references and image settings. Read it with read_setup().",
  "-answers\\.csv$" = "Each input: answer, probabilities, label, tokens, seconds.",
  "^README\\.md$" = "Versions, date, install, key and citation. Import reopens the setup here.",
  "^references/?$" = "The reference images of the run.",
  "^data\\.csv$" = "Your texts. The code reads them."
)
bundle_files_ui <- function(files, zip_name) {
  describe <- \(f) {
    hit <- purrr::detect_index(names(BUNDLE_FILE_TEXT), \(pattern) stringr::str_detect(f, pattern))
    if (hit == 0) "" else BUNDLE_FILE_TEXT[[hit]]
  }
  shiny::tagList(
    shiny::h3(
      class = "eyebrow", shiny::tags$b(`aria-hidden` = "true", "\u203a"), " Bundle \u00b7 ", zip_name
    ),
    shiny::tags$ul(
      class = "files",
      purrr::map(files, \(f) {
        shiny::tags$li(class = "file", shiny::div(class = "n", f), shiny::div(class = "d", describe(f)))
      })
    )
  )
}
