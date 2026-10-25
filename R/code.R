# R code of a run: the text that the visitor copies, downloads or finds in the
# bundle. It uses only functions of decisions and base R (dplyr only for the
# subset of the injuries sample). Every string of the visitor goes through
# encodeString(), so quotes, backticks and new lines stay valid R.

# An R string with double quotes
r_string <- function(x) encodeString(x, quote = '"')

# A name for the left side of `name = value`: bare when R allows it, else quoted
r_name <- function(x) ifelse(make.names(x) == x, x, r_string(x))

# Folder names for the answers: a name with one of `/ \ : * ? " < > |`, a
# control character or a leading dot is cleaned, and the clean names stay
# unique. The names of the result are the answers.
answer_folders <- function(answers) {
  folder <- answers |>
    stringr::str_replace_all('[/\\\\:*?"<>|[:cntrl:]]', "_") |>
    stringr::str_remove("^\\.+") |>
    stringr::str_squish()
  folder[folder == ""] <- "answer"
  stats::setNames(make.unique(folder, sep = "_"), answers)
}

# The reference files of a spec: one row for each file, with its answer, the
# folder of the answer, the source path and the file name in the folder.
# A score question has none (Jev attaches references to noul and choice only).
reference_files <- function(spec) {
  empty <- tibble::tibble(
    answer = character(), folder = character(), path = character(), file = character()
  )
  if (is.null(spec$references) || spec$question$type == "score") return(empty)
  folders <- answer_folders(names(spec$question$answers))
  purrr::imap(spec$references, \(paths, answer) {
    file <- basename(paths)
    twice <- duplicated(file) | duplicated(file, fromLast = TRUE)
    file[twice] <- paste0(seq_along(file)[twice], "_", file[twice])
    tibble::tibble(answer = answer, folder = folders[[answer]], path = paths, file = file)
  }) |>
    purrr::list_rbind()
}

# The name of a run for file names: "my parts" gives "my_parts"
run_name <- function(spec) {
  name <- janitor::make_clean_names(spec$name, case = "snake")
  if (name == "") "run" else name
}

# The variable that holds the question in the code
question_var <- function(spec) {
  switch(spec$source, textiles = "weave", injuries = "injury", "question")
}

# Strings in lines of at most `width` characters, with a comma after each
# string but the last. Used for long lists of files.
pack_lines <- function(items, indent = "  ", width = 80) {
  lines <- character()
  line <- ""
  for (i in seq_along(items)) {
    piece <- if (i < length(items)) paste0(items[i], ",") else items[i]
    if (nzchar(line) && nchar(indent) + nchar(line) + 1 + nchar(piece) > width) {
      lines <- c(lines, paste0(indent, line))
      line <- piece
    } else {
      line <- stringr::str_trim(paste(line, piece))
    }
  }
  c(lines, paste0(indent, line))
}

# c("a", "b") as code: on one line when short, else in packed lines
r_vector <- function(strings, indent = "") {
  one <- paste0("c(", paste(r_string(strings), collapse = ", "), ")")
  if (nchar(indent) + nchar(one) <= 80) return(one)
  lines <- pack_lines(r_string(strings), paste0(indent, "  "))
  paste0("c(\n", paste(lines, collapse = "\n"), "\n", indent, ")")
}

# The six lines of the comment on paths
paths_comment <- function(spec, refs) {
  folders <- if (nrow(refs) > 0) {
    'the folder "images" and the folder "references"'
  } else {
    'the folder "images"'
  }
  working_directory <- c(
    "working directory (RStudio: Session > Set Working Directory > To Source",
    "File Location). getwd() shows where R looks."
  )
  lines <- if (spec$mode == "text" && spec$source == "injuries") {
    c(
      "Paths. This run used the sample texts of the package: the dataset",
      "ohio_injuries comes with decisions, so no path needs a change.",
      'Your own texts use a file next to this script: "data.csv".',
      'Put this script and the file "data.csv" in one folder, and make it the',
      working_directory
    )
  } else if (spec$mode == "text") {
    column <- if (is.null(spec$text_column)) {
      'The bundle holds your texts there, in the column "text".'
    } else {
      sprintf(
        'Name your file "data.csv". The texts are in the column %s.',
        r_string(spec$text_column)
      )
    }
    c(
      "Paths. This run used your own texts. The code reads them from the file",
      '"data.csv" next to this script.',
      column,
      'Put this script and the file "data.csv" in one folder, and make it the',
      working_directory
    )
  } else if (spec$source == "textiles") {
    c(
      "Paths. This run used the sample images of the package: system.file()",
      "finds them on any computer, so no path needs a change.",
      'Your own images use relative paths, such as "images/part_001.jpg".',
      sprintf("Put this script, %s in one", folders),
      "folder, and make it the working directory (RStudio: Session > Set Working",
      "Directory > To Source File Location). getwd() shows where R looks."
    )
  } else {
    c(
      "Paths. This run used your own images. The code reads them with relative",
      'paths, such as "images/part_001.jpg". The bundle holds no input image.',
      sprintf("Put this script, %s in one", folders),
      "folder, and make it the working directory (RStudio: Session > Set Working",
      "Directory > To Source File Location). getwd() shows where R looks.",
      'Put your images in the folder "images", as the code below lists them.'
    )
  }
  paste0("# ", lines, collapse = "\n")
}

# The definition of `references` from relative paths (empty without references)
references_code <- function(refs) {
  if (nrow(refs) == 0) return("")
  by_answer <- refs |>
    dplyr::summarise(
      code = paste0(
        "  ", r_name(dplyr::first(answer)), " = ",
        r_vector(paste0("references/", dplyr::first(folder), "/", file), indent = "  ")
      ),
      .by = answer
    )
  paste0("\nreferences <- list(\n", paste(by_answer$code, collapse = ",\n"), "\n)")
}

# The lines that define `texts` (or `injuries`)
text_code <- function(spec) {
  if (spec$source == "injuries") {
    return(paste(
      "set.seed(2026)",
      "# This line needs the package dplyr.",
      "injuries <- ohio_injuries |>",
      "  dplyr::slice_sample(n = 20, by = collapsed_nature_title)",
      sep = "\n"
    ))
  }
  if (is.null(spec$text_column)) {
    return('texts <- read.csv("data.csv")$text')
  }
  # read.csv() changes odd column names unless check.names is FALSE
  read <- if (make.names(spec$text_column) == spec$text_column) {
    'read.csv("data.csv")'
  } else {
    'read.csv("data.csv", check.names = FALSE)'
  }
  paste0("texts <- ", read, "[[", r_string(spec$text_column), "]]")
}

# The lines that define `images` and `references`
inputs_code <- function(spec, refs) {
  if (spec$mode == "text") return(text_code(spec))
  if (spec$source == "textiles") {
    by_answer <- refs |>
      dplyr::summarise(
        code = paste0(
          "  ", r_name(dplyr::first(answer)), " = file.path(folder, ",
          r_vector(file, indent = "  "), ")"
        ),
        .by = answer
      )
    return(paste0(
      'folder <- system.file("extdata", "textiles", package = "decisions")\n',
      'images <- file.path(folder, textiles$file[textiles$role == "input"])\n',
      "references <- list(\n", paste(by_answer$code, collapse = ",\n"), "\n)"
    ))
  }
  inputs <- spec$inputs
  images <- if (nrow(inputs) == 0) {
    'images <- list.files("images", full.names = TRUE)'
  } else {
    labels <- unique(stats::na.omit(inputs$label))
    folders <- answer_folders(union(names(spec$question$answers), labels))
    sub <- ifelse(is.na(inputs$label), "", paste0(folders[inputs$label], "/"))
    files <- r_string(paste0("images/", sub, inputs$file))
    paste0("images <- c(\n", paste(pack_lines(files), collapse = "\n"), "\n)")
  }
  paste0(images, references_code(refs))
}

# The call of the question constructor, with the exact text of the question
question_code <- function(question, var) {
  answers <- question$answers
  args <- r_string(question$instructions)
  if (question$type == "noul") {
    given <- answers[nzchar(answers)]
    if (length(given) > 0) args <- c(args, paste0(names(given), " = ", r_string(given)))
  } else {
    names_r <- r_name(names(answers))
    padded <- stringr::str_pad(names_r, max(nchar(names_r)), side = "right")
    args <- c(args, paste0(
      "c(\n", paste0("    ", padded, " = ", r_string(answers), collapse = ",\n"), "\n  )"
    ))
  }
  paste0(
    var, " <- question_", question$type, "(\n",
    paste0("  ", args, collapse = ",\n"), "\n)"
  )
}

# The code of a run, as one string. `date` is the day of the run.
run_code <- function(spec, date = Sys.Date()) {
  refs <- reference_files(spec)
  var <- question_var(spec)
  data_arg <- if (spec$mode == "text") {
    paste0("text = ", if (spec$source == "injuries") "injuries$final_narrative" else "texts")
  } else {
    "image = images"
  }
  if (nrow(refs) > 0) data_arg <- paste0(data_arg, ", references = references")
  paste(
    sprintf("# The run of the decisions Space on %s, in R. Run it with your own key.", format(date)),
    "library(decisions)",
    "",
    paths_comment(spec, refs),
    inputs_code(spec, refs),
    "",
    question_code(spec$question, var),
    "",
    "answers <- decide(",
    sprintf('  decider_typesafe("%s"), %s,', MODEL, var),
    paste0("  ", data_arg, ","),
    sprintf(
      "  max_size = %s, upscale = %s",
      IMAGE_SETTINGS$max_size, toupper(IMAGE_SETTINGS$upscale)
    ),
    ")",
    "answers",
    sep = "\n"
  )
}

# The number of lines of the code, for the closed card
code_lines <- function(code) {
  stringr::str_count(code, "\n") + 1L
}
