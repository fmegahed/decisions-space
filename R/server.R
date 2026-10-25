# Server: the wiring of the page. The logic is in the other files of R/; this
# file keeps the state of a session (the question, the data, the answers and
# the spend) and runs the calls to Jev in chunks, as a background task.
#
# app_deps() holds what a test replaces: decide(), the usage store, the status
# reader, the clock, and `async` (FALSE runs the chunks in this R process).
# app.R makes it once for all sessions, so its `state` (the stop for the UTC
# day after a key or credit error) holds for every visitor.

# The dependencies of the server. With SPACE_MOCK=1 (local checks only; the
# Dockerfile never sets it), the mock of decide() and a store in memory
# replace the defaults.
app_deps <- function(decide_fn = decisions::decide, store = usage_store_hf(),
                     status_fn = read_status, now = Sys.time, async = TRUE) {
  if (Sys.getenv("SPACE_MOCK") == "1") {
    mock <- mock_deps()
    if (missing(decide_fn)) decide_fn <- mock$decide_fn
    if (missing(store)) store <- mock$store
  }
  list(
    decide_fn = decide_fn, store = store, status_fn = status_fn, now = now,
    async = async, state = new.env(parent = emptyenv())
  )
}

# The mock of the tests (with the saved answers of both samples) and a store in
# memory. The mock stays in tests/: the app code holds no mock.
mock_deps <- function(path = file.path("tests", "testthat", "helper-mock.R")) {
  if (!file.exists(path)) {
    stop(
      "SPACE_MOCK=1 needs ", path,
      ". Start the app from the repository folder, or unset SPACE_MOCK.",
      call. = FALSE
    )
  }
  env <- new.env(parent = environment(mock_deps))
  sys.source(path, envir = env)
  lookup <- dplyr::bind_rows(env$sample_lookup("textiles"), env$sample_lookup("injuries"))
  list(decide_fn = env$mock_decide(lookup), store = usage_store_memory())
}

# The two samples, read once for each R process
SAMPLE_CACHE <- new.env(parent = emptyenv())
sample_data <- function(id) {
  SAMPLE_CACHE[[id]] <- SAMPLE_CACHE[[id]] %||%
    list(spec = sample_spec(id), answers = sample_answers(id))
  SAMPLE_CACHE[[id]]
}

# The sources of the data that go with a mode
source_choices <- function(mode) {
  SOURCE_CHOICES[SOURCE_CHOICES %in% c(if (mode == "image") "textiles" else "injuries", "upload")]
}

# The answers belong to one setup and one set of inputs
answers_key <- function(spec) rlang::hash(list(spec_fingerprint(spec), spec$inputs$id))

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

# The category that stops the runs today, or NULL
stop_reason <- function(deps) {
  if (identical(deps$state$stop_day, utc_date(deps$now()))) deps$state$stop_reason
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

# The accounts of a chunk, in the main R process (never in a daemon): add the
# usage to the store, stop the day after a key or credit error, and write the
# log line. Rows that failed before any request ("config") cost nothing.
account_chunk <- function(answers, deps) {
  categories <- classify_error(answers$.error)
  use <- chunk_usage(answers[!categories %in% "config", ])
  now <- deps$now()
  usage <- if (use$calls > 0) usage_record(deps$store, now, use$calls, use$tokens, use$cost)
  stop <- dplyr::first(categories[stops_the_day(categories)])
  if (!is.na(stop)) {
    deps$state$stop_day <- utc_date(now)
    deps$state$stop_reason <- stop
  }
  log_chunk(now, nrow(answers), use, categories)
  list(answers = answers, cost = use$cost, usage = usage)
}

# A small JPEG copy of an image (at most 320 pixels on the long side) for the
# page; FALSE when the image cannot be read
make_thumb <- function(path, dest) {
  tryCatch(
    {
      magick::image_read(path)[1] |>
        magick::image_background("white") |>
        magick::image_scale("320x320>") |>
        magick::image_write(dest, format = "jpeg")
      TRUE
    },
    error = \(e) FALSE
  )
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

# The files of the bundle of a spec, as write_bundle() makes them (a folder
# ends in "/"; test-server.R checks that the two agree)
bundle_names <- function(spec) {
  name <- run_name(spec)
  has_refs <- nrow(reference_files(spec)) > 0 && spec$source != "textiles"
  has_data <- spec$mode == "text" && spec$source == "upload" &&
    is.null(spec$text_column) && nrow(spec$inputs) > 0
  c(
    paste0(name, c(".R", "-setup.json", "-answers.csv")),
    if (has_refs) "references/", if (has_data) "data.csv", "README.md"
  )
}

# Delete the files of Shiny uploads, and their folders when they are empty
remove_uploads <- function(paths) {
  unlink(paths)
  folders <- setdiff(
    unique(normalizePath(dirname(paths), winslash = "/", mustWork = FALSE)),
    normalizePath(tempdir(), winslash = "/")
  )
  files <- lengths(lapply(folders, list.files, all.files = TRUE, no.. = TRUE))
  empty <- folders[dir.exists(folders) & files == 0]
  unlink(empty, recursive = TRUE)
}

app_server <- function(input, output, session, deps = app_deps()) {
  # --- The folder of the session: uploads, imports and thumbnails -----------
  dir <- tempfile("session")
  dir.create(file.path(dir, "thumbs"), recursive = TRUE)
  # The thumbnails are served under a prefix that no other visitor can guess
  prefix <- paste(openssl::rand_bytes(16), collapse = "")
  shiny::addResourcePath(prefix, file.path(dir, "thumbs"))
  uploads <- character()
  folders <- 0L
  new_dir <- function(kind) {
    folders <<- folders + 1L
    file.path(dir, kind, folders)
  }
  keep_uploads <- function(files) uploads <<- c(uploads, files$datapath)
  session$onSessionEnded(\() {
    shiny::removeResourcePath(prefix)
    unlink(dir, recursive = TRUE)
    remove_uploads(uploads)
  })

  # The addresses of the thumbnails of images ("" for an image that cannot be read)
  thumb <- function(paths) {
    purrr::map_chr(paths, \(path) {
      name <- paste0(rlang::hash(path), ".jpg")
      dest <- file.path(dir, "thumbs", name)
      if (file.exists(dest) || make_thumb(path, dest)) paste0(prefix, "/", name) else ""
    })
  }

  # --- State --------------------------------------------------------------------
  question <- shiny::reactiveVal()
  refs <- shiny::reactiveVal(list()) # reference paths, one element for each answer
  data_mode <- shiny::reactiveVal("image")
  data_source <- shiny::reactiveVal("textiles")
  image_upload <- shiny::reactiveVal(NULL)
  csv_file <- shiny::reactiveVal(NULL)
  text_kind <- shiny::reactiveVal("paste")
  results <- shiny::reactiveVal(NULL) # list(answers, key, saved)
  mean_tokens <- shiny::reactiveVal(NULL) # list(fingerprint, mean)
  session_cost <- shiny::reactiveVal(0)
  usage <- shiny::reactiveVal(deps$store$read())
  notice <- shiny::reactiveVal(character())
  running <- shiny::reactiveVal(FALSE)
  rows_version <- shiny::reactiveVal(0) # renders the answer rows again
  job <- new.env(parent = emptyenv())

  # --- The spec of the sidebar --------------------------------------------------
  text_upload <- shiny::reactive({
    csv <- csv_file()
    if (text_kind() == "paste" || is.null(csv)) {
      return(c(read_text_paste(input$paste_text), list(name = "texts")))
    }
    if (length(csv$columns) == 0) {
      return(list(
        inputs = empty_inputs(), problems = "The CSV file could not be read.", name = csv$name
      ))
    }
    column <- if (isTRUE(input$csv_text_col %in% csv$columns)) input$csv_text_col else csv$text_col
    label <- if (isTRUE(input$csv_label_col %in% csv$columns)) input$csv_label_col
    c(read_text_csv(csv$path, column, label, csv$file), list(name = csv$name, text_column = column))
  })
  data <- shiny::reactive({
    id <- data_source()
    if (id != "upload") return(list(inputs = sample_data(id)$spec$inputs, name = id))
    upload <- if (data_mode() == "image") image_upload() else text_upload()
    upload %||% list(inputs = empty_inputs(), name = "upload")
  })
  matched <- shiny::reactive(match_labels(data()$inputs, question()$answers))
  spec <- shiny::reactive({
    q <- question()
    references <- if (data_mode() == "image" && q$type != "score") {
      stats::setNames(refs()[seq_along(q$answers)], names(q$answers)) |> purrr::compact()
    }
    list(
      mode = data_mode(), question = q,
      references = if (length(references) > 0) references,
      inputs = matched()$inputs, source = data_source(), name = data()$name,
      text_column = data()$text_column
    )
  })
  key <- shiny::reactive(answers_key(spec()))
  # The type and the names of the answers: the reference rows and the target follow them
  layout <- shiny::reactiveVal()
  shiny::observe(layout(list(type = question()$type, answers = names(question()$answers))))

  # Send the setup of the server to the inputs of the page. The page answers
  # each update with the new value, so an observer never sends its own input
  # (or one above it: mode, then source, then the question) back: two crossing
  # updates would flip the input forever. `send` names the inputs of the data.
  show_setup <- function(send = c("mode", "source")) {
    q <- question()
    if ("mode" %in% send) shiny::updateRadioButtons(session, "mode", selected = data_mode())
    if ("source" %in% send) {
      shiny::updateSelectInput(
        session, "source", choices = source_choices(data_mode()), selected = data_source()
      )
    }
    shiny::updateRadioButtons(session, "qtype", selected = q$type)
    shiny::updateTextAreaInput(session, "instructions", value = q$instructions)
    shiny::updateRadioButtons(
      session, "view", selected = if (data_mode() == "image") "gallery" else "table"
    )
    rows_version(rows_version() + 1)
  }

  # A sample opens with its saved answers and makes no call
  load_sample <- function(id, send = c("mode", "source")) {
    sample <- sample_data(id)
    question(sample$spec$question)
    refs(unname(sample$spec$references) %||% list())
    data_mode(sample$spec$mode)
    data_source(id)
    results(list(answers = sample$answers, key = answers_key(sample$spec), saved = TRUE))
    notice(character())
    show_setup(send)
  }
  # The page opens with Images and the textile sample; only the choices of the source change
  shiny::isolate(load_sample("textiles", send = "source"))

  # --- Data -------------------------------------------------------------------
  shiny::observeEvent(input$sample_textiles, load_sample("textiles"))
  shiny::observeEvent(input$sample_injuries, load_sample("injuries"))
  shiny::observeEvent(input$mode, {
    if (input$mode == data_mode()) return()
    if (data_source() != "upload") {
      return(load_sample(if (input$mode == "image") "textiles" else "injuries", send = "source"))
    }
    data_mode(input$mode)
    refs(list())
    notice(character())
    show_setup(send = "source")
  })
  shiny::observeEvent(input$source, {
    # A source of the other mode is the late answer to an older update
    if (input$source == data_source() || !input$source %in% source_choices(data_mode())) return()
    if (input$source != "upload") return(load_sample(input$source, send = character()))
    data_source("upload")
    refs(list())
    notice(character())
  })
  shiny::observeEvent(input$upload_images, {
    files <- input$upload_images
    keep_uploads(files)
    notice(character())
    unlink(image_upload()$dir, recursive = TRUE)
    read <- read_images(files, dir = new_dir("images"))
    bad <- thumb(read$inputs$path) == ""
    image_upload(list(
      inputs = read$inputs[!bad, ], dir = read$dir,
      problems = c(
        read$problems, sprintf("%s could not be read and was skipped.", read$inputs$file[bad])
      ),
      name = if (nrow(files) == 1) tools::file_path_sans_ext(files$name) else "images"
    ))
  })
  shiny::observeEvent(input$paste_text, text_kind("paste"), ignoreInit = TRUE)
  shiny::observeEvent(input$upload_csv, {
    file <- input$upload_csv
    keep_uploads(file)
    notice(character())
    path <- file.path(new_dir("csv"), "data.csv")
    dir.create(dirname(path), recursive = TRUE)
    file.copy(file$datapath, path)
    columns <- csv_columns(path)
    text_col <- if ("text" %in% columns) "text" else columns[1]
    csv_file(list(
      path = path, file = file$name, name = tools::file_path_sans_ext(file$name),
      columns = columns, text_col = text_col
    ))
    text_kind("csv")
    shiny::updateSelectInput(session, "csv_text_col", choices = columns, selected = text_col)
    shiny::updateSelectInput(
      session, "csv_label_col", choices = c(None = "", columns), selected = ""
    )
  })

  # --- Question ---------------------------------------------------------------
  # Each input changes one part of the question. A change that the server made
  # comes back from the page with the same value, so it changes nothing.
  shiny::observeEvent(input$instructions, {
    q <- question()
    q$instructions <- input$instructions
    question(q)
  })
  edit_answer <- function(i, part, value) {
    q <- question()
    if (i > length(q$answers) || (part == "name" && q$type == "noul")) return()
    if (part == "name") names(q$answers)[i] <- value else q$answers[[i]] <- value
    question(q)
  }
  purrr::walk(seq_len(max(unlist(ANSWER_RANGE))), \(i) {
    shiny::observeEvent(input[[paste0("answer_name_", i)]], {
      edit_answer(i, "name", input[[paste0("answer_name_", i)]])
    })
    shiny::observeEvent(input[[paste0("answer_desc_", i)]], {
      edit_answer(i, "desc", input[[paste0("answer_desc_", i)]])
    })
    shiny::observeEvent(input[[paste0("answer_remove_", i)]], {
      q <- question()
      if (i > length(q$answers) || length(q$answers) <= ANSWER_RANGE[[q$type]][1]) return()
      q$answers <- q$answers[-i]
      question(q)
      refs(refs()[-i])
      rows_version(rows_version() + 1)
    })
  })
  shiny::observeEvent(input$answer_add, {
    q <- question()
    n <- length(q$answers)
    if (q$type == "noul" || n >= ANSWER_RANGE[[q$type]][2]) return()
    name <- utils::tail(make.unique(c(names(q$answers), paste("answer", n + 1)), sep = " "), 1)
    q$answers <- c(q$answers, stats::setNames("", name))
    question(q)
    rows_version(rows_version() + 1)
  })
  shiny::observeEvent(input$qtype, {
    q <- question()
    if (input$qtype == q$type) return()
    q$answers <- if (input$qtype == "noul") {
      c(true = "", false = "")
    } else {
      utils::head(q$answers, ANSWER_RANGE[[input$qtype]][2])
    }
    q$type <- input$qtype
    question(q)
    rows_version(rows_version() + 1)
  })
  # The reference inputs: a new upload replaces the references of its answer
  seen <- character()
  shiny::observe({
    ids <- ref_input_id(shiny::req(layout())$answers)
    for (i in seq_along(ids)) {
      files <- input[[ids[i]]]
      if (is.null(files) || all(files$datapath %in% seen)) next
      seen <<- c(seen, files$datapath)
      keep_uploads(files)
      read <- read_references(files, dir = new_dir("references"))
      shiny::isolate({
        r <- refs()
        r[[i]] <- read$paths
        refs(r)
        notice(read$problems)
      })
    }
  })
  # Import restores the question and the references; it makes no call
  shiny::observeEvent(input$import_file, {
    file <- input$import_file
    keep_uploads(file)
    read <- read_import(file$datapath, dir = new_dir("import"))
    if (is.null(read$question)) return(notice(read$problems))
    question(read$question)
    refs(purrr::map(names(read$question$answers), \(a) read$references[[a]] %||% character()))
    if (length(read$references) > 0 && data_mode() == "text") {
      data_mode("image")
      data_source("upload")
    }
    notice(c("The setup is imported.", read$problems))
    show_setup()
  })
  shiny::observe({
    l <- shiny::req(layout())
    shiny::updateSelectInput(
      session, "target", choices = if (l$type == "noul") "true" else l$answers,
      selected = target_answer(shiny::isolate(question()), shiny::isolate(input$target))
    )
  })

  # The answers show only while the setup and the inputs are those of the
  # answers (table() checks the key). They are not deleted: the page can answer
  # an older update late, and the setup then comes back. A change stops a run.
  shiny::observe({
    k <- key()
    if (shiny::isolate(running()) && !identical(job$key, k)) job$stop <- TRUE
  })

  # --- Runs -------------------------------------------------------------------
  mean_for <- function(s) {
    m <- mean_tokens()
    if (!is.null(m) && identical(m$fingerprint, spec_fingerprint(s))) m$mean
  }
  checks <- shiny::reactive({
    s <- spec()
    status <- spend_status(session_cost(), usage(), deps$now())
    purrr::map(c(try5 = 5, run_all = Inf), \(k) {
      n <- min(k, nrow(s$inputs))
      cost <- estimate_cost(s, n, mean_for(s))
      c(run_allowed(cost, status), list(n = n, cost = cost))
    })
  })
  shiny::observe({
    shiny::updateActionButton(session, "try5", disabled = !checks()$try5$ok)
    shiny::updateActionButton(session, "run_all", disabled = !checks()$run_all$ok)
  })

  # Each chunk runs in a mirai daemon (or here, when async is FALSE); its
  # accounts are kept in this process, also when the session has ended
  task <- shiny::ExtendedTask$new(function(spec, rows) {
    chunk <- tryCatch(
      if (deps$async) {
        mirai::mirai(
          run_chunk(spec, rows, decide_fn),
          .args = list(spec = spec, rows = rows, decide_fn = deps$decide_fn)
        )
      } else {
        run_chunk(spec, rows, deps$decide_fn)
      },
      error = \(e) promises::promise_reject(e)
    )
    promises::then(promises::promise_resolve(chunk), \(answers) account_chunk(answers, deps))
  })

  start_run <- function(n) {
    if (running()) return()
    s <- spec()
    problems <- run_problems(s)
    if (length(problems) > 0) return(notice(problems))
    if (!is.null(stop_reason(deps))) return(notice(error_message(stop_reason(deps))))
    # The latest totals of all sessions decide, not the copy of this session
    latest <- deps$store$read()
    usage(latest)
    n <- min(n, nrow(s$inputs))
    allowed <- run_allowed(
      estimate_cost(s, n, mean_for(s)), spend_status(session_cost(), latest, deps$now())
    )
    if (!allowed$ok) return(notice(limit_message(allowed$reason)))
    notice(character())
    job$spec <- s
    job$key <- key()
    job$chunks <- chunk_ids(n)
    job$done <- 0L
    job$stop <- FALSE
    job$answers <- NULL
    running(TRUE)
    session$sendCustomMessage("run-state", TRUE)
    task$invoke(s, job$chunks[[1]])
  }
  end_run <- function() {
    running(FALSE)
    session$sendCustomMessage("run-state", FALSE)
  }
  # After each chunk: show its answers, then run the next chunk or stop
  finish_chunk <- function(done) {
    if (!running()) return()
    job$done <- job$done + 1L
    if (is.null(done)) {
      notice("The run stopped. Try again.")
      return(end_run())
    }
    session_cost(session_cost() + done$cost)
    if (!is.null(done$usage)) usage(done$usage)
    job$answers <- dplyr::bind_rows(job$answers, done$answers)
    results(list(answers = job$answers, key = job$key, saved = FALSE))
    # Only calls with tokens count: a mean of 0 would make the next run look free
    tokens <- job$answers$.input_tokens
    tokens <- tokens[!is.na(tokens) & tokens > 0]
    if (length(tokens) > 0) {
      mean_tokens(list(fingerprint = spec_fingerprint(job$spec), mean = mean(tokens)))
    }
    reason <- stop_reason(deps)
    if (!is.null(reason)) notice(error_message(reason))
    if (job$stop || !is.null(reason) || job$done >= length(job$chunks)) return(end_run())
    task$invoke(job$spec, job$chunks[[job$done + 1L]])
  }
  shiny::observe({
    if (!task$status() %in% c("success", "error")) return()
    done <- tryCatch(task$result(), error = \(e) NULL)
    shiny::isolate(finish_chunk(done))
  })
  shiny::observeEvent(input$try5, start_run(5))
  shiny::observeEvent(input$run_all, start_run(Inf))
  # Stop takes effect at the end of the current chunk
  shiny::observeEvent(input$stop, job$stop <- TRUE, priority = 10)

  # --- Outputs: sidebar -------------------------------------------------------
  output$answers_ui <- shiny::renderUI({
    rows_version()
    answer_rows(shiny::isolate(question()))
  })
  output$references_ui <- shiny::renderUI({
    l <- shiny::req(layout())
    r <- refs()
    srcs <- purrr::map(seq_along(l$answers), \(i) {
      if (i <= length(r)) thumb(r[[i]]) else character()
    })
    reference_rows(
      list(type = l$type, answers = stats::setNames(character(length(l$answers)), l$answers)),
      stats::setNames(srcs, l$answers)
    )
  })
  # A cost below one cent reads "< $0.01", not "$0.00": a run is never free
  output$run_estimate <- shiny::renderText({
    all <- checks()$run_all
    cost <- if (all$cost > 0 && all$cost < 0.005) "< $0.01" else sprintf("\u2248 $%.2f", all$cost)
    paste(all$n, "\u00b7", cost)
  })
  output$spend_meter <- shiny::renderUI(spend_meter_ui(session_cost()))
  output$limits_text <- shiny::renderText({
    all <- checks()$run_all
    refused <- if (!all$ok) limit_message(all$reason)
    sentences <- unique(c(notice(), data()$problems, matched()$problems, refused))
    paste(stringr::str_replace(sentences, "([^.!?])$", "\\1."), collapse = " ")
  })

  # --- Outputs: answers -------------------------------------------------------
  target <- shiny::reactive(target_answer(question(), input$target))
  table <- shiny::reactive({
    r <- results()
    if (is.null(r) || !identical(r$key, key())) return(NULL)
    answers_table(spec(), r$answers, target())
  })
  stats <- shiny::reactive(if (!is.null(table())) answer_stats(table(), spec(), target()))
  output$stats <- shiny::renderUI({
    if (is.null(stats())) return(NULL)
    r <- results()
    note <- if (r$saved) {
      paste0("Saved run, ", MODEL)
    } else if (running()) {
      sprintf("%d of %d", nrow(r$answers), length(unlist(job$chunks)))
    }
    stats_ui(stats(), spec()$mode, note)
  })
  output$help_answers <- shiny::renderText({
    if (is.null(stats())) return(help_text("answers"))
    help_note(spec(), stats(), target(), saved = results()$saved)
  })
  output$gallery <- shiny::renderUI({
    t <- table()
    if (is.null(t) || spec()$mode != "image") return(NULL)
    inputs <- spec()$inputs
    gallery_ui(t, target(), thumb(inputs$path[match(t$id, inputs$id)]))
  })
  output$table <- shiny::renderTable(
    if (!is.null(table())) table_view(table(), spec()),
    digits = 2, na = ""
  )
  output$download_answers <- shiny::downloadHandler(
    filename = \() paste0(run_name(spec()), "-answers.csv"),
    content = \(file) readr::write_file(answers_csv(table() %||% empty_table(spec())), file)
  )

  # --- Outputs: R code and bundle ---------------------------------------------
  run_date <- function() as.Date(deps$now())
  zip_name <- function() sprintf("%s-%s.zip", run_name(spec()), format(run_date()))
  code <- shiny::reactive(run_code(spec(), run_date()))
  output$code_summary <- shiny::renderText(
    sprintf("R code of this run \u00b7 %d lines", code_lines(code()))
  )
  output$code <- shiny::renderText(code())
  output$bundle_files <- shiny::renderUI(bundle_files_ui(bundle_names(spec()), zip_name()))
  for (id in c("code", "bundle_files", "code_summary")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  output$download_code <- shiny::downloadHandler(
    filename = \() paste0(run_name(spec()), ".R"),
    content = \(file) readr::write_file(paste0(code(), "\n"), file)
  )
  output$download_bundle <- shiny::downloadHandler(
    filename = zip_name,
    content = \(file) write_bundle(spec(), table() %||% empty_table(spec()), file, run_date())
  )

  # --- Outputs: status ----------------------------------------------------------
  # The status file is read at the start and every hour
  status <- shiny::reactive({
    shiny::invalidateLater(3600000)
    status_summary(deps$status_fn(), deps$now())
  })
  output$status_chip <- shiny::renderText(status_chip(status()))
  output$record_table <- shiny::renderTable(
    {
      s <- status()
      s$last |>
        dplyr::arrange(dplyr::desc(time_utc)) |>
        dplyr::transmute(
          `Time (UTC)` = time_utc, HTTP = suppressWarnings(as.integer(http_status)),
          Seconds = suppressWarnings(as.numeric(seconds)),
          !!sprintf("Answered (%d of %d)", s$answered, s$checks) := answered,
          Version = app_version
        )
    },
    digits = 1, na = ""
  )
}
