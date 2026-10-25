# Tests of the server with shiny::testServer(). The mock of decide() makes no
# call, and the usage store is in memory. testServer() takes no `args` for a
# server function that is not a module, so server_for() puts the test deps in
# the default of `deps`.

# A copy of app_server() whose default deps are `deps`
server_for <- function(deps) {
  server <- app_server
  formals(server)$deps <- deps
  server
}

# One check of the status file, five days before the clock of the tests
server_status <- function() {
  tibble::tibble(
    time_utc = "2026-10-20T05:17:00Z", http_status = "200", seconds = "3.1",
    answered = "true", app_version = "1.0.0"
  )
}

# The deps of the tests: no call, a store in memory, a fixed clock, no daemon
test_deps <- function(decide_fn = mock_decide(sample_lookup("textiles")),
                      store = usage_store_memory(),
                      now = \() as.POSIXct("2026-10-25 12:00:00", tz = "UTC")) {
  app_deps(
    decide_fn = decide_fn, store = store, status_fn = server_status,
    now = now, async = FALSE
  )
}

# Flush the session until the run ends (each chunk takes a few flushes)
finish_run <- function(session, running) {
  for (i in seq_len(300)) {
    if (!shiny::isolate(running())) return(invisible(i))
    session$flushReact()
  }
  stop("the run did not end")
}

# The number of data rows of a table output
table_rows <- function(html) {
  if (is.null(html) || !nzchar(html)) return(0L)
  stringr::str_count(html, "<tr>") - 1L
}

# A data frame as shiny::fileInput() gives it
as_upload <- function(paths, names = basename(paths)) {
  data.frame(name = names, size = file.size(paths), type = "", datapath = paths)
}

test_that("a sample shows its saved answers and makes no call", {
  mock <- mock_decide(sample_lookup("textiles"))
  shiny::testServer(server_for(test_deps(mock)), {
    # The page opens with the textile sample
    expect_identical(nrow(table()), 46L)
    session$setInputs(sample_injuries = 1)
    expect_identical(nrow(table()), 75L)
    expect_identical(table_rows(output$table), 75L)
    # 75 x 1,000 tokens x USD 0.042 for each million is less than a cent
    expect_identical(output$run_estimate, "75 \u00b7 < $0.01")
    session$setInputs(source = "upload")
    expect_null(table())
    session$setInputs(mode = "image")
    expect_identical(spec()$source, "upload")
    session$setInputs(source = "textiles")
    expect_identical(nrow(table()), 46L)
    expect_identical(table_rows(output$table), 46L)
    expect_match(output$stats$html, "Saved run, jev-1.14.0", fixed = TRUE)
    expect_match(output$help_answers, "AUC of 0.75", fixed = TRUE)
    expect_length(stringr::str_extract_all(output$gallery$html, "<figure")[[1]], 46)
    expect_identical(environment(mock)$calls, 0L)
    # Every output has a value
    expect_identical(output$status_chip, "in service 5 days")
    expect_match(output$run_estimate, "^46 \u00b7 \u2248 \\$0\\.03$")
    expect_match(output$spend_meter$html, "$0.00 of $0.25", fixed = TRUE)
    expect_identical(output$limits_text, "")
    expect_match(output$code_summary, "^R code of this run \u00b7 [0-9]+ lines$")
    expect_match(output$code, "decider_typesafe(\"jev-1.14.0\")", fixed = TRUE)
    expect_match(output$bundle_files$html, "textiles-2026-10-25.zip", fixed = TRUE)
    expect_match(output$answers_ui$html, "answer_name_2")
    expect_match(output$references_ui$html, "ref_defective")
    expect_identical(table_rows(output$record_table), 1L)
  })
})

test_that("an observer never sends its own input back to the page", {
  # The page answers each update with its value. If the server sent the input
  # that it reacts to, two crossing updates would flip the input forever.
  shiny::testServer(server_for(test_deps()), {
    sent <- new.env()
    session$sendInputMessage <- function(inputId, message) sent$ids <- c(sent$ids, inputId)
    session$setInputs(mode = "text")
    expect_identical(spec()$source, "injuries")
    expect_true("source" %in% sent$ids)
    # The late answer of an older update is a change like any other ...
    session$setInputs(mode = "image")
    expect_identical(spec()$source, "textiles")
    expect_false("mode" %in% sent$ids)
    # ... and a source of the other mode is a late answer: it is ignored
    sent$ids <- NULL
    session$setInputs(source = "injuries")
    expect_identical(spec()$source, "textiles")
    session$setInputs(source = "upload")
    expect_identical(spec()$source, "upload")
    expect_false(any(c("mode", "source") %in% sent$ids))
    # The Samples menu sets both
    session$setInputs(sample_injuries = 1)
    expect_true(all(c("mode", "source") %in% sent$ids))
  })
})

test_that("the end of the session deletes the uploads and the imports", {
  src <- withr::local_tempdir()
  jpgs <- c(tiny_jpg(file.path(src, "up", "a.jpg")), tiny_jpg(file.path(src, "up", "b.jpg")))
  spec <- upload_spec(labelled = TRUE)
  dir.create(file.path(src, "imp"))
  zipfile <- write_bundle(
    spec, answers_table(spec, upload_answers(spec), "defective"),
    file.path(src, "imp", "0.zip")
  )
  shiny::testServer(server_for(test_deps()), {
    session$setInputs(mode = "image", source = "upload")
    session$setInputs(upload_images = as_upload(jpgs))
    expect_identical(spec()$inputs$file, c("a.jpg", "b.jpg"))
    session$setInputs(import_file = as_upload(zipfile))
    expect_identical(names(spec()$references), c("nominal", "defective"))
    kept <- c(spec()$inputs$path, unlist(spec()$references))
    expect_true(all(file.exists(kept)))
    expect_true(all(startsWith(normalizePath(kept), normalizePath(dir))))
    session$close()
    expect_false(dir.exists(dir))
    expect_false(any(file.exists(c(jpgs, zipfile))))
  })
})

test_that("editing the instructions clears the answers", {
  shiny::testServer(server_for(test_deps()), {
    session$setInputs(sample_textiles = 1)
    expect_identical(nrow(table()), 46L)
    session$setInputs(instructions = "Is the weave flawed?")
    expect_null(table())
    expect_identical(table_rows(output$table), 0L)
    expect_null(output$stats)
    # The answers show only while the setup is the setup of the answers: a late
    # answer of the page to an older update must not delete them
    session$setInputs(instructions = sample_question("textiles")$instructions)
    expect_identical(nrow(table()), 46L)
    expect_match(output$stats$html, "Saved run", fixed = TRUE)
  })
})

test_that("a change of the setup during a run stops the run", {
  mock <- mock_decide(sample_lookup("injuries"))
  shiny::testServer(server_for(test_deps(mock)), {
    session$setInputs(sample_injuries = 1)
    session$setInputs(run_all = 1)
    session$setInputs(instructions = "Which injury is it?")
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_null(table())
  })
})

test_that("Run all on the injuries sample calls Jev in chunks of 10", {
  mock <- mock_decide(sample_lookup("injuries"))
  store <- usage_store_memory()
  shiny::testServer(server_for(test_deps(mock, store)), {
    session$setInputs(sample_injuries = 1)
    session$setInputs(run_all = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 8L)
    day <- store$read()
    expect_identical(day$date, "2026-10-25")
    expect_identical(day$calls, 75L)
    tokens <- sum(sample_answers("injuries")$.input_tokens)
    expect_equal(day$input_tokens, tokens)
    expect_equal(session_cost(), tokens * PRICE_PER_TOKEN)
    expect_identical(nrow(table()), 75L)
    expect_no_match(output$stats$html, "Saved run")
    expect_no_match(output$help_answers, "accuracy of 0.96", fixed = TRUE)
  })
})

test_that("a run over the limit of the day does not start", {
  mock <- mock_decide(sample_lookup("textiles"))
  store <- usage_store_memory()
  shiny::testServer(server_for(test_deps(mock, store)), {
    session$setInputs(sample_textiles = 1)
    # Another session spends after this session started: the check reads the store again
    store$write(tibble::tibble(date = "2026-10-25", calls = 900L, input_tokens = 47e6, cost = 1.99))
    session$setInputs(run_all = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 0L)
    expect_identical(output$limits_text, "The limit of today is reached. Try again tomorrow.")
    expect_identical(nrow(table()), 46L)
  })
})

test_that("a credit error stops the run and the runs of the day", {
  mock <- mock_decide(error = "HTTP 402 Payment Required: no credit left")
  deps <- test_deps(mock)
  shiny::testServer(server_for(deps), {
    session$setInputs(sample_injuries = 1)
    session$setInputs(run_all = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(output$limits_text, "The credit of this demo is used up. Try again later.")
    session$setInputs(run_all = 2)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(output$limits_text, "The credit of this demo is used up. Try again later.")
  })
  # The stop holds for every visitor of the app until the UTC date changes
  shiny::testServer(server_for(deps), {
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(output$limits_text, "The credit of this demo is used up. Try again later.")
  })
  tomorrow <- deps
  tomorrow$now <- \() as.POSIXct("2026-10-26 00:00:05", tz = "UTC")
  shiny::testServer(server_for(tomorrow), {
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 2L)
  })
})

test_that("a missing key gives a short message and no spend", {
  mock <- mock_decide(abort = TRUE)
  store <- usage_store_memory()
  shiny::testServer(server_for(test_deps(mock, store)), {
    session$setInputs(sample_injuries = 1)
    session$setInputs(run_all = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(output$limits_text, "The demo is not available right now.")
    expect_identical(session_cost(), 0)
    expect_identical(sum(store$read()$cost), 0)
    expect_match(output$spend_meter$html, "$0.00 of $0.25", fixed = TRUE)
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
  })
})

test_that("Stop after the first chunk keeps its answers", {
  mock <- mock_decide(sample_lookup("injuries"))
  shiny::testServer(server_for(test_deps(mock)), {
    session$setInputs(sample_injuries = 1)
    session$setInputs(run_all = 1)
    session$setInputs(stop = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(nrow(table()), 10L)
    expect_identical(table_rows(output$table), 10L)
  })
})

test_that("the log has counts only: no input, no file name, no error text", {
  secret <- "HTTP 401 Incorrect API key provided: sk-test-abcd1234"
  logged <- testthat::capture_messages({
    shiny::testServer(server_for(test_deps(mock_decide(error = secret))), {
      session$setInputs(sample_textiles = 1)
      session$setInputs(try5 = 1)
      finish_run(session, running)
    })
    shiny::testServer(server_for(test_deps(mock_decide(sample_lookup("injuries")))), {
      session$setInputs(sample_injuries = 1)
      session$setInputs(run_all = 1)
      finish_run(session, running)
    })
  })
  chunks <- stringr::str_subset(logged, "chunk:")
  expect_length(chunks, 9)
  expect_match(chunks[1], "5 inputs.*auth 5")
  inputs <- c(
    sample_spec("textiles")$inputs$file, sample_spec("injuries")$inputs$text,
    sample_question("textiles")$instructions, sample_question("injuries")$instructions
  )
  found <- purrr::map_lgl(inputs, \(x) any(stringr::str_detect(logged, stringr::fixed(x))))
  expect_false(any(found))
  expect_false(any(stringr::str_detect(logged, "sk-test|Incorrect API key|HTTP 401")))
})

test_that("Try on 5 gives the mean tokens for the estimate of Run all", {
  mock <- mock_decide(sample_lookup("textiles"))
  shiny::testServer(server_for(test_deps(mock)), {
    session$setInputs(sample_textiles = 1)
    expect_match(output$run_estimate, "\\$0\\.03$")
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(nrow(table()), 5L)
    expect_no_match(output$stats$html, "Saved run")
    # 46 x 8253 tokens x USD 0.042 for each million is USD 0.016
    expect_match(output$run_estimate, "^46 \u00b7 \u2248 \\$0\\.02$")
  })
})

test_that("a run with no tokens keeps the guess for the estimate", {
  shiny::testServer(server_for(test_deps(mock_decide())), {
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(nrow(table()), 5L)
    expect_match(output$run_estimate, "^46 \u00b7 \u2248 \\$0\\.03$")
  })
})

test_that("the message line holds the latest notice, in full sentences", {
  bad <- withr::local_tempfile(fileext = ".json")
  writeLines("{}", bad)
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("review,tag", "Fine,a", ",b"), csv)
  shiny::testServer(server_for(test_deps()), {
    session$setInputs(import_file = as_upload(bad, "x.json"))
    expect_identical(output$limits_text, "This file was not made by this app.")
    session$setInputs(source = "upload")
    expect_identical(output$limits_text, "")
    session$setInputs(import_file = as_upload(bad, "x.json"))
    session$setInputs(mode = "text")
    session$setInputs(upload_csv = as_upload(csv, "reviews.csv"))
    expect_identical(output$limits_text, "1 empty text was dropped.")
  })
})

test_that("a question that decisions refuses makes no call and no stop", {
  mock <- mock_decide(sample_lookup("textiles"))
  deps <- test_deps(mock)
  shiny::testServer(server_for(deps), {
    session$setInputs(sample_textiles = 1)
    session$setInputs(instructions = "")
    session$setInputs(try5 = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 0L)
    expect_match(output$limits_text, "The app cannot use this question")
    session$setInputs(instructions = "Does the weave have a flaw?")
    session$setInputs(try5 = 2)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_null(deps$state$stop_day)
  })
})

test_that("the answers of the question follow the inputs of the page", {
  shiny::testServer(server_for(test_deps()), {
    session$setInputs(sample_textiles = 1)
    session$setInputs(answer_name_2 = "flawed", answer_desc_1 = "Regular.")
    defective <- sample_question("textiles")$answers[["defective"]]
    expect_identical(question()$answers, c(nominal = "Regular.", flawed = defective))
    session$setInputs(answer_add = 1)
    expect_identical(names(question()$answers), c("nominal", "flawed", "answer 3"))
    session$setInputs(answer_remove_1 = 1)
    expect_identical(names(question()$answers), c("flawed", "answer 3"))
    session$setInputs(qtype = "noul")
    expect_identical(question()$answers, c(true = "", false = ""))
    expect_identical(target(), "true")
  })
})

test_that("an upload of labelled images matches the folders to the answers", {
  root <- withr::local_tempdir()
  rel <- c("Defective/a.jpg", "nominal/b.jpg", "other/c.jpg")
  for (r in rel) tiny_jpg(file.path(root, r))
  zf <- file.path(withr::local_tempdir(), "parts.zip")
  zip::zip(zf, files = rel, root = root, mode = "mirror")
  mock <- mock_decide()
  shiny::testServer(server_for(test_deps(mock)), {
    session$setInputs(mode = "image", source = "upload")
    session$setInputs(upload_images = as_upload(zf))
    expect_identical(spec()$name, "parts")
    expect_identical(spec()$inputs$label, c("defective", "nominal", NA))
    expect_match(output$limits_text, "other is not an answer", fixed = TRUE)
    session$setInputs(run_all = 1)
    finish_run(session, running)
    expect_identical(environment(mock)$calls, 1L)
    expect_identical(nrow(table()), 3L)
    expect_length(stringr::str_extract_all(output$gallery$html, "<figure")[[1]], 3)
    expect_match(output$code, "images/defective/a.jpg", fixed = TRUE)
  })
})

test_that("texts come from the paste box or from a CSV file", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("review,tag", "I want my money back,true", "Thanks a lot,false"), csv)
  shiny::testServer(server_for(test_deps()), {
    session$setInputs(mode = "text")
    expect_identical(spec()$source, "injuries")
    session$setInputs(source = "upload")
    session$setInputs(paste_text = "One text.\n\nTwo texts.")
    expect_identical(spec()$inputs$text, c("One text.", "Two texts."))
    expect_null(spec()$text_column)
    session$setInputs(upload_csv = as_upload(csv, "reviews.csv"))
    session$setInputs(csv_text_col = "review", csv_label_col = "tag", qtype = "noul")
    expect_identical(spec()$inputs$text, c("I want my money back", "Thanks a lot"))
    expect_identical(spec()$inputs$label, c("true", "false"))
    expect_identical(spec()$text_column, "review")
    expect_identical(spec()$name, "reviews")
  })
})

test_that("the bundle files of the card are the files of the bundle", {
  specs <- list(
    sample_spec("textiles"), sample_spec("injuries"), upload_spec(labelled = TRUE),
    text_spec(csv = FALSE), text_spec(csv = TRUE)
  )
  for (spec in specs) {
    answers <- empty_answers(spec$question, 0) |> dplyr::mutate(id = character(), .before = 1)
    table <- answers_table(spec, answers, target_answer(spec$question))
    zf <- write_bundle(spec, table, withr::local_tempfile(fileext = ".zip"))
    top <- stringr::str_replace(zip::zip_list(zf)$filename, "/.*", "/") |> unique()
    expect_setequal(bundle_names(spec), top)
  }
})

test_that("SPACE_MOCK=1 uses the mock and a store in memory", {
  withr::local_envvar(SPACE_MOCK = "1")
  withr::local_dir(APP_ROOT)
  deps <- app_deps()
  out <- run_chunk(sample_spec("textiles"), 1:3, deps$decide_fn)
  expect_equal(out$.pred_defective, sample_answers("textiles")$.pred_defective[1:3])
  out <- run_chunk(sample_spec("injuries"), 1:3, deps$decide_fn)
  expect_equal(out$.pred_amputation, sample_answers("injuries")$.pred_amputation[1:3])
  expect_identical(nrow(deps$store$read()), 0L)
  withr::local_dir(withr::local_tempdir())
  expect_error(app_deps(), "SPACE_MOCK")
})
