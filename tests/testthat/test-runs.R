test_that("classify_error() maps provider texts to categories", {
  expect_identical(
    classify_error(c(NA, "HTTP 402 {...}", "HTTP 400 insufficient balance", "HTTP 401 bad key ...abcd",
                     "HTTP 429", "HTTP 503", "HTTP 422 bad image", "answer is not allowed", "config")),
    c(NA, "credit", "credit", "auth", "rate", "server", "input", "answer", "config"))
})

test_that("the raw error text never reaches the visitor text", {
  msg <- error_message(classify_error("HTTP 401 Incorrect API key provided: sk-...wxyz"))
  expect_false(stringr::str_detect(msg, "wxyz|sk-|HTTP"))
})

test_that("run_chunk() returns one row per input and keeps the ids", {
  tex <- sample_spec("textiles")
  out <- run_chunk(tex, 11:20, mock_decide(sample_lookup("textiles")))
  expect_identical(out$id, tex$inputs$id[11:20])
  expect_equal(out$.pred_defective, dplyr::filter(sample_answers("textiles"), id %in% tex$inputs$id[11:20])$.pred_defective)
  out <- run_chunk(tex, 1:10, mock_decide(sample_lookup("textiles")))
  expect_identical(out$id, tex$inputs$id[1:10])
})

test_that("a missing key stops the run without a crash", {
  withr::local_envvar(HF_JEV_API = "")
  out <- run_chunk(sample_spec("injuries"), 1:3, mock_decide(abort = TRUE))
  expect_identical(unique(classify_error(out$.error)), "config")
  expect_true(stops_the_day("config"))
})

test_that("an abort row has the columns of a normal chunk", {
  inj <- sample_spec("injuries")
  ok <- run_chunk(inj, 1:3, mock_decide(sample_lookup("injuries")))
  bad <- run_chunk(inj, 1:3, mock_decide(abort = TRUE))
  expect_setequal(names(bad), names(ok))
  expect_no_error(dplyr::bind_rows(ok, bad))
  expect_true(all(is.na(bad$.pred_crushing_injury)))
  expect_true(all(is.na(bad$.input_tokens)))
})

test_that("mock_decide() counts calls and fills a score question", {
  tex <- sample_spec("textiles")
  f <- mock_decide(error = "HTTP 401 x")
  out <- run_chunk(tex, 1:2, f)
  expect_identical(environment(f)$calls, 1L)
  expect_identical(unique(classify_error(out$.error)), "auth")
  expect_true(all(is.na(out$.pred_nominal)))
  score <- modifyList(tex, list(question = list(
    type = "score", instructions = "Rate it.", answers = c(low = "", mid = "", high = ""))))
  out <- run_chunk(score, 1:2, mock_decide())
  expect_true(".score" %in% names(out))
  expect_identical(out$.pred_low, c(1, 1))
})

test_that("chunk_usage() sums the tokens and the cost", {
  u <- chunk_usage(tibble::tibble(.input_tokens = c(1000, NA, 3000)))
  expect_identical(u$calls, 3L)
  expect_equal(u$tokens, 4000)
  expect_equal(u$cost, 4000 * 0.042 / 1e6)
})

test_that("stops_the_day() is TRUE for config, auth and credit only", {
  expect_identical(
    stops_the_day(c("config", "auth", "credit", "rate", "server", "input", "answer")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("estimate_cost() uses the guess, then the measured mean", {
  tex <- sample_spec("textiles")
  expect_equal(estimate_cost(tex, 46), 46 * 16000 * 0.042 / 1e6)
  expect_equal(estimate_cost(tex, 46, mean_tokens = 8253), 46 * 8253 * 0.042 / 1e6)
})

test_that("chunk_ids() makes chunks of 10", {
  expect_identical(lengths(chunk_ids(46)), c(10L, 10L, 10L, 10L, 6L))
})

test_that("make_decider() reads the key only through the credentials function", {
  withr::local_envvar(HF_JEV_API = "fake-key-1234567890")
  decider <- make_decider()
  creds <- decider@credentials
  expect_identical(creds(), "fake-key-1234567890")
  shown <- utils::capture.output(print(decider))
  expect_false(any(stringr::str_detect(shown, "fake-key-1234567890")))
})

test_that("the real decide() with an empty key gives config rows and no request", {
  withr::local_envvar(HF_JEV_API = "", TYPESAFE_API_KEY = "")
  httr2::local_mocked_responses(function(req) stop("no network in tests"))
  out <- run_chunk(sample_spec("injuries"), 1:2)
  expect_identical(nrow(out), 2L)
  expect_identical(classify_error(out$.error), c("config", "config"))
})

test_that("run_chunk() passes the inputs, references and image settings", {
  seen <- NULL
  capture <- function(...) {
    seen <<- list(...)
    mock_decide()(...)
  }
  tex <- sample_spec("textiles")
  run_chunk(tex, 1:10, capture)
  expect_identical(seen$image, tex$inputs$path[1:10])
  expect_null(seen$text)
  expect_identical(seen$references, tex$references)
  expect_identical(seen$max_size, IMAGE_SETTINGS$max_size)
  expect_identical(seen$upscale, IMAGE_SETTINGS$upscale)
  expect_identical(seen$crop, IMAGE_SETTINGS$crop)
  inj <- sample_spec("injuries")
  run_chunk(inj, 1:10, capture)
  expect_identical(seen$text, inj$inputs$text[1:10])
  expect_null(seen$image)
})

test_that("chunk_files() gives the images of the rows and every reference", {
  tex <- sample_spec("textiles")
  files <- chunk_files(tex, 1:3)
  expect_identical(files, c(tex$inputs$path[1:3], unlist(tex$references, use.names = FALSE)))
  expect_identical(chunk_files(sample_spec("injuries"), 1:3), character())
})
