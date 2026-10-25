# Tests of R/spending.R: usage totals, limits, and the stores (no network).

utc <- function(x) as.POSIXct(x, tz = "UTC")
now <- utc("2026-11-03 12:00:00")

usage_fixture <- function() {
  tibble::tibble(
    date = c("2026-10-31", "2026-11-02", "2026-11-03"),
    calls = c(10L, 20L, 30L),
    input_tokens = c(1e5, 2e5, 3e5),
    cost = c(5, 1, 0.5)
  )
}

test_that("spend_status sums the day and the month for a fixed now", {
  st <- spend_status(0.05, usage_fixture(), now)
  expect_equal(st$session_left, LIMIT_SESSION - 0.05)
  expect_equal(st$day_left, LIMIT_DAY - 0.5)
  expect_equal(st$month_left, LIMIT_MONTH - 1.5)
})

test_that("run_allowed refuses by session, day and month", {
  st0 <- spend_status(0, empty_usage(), now)
  r <- run_allowed(0.30, st0)
  expect_false(r$ok)
  expect_equal(r$reason, "session")

  st_day <- spend_status(0, tibble::tibble(
    date = "2026-11-03", calls = 1L, input_tokens = 1, cost = 1.99
  ), now)
  r <- run_allowed(0.02, st_day)
  expect_false(r$ok)
  expect_equal(r$reason, "day")

  st_month <- spend_status(0, tibble::tibble(
    date = c("2026-11-01", "2026-11-02"), calls = c(1L, 1L),
    input_tokens = c(1, 1), cost = c(10, 9.99)
  ), now)
  r <- run_allowed(0.02, st_month)
  expect_false(r$ok)
  expect_equal(r$reason, "month")

  r <- run_allowed(0.02, st0)
  expect_true(r$ok)
  expect_true(is.na(r$reason))
})

test_that("limit_message gives the three texts of the design", {
  expect_equal(limit_message("session"), "The limit of this visit is reached.")
  expect_equal(
    limit_message("day"), "The limit of today is reached. Try again tomorrow."
  )
  expect_equal(limit_message("month"), "The limit of this month is reached.")
})

test_that("usage_add writes two rows across midnight UTC", {
  u <- empty_usage() |>
    usage_add(utc("2026-11-03 23:59:59"), 10L, 1000, 0.1) |>
    usage_add(utc("2026-11-04 00:00:01"), 5L, 500, 0.05) |>
    usage_add(utc("2026-11-04 00:00:02"), 5L, 500, 0.05)
  expect_equal(u$date, c("2026-11-03", "2026-11-04"))
  expect_equal(u$calls, c(10L, 10L))
  expect_equal(u$input_tokens, c(1000, 1000))
  expect_equal(u$cost, c(0.1, 0.1))
})

test_that("usage_add uses the UTC date for a time zone with another date", {
  t <- as.POSIXct("2026-11-03 23:30:00", tz = "America/New_York")
  u <- usage_add(empty_usage(), t, 1L, 1, 0.01)
  expect_equal(u$date, "2026-11-04")
})

test_that("a memory store gives back what was written", {
  store <- usage_store_memory()
  expect_equal(nrow(store$read()), 0)
  u <- usage_fixture()
  store$write(u)
  expect_equal(store$read(), u)
})

test_that("usage_record: two sessions on one store give the sum", {
  store <- usage_store_memory()
  usage_record(store, now, 10L, 1000, 0.1)
  u <- usage_record(store, now, 5L, 500, 0.05)
  expect_equal(u$calls, 15L)
  expect_equal(u$input_tokens, 1500)
  expect_equal(u$cost, 0.15)
  expect_equal(store$read(), u)
})

# ---- the Hugging Face store, with mocked HTTP --------------------------------

fake_token <- "hf_FAKE_TOKEN_for_tests_0123456789"

is_post <- function(req) identical(httr2::req_get_method(req), "POST")

csv_response <- function(usage, status = 200) {
  httr2::response(
    status_code = status, headers = list(`Content-Type` = "text/csv"),
    body = charToRaw(readr::format_csv(usage))
  )
}

test_that("hf store: a 404 read gives an empty usage", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 404))
  u <- usage_store_hf("x/y")$read()
  expect_equal(nrow(u), 0)
  expect_named(u, c("date", "calls", "input_tokens", "cost"))
})

test_that("hf store: read parses the file and sends the Bearer token", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    csv_response(usage_fixture())
  })
  u <- usage_store_hf("x/y")$read()
  expect_equal(u, usage_fixture())
  expect_equal(
    seen$url, "https://huggingface.co/datasets/x/y/resolve/main/usage.csv"
  )
  expect_equal(httr2::req_get_headers(seen, redacted = "reveal")$Authorization, paste("Bearer", fake_token))
})

test_that("hf store: a write sends a header line and a file line", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response(status_code = 200, body = charToRaw("{}"))
  })
  u <- usage_fixture()
  expect_true(usage_store_hf("x/y")$write(u))
  expect_equal(seen$url, "https://huggingface.co/api/datasets/x/y/commit/main")
  expect_true(is_post(seen))
  expect_equal(seen$body$content_type, "application/x-ndjson")
  lines <- strsplit(seen$body$data, "\n")[[1]]
  expect_length(lines, 2)
  head <- jsonlite::fromJSON(lines[1])
  expect_equal(head$key, "header")
  expect_match(head$value$summary, "^usage ")
  file <- jsonlite::fromJSON(lines[2])
  expect_equal(file$key, "file")
  expect_equal(file$value$path, "usage.csv")
  expect_equal(file$value$encoding, "base64")
  csv <- rawToChar(jsonlite::base64_dec(file$value$content))
  expect_equal(readr::read_csv(I(csv), show_col_types = FALSE)$cost, u$cost)
})

test_that("hf store: the token never appears in a warning or message", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500, body = charToRaw(fake_token))
  })
  store <- usage_store_hf("x/y")
  w <- character()
  m <- character()
  withCallingHandlers(
    {
      store$write(usage_fixture())
      store$read()
    },
    warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    },
    message = function(c) {
      m <<- c(m, conditionMessage(c))
      invokeRestart("muffleMessage")
    }
  )
  expect_gt(length(w), 0)
  expect_false(any(grepl("hf_FAKE", c(w, m), fixed = TRUE)))
})

test_that("hf store: a failed write keeps the totals for the next read", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  old <- usage_fixture()[1:2, ]
  httr2::local_mocked_responses(function(req) {
    if (is_post(req)) httr2::response(status_code = 500) else csv_response(old)
  })
  store <- usage_store_hf("x/y")
  new <- usage_fixture()
  new$cost <- new$cost + 1
  expect_warning(ok <- store$write(new))
  expect_false(isTRUE(ok))
  expect_equal(store$read(), new)
})

test_that("hf store: a failed read gives the cache", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) {
    if (is_post(req)) httr2::response(status_code = 200) else {
      httr2::response(status_code = 503)
    }
  })
  store <- usage_store_hf("x/y")
  expect_warning(first <- store$read())
  expect_equal(nrow(first), 0)
  store$write(usage_fixture())
  expect_warning(second <- store$read())
  expect_equal(second, usage_fixture())
})

test_that("hf store: read takes the larger total for each date", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  remote <- usage_fixture()
  remote$cost[3] <- 0.9
  remote$calls[1] <- 50L
  httr2::local_mocked_responses(function(req) {
    if (is_post(req)) httr2::response(status_code = 200) else csv_response(remote)
  })
  store <- usage_store_hf("x/y")
  store$write(usage_fixture())
  u <- store$read()
  expect_equal(u$cost[3], 0.9)
  expect_equal(u$calls[1], 50L)
  expect_equal(u$calls[2], 20L)
})

test_that("hf store: a file with wrong columns gives the cache, no dplyr warning", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw("a,b\n1,2\n"))
  })
  store <- usage_store_hf("x/y")
  w <- character()
  u <- withCallingHandlers(
    store$read(),
    warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(nrow(u), 0)
  expect_length(w, 1)
  expect_match(w, "bad file")
  expect_false(any(grepl("non-missing", w)))
})

test_that("hf store: a bad date or an NA total is a failed read", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw("date,calls,input_tokens,cost\nsoon,1,1,x\n")
    )
  })
  expect_warning(u <- usage_store_hf("x/y")$read(), "bad file")
  expect_equal(nrow(u), 0)
})

test_that("hf store: a timeout on read gives the cache with a safe warning", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  httr2::local_mocked_responses(function(req) stop("Timeout was reached"))
  expect_warning(u <- usage_store_hf("x/y")$read(), "failed")
  expect_equal(nrow(u), 0)
  w <- testthat::capture_warnings(usage_store_hf("x/y")$read())
  expect_false(any(grepl("hf_FAKE", w, fixed = TRUE)))
})

test_that("hf store: the requests have a timeout", {
  withr::local_envvar(HF_USAGE_TOKEN = fake_token)
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response(status_code = 404)
  })
  usage_store_hf("x/y")$read()
  expect_equal(seen$options$timeout_ms, USAGE_TIMEOUT * 1000)
})

test_that("run_allowed refuses when a left amount is NA", {
  st <- list(session_left = 0.25, day_left = NA_real_, month_left = 20)
  r <- run_allowed(0.01, st)
  expect_false(r$ok)
  expect_equal(r$reason, "day")
  r <- run_allowed(0.01, list(session_left = NA_real_, day_left = 2, month_left = 20))
  expect_equal(r$reason, "session")
  expect_false(run_allowed(NA_real_, spend_status(0, empty_usage(), now))$ok)
})
