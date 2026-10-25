# Five checks: the first failed (the Space was private), four were answered
status_fixture <- function() {
  tibble::tibble(
    time_utc = c(
      "2026-11-03T00:00:00Z", "2026-11-03T05:17:00Z", "2026-11-03T17:17:00Z",
      "2026-11-10T05:17:00Z", "2026-11-10T11:30:00Z"
    ),
    http_status = c(404, 200, 200, 200, 200),
    seconds = c(0.2, 3.1, 0.8, 0.9, 1.1),
    answered = c("false", "true", "true", "true", "true"),
    app_version = c("", "1.0.0", "1.0.0", "1.0.0", "1.0.0")
  )
}
NOW <- as.POSIXct("2026-11-10 12:00:00", tz = "UTC")

test_that("read_status returns the rows that read_fn gives", {
  out <- read_status(read_fn = \(u) status_fixture())
  expect_equal(nrow(out), 5)
})

test_that("read_status returns an empty tibble when the read fails", {
  out <- read_status(read_fn = \(u) stop("offline"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c("time_utc", "http_status", "seconds", "answered", "app_version"))
})

test_that("status_summary counts days, answered checks and checks", {
  s <- status_summary(status_fixture(), now = NOW)
  expect_equal(s$days, 7)
  expect_equal(s$answered, 4)
  expect_equal(s$checks, 5)
  expect_equal(nrow(s$last), 5)
})

test_that("status_summary keeps only the last 60 rows", {
  many <- dplyr::bind_rows(rep(list(status_fixture()), 15))
  expect_equal(nrow(status_summary(many, now = NOW)$last), 60)
})

test_that("status_summary counts answered only when the value is true", {
  st <- status_fixture()
  st$answered <- c("true", "TRUE", NA, "yes", "false")
  expect_equal(status_summary(st, now = NOW)$answered, 2)
})

test_that("status_summary also accepts a logical answered column", {
  st <- status_fixture()
  st$answered <- c(FALSE, TRUE, TRUE, NA, TRUE)
  expect_equal(status_summary(st, now = NOW)$answered, 3)
})

test_that("status_summary ignores a row with an unparsable time", {
  st <- status_fixture()
  st$time_utc[2] <- "not a time"
  s <- status_summary(st, now = NOW)
  expect_equal(s$checks, 4)
  expect_equal(s$answered, 3)
  expect_equal(s$days, 6) # the next answered check is 17:17Z
})

test_that("a failed read gives 0 days and the chip new", {
  s <- status_summary(read_status(read_fn = \(u) stop("offline")), now = NOW)
  expect_equal(s$days, 0)
  expect_equal(s$checks, 0)
  expect_equal(status_chip(s), "new")
})

test_that("status_chip gives a short noun phrase", {
  expect_equal(status_chip(list(days = 37)), "in service 37 days")
  expect_equal(status_chip(list(days = 1)), "in service 1 day")
  expect_equal(status_chip(list(days = 0)), "new")
})
