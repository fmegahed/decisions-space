# The call-log rows of tests/live_smoke.R (the script itself makes paid calls
# and never runs in the suite)
source(file.path(APP_ROOT, "tests", "smoke_log.R"), local = TRUE)

ok_row <- tibble::tibble(
  id = "frame1.jpg", .model = "jev-1.14.0", .input_tokens = 2000,
  .seconds = 0.4321, .error = NA_character_
)
now <- as.POSIXct("2026-11-02 10:11:12", tz = "UTC")

test_that("an answered call becomes a row with status 200 and the cost", {
  row <- smoke_row(ok_row, now)
  expect_named(row, SMOKE_COLUMNS)
  expect_identical(row$time_utc, "2026-11-02T10:11:12Z")
  expect_identical(row$call_id, "frame1.jpg")
  expect_identical(row$purpose, "space-smoke")
  expect_identical(row$model, "jev-1.14.0")
  expect_identical(row$request_id, "")
  expect_identical(row$status, "200")
  expect_identical(row$latency_ms, "432")
  expect_identical(row$input_tokens, "2000")
  expect_identical(row$output_tokens, "")
  expect_equal(as.numeric(row$cost_usd), 2000 * PRICE_PER_TOKEN)
})

test_that("a failed call gets the category, never the raw error text", {
  bad <- ok_row |>
    dplyr::mutate(
      .error = "HTTP 429 secret-key-fragment", .input_tokens = NA_real_,
      .seconds = NA_real_
    )
  row <- smoke_row(bad, now)
  expect_identical(row$status, "rate")
  expect_identical(row$latency_ms, "")
  expect_identical(row$input_tokens, "")
  expect_identical(row$cost_usd, "")
  expect_false(any(grepl("secret", unlist(row))))
})

test_that("append_smoke_rows adds lines and keeps the old ones", {
  log <- withr::local_tempfile(fileext = ".csv")
  writeLines(paste(SMOKE_COLUMNS, collapse = ","), log)
  append_smoke_rows(dplyr::bind_rows(smoke_row(ok_row, now), smoke_row(ok_row, now)), log)
  lines <- readLines(log)
  expect_length(lines, 3)
  expect_identical(lines[1], paste(SMOKE_COLUMNS, collapse = ","))
  expect_match(lines[2], "^2026-11-02T10:11:12Z,frame1.jpg,space-smoke,jev-1.14.0,,200,432,2000,,")
})
