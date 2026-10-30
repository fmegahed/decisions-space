test_that("the limits and the model are the values of the design", {
  expect_identical(MODEL, "jev-1.14.0")
  expect_equal(c(LIMIT_SESSION, LIMIT_DAY, LIMIT_MONTH), c(0.25, 2, 20))
  expect_equal(PRICE_PER_TOKEN * 1e6, 0.042)
  expect_identical(IMAGE_SETTINGS, list(max_size = 768, upscale = TRUE, crop = "none"))
  expect_identical(VIDEO_URL, "https://youtu.be/4WE-ykXzK1g")
})

test_that("the model id is written only in R/config.R", {
  withr::local_dir(APP_ROOT)
  files <- c("app.R", list.files("R", full.names = TRUE)) |> setdiff("R/config.R")
  # The check must read at least app.R, or it checks nothing
  expect_gt(length(files), 0)
  hits <- purrr::keep(files, \(f) any(stringr::str_detect(readLines(f), "jev-[0-9]")))
  expect_length(hits, 0)
})
