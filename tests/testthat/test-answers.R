test_that("the textile table is sorted with the local flaws on top", {
  spec <- sample_spec("textiles")
  tab <- answers_table(spec, sample_answers("textiles"), "defective")
  expect_equal(nrow(tab), 46)
  expect_true(all(stringr::str_detect(tab$file[1:14], "^local_")))
  expect_false(is.unsorted(rev(tab$p_target), na.rm = TRUE))
  expect_true(all(c("id", "file", "text", "answer", "p_target", "p_nominal",
                    "p_defective", "label", "tokens", "seconds", "problem") %in% names(tab)))
})

test_that("pred_column() cleans the answer names", {
  expect_identical(pred_column(c("crushing injury", "Yes")), c(".pred_crushing_injury", ".pred_Yes"))
})

test_that("the stats of the saved textile run are 0.80 and 0.75", {
  spec <- sample_spec("textiles")
  tab <- answers_table(spec, sample_answers("textiles"), "defective")
  st <- answer_stats(tab, spec, "defective")
  expect_equal(st$n, 46)
  expect_equal(round(st$accuracy, 2), 0.80)
  expect_equal(round(st$auc, 2), 0.75)
  expect_true(is.numeric(st$cost) && st$cost >= 0)
})

test_that("the injuries stats have an accuracy and no AUC", {
  spec <- sample_spec("injuries")
  tab <- answers_table(spec, sample_answers("injuries"), "thermal burn")
  expect_true("p_crushing_injury" %in% names(tab))
  st <- answer_stats(tab, spec, "thermal burn")
  expect_equal(round(st$accuracy, 2), 0.96)
  expect_true(is.na(st$auc))
})

test_that("an unlabelled spec gives no accuracy and no AUC", {
  spec <- sample_spec("textiles")
  spec$inputs$label <- NA_character_
  tab <- answers_table(spec, sample_answers("textiles"), "defective")
  st <- answer_stats(tab, spec, "defective")
  expect_true(is.na(st$accuracy))
  expect_true(is.na(st$auc))
})

test_that("a text of 550 characters stays whole in the table and the CSV", {
  spec <- sample_spec("injuries")
  long <- spec$inputs$text[nchar(spec$inputs$text) == 550][1]
  expect_false(is.na(long))
  tab <- answers_table(spec, sample_answers("injuries"), "thermal burn")
  expect_true(long %in% tab$text)
  expect_true(long %in% table_view(tab, spec)$Input)
  csv <- readr::read_csv(
    I(answers_csv(tab)), col_types = readr::cols(.default = "c"), trim_ws = FALSE
  )
  expect_true(long %in% csv$text)
})

test_that("an error never reaches the table or the CSV", {
  spec <- sample_spec("textiles")
  ans <- sample_answers("textiles")
  ans$.error <- NA_character_
  ans$.error[1] <- "HTTP 401 ... wxyz"
  tab <- answers_table(spec, ans, "defective")
  expect_identical(tab$problem[!is.na(tab$problem)], "The demo is not available right now.")
  expect_false(stringr::str_detect(answers_csv(tab), "wxyz"))
  expect_false(".error" %in% names(tab))
})

test_that("alt_text() names the file, the answer and the probability", {
  spec <- sample_spec("textiles")
  tab <- answers_table(spec, sample_answers("textiles"), "defective")
  txt <- alt_text(tab[1, ], "defective")
  expect_match(txt, paste0("^", tab$file[1], ": answer ", tab$answer[1], ", probability of defective "))
})

test_that("help_note() adds the sample facts only for the saved target", {
  spec <- sample_spec("textiles")
  generic <- "The probability is the raw probability of Jev for defective. It is not calibrated."
  expect_match(help_note(spec, list(), "defective"), generic, fixed = TRUE)
  expect_false(stringr::str_detect(help_note(spec, list(), "defective"), "saved run"))
  expect_match(help_note(spec, list(), "defective", saved = TRUE), "AUC of 0.75")
  expect_false(stringr::str_detect(help_note(spec, list(), "nominal", saved = TRUE), "saved run"))
  spec$source <- "upload"
  expect_false(stringr::str_detect(help_note(spec, list(), "defective", saved = TRUE), "saved run"))
})

test_that("help_note() names the reuse of a saved run, and the facts only of the full run", {
  spec <- sample_spec("textiles")
  reuse <- "The app reuses this run to save cost. Change the question or a description to ask Jev again."
  full <- help_note(spec, list(), "defective", saved = TRUE)
  expect_match(full, "AUC of 0.75", fixed = TRUE)
  expect_match(full, reuse, fixed = TRUE)
  five <- help_note(spec, list(), "defective", saved = TRUE, full = FALSE)
  expect_match(five, reuse, fixed = TRUE)
  expect_no_match(five, "AUC of 0.75", fixed = TRUE)
  expect_no_match(five, "14 local flaws", fixed = TRUE)
  expect_no_match(help_note(spec, list(), "defective"), reuse, fixed = TRUE)
})
