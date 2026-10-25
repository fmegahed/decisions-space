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

test_that("text is cut at 150 characters", {
  spec <- sample_spec("injuries")
  tab <- answers_table(spec, sample_answers("injuries"), "thermal burn")
  expect_true(all(nchar(tab$text) <= 150, na.rm = TRUE))
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
