test_that("check_question() enforces the limits of the design", {
  ok <- list(type = "choice", instructions = "Which?", answers = c(a = "", b = ""))
  expect_length(check_question(ok), 0)
  expect_match(check_question(modifyList(ok, list(answers = c(a = "")))), "2 to 6")
  seven <- setNames(rep("", 7), letters[1:7])
  expect_match(check_question(modifyList(ok, list(answers = seven))), "2 to 6")
  expect_match(check_question(modifyList(ok, list(type = "score", answers = seven[1:6]))), "2 to 5")
  long <- strrep("x", 301)
  expect_match(check_question(modifyList(ok, list(answers = c(a = long, b = "")))), "300")
  expect_match(check_question(modifyList(ok, list(instructions = strrep("x", 501)))), "500")
  expect_match(check_question(modifyList(ok, list(answers = c(a = "", A = "")))), "unique")
})

test_that("as_decisions_question() makes the question of the package", {
  q <- list(type = "score", instructions = "How bad?", answers = c(low = "", high = "Very bad."))
  expect_true(S7::S7_inherits(as_decisions_question(q), decisions::question_score))
})

test_that("target_answer() follows the design", {
  noul <- list(type = "noul", instructions = "?", answers = c(true = "", false = ""))
  ch <- list(type = "choice", instructions = "?", answers = c(nominal = "", defective = ""))
  expect_identical(target_answer(noul), "true")
  expect_identical(target_answer(ch), "defective")
  expect_identical(target_answer(ch, "nominal"), "nominal")
  expect_identical(target_answer(ch, "other"), "defective")
})

test_that("the fingerprint changes when the question changes", {
  tex <- sample_spec("textiles")
  edited <- tex
  edited$question$instructions <- "Is there a flaw?"
  expect_false(identical(spec_fingerprint(tex), spec_fingerprint(edited)))
  expect_identical(spec_fingerprint(tex), spec_fingerprint(sample_spec("textiles")))
})
