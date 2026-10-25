test_that("the samples match the saved runs of the vignettes", {
  tex <- sample_spec("textiles")
  expect_identical(tex$question$instructions, "Does the woven pattern in `inspection_image` have a flaw?")
  expect_identical(names(tex$question$answers), c("nominal", "defective"))
  expect_identical(basename(tex$references$nominal), c("nominal_textile_0731.jpg", "nominal_textile_0630.jpg"))
  expect_identical(basename(tex$references$defective), c("local_textile_0471.jpg", "global_textile_0330.jpg"))
  expect_equal(nrow(tex$inputs), 46)
  expect_identical(nrow(sample_answers("textiles")), 46L)
  inj <- sample_spec("injuries")
  expect_equal(nrow(inj$inputs), 75)
  expect_identical(names(inj$question$answers), c("amputation", "fracture", "crushing injury", "thermal burn", "concussion"))
  expect_true(all(file.exists(tex$inputs$path)))
})

test_that("the samples have the shape of a spec", {
  for (id in c("textiles", "injuries")) {
    s <- sample_spec(id)
    expect_identical(s$source, id)
    expect_identical(names(s$inputs), c("id", "path", "text", "file", "label"))
    expect_false(anyDuplicated(s$inputs$id) > 0)
    expect_length(check_question(s$question), 0)
    expect_true(all(s$inputs$label %in% names(s$question$answers)))
  }
  expect_identical(sample_spec("textiles")$mode, "image")
  expect_identical(sample_spec("injuries")$mode, "text")
})

test_that("sample_answers() follows the row order of the inputs", {
  for (id in c("textiles", "injuries")) {
    expect_identical(sample_answers(id)$id, sample_spec(id)$inputs$id)
  }
})

test_that("sample_unchanged() is TRUE only for the setup of a sample", {
  tex <- sample_spec("textiles")
  expect_true(sample_unchanged(tex))
  expect_true(sample_unchanged(sample_spec("injuries")))
  # Edits of spaces only do not count
  spaced <- tex
  spaced$question$instructions <- paste0("  ", gsub(" ", "   ", tex$question$instructions), " ")
  spaced$question$answers[["nominal"]] <- paste0(tex$question$answers[["nominal"]], "  ")
  expect_true(sample_unchanged(spaced))
  # Every other change counts
  change <- function(path, value) {
    s <- tex
    s[[path]] <- value
    s
  }
  q <- tex$question
  changed <- list(
    change("question", modifyList(q, list(instructions = paste(q$instructions, "Now.")))),
    change("question", modifyList(q, list(answers = c(q$answers[1], defective = "A flaw.")))),
    change("question", modifyList(q, list(answers = stats::setNames(q$answers, c("nominal", "flawed"))))),
    change("question", modifyList(q, list(type = "score"))),
    change("references", list(nominal = tex$references$nominal, defective = tex$references$defective[1])),
    change("inputs", tex$inputs[-1, ]),
    change("mode", "text"),
    change("source", "upload")
  )
  for (s in changed) expect_false(sample_unchanged(s))
  expect_false(sample_unchanged(upload_spec()))
})

test_that("SAMPLE_FACTS holds the numbers of the saved runs", {
  expect_identical(SAMPLE_FACTS$textiles, c(auc = 0.75, accuracy = 0.80))
  expect_identical(SAMPLE_FACTS$injuries, c(accuracy = 0.96))
})

test_that("the help sentences hold the facts of the saved runs", {
  tex <- paste(SAMPLE_FACTS_TEXT$textiles, collapse = " ")
  for (fact in c("14 local flaws", "0.14 to 0.26", "0.75", "0.80")) {
    expect_match(tex, fact, fixed = TRUE)
  }
  expect_match(paste(SAMPLE_FACTS_TEXT$injuries, collapse = " "), "0.96", fixed = TRUE)
})
