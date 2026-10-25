test_that("the code of the textile sample parses and runs with a stub", {
  spec <- sample_spec("textiles")
  code <- run_code(spec, date = as.Date("2026-11-03"))
  expect_no_error(parse(text = code))
  seen <- run_stub(code)
  expect_identical(seen$max_size, 768)
  expect_true(seen$upscale)
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
  expect_length(seen$image, 46)
  expect_true(all(file.exists(seen$image)))
  expect_identical(seen$references, spec$references)
  expect_identical(seen[[1]]@model, MODEL)
})

test_that("the textile code has the first comment, the paths comment and system.file()", {
  code <- run_code(sample_spec("textiles"), date = as.Date("2026-11-03"))
  lines <- stringr::str_split_1(code, "\n")
  expect_identical(
    lines[1], "# The run of the decisions Space on 2026-11-03, in R. Run it with your own key."
  )
  expect_identical(lines[2], "library(decisions)")
  expect_true(stringr::str_detect(code, stringr::fixed('system.file("extdata", "textiles", package = "decisions")')))
  expect_true(stringr::str_detect(code, stringr::fixed("# Paths. This run used the sample images of the package")))
  expect_true(stringr::str_detect(code, stringr::fixed("getwd() shows where R looks.")))
  expect_true(stringr::str_detect(code, stringr::fixed(sprintf('decider_typesafe("%s")', MODEL))))
})

test_that("the injuries code has the seed and the subset of the vignette", {
  spec <- sample_spec("injuries")
  code <- run_code(spec)
  expect_no_error(parse(text = code))
  expect_true(stringr::str_detect(code, stringr::fixed("set.seed(2026)")))
  expect_true(stringr::str_detect(code, stringr::fixed("dplyr::slice_sample(n = 20, by = collapsed_nature_title)")))
  expect_true(stringr::str_detect(code, "needs the package dplyr"))
  seen <- run_stub(code)
  expect_length(seen$text, 75)
  expect_identical(seen$max_size, 768)
  expect_true(seen$upscale)
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
  expect_null(seen$image)
})

test_that("the code of a labelled image upload uses images/<label>/<file>", {
  spec <- upload_spec(labelled = TRUE)
  code <- run_code(spec)
  expect_no_error(parse(text = code))
  expect_true(stringr::str_detect(code, stringr::fixed("images/")))
  expect_true(stringr::str_detect(code, stringr::fixed('"images/nominal/a.jpg"')))
  expect_true(stringr::str_detect(code, stringr::fixed('"references/defective/')))
  seen <- run_stub(code)
  expect_setequal(seen$image, c("images/nominal/a.jpg", "images/nominal/b.jpg", "images/defective/c.jpg"))
  expect_identical(seen$max_size, 768)
  expect_true(seen$upscale)
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
  expect_named(seen$references, c("nominal", "defective"))
  expect_true(all(lengths(seen$references) == 2))
})

test_that("the code of an unlabelled upload uses images/<file>", {
  spec <- upload_spec(labelled = FALSE)
  code <- run_code(spec)
  seen <- run_stub(code)
  expect_setequal(seen$image, c("images/a.jpg", "images/b.jpg", "images/c.jpg"))
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
})

test_that("an answer name that is not safe as a folder name is cleaned in the code", {
  spec <- upload_spec(labelled = FALSE)
  names(spec$question$answers) <- c("nominal", "a/b:c")
  names(spec$references) <- c("nominal", "a/b:c")
  seen <- run_stub(run_code(spec))
  expect_true(all(stringr::str_detect(seen$references[["a/b:c"]], "^references/a_b_c/")))
  expect_named(seen[[2]]@answers, c("nominal", "a/b:c"))
})

test_that("the code of pasted text reads data.csv", {
  spec <- text_spec(csv = FALSE)
  code <- run_code(spec)
  expect_no_error(parse(text = code))
  expect_true(stringr::str_detect(code, stringr::fixed('read.csv("data.csv")$text')))
  withr::local_dir(withr::local_tempdir())
  utils::write.csv(data.frame(text = spec$inputs$text), "data.csv", row.names = FALSE)
  seen <- run_stub(code)
  expect_identical(seen$text, spec$inputs$text)
  expect_identical(seen$max_size, 768)
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
})

test_that("the code of a CSV upload reads the text column of data.csv", {
  spec <- text_spec(csv = TRUE)
  code <- run_code(spec)
  expect_true(stringr::str_detect(code, stringr::fixed('read.csv("data.csv")[["review"]]')))
  withr::local_dir(withr::local_tempdir())
  writeLines(c("review,tag", "I want my money back,a", "Thanks a lot,b"), "data.csv")
  seen <- run_stub(code)
  expect_identical(seen$text, c("I want my money back", "Thanks a lot"))
})

test_that("the code of an import uses the references of the import", {
  spec <- upload_spec(labelled = FALSE)
  spec$source <- "import"
  spec$inputs <- empty_inputs()
  code <- run_code(spec)
  expect_no_error(parse(text = code))
  seen <- run_stub(code)
  expect_named(seen$references, c("nominal", "defective"))
  expect_identical(seen$max_size, 768)
  expect_identical(seen[[2]]@instructions, spec$question$instructions)
})

test_that("a score question has no references in the code", {
  spec <- upload_spec(labelled = FALSE)
  spec$question <- list(
    type = "score", instructions = "How bad is the flaw?",
    answers = c(none = "", mild = "", severe = "")
  )
  code <- run_code(spec)
  expect_false(stringr::str_detect(code, "references ="))
  seen <- run_stub(code)
  expect_null(seen$references)
})

test_that("code_lines() counts the lines", {
  expect_identical(code_lines("a\nb\nc"), 3L)
  expect_identical(code_lines("a"), 1L)
  expect_gt(code_lines(run_code(sample_spec("textiles"))), 20)
})
