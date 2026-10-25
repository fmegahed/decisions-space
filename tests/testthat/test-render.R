# The output builders of R/render.R: the dynamic inputs of the question and
# the parts of the outputs. The helpers are in helper-ui.R.

test_that("answer_rows gives the inputs of each answer", {
  choice <- sample_question("textiles")
  html <- as.character(answer_rows(choice))
  for (id in c("answer_name_1", "answer_desc_1", "answer_name_2", "answer_desc_2", "answer_add")) {
    expect_match(html, sprintf('id="%s"', id), fixed = TRUE)
  }
  # Two answers is the minimum of a choice question: no remove button
  expect_no_match(html, "answer_remove_", fixed = TRUE)
  three <- list(type = "choice", instructions = "x", answers = c(a = "", b = "", c = ""))
  html <- as.character(answer_rows(three))
  for (i in 1:3) expect_match(html, sprintf('id="answer_remove_%d"', i), fixed = TRUE)
  # Six answers is the maximum: no add button
  six <- list(type = "choice", instructions = "x", answers = stats::setNames(rep("", 6), letters[1:6]))
  expect_no_match(as.character(answer_rows(six)), 'id="answer_add"', fixed = TRUE)
  # Yes / no has fixed names: descriptions only, no buttons
  noul <- list(type = "noul", instructions = "x", answers = c(true = "", false = ""))
  html <- as.character(answer_rows(noul))
  expect_no_match(html, "answer_name_", fixed = TRUE)
  expect_match(html, 'id="answer_desc_2"', fixed = TRUE)
  expect_no_match(html, "answer_(add|remove)")
})

test_that("reference_rows gives one file input ref_<answer> for each answer", {
  html <- as.character(reference_rows(sample_question("textiles")))
  expect_match(html, 'id="ref_nominal"', fixed = TRUE)
  expect_match(html, 'id="ref_defective"', fixed = TRUE)
  expect_identical(ref_input_id(c("crushing injury", "burn")), c("ref_crushing_injury", "ref_burn"))
  score <- list(type = "score", instructions = "x", answers = c(low = "", high = ""))
  expect_length(reference_rows(score), 0)
})

test_that("the file name box of each reference input has a name and no tab stop", {
  rows <- shiny::tagList(reference_rows(sample_question("textiles")))
  boxes <- find_tags(rows, "input.form-control") |>
    purrr::keep(\(t) !is.null(htmltools::tagGetAttribute(t, "readonly")))
  expect_length(boxes, 2)
  for (box in boxes) {
    expect_identical(htmltools::tagGetAttribute(box, "aria-label"), "Chosen file")
    expect_identical(htmltools::tagGetAttribute(box, "tabindex"), "-1")
  }
})

test_that("every image of the gallery and of the references has an alt attribute", {
  textiles <- sample_spec("textiles")
  table <- answers_table(textiles, sample_answers("textiles"), "defective") |>
    utils::head(3)
  gallery_imgs <- find_tags(
    shiny::tagList(gallery_ui(table, "defective", src = c("a.jpg", "b.jpg", "c.jpg"))), "img"
  )
  ref_imgs <- find_tags(shiny::tagList(reference_rows(
    textiles$question, list(nominal = c("n1.jpg", "n2.jpg"), defective = "d1.jpg")
  )), "img")
  expect_length(gallery_imgs, 3)
  expect_length(ref_imgs, 3)
  alts <- purrr::map(c(gallery_imgs, ref_imgs), \(t) htmltools::tagGetAttribute(t, "alt"))
  expect_false(any(purrr::map_lgl(alts, is.null)))
  # The images of the answers name the file, the answer and the probability
  expect_identical(
    htmltools::tagGetAttribute(gallery_imgs[[1]], "alt"), alt_text(table[1, ], "defective")
  )
})

test_that("the output helpers give the parts of the mockup", {
  meter <- as.character(spend_meter_ui(0.02))
  expect_match(meter, "$0.02 of $0.25", fixed = TRUE)
  stats <- list(n = 46, cost = 0.016, accuracy = 0.8, auc = 0.75)
  html <- as.character(stats_ui(stats, "image", note = "Saved run"))
  for (text in c("0.75", "AUC", "0.80", "accuracy", "46", "images", "$0.016", "Saved run")) {
    expect_match(html, text, fixed = TRUE)
  }
  text_stats <- list(n = 75, cost = 0, accuracy = NA_real_, auc = NA_real_)
  html <- as.character(stats_ui(text_stats, "text"))
  expect_no_match(html, "AUC|accuracy")
  expect_match(html, "texts", fixed = TRUE)
  files <- as.character(bundle_files_ui(
    c("textiles.R", "textiles-setup.json", "textiles-answers.csv", "README.md"),
    "textiles-2026-11-03.zip"
  ))
  expect_match(files, "textiles-2026-11-03.zip", fixed = TRUE)
  expect_match(files, "read_setup()", fixed = TRUE)
})
