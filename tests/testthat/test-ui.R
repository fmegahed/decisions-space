# The page: the ids that the server uses, the help text, and the rules for
# accessibility, privacy and labels. The helpers are in helper-ui.R; the
# output builders of R/render.R have their tests in test-render.R.

HELP_KEYS_BRIEF <- c(
  "intro", "data", "question", "references", "references_score", "answers",
  "code", "privacy", "about"
)

test_that("the page has the version in a meta tag that the status workflow reads", {
  meta <- find_tags(app_ui(NULL), "meta") |>
    purrr::keep(\(t) identical(htmltools::tagGetAttribute(t, "name"), "app-version"))
  expect_length(meta, 1)
  expect_identical(htmltools::tagGetAttribute(meta[[1]], "content"), "1.0.0")
  # The same pattern as the grep of .github/workflows/status.yml
  expect_match(page_html(), '<meta name="app-version" content="1.0.0"', fixed = TRUE)
})

test_that("no label or header says How to read", {
  expect_no_match(page_html(), "how to read", ignore.case = TRUE)
})

test_that("the top bar has the tabs Build, About and Record", {
  tabs <- find_tags(app_ui(NULL), "button") |>
    purrr::keep(\(t) identical(htmltools::tagGetAttribute(t, "role"), "tab"))
  expect_identical(unname(purrr::map_chr(tabs, tag_text)), c("Build", "About", "Record"))
})

test_that("the Video button and its dialog show only when VIDEO_URL is set", {
  html <- page_html()
  expect_no_match(html, 'id="open_video"', fixed = TRUE)
  expect_no_match(html, "youtube", fixed = TRUE)
  local_global("video_url", \() "https://www.youtube.com/watch?v=abcdefghijk")
  html <- page_html()
  expect_match(html, 'id="open_video"', fixed = TRUE)
  expect_match(html, '<dialog[^>]*id="video_dialog"')
  expect_match(
    html, 'data-src="https://www.youtube-nocookie.com/embed/abcdefghijk"', fixed = TRUE
  )
  # The embed loads only when the dialog opens: the iframe has no src
  expect_no_match(html, "<iframe[^>]* src=")
})

test_that("video_embed() takes the id from the usual YouTube addresses", {
  embed <- "https://www.youtube-nocookie.com/embed/abcdefghijk"
  expect_identical(video_embed("https://www.youtube.com/watch?v=abcdefghijk"), embed)
  expect_identical(video_embed("https://youtu.be/abcdefghijk?t=3"), embed)
  expect_identical(video_embed("https://www.youtube.com/embed/abcdefghijk"), embed)
  expect_identical(video_embed("https://example.com/video"), "")
})

test_that("the code block takes keyboard focus and has a name", {
  code <- find_tags(app_ui(NULL), "#code")
  expect_length(code, 1)
  expect_identical(htmltools::tagGetAttribute(code[[1]], "tabindex"), "0")
  expect_identical(htmltools::tagGetAttribute(code[[1]], "role"), "region")
  expect_identical(htmltools::tagGetAttribute(code[[1]], "aria-label"), "R code of this run")
})

test_that("the R code card has a heading that names it", {
  card <- find_tags(app_ui(NULL), "section.code-card")[[1]]
  expect_identical(htmltools::tagGetAttribute(card, "aria-labelledby"), "code_title")
  expect_length(find_tags(card, "h2#code_title"), 1)
})

test_that("the R code card is a closed details element with no control in its summary", {
  details <- find_tags(app_ui(NULL), "details.code-details")
  expect_length(details, 1)
  expect_null(htmltools::tagGetAttribute(details[[1]], "open"))
  summary <- htmltools::tagQuery(details[[1]])$children("summary")$selectedTags()
  expect_length(summary, 1)
  html <- as.character(summary[[1]])
  expect_match(html, 'id="code_summary"', fixed = TRUE)
  expect_no_match(html, "<(button|a|input|select|textarea)\\b")
  # Copy, Download and Bundle sit in the same card, outside the summary
  card <- as.character(find_tags(app_ui(NULL), "section.code-card")[[1]])
  for (id in c('class="btn btn-quiet sm copy-btn"', 'id="download_code"', 'id="download_bundle"')) {
    expect_match(card, id, fixed = TRUE)
  }
})

test_that("Copy does not report success while the code is empty", {
  # The code output sits in the closed R code card, and Shiny renders a hidden
  # output only when it shows. Copy must not say "Copied" for an empty text.
  js <- readr::read_file(file.path(APP_ROOT, "www", "app.js"))
  copy <- stringr::str_extract(js, "function copyCode\\(button\\) \\{[\\s\\S]*?\\n  \\}\\n")
  expect_false(is.na(copy))
  guard <- stringr::str_locate(copy, stringr::fixed("if (!text)"))[1, "start"]
  write <- stringr::str_locate(copy, stringr::fixed("writeCopy("))[1, "start"]
  expect_false(is.na(guard))
  expect_lt(guard, write)
  # The guard opens the card, and the copy waits for the code
  expect_match(copy, "details.open = true", fixed = TRUE)
  expect_match(js, "Nothing to copy yet.", fixed = TRUE)
})

test_that("the page loads no font from Google", {
  www <- file.path(APP_ROOT, "www")
  text <- c(
    page_html(),
    readLines(file.path(www, "theme.css"), warn = FALSE),
    readLines(file.path(www, "app.js"), warn = FALSE)
  )
  expect_false(any(stringr::str_detect(text, "fonts\\.googleapis\\.com|fonts\\.gstatic\\.com")))
})

test_that("every font of theme.css is in www/fonts, with the license of each family", {
  www <- file.path(APP_ROOT, "www")
  css <- readr::read_file(file.path(www, "theme.css"))
  urls <- stringr::str_match_all(css, "url\\([\"']?(fonts/[^\"')]+)")[[1]][, 2]
  expect_gte(length(urls), 3)
  expect_true(all(file.exists(file.path(www, urls))))
  expect_true(all(stringr::str_detect(urls, "\\.woff2$")))
  licenses <- c("OFL-source-serif-4.txt", "OFL-source-sans-3.txt", "OFL-ibm-plex-mono.txt")
  expect_true(all(file.exists(file.path(www, "fonts", licenses))))
})

test_that("the page loads its scripts and styles only from the app", {
  html <- page_html()
  sources <- stringr::str_match_all(html, '<(?:script|link)[^>]*(?:src|href)="([^"]+)"')[[1]][, 2]
  expect_gt(length(sources), 0)
  expect_false(any(stringr::str_detect(sources, "^(https?:)?//")))
})

test_that("every image of the page has an alt attribute", {
  page_imgs <- find_tags(app_ui(NULL), "img")
  expect_gte(length(page_imgs), 1)
  alts <- purrr::map(page_imgs, \(t) htmltools::tagGetAttribute(t, "alt"))
  expect_false(any(purrr::map_lgl(alts, is.null)))
})

test_that("the page has the input and output ids of the server", {
  html <- page_html()
  ids <- c(
    "mode", "source", "upload_images", "upload_csv", "csv_text_col", "csv_label_col",
    "paste_text", "qtype", "instructions", "answers_ui", "references_ui", "try5",
    "run_all", "stop", "target", "view", "help_on", "import_file", "intro_hide",
    "samples_menu", "sample_textiles", "sample_injuries",
    "status_chip", "spend_meter", "run_estimate", "stats", "help_answers", "gallery",
    "table", "code_summary", "code", "bundle_files", "download_answers",
    "download_code", "download_bundle", "record_table", "limits_text"
  )
  missing <- purrr::discard(ids, \(id) stringr::str_detect(html, stringr::fixed(sprintf('id="%s"', id))))
  expect_identical(missing, character())
  # The question types and the views use the values of the server
  expect_match(html, 'value="noul"', fixed = TRUE)
  expect_match(html, 'value="choice"', fixed = TRUE)
  expect_match(html, 'value="score"', fixed = TRUE)
  expect_match(html, 'value="gallery"', fixed = TRUE)
  expect_match(html, 'value="table"', fixed = TRUE)
  # The first page lists the sources of Images; the server sends the others with Text
  for (source in c("textiles", "upload")) {
    expect_match(html, sprintf('value="%s"', source), fixed = TRUE)
  }
})

test_that("the first HTML holds the start state, so that no update is sent at the start", {
  ui <- app_ui(NULL)
  html <- page_html(ui)
  expect_match(html, '<option value="upload" selected>', fixed = TRUE)
  expect_match(html, '<input type="radio" name="qtype" value="choice" checked="checked"', fixed = TRUE)
  expect_match(html, '<input type="radio" name="mode" value="image" checked="checked"', fixed = TRUE)
  expect_identical(tag_text(find_tags(ui, "textarea#instructions")[[1]]), "")
  # The sidebar no longer reads the textile question
  expect_no_match(page_html(ui), "woven pattern", fixed = TRUE)
  # The line of the empty state sits outside the Help note
  expect_length(find_tags(ui, "#empty_answers"), 1)
  expect_length(find_tags(ui, ".howto #empty_answers"), 0)
})

test_that("the Help switch is a checkbox with the role switch and the label Help", {
  switch_tag <- find_tags(app_ui(NULL), "#help_on")
  expect_length(switch_tag, 1)
  expect_identical(htmltools::tagGetAttribute(switch_tag[[1]], "type"), "checkbox")
  expect_identical(htmltools::tagGetAttribute(switch_tag[[1]], "role"), "switch")
  label <- find_tags(app_ui(NULL), "label.switch")
  expect_length(label, 1)
  expect_identical(tag_text(label[[1]]), "Help")
  # The help lines and notes carry the class that the switch hides
  helps <- find_tags(app_ui(NULL), ".help")
  expect_gte(length(helps), 5)
})

test_that("the file name box of each file input has a name and no tab stop", {
  # import_file, upload_images and upload_csv
  boxes <- find_tags(app_ui(NULL), "input.form-control") |>
    purrr::keep(\(t) !is.null(htmltools::tagGetAttribute(t, "readonly")))
  expect_length(boxes, 3)
  for (box in boxes) {
    expect_identical(htmltools::tagGetAttribute(box, "aria-label"), "Chosen file")
    expect_identical(htmltools::tagGetAttribute(box, "tabindex"), "-1")
  }
})

test_that("the sidebar comes before the cards in the page", {
  html <- page_html()
  expect_lt(
    stringr::str_locate(html, 'class="card sidebar"')[1, 1],
    stringr::str_locate(html, 'id="intro"')[1, 1]
  )
})

test_that("the footer has the versions, the model and the authors", {
  html <- page_html()
  expect_match(html, sprintf("decisions-space %s", APP_VERSION), fixed = TRUE)
  expect_match(html, sprintf("@ %s", substr(DECISIONS_SHA, 1, 7)), fixed = TRUE)
  expect_match(html, MODEL, fixed = TRUE)
  expect_match(html, "Ying-Ju (Tessa) Chen (University of Dayton)", fixed = TRUE)
})

test_that("help_text gives text for each key", {
  for (key in HELP_KEYS_BRIEF) {
    text <- help_text(key)
    expect_type(text, "character")
    expect_length(text, 1)
    expect_true(nzchar(text))
  }
  expect_error(help_text("nothing"))
})

test_that("the help of the answers says that a run shows the saved run of an unchanged sample", {
  expect_identical(
    help_text("answers"),
    "The answers show here after a run. A run of an unchanged sample shows its saved run, at no cost."
  )
})

test_that("About is built from app_config()", {
  about <- help_text("about")
  cfg <- app_config()
  expect_match(about, cfg$model, fixed = TRUE)
  expect_match(about, "USD 0.25", fixed = TRUE)
  expect_match(about, "USD 2 ", fixed = TRUE)
  expect_match(about, "USD 20 ", fixed = TRUE)
  expect_match(about, "TypeSafe", fixed = TRUE)
  expect_match(about, "confidential", fixed = TRUE)
  expect_match(about, "https://github.com/fmegahed/decisions)", fixed = TRUE)
  expect_match(about, "https://fmegahed.github.io/decisions/record/", fixed = TRUE)
  expect_match(about, "Fadel M. Megahed (Miami University)", fixed = TRUE)
  expect_match(about, "MIT", fixed = TRUE)
  # A change of the configuration changes the text
  local_global("LIMIT_DAY", 3)
  expect_match(help_text("about"), "USD 3 ", fixed = TRUE)
})

test_that("the help text has no contraction and no claim to be the first", {
  text <- paste(purrr::map_chr(HELP_KEYS_BRIEF, help_text), collapse = " ")
  expect_no_match(text, "n't|'re|'ll|'ve|'m\\b")
  expect_no_match(text, "\\bfirst\\s+(\\w+\\s+)?(model|tool|method|package|app)", ignore.case = TRUE)
})
