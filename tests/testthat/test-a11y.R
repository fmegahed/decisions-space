# Accessibility check: axe-core 4.10.2 in headless Chrome, on the page in
# several states, plus the reflow check at 320 CSS pixels (WCAG 1.4.10, which
# axe does not test). The app runs with SPACE_MOCK=1, so no call is paid.

# The path of Chrome, or "" when it is not there (also for a path that does not exist)
chrome_path <- function() {
  path <- tryCatch(chromote::find_chrome(), error = \(e) NULL)
  if (is.null(path) || !nzchar(path) || !file.exists(path)) "" else path
}

# No Chrome: skip on a local run, fail on CI, so that a broken Chrome path
# on CI cannot give a green run without the accessibility check
require_chrome <- function(path = chrome_path(), ci = identical(Sys.getenv("CI"), "true")) {
  if (nzchar(path)) return(invisible(path))
  if (ci) stop("Chrome is not found on CI; the accessibility check did not run", call. = FALSE)
  skip("Chrome is not installed")
}

test_that("a missing Chrome fails on CI and skips on a local run", {
  expect_error(require_chrome("", ci = TRUE), "Chrome is not found on CI")
  expect_identical(tryCatch(require_chrome("", ci = FALSE), skip = function(s) "skipped"), "skipped")
  withr::local_envvar(CHROMOTE_CHROME = file.path(tempdir(), "no-such-chrome"), CI = "true")
  expect_identical(chrome_path(), "")
  expect_error(require_chrome(), "Chrome is not found on CI")
})

AXE_FILE <- file.path(APP_ROOT, "tests", "a11y", "axe.min.js")
AXE_TAGS <- c("wcag2a", "wcag2aa", "wcag21aa", "wcag22aa")

# The app in a browser window of the given size
open_app <- function(width = 1280, height = 900, env = parent.frame()) {
  require_chrome()
  # NOT_CRAN: shinytest2 skips its driver on CRAN unless this is set
  withr::local_envvar(SPACE_MOCK = "1", NOT_CRAN = "true", .local_envir = env)
  app <- shinytest2::AppDriver$new(APP_ROOT, variant = NULL, width = width, height = height, load_timeout = 60000)
  withr::defer(app$stop(), envir = env)
  app
}

# The violations of axe on the page now, as lines "rule: target"
axe_violations <- function(app) {
  app$run_js(paste(readLines(AXE_FILE, warn = FALSE), collapse = "\n"))
  app$run_js(sprintf(
    "window.axeOut = null; axe.run(document, {runOnly: {type: 'tag', values: %s}}).then(function(r) { window.axeOut = r.violations.map(function(v) { return v.nodes.map(function(n) { return v.id + ': ' + n.target.join(' ') + ' | ' + n.html.slice(0, 100); }); }).flat(); });",
    jsonlite::toJSON(AXE_TAGS)
  ))
  app$wait_for_js("window.axeOut !== null", timeout = 60000)
  as.character(unlist(app$get_js("window.axeOut")))
}

expect_no_violations <- function(app) {
  found <- axe_violations(app)
  expect(length(found) == 0, paste0("axe found:\n", paste(found, collapse = "\n")))
}

# Load the textiles sample and show its answers: Try on 5 of the unchanged
# sample shows the saved run, with no call
show_textiles_answers <- function(app) {
  app$click("sample_textiles")
  app$run_js("document.getElementById('samples_menu').open = false")
  app$click("try5")
  # The run has ended and all 5 tiles are drawn
  app$wait_for_js("!document.body.classList.contains('is-running') && document.querySelectorAll('#gallery .tile').length === 5", timeout = 60000)
  app$wait_for_idle()
  expect_match(app$get_text("#stats"), "Saved run of", fixed = TRUE)
}

# Load the injuries sample and show all its saved answers in Table view
show_injuries_table <- function(app) {
  app$click("sample_injuries")
  app$run_js("document.getElementById('samples_menu').open = false")
  app$click("run_all")
  app$set_inputs(view = "table")
  app$wait_for_js("!document.body.classList.contains('is-running') && document.querySelectorAll('#table table tbody tr').length === 75", timeout = 60000)
  app$wait_for_idle()
  expect_match(app$get_text("#stats"), "Saved run of", fixed = TRUE)
}

test_that("axe finds no violation on the start page", {
  app <- open_app()
  expect_no_violations(app)
})

test_that("axe finds no violation with the textiles answers", {
  app <- open_app()
  show_textiles_answers(app)
  expect_no_violations(app)
})

test_that("axe finds no violation with the R code open", {
  app <- open_app()
  show_textiles_answers(app)
  app$run_js("document.querySelector('.code-details').open = true")
  app$wait_for_js("document.getElementById('code').textContent.length > 0", timeout = 30000)
  expect_no_violations(app)
})

test_that("axe finds no violation on the About tab", {
  app <- open_app()
  app$click(selector = "#tab-about")
  expect_no_violations(app)
})

test_that("axe finds no violation on the injuries sample in Table view", {
  app <- open_app()
  app$click("sample_injuries")
  app$run_js("document.getElementById('samples_menu').open = false")
  app$click("try5")
  app$set_inputs(view = "table")
  app$wait_for_js("!document.body.classList.contains('is-running') && document.querySelectorAll('#table table tbody tr').length === 5", timeout = 60000)
  expect_match(app$get_text("#stats"), "Saved run of", fixed = TRUE)
  expect_no_violations(app)
})

test_that("the page does not scroll sideways at 320 CSS pixels", {
  app <- open_app(width = 320, height = 800)
  show_textiles_answers(app)
  expect_equal(app$get_js("window.innerWidth"), 320)
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
  expect_no_violations(app)
})

test_that("the full texts wrap in the table at 320 CSS pixels", {
  app <- open_app(width = 320, height = 800)
  show_injuries_table(app)
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
  # The longest text (550 characters) is whole in its cell
  lengths <- app$get_js("Array.from(document.querySelectorAll('#table tbody td:first-child')).map(function(td) { return td.textContent.trim().length; })")
  expect_identical(max(unlist(lengths)), max(nchar(sample_spec("injuries")$inputs$text)))
  # The column of the texts is wide enough to read (at least 16 em)
  expect_true(app$get_js("(function() { var td = document.querySelector('#table tbody td'); return td.offsetWidth >= 16 * parseFloat(getComputedStyle(td).fontSize); })()"))
  expect_no_violations(app)
})

test_that("axe finds no violation with the video dialog open", {
  skip_if(!nzchar(VIDEO_URL), "VIDEO_URL is empty until the video is ready (Task 13)")
  app <- open_app()
  app$click(selector = "#open_video")
  app$wait_for_js("document.getElementById('video_dialog').open")
  expect_no_violations(app)
})
