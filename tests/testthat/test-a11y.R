# Accessibility check: axe-core 4.10.2 in headless Chrome, on the page in
# several states, plus the reflow check at 320 CSS pixels (WCAG 1.4.10, which
# axe does not test). The app runs with SPACE_MOCK=1, so no call is paid.

skip_if_not(nzchar(chromote::find_chrome() %||% ""), "Chrome is not installed")

AXE_FILE <- file.path(APP_ROOT, "tests", "a11y", "axe.min.js")
AXE_TAGS <- c("wcag2a", "wcag2aa", "wcag21aa", "wcag22aa")

# The app in a browser window of the given size
open_app <- function(width = 1280, height = 900, env = parent.frame()) {
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

# Load the textiles sample and show its answers
show_textiles_answers <- function(app) {
  app$click("sample_textiles")
  app$run_js("document.getElementById('samples_menu').open = false")
  app$click("try5")
  app$wait_for_js("document.querySelectorAll('.answers-card .bd img, .answers-card .bd .tile').length > 0 || document.querySelector('#gallery') && document.querySelector('#gallery').children.length > 0", timeout = 60000)
  app$wait_for_idle()
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
  app$wait_for_js("document.querySelectorAll('#table table tr').length > 1", timeout = 60000)
  expect_no_violations(app)
})

test_that("the page does not scroll sideways at 320 CSS pixels", {
  app <- open_app(width = 320, height = 800)
  show_textiles_answers(app)
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
  expect_no_violations(app)
})

test_that("axe finds no violation with the video dialog open", {
  skip_if(!nzchar(VIDEO_URL), "VIDEO_URL is empty until the video is ready (Task 13)")
  app <- open_app()
  app$click(selector = "#open_video")
  app$wait_for_js("document.getElementById('video_dialog').open")
  expect_no_violations(app)
})
