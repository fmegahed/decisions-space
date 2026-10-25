# Helpers for the tests of the page (test-ui.R) and of the output builders
# (test-render.R)

# The page as one HTML string (head and body)
page_html <- function(ui = app_ui(NULL)) {
  rendered <- htmltools::renderTags(ui)
  paste(rendered$head, rendered$html)
}

# The tags of the page that match a CSS selector
find_tags <- function(ui, selector) {
  htmltools::tagQuery(ui)$find(selector)$selectedTags()
}

# The text of a tag and of its children, with no markup
tag_text <- function(tag) {
  stringr::str_squish(stringr::str_remove_all(as.character(tag), "<[^>]+>"))
}

# Replace a value of the app code (sourced into the global environment) for
# one test, and put it back at the end of the test
local_global <- function(name, value, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, value, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
}
