# App logic lives in R/ and is sourced here in a fixed order
# (R/_disable_autoload.R stops Shiny from sourcing the directory a second time).
# APP_ROOT is the app folder; the mock mode finds the mock of the tests there.
APP_ROOT <- normalizePath(".")
APP_FILES <- c("config.R", "spec.R", "samples.R", "data.R", "runs.R", "spending.R", "answers.R", "code.R", "bundle.R", "status.R", "help.R", "render.R", "ui.R", "server.R")
for (r_file in APP_FILES) {
  source(file.path("R", r_file), local = TRUE)
}

# Uploads larger than this are rejected by Shiny
options(shiny.maxRequestSize = MAX_ZIP_MB * 1024^2)

# Two background R processes run the calls to Jev, so a run does not block the
# app. They load the same code; the key reaches them only as the environment
# variable that they inherit.
mirai::daemons(2)
mirai::everywhere(
  for (f in files) source(f),
  .args = list(files = normalizePath(file.path("R", APP_FILES)))
)
shiny::onStop(\() mirai::daemons(0))

# One set of dependencies for all sessions: the stop for the day after a key
# or credit error holds for every visitor (R/server.R)
deps <- app_deps()

shiny::shinyApp(app_ui, \(input, output, session) app_server(input, output, session, deps))
