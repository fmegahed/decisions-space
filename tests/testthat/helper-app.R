# Load the app code for the tests: find the repository root, then source R/*.R
# in the order of app.R. testthat runs this file with tests/testthat as the
# working folder. APP_ROOT and APP_FILES go to the global environment, next to
# the app code, as in app.R (mock_deps() finds the mock from APP_ROOT).
assign(
  "APP_ROOT", normalizePath(if (file.exists("app.R")) "." else file.path("..", "..")),
  envir = globalenv()
)
assign(
  "APP_FILES",
  c("config.R", "spec.R", "samples.R", "data.R", "runs.R", "spending.R", "answers.R", "code.R", "bundle.R", "status.R", "help.R", "render.R", "ui.R", "server.R"),
  envir = globalenv()
)
for (r_file in APP_FILES) {
  source(file.path(APP_ROOT, "R", r_file), local = FALSE)
}
