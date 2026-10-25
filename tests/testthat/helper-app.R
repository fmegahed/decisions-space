# Load the app code for the tests: find the repository root, then source R/*.R
# in the order of app.R. testthat runs this file with tests/testthat as the
# working folder.
APP_ROOT <- normalizePath(
  if (file.exists("app.R")) "." else file.path("..", "..")
)
for (r_file in c("config.R", "spec.R", "samples.R")) {
  source(file.path(APP_ROOT, "R", r_file), local = FALSE)
}
