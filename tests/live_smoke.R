# Live smoke test: 2 paid calls to Jev (one input of each sample).
# NOT part of the suite (it lives outside tests/testthat). Run it by hand,
# only after a budget row exists in calls/README.md:
#
#   Rscript tests/live_smoke.R <path-to-.Renviron> [<path-to-call-log>]
#
# It prints only categories, answers and token counts. It never prints the
# key (only set or unset) and never prints a raw .error text.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Give the path of the .Renviron as the first argument.", call. = FALSE)
}
renviron <- args[[1]]
log <- if (length(args) >= 2) {
  args[[2]]
} else {
  "C:/Users/megahefm/Dropbox/Miami/Code/claude/fine_tuned_jev/replication_materials/calls/typesafe-calls.csv"
}

# Run from the repository root, as the app does
for (r_file in c("config.R", "spec.R", "samples.R", "data.R", "runs.R")) {
  source(file.path("R", r_file))
}
source(file.path("tests", "smoke_log.R"))

if (file.exists(renviron)) readRenviron(renviron)
cat("Key", KEY_VAR, ":", if (nzchar(Sys.getenv(KEY_VAR))) "set" else "unset", "\n")
if (!nzchar(Sys.getenv(KEY_VAR))) {
  stop(KEY_VAR, " is unset. No call was made and nothing was logged.", call. = FALSE)
}
if (!file.exists(log)) stop("The call log does not exist: ", log, call. = FALSE)

# The first input of each sample: one call each
now <- Sys.time()
answers <- purrr::map(c("textiles", "injuries"), \(id) {
  run_chunk(sample_spec(id), 1L) |> dplyr::mutate(sample = id, .before = 1)
}) |>
  dplyr::bind_rows()

# Only categories, answers and tokens
answers |>
  dplyr::transmute(
    sample, id, answer = as.character(.pred_class),
    error = classify_error(.error), tokens = .input_tokens
  ) |>
  print()
cat("Tokens:", chunk_usage(answers)$tokens, "\n")

append_smoke_rows(smoke_row(answers, now), log)
cat("Logged", nrow(answers), "rows in", log, "\n")
