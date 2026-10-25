# Rows of the call log (fine_tuned_jev/replication_materials/calls/) for
# tests/live_smoke.R. Needs R/config.R and R/runs.R. The rows hold no input
# and no raw .error text: only the category.

SMOKE_PURPOSE <- "space-smoke"
SMOKE_COLUMNS <- c(
  "time_utc", "call_id", "purpose", "model", "request_id", "status",
  "latency_ms", "input_tokens", "output_tokens", "cost_usd"
)

# One log row (all text) for each row of run_chunk(). Empty means not known.
smoke_row <- function(answers, now = Sys.time()) {
  text <- \(x, f = as.character) ifelse(is.na(x), "", f(x))
  tokens <- answers$.input_tokens
  tibble::tibble(
    time_utc = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    call_id = answers$id,
    purpose = SMOKE_PURPOSE,
    model = answers$.model,
    request_id = "",
    status = ifelse(is.na(answers$.error), "200", classify_error(answers$.error)),
    latency_ms = text(answers$.seconds, \(x) as.character(round(x * 1000))),
    input_tokens = text(tokens),
    output_tokens = "",
    cost_usd = text(tokens, \(x) as.character(signif(x * PRICE_PER_TOKEN, 5)))
  )
}

# Append rows to the log. The old lines stay as they are.
append_smoke_rows <- function(rows, log) {
  readr::write_csv(rows[SMOKE_COLUMNS], log, append = TRUE, na = "")
  invisible(log)
}
