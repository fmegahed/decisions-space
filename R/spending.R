# Spending: the usage totals, the limits, and the stores that keep the totals.
# A usage store is list(read = function() <tibble>, write = function(usage)).
# The tibble has one row for each UTC date: date, calls, input_tokens, cost.
# The usage token is read only here. It is never in a message or a warning.

# A usage with no rows
empty_usage <- function() {
  tibble::tibble(
    date = character(), calls = integer(),
    input_tokens = double(), cost = double()
  )
}

# UTC date (YYYY-MM-DD) of a time
utc_date <- function(now) format(now, "%Y-%m-%d", tz = "UTC")

# Add a chunk to the row of the UTC date of `now`
usage_add <- function(usage, now, calls, tokens, cost) {
  day <- utc_date(now)
  add <- tibble::tibble(
    date = day, calls = as.integer(calls),
    input_tokens = as.numeric(tokens), cost = as.numeric(cost)
  )
  dplyr::bind_rows(usage, add) |>
    dplyr::summarise(
      dplyr::across(c(calls, input_tokens, cost), sum), .by = date
    ) |>
    dplyr::arrange(date)
}

# For each date, the larger totals of two usages (the totals only grow)
usage_merge <- function(a, b) {
  dplyr::bind_rows(a, b) |>
    dplyr::summarise(
      dplyr::across(c(calls, input_tokens, cost), max), .by = date
    ) |>
    dplyr::arrange(date)
}

# Read the latest totals, add the chunk, write, and return the new usage.
# Each session adds to the latest totals, so it never writes a stale copy.
usage_record <- function(store, now, calls, tokens, cost) {
  usage <- store$read() |>
    usage_add(now, calls, tokens, cost)
  store$write(usage)
  usage
}

# Seconds to wait for Hugging Face (a stalled call would block every visitor)
USAGE_TIMEOUT <- 10

# Parse the text of usage.csv; NULL when the columns, a date or a total is bad
parse_usage <- function(text) {
  usage <- tryCatch(
    suppressWarnings(readr::read_csv(
      I(text),
      col_types = readr::cols(
        date = "c", calls = "i", input_tokens = "d", cost = "d"
      ),
      na = character(), show_col_types = FALSE
    )),
    error = \(e) NULL
  )
  ok <- !is.null(usage) &&
    all(c("date", "calls", "input_tokens", "cost") %in% names(usage)) &&
    all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", usage$date)) &&
    !anyNA(usage[c("calls", "input_tokens", "cost")])
  if (!ok) return(NULL)
  usage[c("date", "calls", "input_tokens", "cost")]
}

# Store in memory (tests and the local mock mode)
usage_store_memory <- function(usage = empty_usage()) {
  list(
    read = function() usage,
    write = function(new) {
      usage <<- new
      invisible(TRUE)
    }
  )
}

# Store in a private Hugging Face dataset (the file usage.csv).
# The cache holds the last totals written or read: if a request fails, the app
# goes on with the cache.
usage_store_hf <- function(repo = USAGE_REPO,
                           token = Sys.getenv(USAGE_TOKEN_VAR)) {
  cache <- empty_usage()
  auth <- paste("Bearer", token)

  # Send a request; NULL when it fails. The text of a failure has no token.
  send <- function(req, what) {
    resp <- tryCatch(
      req |>
        httr2::req_headers(Authorization = auth) |>
        httr2::req_timeout(USAGE_TIMEOUT) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(),
      error = \(e) NULL
    )
    if (is.null(resp)) {
      warning("usage store: ", what, " failed (no response)", call. = FALSE)
    }
    resp
  }

  read <- function() {
    url <- paste0("https://huggingface.co/datasets/", repo, "/resolve/main/usage.csv")
    resp <- send(httr2::request(url), "read")
    if (is.null(resp)) return(cache)
    status <- httr2::resp_status(resp)
    if (status == 404) return(cache)
    if (status >= 400) {
      warning("usage store: read failed (HTTP ", status, ")", call. = FALSE)
      return(cache)
    }
    remote <- parse_usage(httr2::resp_body_string(resp))
    if (is.null(remote)) {
      warning("usage store: read failed (bad file)", call. = FALSE)
      return(cache)
    }
    cache <<- usage_merge(remote, cache)
    cache
  }

  write <- function(usage) {
    cache <<- usage_merge(cache, usage)
    csv <- readr::format_csv(cache)
    lines <- list(
      list(key = "header", value = list(summary = paste("usage", utc_date(Sys.time())))),
      list(key = "file", value = list(
        path = "usage.csv",
        content = jsonlite::base64_enc(charToRaw(csv)),
        encoding = "base64"
      ))
    ) |>
      purrr::map_chr(\(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE)))
    body <- paste0(paste(lines, collapse = "\n"), "\n")
    url <- paste0("https://huggingface.co/api/datasets/", repo, "/commit/main")
    resp <- send(
      httr2::request(url) |> httr2::req_body_raw(body, "application/x-ndjson"),
      "write"
    )
    if (is.null(resp)) return(invisible(FALSE))
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      warning("usage store: write failed (HTTP ", status, ")", call. = FALSE)
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }

  list(read = read, write = write)
}

# Money left (USD) in the session, the UTC day and the UTC month of `now`
spend_status <- function(session_cost, usage, now = Sys.time()) {
  day <- utc_date(now)
  spent <- function(rows) sum(usage$cost[rows])
  list(
    session_left = LIMIT_SESSION - session_cost,
    day_left = LIMIT_DAY - spent(usage$date == day),
    month_left = LIMIT_MONTH - spent(substr(usage$date, 1, 7) == substr(day, 1, 7))
  )
}

# Is a run with this estimated cost allowed? The first limit that fails wins.
# An NA amount fails the limit, so the check fails closed.
run_allowed <- function(estimate, status) {
  fits <- \(left) isTRUE(estimate <= left)
  reason <- dplyr::case_when(
    !fits(status$session_left) ~ "session",
    !fits(status$day_left) ~ "day",
    !fits(status$month_left) ~ "month",
    .default = NA_character_
  )
  list(ok = is.na(reason), reason = reason)
}

# The text for a refused run (the three texts of the design, section 6)
limit_message <- function(reason) {
  c(
    session = "The limit of this visit is reached.",
    day = "The limit of today is reached. Try again tomorrow.",
    month = "The limit of this month is reached."
  )[[reason]]
}
