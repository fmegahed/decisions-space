# Uptime record. A GitHub workflow adds a row to status/status.csv twice a day.
# The app reads this file from GitHub and shows the days in service (chip) and
# the last checks (Uptime tab).

# The columns of status.csv
STATUS_COLUMNS <- c("time_utc", "http_status", "seconds", "answered", "app_version")

# Read the status file. A failed read gives an empty tibble: the chip then
# shows "new" and the app does not crash. All columns are read as text, so
# that an odd value cannot stop the read.
read_status <- function(url = STATUS_URL,
                        read_fn = \(u) readr::read_csv(
                          u, col_types = readr::cols(.default = "c")
                        )) {
  empty <- tibble::tibble(
    time_utc = character(), http_status = character(), seconds = character(),
    answered = character(), app_version = character()
  )
  tryCatch(
    read_fn(url),
    error = \(e) empty
  )
}

# Days in service, checks answered, checks made, and the last 60 checks.
# A row with a time that cannot be read is ignored. A check counts as answered
# only when its value is true (in any case).
status_summary <- function(status, now = Sys.time()) {
  checks <- status |>
    dplyr::mutate(
      time = as.POSIXct(
        as.character(.data$time_utc), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
      ),
      ok = stringr::str_to_lower(as.character(.data$answered)) %in% "true"
    ) |>
    dplyr::filter(!is.na(.data$time)) |>
    dplyr::arrange(.data$time)
  first_ok <- checks |>
    dplyr::filter(.data$ok) |>
    dplyr::slice_head(n = 1)
  days <- if (nrow(first_ok) == 0) {
    0
  } else {
    max(0, floor(as.numeric(difftime(now, first_ok$time, units = "days"))))
  }
  list(
    days = days,
    answered = sum(checks$ok),
    checks = nrow(checks),
    last = checks |>
      dplyr::select(-"time", -"ok") |>
      utils::tail(60)
  )
}

# Short text for the chip: "in service 37 days", "in service 1 day" or "new"
status_chip <- function(summary) {
  if (summary$days < 1) {
    return("new")
  }
  stringr::str_glue(
    "in service {summary$days} {if (summary$days == 1) 'day' else 'days'}"
  ) |>
    as.character()
}
