parse_game_times_utc <- function(values) {
  values <- trimws(as.character(values))
  parsed <- as.POSIXct(
    rep(NA_real_, length(values)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS %Z",
    "%Y-%m-%d %H:%M:%OS"
  )
  for (format in formats) {
    missing <- is.na(parsed) & nzchar(values)
    if (!any(missing)) break
    candidate_values <- values[missing]
    if (identical(format, "%Y-%m-%dT%H:%M:%OS%z")) {
      candidate_values <- sub(
        "([+-][0-9]{2}):([0-9]{2})$",
        "\\1\\2",
        candidate_values,
        perl = TRUE
      )
    }
    parsed[missing] <- as.POSIXct(
      strptime(candidate_values, format = format, tz = "UTC")
    )
  }
  parsed
}

evaluate_lineup_refresh_gate <- function(games, now_utc, slate_date) {
  required <- c("game_date", "game_time_utc")
  if (!nrow(games) || !all(required %in% names(games))) {
    return(list(
      active = FALSE,
      reason = "daily game inputs are empty or incomplete"
    ))
  }

  game_dates <- suppressWarnings(as.Date(games$game_date))
  times <- suppressWarnings(parse_game_times_utc(games$game_time_utc))
  minutes_to_first_pitch <- as.numeric(difftime(times, now_utc, units = "mins"))
  in_window <- game_dates == slate_date & is.finite(minutes_to_first_pitch) &
    minutes_to_first_pitch >= -20 & minutes_to_first_pitch <= 240

  if (any(in_window)) {
    return(list(
      active = TRUE,
      reason = paste0(
        sum(in_window), " game(s) between ",
        round(min(minutes_to_first_pitch[in_window])), " and ",
        round(max(minutes_to_first_pitch[in_window])), " minutes from first pitch"
      )
    ))
  }

  list(
    active = FALSE,
    reason = "no current-date games are within four hours of first pitch"
  )
}

emit_gate_output <- function(active, reason, slate_date) {
  cat("active_window=", ifelse(active, "true", "false"), "\n", sep = "")
  cat("slate_date=", as.character(slate_date), "\n", sep = "")
  cat("gate_reason=", gsub("[\r\n]+", " ", reason), "\n", sep = "")
}

run_lineup_refresh_gate <- function() {
  workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
  games_path <- file.path(workspace, "data", "derived", "daily-game-inputs.csv")
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  requested_date <- Sys.getenv("SABRHOOD_DATE", unset = "")
  slate_date <- if (nzchar(requested_date)) {
    as.Date(requested_date)
  } else {
    as.Date(format(now_utc, tz = "America/New_York", format = "%Y-%m-%d"))
  }
  if (is.na(slate_date)) stop("SABRHOOD_DATE must use YYYY-MM-DD.", call. = FALSE)

  if (!file.exists(games_path)) {
    emit_gate_output(FALSE, "daily game inputs are not available", slate_date)
    return(invisible(NULL))
  }

  games <- tryCatch(
    utils::read.csv(games_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(error) data.frame()
  )
  result <- evaluate_lineup_refresh_gate(games, now_utc, slate_date)
  emit_gate_output(result$active, result$reason, slate_date)
  invisible(result)
}

file_arguments <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
executed_file <- if (length(file_arguments)) {
  basename(sub("^--file=", "", file_arguments[[1L]]))
} else {
  ""
}
if (identical(executed_file, "lineup_refresh_gate.R")) {
  run_lineup_refresh_gate()
}
