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

emit <- function(active, reason) {
  cat("active_window=", ifelse(active, "true", "false"), "\n", sep = "")
  cat("slate_date=", as.character(slate_date), "\n", sep = "")
  cat("gate_reason=", gsub("[\r\n]+", " ", reason), "\n", sep = "")
}

if (!file.exists(games_path)) {
  emit(FALSE, "daily game inputs are not available")
  quit(save = "no", status = 0L)
}

games <- tryCatch(
  utils::read.csv(games_path, stringsAsFactors = FALSE, check.names = FALSE),
  error = function(error) data.frame()
)
required <- c("game_date", "game_time_utc")
if (!nrow(games) || !all(required %in% names(games))) {
  emit(FALSE, "daily game inputs are empty or incomplete")
  quit(save = "no", status = 0L)
}

game_dates <- suppressWarnings(as.Date(games$game_date))
times <- suppressWarnings(as.POSIXct(games$game_time_utc, tz = "UTC"))
minutes_to_first_pitch <- as.numeric(difftime(times, now_utc, units = "mins"))
in_window <- game_dates == slate_date & is.finite(minutes_to_first_pitch) &
  minutes_to_first_pitch >= -20 & minutes_to_first_pitch <= 240

if (any(in_window)) {
  range_text <- paste0(
    sum(in_window), " game(s) between ",
    round(min(minutes_to_first_pitch[in_window])), " and ",
    round(max(minutes_to_first_pitch[in_window])), " minutes from first pitch"
  )
  emit(TRUE, range_text)
} else {
  emit(FALSE, "no current-date games are within four hours of first pitch")
}
