source(file.path("scripts", "lineup_refresh_gate.R"), local = FALSE)

iso_values <- c(
  "2026-07-26T16:15:00Z",
  "2026-07-26T17:35:00Z",
  "2026-07-26T20:05:00+00:00"
)
parsed <- parse_game_times_utc(iso_values)
stopifnot(
  identical(
    format(parsed, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
    c(
      "2026-07-26 16:15:00",
      "2026-07-26 17:35:00",
      "2026-07-26 20:05:00"
    )
  )
)

games <- data.frame(
  game_date = rep("2026-07-26", 3L),
  game_time_utc = iso_values,
  stringsAsFactors = FALSE
)
active <- evaluate_lineup_refresh_gate(
  games,
  as.POSIXct("2026-07-26 16:29:00", tz = "UTC"),
  as.Date("2026-07-26")
)
stopifnot(
  isTRUE(active$active),
  grepl("game\\(s\\)", active$reason)
)

inactive <- evaluate_lineup_refresh_gate(
  games,
  as.POSIXct("2026-07-26 23:00:00", tz = "UTC"),
  as.Date("2026-07-26")
)
stopifnot(
  identical(inactive$active, FALSE),
  identical(
    inactive$reason,
    "no current-date games are within four hours of first pitch"
  )
)

incomplete <- evaluate_lineup_refresh_gate(
  data.frame(game_date = "2026-07-26"),
  as.POSIXct("2026-07-26 16:29:00", tz = "UTC"),
  as.Date("2026-07-26")
)
stopifnot(
  identical(incomplete$active, FALSE),
  identical(incomplete$reason, "daily game inputs are empty or incomplete")
)

cat("Lineup refresh gate tests passed.\n")
