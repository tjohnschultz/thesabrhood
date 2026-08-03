source(file.path("scripts", "phase5_refresh_gate.R"), local = FALSE)

partial <- data.frame(
  projection_ready = c(FALSE, FALSE),
  away_lineup_count = c(9L, 0L),
  home_lineup_count = c(0L, 9L),
  stringsAsFactors = FALSE
)
partial_result <- evaluate_phase5_refresh_gate(partial)
stopifnot(
  identical(partial_result$active, FALSE),
  identical(partial_result$ready_games, 0L),
  grepl("no game has two complete lineups", partial_result$reason, fixed = TRUE)
)

ready <- rbind(
  partial,
  data.frame(
    projection_ready = TRUE,
    away_lineup_count = 9L,
    home_lineup_count = 9L
  )
)
ready_result <- evaluate_phase5_refresh_gate(ready)
stopifnot(
  isTRUE(ready_result$active),
  identical(ready_result$ready_games, 1L)
)

not_projection_ready <- data.frame(
  projection_ready = FALSE,
  away_lineup_count = 9L,
  home_lineup_count = 9L
)
not_ready_result <- evaluate_phase5_refresh_gate(not_projection_ready)
stopifnot(
  identical(not_ready_result$active, FALSE),
  identical(not_ready_result$ready_games, 0L)
)

incomplete <- evaluate_phase5_refresh_gate(
  data.frame(away_lineup_count = 9L, home_lineup_count = 9L)
)
stopifnot(
  identical(incomplete$active, FALSE),
  grepl("projection_ready", incomplete$reason, fixed = TRUE)
)

cat("Phase 5 refresh gate tests passed.\n")
