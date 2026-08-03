evaluate_phase5_refresh_gate <- function(games) {
  required <- c(
    "projection_ready",
    "away_lineup_count",
    "home_lineup_count"
  )
  missing <- setdiff(required, names(games))
  if (!is.data.frame(games) || !nrow(games) || length(missing)) {
    return(list(
      active = FALSE,
      ready_games = 0L,
      reason = if (length(missing)) {
        paste("daily game inputs are missing", paste(missing, collapse = ", "))
      } else {
        "daily game inputs are empty"
      }
    ))
  }

  projection_ready <- toupper(trimws(as.character(games$projection_ready))) %in%
    c("TRUE", "1")
  away_count <- suppressWarnings(as.integer(games$away_lineup_count))
  home_count <- suppressWarnings(as.integer(games$home_lineup_count))
  ready <- projection_ready & away_count >= 9L & home_count >= 9L
  ready[is.na(ready)] <- FALSE
  ready_games <- sum(ready)

  list(
    active = ready_games > 0L,
    ready_games = as.integer(ready_games),
    reason = if (ready_games > 0L) {
      paste(ready_games, "game(s) have complete Phase 5 inputs")
    } else {
      "no game has two complete lineups and all projection inputs"
    }
  )
}

emit_phase5_refresh_gate <- function(result) {
  cat("has_phase5_inputs=", ifelse(result$active, "true", "false"), "\n", sep = "")
  cat("phase5_ready_games=", result$ready_games, "\n", sep = "")
  cat(
    "phase5_gate_reason=",
    gsub("[\r\n]+", " ", result$reason),
    "\n",
    sep = ""
  )
}

run_phase5_refresh_gate <- function() {
  workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
  derived_dir <- Sys.getenv(
    "SABRHOOD_DERIVED_DIR",
    unset = file.path(workspace, "data", "derived")
  )
  games_path <- file.path(derived_dir, "daily-game-inputs.csv")
  games <- if (file.exists(games_path)) {
    tryCatch(
      utils::read.csv(games_path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(error) data.frame()
    )
  } else {
    data.frame()
  }
  result <- evaluate_phase5_refresh_gate(games)
  emit_phase5_refresh_gate(result)
  invisible(result)
}

file_arguments <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
executed_file <- if (length(file_arguments)) {
  basename(sub("^--file=", "", file_arguments[[1L]]))
} else {
  ""
}
if (identical(executed_file, "phase5_refresh_gate.R")) {
  run_phase5_refresh_gate()
}
