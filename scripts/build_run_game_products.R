workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
local_library <- file.path(dirname(workspace), "sabrhoodR", ".Rlib")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(sabrhoodR)
})

season <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
pbp_path <- file.path(workspace, ".private-data", "pbp", season, "current.rds")
derived_dir <- file.path(workspace, "data", "derived")
private_model_dir <- file.path(workspace, ".private-data", "models")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(private_model_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(pbp_path)) {
  stop("Missing current PBP cache: ", pbp_path, call. = FALSE)
}
player_path <- file.path(derived_dir, "fangraphs-season-hitters.csv")
if (!file.exists(player_path)) {
  stop("Missing catcher position reference: ", player_path, call. = FALSE)
}

pbp <- readRDS(pbp_path)
player_reference <- utils::read.csv(
  player_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

catcher_stints <- build_catcher_stints(
  pbp,
  player_reference = player_reference
)
opportunities <- build_run_game_pitch_opportunities(
  pbp,
  catcher_stints = catcher_stints
)
if (!nrow(opportunities)) {
  stop("No pitch-level run-game opportunities were reconstructed.", call. = FALSE)
}
ratings <- build_run_game_ratings(opportunities)
count_windows <- build_run_game_count_windows(opportunities)
run_notes <- build_run_game_notes(count_windows)

called_pitches <- build_called_pitch_view(
  pbp,
  catcher_stints = catcher_stints
)
framing <- score_catcher_framing(called_pitches)

saveRDS(
  catcher_stints,
  file.path(private_model_dir, "run-game-catcher-stints.rds"),
  compress = "gzip"
)
saveRDS(
  opportunities,
  file.path(private_model_dir, "run-game-pitch-opportunities.rds"),
  compress = "gzip"
)
saveRDS(
  called_pitches,
  file.path(private_model_dir, "catcher-called-pitches.rds"),
  compress = "gzip"
)

write_product <- function(data, filename) {
  data <- as.data.frame(data)
  if (nrow(data)) {
    data$source_through <- as.character(max(opportunities$game_date, na.rm = TRUE))
    data$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  }
  utils::write.csv(
    data,
    file.path(derived_dir, filename),
    row.names = FALSE,
    na = ""
  )
}

write_product(ratings$pitcher, "run-game-pitcher-ratings.csv")
write_product(ratings$catcher, "run-game-catcher-ratings.csv")
write_product(ratings$runner, "run-game-runner-ratings.csv")
write_product(ratings$battery, "run-game-battery-ratings.csv")
write_product(ratings$model_card, "run-game-model-card.csv")
write_product(count_windows, "run-game-count-windows.csv")
write_product(run_notes, "run-game-notes.csv")
write_product(framing$catcher, "catcher-framing-ratings.csv")
write_product(framing$model_card, "catcher-framing-model-card.csv")

read_abs_data <- function(challenge_type) {
  query <- paste0(
    "https://baseballsavant.mlb.com/leaderboard/abs-challenges",
    "?challengeType=", utils::URLencode(challenge_type, reserved = TRUE),
    "&gameType=regular&level=mlb&minChal=0&minOppChal=0",
    "&page=0&pageSize=1000&sort=n_challenges&sortDir=desc&year=", season
  )
  connection <- url(query, open = "rb", headers = c(
    "User-Agent" = "The-SABRhood-data-pipeline/1.0"
  ))
  on.exit(close(connection), add = TRUE)
  html <- paste(readLines(connection, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  marker <- "const absData = "
  marker_start <- regexpr(marker, html, fixed = TRUE)[[1L]]
  if (marker_start < 0L) {
    stop("Baseball Savant ABS response did not contain absData.", call. = FALSE)
  }
  json_start <- marker_start + nchar(marker)
  closing_arrays <- gregexpr("];", html, fixed = TRUE)[[1L]]
  closing_arrays <- closing_arrays[closing_arrays >= json_start]
  if (!length(closing_arrays)) {
    stop("Baseball Savant ABS data did not have a closing array.", call. = FALSE)
  }
  json <- substring(html, json_start, closing_arrays[[1L]])
  parsed <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
  standardize_abs_challenge_leaderboard(parsed, challenge_type)
}

abs_types <- c("catcher", "pitcher", "batter", "team-summary")
abs_status <- lapply(abs_types, function(challenge_type) {
  tryCatch(
    list(
      type = challenge_type,
      data = read_abs_data(challenge_type),
      status = "current"
    ),
    error = function(error) {
      warning(
        "ABS refresh failed for ", challenge_type, ": ",
        conditionMessage(error),
        call. = FALSE
      )
      list(type = challenge_type, data = data.frame(), status = "unavailable")
    }
  )
})
abs_board <- dplyr::bind_rows(lapply(abs_status, `[[`, "data"))
abs_path <- file.path(derived_dir, "abs-challenge-leaderboard.csv")
if (nrow(abs_board)) {
  write_product(abs_board, "abs-challenge-leaderboard.csv")
} else if (!file.exists(abs_path)) {
  write_product(tibble::tibble(), "abs-challenge-leaderboard.csv")
}
abs_model_card <- tibble::tibble(
  season = season,
  challenge_types = paste(abs_types, collapse = ","),
  rows = nrow(abs_board),
  status = paste(
    vapply(abs_status, function(value) {
      paste(value$type, value$status, sep = "=")
    }, character(1)),
    collapse = ";"
  ),
  source = "Baseball Savant ABS Challenge Leaderboard",
  model_version = "savant_abs_contract_v1",
  publication_status = if (nrow(abs_board)) "official current leaderboard" else "prior product retained"
)
write_product(abs_model_card, "abs-challenge-model-card.csv")

cat(
  "Run Game Engine built", format(nrow(opportunities), big.mark = ","),
  "eligible pitch windows,", format(sum(opportunities$attempted), big.mark = ","),
  "attempts, catcher coverage",
  paste0(round(100 * mean(!is.na(opportunities$catcher_id)), 1), "%"), "and",
  format(nrow(abs_board), big.mark = ","), "ABS leaderboard rows.\n"
)
