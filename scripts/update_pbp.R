suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

bind_safely <- function(items) {
  items <- items[vapply(items, function(item) is.data.frame(item) && nrow(item), logical(1))]
  if (!length(items)) return(data.frame())
  dplyr::bind_rows(items)
}

scalar_or_na <- function(value) if (length(value)) value[[1L]] else NA

completed_schedule <- function(date) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&gameType=R&date=",
    format(as.Date(date), "%Y-%m-%d"), "&hydrate=team,venue,probablePitcher"
  )
  payload <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  if (!length(payload$dates)) return(data.frame())
  games <- payload$dates[[1L]]$games
  if (!is.list(games) || !length(games)) return(data.frame())
  rows <- lapply(games, function(game) {
    is_final <- identical(as.character(game$status$abstractGameState), "Final") ||
      identical(as.character(game$status$codedGameState), "F")
    if (!is_final) return(data.frame())
    data.frame(
      game_pk = as.character(game$gamePk),
      game_date = as.character(as.Date(date)),
      away_team = as.character(game$teams$away$team$name),
      home_team = as.character(game$teams$home$team$name),
      status = as.character(game$status$detailedState),
      stringsAsFactors = FALSE
    )
  })
  bind_safely(rows)
}

fetch_complete_game <- function(game_pk, official_date) {
  url <- paste0("https://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  payload <- jsonlite::fromJSON(url, flatten = TRUE)
  if (!identical(as.character(scalar_or_na(payload$gameData$status$abstractGameState)), "Final") &&
      !identical(as.character(scalar_or_na(payload$gameData$status$codedGameState)), "F")) {
    stop("Game is not final: ", game_pk, call. = FALSE)
  }
  at_bats <- payload$liveData$plays$allPlays
  if (!is.data.frame(at_bats) || !nrow(at_bats) || !"playEvents" %in% names(at_bats)) {
    stop("Final game has no usable play events: ", game_pk, call. = FALSE)
  }
  event_rows <- lapply(seq_len(nrow(at_bats)), function(index) {
    events <- at_bats$playEvents[[index]]
    if (!is.data.frame(events) || !nrow(events)) return(data.frame())
    events$.atbat_row <- index
    events
  })
  plays <- bind_safely(event_rows)
  list_columns <- names(at_bats)[vapply(at_bats, is.list, logical(1))]
  metadata <- at_bats[, setdiff(names(at_bats), list_columns), drop = FALSE]
  metadata$.atbat_row <- seq_len(nrow(metadata))
  game <- dplyr::left_join(plays, metadata, by = ".atbat_row", suffix = c(".x", ".y"))
  game$.atbat_row <- NULL
  rename_if_present <- function(data, old, new) {
    if (old %in% names(data)) names(data)[names(data) == old] <- new
    data
  }
  game <- rename_if_present(game, "count.balls.x", "count.balls.start")
  game <- rename_if_present(game, "count.strikes.x", "count.strikes.start")
  game <- rename_if_present(game, "count.outs.x", "count.outs.start")
  game <- rename_if_present(game, "count.balls.y", "count.balls.end")
  game <- rename_if_present(game, "count.strikes.y", "count.strikes.end")
  game <- rename_if_present(game, "count.outs.y", "count.outs.end")
  game$game_pk <- as.character(game_pk)
  game$game_date <- as.character(as.Date(official_date))
  game$home_team <- as.character(scalar_or_na(payload$gameData$teams$home$name))
  game$away_team <- as.character(scalar_or_na(payload$gameData$teams$away$name))
  game$batting_team <- ifelse(game$about.halfInning == "bottom", game$home_team, game$away_team)
  game$fielding_team <- ifelse(game$about.halfInning == "bottom", game$away_team, game$home_team)
  game <- game[order(suppressWarnings(as.numeric(game$atBatIndex)), suppressWarnings(as.numeric(game$pitchNumber)), na.last = TRUE), , drop = FALSE]
  game[, c("game_pk", "game_date", setdiff(names(game), c("game_pk", "game_date"))), drop = FALSE]
}

dedupe_pbp <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(data)
  data$.source_row <- seq_len(nrow(data))
  play_column <- if ("playId" %in% names(data)) "playId" else if ("play_id" %in% names(data)) "play_id" else NA_character_
  if (is.na(play_column)) stop("PBP lacks a stable play identifier.", call. = FALSE)
  has_id <- !is.na(data[[play_column]]) & nzchar(as.character(data[[play_column]]))
  identified <- dplyr::distinct(
    data[has_id, , drop = FALSE],
    dplyr::across(dplyr::all_of(c("game_pk", play_column))),
    .keep_all = TRUE
  )
  unidentified <- data[!has_id, , drop = FALSE]
  fallback <- if (all(c("game_pk", "atBatIndex", "index", "type") %in% names(unidentified))) {
    c("game_pk", "atBatIndex", "index", "type")
  } else {
    c("game_pk", "at_bat_index", "index", "type")
  }
  if (nrow(unidentified)) {
    if (!all(fallback %in% names(unidentified))) stop("PBP action rows lack stable fallback identifiers.", call. = FALSE)
    unidentified <- dplyr::distinct(unidentified, dplyr::across(dplyr::all_of(fallback)), .keep_all = TRUE)
  }
  output <- dplyr::bind_rows(identified, unidentified)
  output <- output[order(output$.source_row), , drop = FALSE]
  output$.source_row <- NULL
  output
}

update_pbp <- function(
    target_date = Sys.Date() - 1L,
    season = as.integer(format(target_date, "%Y")),
    cache_path = Sys.getenv(
      "SABRHOOD_PBP_PATH",
      unset = file.path(".private-data", "pbp", as.character(season), "current.rds")
    )) {
  target_date <- as.Date(target_date)
  if (is.na(target_date)) stop("PBP target date must use YYYY-MM-DD.", call. = FALSE)
  if (target_date >= Sys.Date()) {
    target_date <- Sys.Date() - 1L
    message("PBP endpoint clamped to yesterday so in-progress games cannot enter the cache.")
  }
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  game_cache_dir <- file.path(dirname(cache_path), "games")
  dir.create(game_cache_dir, recursive = TRUE, showWarnings = FALSE)
  current <- if (file.exists(cache_path)) readRDS(cache_path) else data.frame()
  existing_game_pks <- if (nrow(current) && "game_pk" %in% names(current)) unique(as.character(current$game_pk)) else character()
  season_start <- as.Date(Sys.getenv("SABRHOOD_SEASON_START", unset = paste0(season, "-03-20")))
  schedule_start <- if (length(existing_game_pks)) {
    current_date_column <- if ("game_date" %in% names(current)) "game_date" else "game_date_date"
    max(season_start, max(as.Date(current[[current_date_column]]), na.rm = TRUE) - 2L)
  } else {
    season_start
  }
  dates <- seq.Date(schedule_start, target_date, by = "day")
  schedule <- bind_safely(lapply(dates, completed_schedule))
  schedule <- dplyr::distinct(schedule, .data$game_pk, .keep_all = TRUE)
  missing <- schedule[!schedule$game_pk %in% existing_game_pks, , drop = FALSE]
  cat("Completed games found:", nrow(schedule), "| new games:", nrow(missing), "\n")

  acquired <- lapply(seq_len(nrow(missing)), function(index) {
    row <- missing[index, , drop = FALSE]
    game_cache_path <- file.path(game_cache_dir, paste0(row$game_pk[[1L]], ".rds"))
    game <- if (file.exists(game_cache_path)) {
      cached_game <- tryCatch(readRDS(game_cache_path), error = function(error) data.frame())
      if (is.data.frame(cached_game) && nrow(cached_game) >= 100L) cached_game else data.frame()
    } else {
      data.frame()
    }
    if (!nrow(game)) {
      game <- tryCatch(
        fetch_complete_game(row$game_pk[[1L]], row$game_date[[1L]]),
        error = function(error) {
          message("PBP fetch failed for ", row$game_pk[[1L]], ": ", conditionMessage(error))
          data.frame()
        }
      )
      if (nrow(game) >= 100L) saveRDS(game, game_cache_path, compress = TRUE)
    }
    if (nrow(game) < 100L) return(data.frame())
    game
  })
  new_pbp <- bind_safely(acquired)
  acquired_game_pks <- if (nrow(new_pbp)) unique(as.character(new_pbp$game_pk)) else character()
  failed_game_pks <- setdiff(missing$game_pk, acquired_game_pks)
  if (length(failed_game_pks)) {
    stop("Refusing to replace PBP cache; final games failed acquisition: ", paste(failed_game_pks, collapse = ", "), call. = FALSE)
  }

  combined <- if (nrow(current)) dplyr::bind_rows(current, new_pbp) else new_pbp
  if (nrow(combined)) combined <- dedupe_pbp(combined)
  if (!nrow(combined)) stop("No completed-game PBP is available.", call. = FALSE)
  game_counts <- table(as.character(combined$game_pk))
  if (any(game_counts < 100L)) stop("PBP cache contains an implausibly small final game.", call. = FALSE)
  game_date <- as.Date(combined$game_date)
  if (any(game_date >= Sys.Date(), na.rm = TRUE)) stop("Current-day rows reached the PBP cache.", call. = FALSE)

  temporary_path <- paste0(cache_path, ".tmp")
  saveRDS(combined, temporary_path, compress = TRUE)
  if (!file.copy(temporary_path, cache_path, overwrite = TRUE, copy.mode = FALSE)) stop("Could not replace private PBP cache.", call. = FALSE)
  unlink(temporary_path)

  status <- data.frame(
    refreshed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    season = season,
    source_through = as.character(max(game_date, na.rm = TRUE)),
    rows = nrow(combined),
    games = length(game_counts),
    new_games = length(acquired_game_pks),
    current_day_rows = sum(game_date >= Sys.Date(), na.rm = TRUE),
    storage = "private_github_actions_cache",
    status = "pass",
    stringsAsFactors = FALSE
  )
  dir.create(file.path("data", "derived"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(status, file.path("data", "derived", "data-refresh-status.csv"), row.names = FALSE)
  cat("PBP cache ready:", format(nrow(combined), big.mark = ","), "rows across", length(game_counts), "games through", status$source_through, "\n")
  invisible(status)
}

if (sys.nframe() == 0L) {
  end_date <- as.Date(Sys.getenv("SABRHOOD_PBP_END", unset = as.character(Sys.Date() - 1L)))
  update_pbp(target_date = end_date)
}
