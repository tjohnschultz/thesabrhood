suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing prerequisite product: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
read_optional_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) return(data.frame())
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
write_product <- function(data, name, source_through_override = NULL) {
  source_dates <- c(
    suppressWarnings(as.Date(hitters$last_game)),
    suppressWarnings(as.Date(pitchers$last_game))
  )
  source_dates <- source_dates[!is.na(source_dates)]
  product_source_through <- if (!is.null(source_through_override)) {
    as.character(source_through_override)
  } else if (length(source_dates)) {
    as.character(max(source_dates))
  } else {
    NA_character_
  }
  data$source_through <- product_source_through
  data$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  utils::write.csv(data, file.path(output_dir, name), row.names = FALSE, na = "")
  cat(sprintf("%-42s %s rows\n", name, format(nrow(data), big.mark = ",")))
}

hitters <- read_product("hitter-performance-summary.csv")
pitchers <- read_product("pitcher-performance-summary.csv")
hitter_form <- read_product("hitter-recent-form.csv")
pitcher_form <- read_product("pitcher-recent-form.csv")
bullpen <- read_product("bullpen-availability.csv")
career_profiles <- read_product("historical-player-profiles.csv")
hitter_platoon <- read_product("hitter-platoon-summary.csv")
pitcher_platoon <- read_product("pitcher-platoon-summary.csv")
pitch_types <- read_product("pitch-type-summary.csv")
historical <- read_product("historical-anniversary-notes.csv")
fangraphs_hitters <- read_optional_product("fangraphs-season-hitters.csv")
fangraphs_pitchers <- read_optional_product("fangraphs-season-pitchers.csv")
hitter_game_lines <- read_optional_product("current-season-hitter-game-lines.csv")
pitcher_game_lines <- read_optional_product("current-season-pitcher-game-lines.csv")

season_year <- max(as.integer(format(as.Date(hitters$last_game), "%Y")), na.rm = TRUE)
source_dates <- c(
  suppressWarnings(as.Date(hitters$last_game)),
  suppressWarnings(as.Date(pitchers$last_game))
)
source_dates <- source_dates[!is.na(source_dates)]
source_through <- if (length(source_dates)) max(source_dates) else Sys.Date() - 1L
career_data_through <- season_year - 1L
if (requireNamespace("Lahman", quietly = TRUE)) {
  career_data_through <- max(Lahman::Batting$yearID, na.rm = TRUE)
}

game_log_cache_dir <- Sys.getenv(
  "SABRHOOD_GAME_LOG_CACHE",
  unset = file.path(".private-data", "mlb-game-logs")
)
dir.create(game_log_cache_dir, recursive = TRUE, showWarnings = FALSE)
game_log_memory <- new.env(parent = emptyenv())

read_mlb_game_log <- function(player_id, role) {
  player_id <- as.character(player_id)
  group <- if (identical(role, "pitcher")) "pitching" else "hitting"
  cache_key <- paste(player_id, group, season_year, sep = "-")
  if (exists(cache_key, envir = game_log_memory, inherits = FALSE)) {
    return(get(cache_key, envir = game_log_memory, inherits = FALSE))
  }
  cache_path <- file.path(game_log_cache_dir, paste0(cache_key, ".rds"))
  cached <- if (file.exists(cache_path)) {
    tryCatch(readRDS(cache_path), error = function(error) NULL)
  } else {
    NULL
  }
  cache_is_current <- is.list(cached) &&
    all(c("source_through", "splits") %in% names(cached)) &&
    !is.na(suppressWarnings(as.Date(cached$source_through))) &&
    as.Date(cached$source_through) >= source_through
  if (cache_is_current) {
    assign(cache_key, cached$splits, envir = game_log_memory)
    return(cached$splits)
  }
  splits <- tryCatch({
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("The jsonlite package is required for MLB game-log milestone dates.")
    }
    endpoint <- paste0(
      "https://statsapi.mlb.com/api/v1/people/", player_id,
      "/stats?stats=gameLog&group=", group, "&season=", season_year
    )
    payload <- jsonlite::fromJSON(endpoint)
    payload[["stats"]][["splits"]][[1L]]
  }, error = function(error) {
    message(
      "MLB game log unavailable for ", player_id, " (", group, "): ",
      conditionMessage(error)
    )
    if (is.list(cached) && "splits" %in% names(cached)) cached$splits else data.frame()
  })
  if (!is.data.frame(splits)) splits <- data.frame()
  if (nrow(splits)) {
    splits <- splits[
      !is.na(suppressWarnings(as.Date(splits$date))) &
        as.Date(splits$date) <= source_through,
      ,
      drop = FALSE
    ]
    saveRDS(
      list(source_through = as.character(source_through), splits = splits),
      cache_path
    )
  }
  assign(cache_key, splits, envir = game_log_memory)
  splits
}

milestone_hitters <- if (nrow(fangraphs_hitters)) fangraphs_hitters else hitters
milestone_pitchers <- if (nrow(fangraphs_pitchers)) fangraphs_pitchers else pitchers
milestone_source_dates <- c(
  if (nrow(fangraphs_hitters)) {
    suppressWarnings(as.Date(substr(fangraphs_hitters$source_acquired_at_utc, 1L, 10L)))
  } else {
    suppressWarnings(as.Date(hitters$last_game))
  },
  if (nrow(fangraphs_pitchers)) {
    suppressWarnings(as.Date(substr(fangraphs_pitchers$source_acquired_at_utc, 1L, 10L)))
  } else {
    suppressWarnings(as.Date(pitchers$last_game))
  }
)
milestone_source_dates <- milestone_source_dates[!is.na(milestone_source_dates)]
milestone_source_through <- if (length(milestone_source_dates)) {
  max(milestone_source_dates)
} else {
  source_through
}
milestones <- build_active_milestone_watch(
  milestone_hitters,
  milestone_pitchers,
  career_profiles,
  season_year = season_year,
  career_data_through = career_data_through,
  minimum_significance = 20
)
reached_dates <- data.frame()
if (nrow(milestones)) {
  reached <- milestones[milestones$milestone_status == "reached_this_season", , drop = FALSE]
  date_rows <- lapply(seq_len(nrow(reached)), function(index) {
    row <- reached[index, , drop = FALSE]
    source <- if (row$role[[1L]] == "hitter") hitter_game_lines else pitcher_game_lines
    stat_column <- unname(c(
      hits = "hits", `home runs` = "home_runs", doubles = "doubles",
      RBI = "rbi", strikeouts = "strikeouts"
    )[row$milestone_stat[[1L]]])
    if (!length(stat_column) || is.na(stat_column) || !nrow(source) || !stat_column %in% names(source)) return(NULL)
    player <- source[as.character(source$player_id) == as.character(row$player_id[[1L]]), , drop = FALSE]
    if (!nrow(player)) return(NULL)
    player <- player[order(as.Date(player$game_date), player$game_pk), , drop = FALSE]
    needed <- as.numeric(row$milestone_target[[1L]]) - as.numeric(row$prior_career_value[[1L]])
    cumulative <- cumsum(suppressWarnings(as.numeric(player[[stat_column]])))
    reached_index <- which(cumulative >= needed)[1L]
    if (!length(reached_index) || is.na(reached_index)) return(NULL)
    data.frame(
      player_id = row$player_id[[1L]],
      milestone_stat = row$milestone_stat[[1L]],
      milestone_target = row$milestone_target[[1L]],
      reached_date = as.Date(player$game_date[[reached_index]]),
      stringsAsFactors = FALSE
    )
  })
  date_rows <- date_rows[!vapply(date_rows, is.null, logical(1))]
  if (length(date_rows)) reached_dates <- dplyr::bind_rows(date_rows)
  reached_key <- paste(
    reached$player_id, reached$milestone_stat, reached$milestone_target,
    sep = "|"
  )
  dated_key <- if (nrow(reached_dates)) {
    paste(
      reached_dates$player_id,
      reached_dates$milestone_stat,
      reached_dates$milestone_target,
      sep = "|"
    )
  } else {
    character()
  }
  game_log_columns <- c(
    runs = "runs",
    `stolen bases` = "stolenBases",
    wins = "wins",
    saves = "saves"
  )
  unresolved <- reached[
    !reached_key %in% dated_key &
      reached$milestone_stat %in% names(game_log_columns),
    ,
    drop = FALSE
  ]
  game_log_dates <- lapply(seq_len(nrow(unresolved)), function(index) {
    row <- unresolved[index, , drop = FALSE]
    game_log <- read_mlb_game_log(row$player_id[[1L]], row$role[[1L]])
    stat_column <- unname(game_log_columns[row$milestone_stat[[1L]]])
    if (
      !nrow(game_log) ||
        !"stat" %in% names(game_log) ||
        !is.data.frame(game_log$stat) ||
        !stat_column %in% names(game_log$stat)
    ) {
      return(NULL)
    }
    order_index <- order(as.Date(game_log$date), game_log$game$gamePk)
    game_log <- game_log[order_index, , drop = FALSE]
    needed <- as.numeric(row$milestone_target[[1L]]) -
      as.numeric(row$prior_career_value[[1L]])
    cumulative <- cumsum(
      suppressWarnings(as.numeric(game_log$stat[[stat_column]]))
    )
    reached_index <- which(cumulative >= needed)[1L]
    if (!length(reached_index) || is.na(reached_index)) return(NULL)
    data.frame(
      player_id = row$player_id[[1L]],
      milestone_stat = row$milestone_stat[[1L]],
      milestone_target = row$milestone_target[[1L]],
      reached_date = as.Date(game_log$date[[reached_index]]),
      stringsAsFactors = FALSE
    )
  })
  game_log_dates <- game_log_dates[
    !vapply(game_log_dates, is.null, logical(1))
  ]
  if (length(game_log_dates)) {
    reached_dates <- dplyr::bind_rows(
      reached_dates,
      dplyr::bind_rows(game_log_dates)
    )
  }
  milestones <- build_active_milestone_watch(
    milestone_hitters,
    milestone_pitchers,
    career_profiles,
    season_year = season_year,
    career_data_through = career_data_through,
    minimum_significance = 20,
    reached_dates = reached_dates
  )
}
races <- build_league_race_boards(hitters, pitchers, minimum_pa = 100L, minimum_bf = 75L)
teams <- summarize_team_intelligence(hitters, pitchers, hitter_form, pitcher_form, bullpen)
bullpen_matchups <- build_bullpen_matchup_board(
  bullpen,
  pitchers,
  leverage_index = 2,
  top_n = 3L
)
matchup_edges <- build_platoon_edge_boards(
  hitter_platoon,
  pitcher_platoon,
  minimum_pa = 40L
)
signature_pitches <- build_signature_pitch_board(
  pitch_types,
  minimum_pitches = 100L,
  minimum_swings = 35L
)
hitter_changes <- build_player_change_profiles(hitter_form, hitters)
pitcher_changes <- build_player_change_profiles(pitcher_form, pitchers)
story_queue <- build_daily_story_queue(
  hitter_form,
  pitcher_form,
  races$offense,
  races$run_prevention,
  milestones,
  historical,
  teams,
  signature_pitches
)
team_broadcast_notes <- build_team_broadcast_notes(
  teams,
  hitter_form,
  pitcher_form,
  signature_pitches,
  matchup_edges$hitters,
  matchup_edges$pitchers,
  milestones
)

write_product(
  milestones,
  "active-milestone-watch.csv",
  source_through_override = milestone_source_through
)
write_product(races$offense, "offensive-race-board.csv")
write_product(races$run_prevention, "run-prevention-race-board.csv")
write_product(teams, "team-intelligence-summary.csv")
write_product(bullpen_matchups, "bullpen-matchup-selector.csv")
write_product(matchup_edges$hitters, "hitter-matchup-edges.csv")
write_product(matchup_edges$pitchers, "pitcher-matchup-edges.csv")
write_product(signature_pitches, "signature-pitch-board.csv")
write_product(story_queue, "daily-story-queue.csv")
write_product(team_broadcast_notes, "team-broadcast-notes.csv")
write_product(hitter_changes, "hitter-change-profiles.csv")
write_product(pitcher_changes, "pitcher-change-profiles.csv")
cat("Completed editorial, matchup, story and team-intelligence products.\n")
