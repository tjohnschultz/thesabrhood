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
write_product <- function(data, name) {
  source_dates <- c(
    suppressWarnings(as.Date(hitters$last_game)),
    suppressWarnings(as.Date(pitchers$last_game))
  )
  source_dates <- source_dates[!is.na(source_dates)]
  data$source_through <- if (length(source_dates)) as.character(max(source_dates)) else NA_character_
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
career_data_through <- season_year - 1L
if (requireNamespace("Lahman", quietly = TRUE)) {
  career_data_through <- max(Lahman::Batting$yearID, na.rm = TRUE)
}

milestone_hitters <- if (nrow(fangraphs_hitters)) fangraphs_hitters else hitters
milestone_pitchers <- if (nrow(fangraphs_pitchers)) fangraphs_pitchers else pitchers
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

write_product(milestones, "active-milestone-watch.csv")
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
