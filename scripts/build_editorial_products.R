suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing prerequisite product: ", path, call. = FALSE)
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

season_year <- max(as.integer(format(as.Date(hitters$last_game), "%Y")), na.rm = TRUE)
career_data_through <- season_year - 1L
if (requireNamespace("Lahman", quietly = TRUE)) {
  career_data_through <- max(Lahman::Batting$yearID, na.rm = TRUE)
}

milestones <- build_active_milestone_watch(
  hitters,
  pitchers,
  career_profiles,
  season_year = season_year,
  career_data_through = career_data_through,
  minimum_significance = 20
)
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
