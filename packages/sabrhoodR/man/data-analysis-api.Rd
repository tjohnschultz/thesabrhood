\name{data-analysis-api}
\alias{build_pitch_view}
\alias{build_plate_appearance_view}
\alias{build_batted_ball_view}
\alias{build_pitcher_appearance_view}
\alias{classify_pitcher_role}
\alias{build_pitcher_registry}
\alias{build_bullpen_availability}
\alias{build_manager_pitching_decisions}
\alias{fit_manager_hook_model}
\alias{predict_manager_hook_probability}
\alias{validate_manager_hook_model}
\alias{rank_reliever_options}
\alias{add_pitch_sequence_features}
\alias{build_run_expectancy_table}
\alias{add_run_expectancy}
\alias{add_game_context}
\alias{summarize_player_performance}
\alias{summarize_hitters}
\alias{summarize_pitchers}
\alias{summarize_pitch_types}
\alias{compare_recent_player_form}
\alias{build_player_change_profiles}
\alias{score_story_candidates}
\alias{build_debut_anniversary_notes}
\alias{build_final_game_anniversary_notes}
\alias{build_historical_anniversary_notes}
\alias{build_player_career_profiles}
\alias{build_career_milestone_notes}
\alias{build_active_milestone_watch}
\alias{build_league_race_boards}
\alias{summarize_team_intelligence}
\alias{build_hook_scenario_board}
\alias{build_bullpen_matchup_board}
\alias{build_platoon_edge_boards}
\alias{build_signature_pitch_board}
\alias{build_daily_story_queue}
\alias{build_team_broadcast_notes}
\title{Canonical MLB analysis and editorial data interfaces}
\usage{
build_pitch_view(pbp)

build_plate_appearance_view(pbp)

build_batted_ball_view(pbp)

build_pitcher_appearance_view(pbp)

classify_pitcher_role(
  games,
  starts,
  average_starter_bf = NA_real_,
  average_relief_bf = NA_real_
)

build_pitcher_registry(appearances, as_of_date = NULL)

build_bullpen_availability(appearances, registry = NULL,
  as_of_date = Sys.Date())

build_manager_pitching_decisions(plate_appearances, registry = NULL)

fit_manager_hook_model(decisions)

predict_manager_hook_probability(model, context)

validate_manager_hook_model(decisions, validation_fraction = 0.2,
  train_through = NULL, calibration_bins = 10L)

rank_reliever_options(candidates, upcoming_batter_side, leverage_index = 1,
  max_days_since_appearance = 30L)

add_pitch_sequence_features(pitches)

build_run_expectancy_table(plate_appearances, minimum_pa = 25L)

add_run_expectancy(plate_appearances, run_expectancy = NULL)

add_game_context(plate_appearances)

summarize_player_performance(plate_appearances,
  perspective = c("batter", "pitcher"), group_by = NULL,
  minimum_pa = 1L, as_of_date = NULL)

summarize_hitters(plate_appearances, group_by = NULL,
  minimum_pa = 1L, as_of_date = NULL)

summarize_pitchers(plate_appearances, group_by = NULL,
  minimum_pa = 1L, as_of_date = NULL)

summarize_pitch_types(pitches, perspective = c("pitcher", "batter"),
  group_by = NULL, minimum_pitches = 10L)

compare_recent_player_form(plate_appearances,
  perspective = c("batter", "pitcher"), recent_days = 14L,
  minimum_recent_pa = 15L, minimum_baseline_pa = 30L,
  as_of_date = NULL)

build_player_change_profiles(recent_form, season_summary)

score_story_candidates(candidates, weights = c(rarity = 1.25,
  magnitude = 1, timeliness = 1.15, relevance = 1.1,
  confidence = 1.35, novelty = 0.85))

build_debut_anniversary_notes(people, report_date = Sys.Date(),
  minimum_years = 1L, career_profiles = NULL)

build_final_game_anniversary_notes(people, report_date = Sys.Date(),
  minimum_years = 1L, career_profiles = NULL)

build_historical_anniversary_notes(people, report_date = Sys.Date(),
  minimum_years = 1L, career_profiles = NULL)

build_player_career_profiles(people, batting, pitching,
  awards_players = NULL, allstar = NULL, hall_of_fame = NULL,
  batting_post = NULL, pitching_post = NULL, war = NULL)

build_career_milestone_notes(career_profiles,
  minimum_significance = 45, record_top_n = 10L)

build_active_milestone_watch(hitters, pitchers, career_profiles,
  season_year, career_data_through = season_year - 1L,
  minimum_significance = 20)

build_league_race_boards(hitters, pitchers,
  minimum_pa = 100L, minimum_bf = 75L)

summarize_team_intelligence(hitters, pitchers, hitter_form,
  pitcher_form, bullpen)

build_hook_scenario_board(model, scenarios = NULL)

build_bullpen_matchup_board(availability, pitcher_summary,
  leverage_index = 2, top_n = 3L)

build_platoon_edge_boards(hitter_platoon, pitcher_platoon,
  minimum_pa = 40L)

build_signature_pitch_board(pitch_types, minimum_pitches = 100L,
  minimum_swings = 35L)

build_daily_story_queue(hitter_form, pitcher_form, offensive_race,
  prevention_race, active_milestones, historical,
  team_intelligence, signature_pitches)

build_team_broadcast_notes(team_intelligence, hitter_form,
  pitcher_form, signature_pitches, hitter_matchups,
  pitcher_matchups, active_milestones)
}
\arguments{
  \item{pbp}{Raw BaseballR MLB play-by-play data or an appropriate canonical view.}
  \item{games}{Number of pitching appearances.}
  \item{starts}{Number of starts.}
  \item{average_starter_bf}{Average batters faced in starts.}
  \item{average_relief_bf}{Average batters faced in relief.}
  \item{appearances}{Canonical pitcher-appearance data.}
  \item{as_of_date}{Date through which information is known. Bullpen workloads use only earlier games.}
  \item{registry}{Optional pitcher registry containing roles and role confidence.}
  \item{plate_appearances}{Canonical plate-appearance data.}
  \item{decisions}{Output from \code{build_manager_pitching_decisions()}.}
  \item{model}{Output from \code{fit_manager_hook_model()}.}
  \item{context}{Completed plate-appearance context rows to score.}
  \item{validation_fraction}{Fraction of the latest observations reserved for out-of-time validation.}
  \item{train_through}{Optional final training date; later games form the validation set.}
  \item{calibration_bins}{Number of approximately equal-size probability bins.}
  \item{candidates}{Candidate editorial stories or reliever availability rows, as appropriate.}
  \item{upcoming_batter_side}{Upcoming batter side: \code{L}, \code{R}, or \code{S}.}
  \item{leverage_index}{Current non-negative leverage index.}
  \item{max_days_since_appearance}{Fallback recency screen used when a supplied active-roster flag is unavailable.}
  \item{pitches}{Canonical pitch rows.}
  \item{run_expectancy}{Optional empirical run expectancy table.}
  \item{perspective}{Whether results are summarized from the batter or pitcher perspective.}
  \item{group_by}{Optional canonical column names used for splits.}
  \item{minimum_pa}{Minimum plate appearances required.}
  \item{minimum_pitches}{Minimum pitches required.}
  \item{recent_days}{Inclusive recent-window length.}
  \item{minimum_recent_pa}{Minimum recent plate appearances.}
  \item{minimum_baseline_pa}{Minimum earlier baseline plate appearances.}
  \item{recent_form}{Recent-versus-prior form output for one player perspective.}
  \item{season_summary}{Full-season player summary for the same perspective.}
  \item{weights}{Named positive weights for story-score components.}
  \item{people}{A Lahman-compatible People table.}
  \item{batting}{A Lahman-compatible Batting table.}
  \item{pitching}{A Lahman-compatible Pitching table.}
  \item{awards_players}{Optional Lahman AwardsPlayers data.}
  \item{allstar}{Optional Lahman AllstarFull data.}
  \item{hall_of_fame}{Optional Lahman HallOfFame data.}
  \item{batting_post}{Optional Lahman BattingPost data.}
  \item{pitching_post}{Optional Lahman PitchingPost data.}
  \item{war}{Optional separately sourced player WAR data; Lahman does not supply WAR.}
  \item{career_profiles}{Optional output from \code{build_player_career_profiles()}.}
  \item{minimum_significance}{Minimum zero-to-100 career significance score for milestone candidates.}
  \item{hitters}{Current-season hitter performance summary.}
  \item{pitchers}{Current-season pitcher performance summary, expressed as results allowed.}
  \item{hitter_form}{Current recent-form summary for hitters.}
  \item{pitcher_form}{Current recent-form summary for pitchers.}
  \item{bullpen}{Current bullpen availability summary.}
  \item{season_year}{Season represented by the current summaries.}
  \item{career_data_through}{Last season already included in historical career totals.}
  \item{minimum_bf}{Minimum batters faced required for pitcher race boards.}
  \item{scenarios}{Optional representative completed plate-appearance contexts.}
  \item{availability}{Bullpen availability summary.}
  \item{pitcher_summary}{Current pitcher performance summary.}
  \item{top_n}{Number of reliever options retained per team and batter side.}
  \item{hitter_platoon}{Hitter summaries grouped by opponent hand.}
  \item{pitcher_platoon}{Pitcher summaries grouped by opponent hand.}
  \item{minimum_swings}{Minimum swings required for a pitch identity.}
  \item{pitch_types}{Pitch-type performance summaries.}
  \item{offensive_race}{Descriptive offensive race board.}
  \item{prevention_race}{Descriptive run-prevention race board.}
  \item{active_milestones}{Active-player career milestone watch.}
  \item{historical}{Historical anniversary candidates.}
  \item{team_intelligence}{Team intelligence summary.}
  \item{signature_pitches}{Signature pitch identity board.}
  \item{hitter_matchups}{Qualified hitter matchup-edge board.}
  \item{pitcher_matchups}{Qualified pitcher matchup-edge board.}
  \item{record_top_n}{Career leaderboard depth flagged as record-level context.}
  \item{report_date}{Date for which historical notes are generated.}
  \item{minimum_years}{Minimum age of an anniversary.}
}
\value{
Tibbles at a documented baseball grain: pitch, plate appearance, batted ball,
pitcher appearance, pitcher, manager decision, or editorial story. The role
classifier returns a character vector.
Model validation returns a list containing the training model, row-level
holdout predictions, overall accuracy and calibration metrics, and a binned
calibration table.
}
\description{
These functions form the stable data-contract layer between raw BaseballR or
Lahman inputs and SABRhood simulations, reports, applications, and publishing
products.
}
\details{
Bullpen availability and story scores are transparent versioned heuristics.
They are intended as auditable features and rankings, not calibrated claims of
certainty. Manager decision rows can later train separate pitcher-hook and
replacement-selection models without contaminating pregame inputs with future
information.
}
