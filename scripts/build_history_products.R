suppressPackageStartupMessages(library(sabrhoodR))
if (!requireNamespace("Lahman", quietly = TRUE)) stop("Lahman is required.", call. = FALSE)

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
configured_date <- Sys.getenv("SABRHOOD_DATE", unset = "")
report_date <- as.Date(if (nzchar(configured_date)) configured_date else Sys.Date())
if (is.na(report_date)) stop("SABRHOOD_DATE must use YYYY-MM-DD.", call. = FALSE)

cat("Building historical career profiles and recognition layers for", as.character(report_date), "...\n")
career_profiles <- build_player_career_profiles(
  people = Lahman::People,
  batting = Lahman::Batting,
  pitching = Lahman::Pitching,
  awards_players = Lahman::AwardsPlayers,
  allstar = Lahman::AllstarFull,
  hall_of_fame = Lahman::HallOfFame,
  batting_post = Lahman::BattingPost,
  pitching_post = Lahman::PitchingPost
)
historical <- build_historical_anniversary_notes(
  Lahman::People,
  report_date = report_date,
  career_profiles = career_profiles
)
milestone_notes <- build_career_milestone_notes(career_profiles)
public_profiles <- career_profiles[
  career_profiles$career_significance_score >= 20,
  ,
  drop = FALSE
]
public_profiles <- public_profiles[
  order(-public_profiles$career_significance_score),
  ,
  drop = FALSE
]

generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
for (name in c("historical", "milestone_notes", "public_profiles")) {
  value <- get(name)
  value$report_date <- as.character(report_date)
  value$generated_at_utc <- generated_at
  assign(name, value)
}

utils::write.csv(historical, file.path(output_dir, "historical-anniversary-notes.csv"), row.names = FALSE, na = "")
utils::write.csv(milestone_notes, file.path(output_dir, "historical-milestone-notes.csv"), row.names = FALSE, na = "")
utils::write.csv(public_profiles, file.path(output_dir, "historical-player-profiles.csv"), row.names = FALSE, na = "")
cat("Historical profiles:", nrow(public_profiles), "| anniversary notes:", nrow(historical), "| milestone notes:", nrow(milestone_notes), "\n")
