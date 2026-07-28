suppressPackageStartupMessages({
  library(baseballr)
  library(sabrhoodR)
})

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Pulling Triple-A hitter, pitcher, fielding, and catching season lines for", season_year, "...\n")
hitting <- baseballr::mlb_stats(
  stat_type = "season",
  stat_group = "hitting",
  season = season_year,
  sport_ids = 11,
  limit = 1000
)
pitching <- baseballr::mlb_stats(
  stat_type = "season",
  stat_group = "pitching",
  season = season_year,
  sport_ids = 11,
  limit = 1000
)
fielding <- baseballr::mlb_stats(
  stat_type = "season",
  player_pool = "ALL",
  stat_group = "fielding",
  season = season_year,
  sport_ids = 11,
  limit = 5000
)
catching <- baseballr::mlb_stats(
  stat_type = "season",
  player_pool = "ALL",
  stat_group = "catching",
  season = season_year,
  sport_ids = 11,
  limit = 5000
)
watch <- build_aaa_performance_watch(
  hitting,
  pitching,
  minimum_pa = 100L,
  minimum_ip = 25,
  prospect_age = 24L,
  fielding = fielding,
  catching = catching
)
affiliate_path <- file.path("config", "aaa-affiliates.csv")
positional_war_path <- file.path(output_dir, "team-positional-war.csv")
if (!file.exists(affiliate_path)) stop("Missing Triple-A affiliate mapping: ", affiliate_path, call. = FALSE)
if (!file.exists(positional_war_path)) stop("Missing MLB positional WAR product: ", positional_war_path, call. = FALSE)
affiliates <- utils::read.csv(affiliate_path, stringsAsFactors = FALSE, check.names = FALSE)
positional_war <- utils::read.csv(positional_war_path, stringsAsFactors = FALSE, check.names = FALSE)
callup_radar <- build_aaa_callup_radar(
  watch$hitters,
  watch$pitchers,
  affiliates,
  positional_war,
  maximum_age = 28L
)
aaa_standings_path <- file.path(output_dir, "aaa-standings-current.csv")
aaa_movement_path <- file.path(output_dir, "aaa-standings-movement.csv")
aaa_team_rankings <- data.frame()
if (file.exists(aaa_standings_path) && file.exists(aaa_movement_path)) {
  aaa_standings <- utils::read.csv(aaa_standings_path, stringsAsFactors = FALSE, check.names = FALSE)
  aaa_movement <- utils::read.csv(aaa_movement_path, stringsAsFactors = FALSE, check.names = FALSE)
  aaa_team_rankings <- build_aaa_team_rankings(
    aaa_standings,
    aaa_movement,
    watch$hitters,
    watch$pitchers,
    callup_radar
  )
}
generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
watch$hitters$source_acquired_at_utc <- generated_at
watch$pitchers$source_acquired_at_utc <- generated_at
watch$hitters$season <- season_year
watch$pitchers$season <- season_year
callup_radar$source_acquired_at_utc <- generated_at
callup_radar$season <- season_year
if (nrow(aaa_team_rankings)) {
  aaa_team_rankings$source_acquired_at_utc <- generated_at
  aaa_team_rankings$season <- season_year
}
utils::write.csv(watch$hitters, file.path(output_dir, "aaa-hitter-watch.csv"), row.names = FALSE, na = "")
utils::write.csv(watch$pitchers, file.path(output_dir, "aaa-pitcher-watch.csv"), row.names = FALSE, na = "")
utils::write.csv(callup_radar, file.path(output_dir, "aaa-call-up-radar.csv"), row.names = FALSE, na = "")
if (nrow(aaa_team_rankings)) {
  utils::write.csv(
    aaa_team_rankings,
    file.path(output_dir, "aaa-team-rankings.csv"),
    row.names = FALSE,
    na = ""
  )
}
cat(
  "Built", nrow(watch$hitters), "Triple-A hitter rows,",
  nrow(watch$pitchers), "pitcher rows,", nrow(callup_radar),
  "call-up radar candidates, and", nrow(aaa_team_rankings), "team rankings.\n"
)
