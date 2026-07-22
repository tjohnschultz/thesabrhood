suppressPackageStartupMessages({
  library(baseballr)
  library(sabrhoodR)
})

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Pulling Triple-A hitter and pitcher season lines for", season_year, "...\n")
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
watch <- build_aaa_performance_watch(
  hitting,
  pitching,
  minimum_pa = 100L,
  minimum_ip = 25,
  prospect_age = 24L
)
generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
watch$hitters$source_acquired_at_utc <- generated_at
watch$pitchers$source_acquired_at_utc <- generated_at
watch$hitters$season <- season_year
watch$pitchers$season <- season_year
utils::write.csv(watch$hitters, file.path(output_dir, "aaa-hitter-watch.csv"), row.names = FALSE, na = "")
utils::write.csv(watch$pitchers, file.path(output_dir, "aaa-pitcher-watch.csv"), row.names = FALSE, na = "")
cat("Built", nrow(watch$hitters), "Triple-A hitter rows and", nrow(watch$pitchers), "pitcher rows.\n")
