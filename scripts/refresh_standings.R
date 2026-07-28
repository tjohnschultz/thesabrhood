suppressPackageStartupMessages({
  library(baseballr)
  library(dplyr)
  library(sabrhoodR)
})

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
configured_report_date <- trimws(Sys.getenv("SABRHOOD_PBP_END", unset = ""))
report_date <- as.Date(if (nzchar(configured_report_date)) configured_report_date else Sys.Date() - 1)
prior_date <- report_date - 7
output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

fetch_leagues <- function(ids, date, level) {
  rows <- lapply(ids, function(id) {
    raw <- baseballr::mlb_standings(
      season = season_year,
      date = as.character(date),
      league_id = id
    )
    standardize_standings_snapshot(raw, date, level)
  })
  dplyr::bind_rows(rows)
}

cat("Pulling MLB and Triple-A standings for", as.character(report_date), "...\n")
mlb_current <- fetch_leagues(c(103, 104), report_date, "MLB")
mlb_prior <- fetch_leagues(c(103, 104), prior_date, "MLB")
aaa_current <- fetch_leagues(c(117, 112), report_date, "AAA")
aaa_prior <- fetch_leagues(c(117, 112), prior_date, "AAA")

mlb_movement <- build_standings_movement(mlb_current, mlb_prior)
aaa_movement <- build_standings_movement(aaa_current, aaa_prior)
generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

add_metadata <- function(data) {
  data$season <- season_year
  data$source_through <- as.character(report_date)
  data$source_acquired_at_utc <- generated_at
  data
}
mlb_current <- add_metadata(mlb_current)
mlb_movement <- add_metadata(mlb_movement)
aaa_current <- add_metadata(aaa_current)
aaa_movement <- add_metadata(aaa_movement)

utils::write.csv(
  mlb_current,
  file.path(output_dir, "mlb-standings-current.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  mlb_movement,
  file.path(output_dir, "mlb-standings-movement.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  aaa_current,
  file.path(output_dir, "aaa-standings-current.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  aaa_movement,
  file.path(output_dir, "aaa-standings-movement.csv"),
  row.names = FALSE,
  na = ""
)
cat(
  "Built", nrow(mlb_current), "MLB standings rows and",
  nrow(aaa_current), "Triple-A standings rows with seven-day movement.\n"
)
