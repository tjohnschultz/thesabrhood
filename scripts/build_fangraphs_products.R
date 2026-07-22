workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
source_path <- file.path(workspace, ".private-data", "sources", paste0("fangraphs-season-", season_year, ".rds"))
if (!file.exists(source_path)) stop("Run scripts/fetch_fangraphs_season_source.R first.", call. = FALSE)

snapshot <- readRDS(source_path)
compact <- standardize_fangraphs_season(snapshot$hitters, snapshot$pitchers, season_year)
prior <- standardize_fangraphs_season(snapshot$prior_hitters, snapshot$prior_pitchers, season_year - 1L)
awards <- build_award_race_boards(compact$hitters, compact$pitchers, prior$hitters, prior$pitchers)
positional_war <- build_team_positional_war(compact$hitters, compact$pitchers)
compact$hitters$source_acquired_at_utc <- snapshot$acquired_at_utc
compact$pitchers$source_acquired_at_utc <- snapshot$acquired_at_utc
positional_war$source_acquired_at_utc <- snapshot$acquired_at_utc
awards$source_acquired_at_utc <- snapshot$acquired_at_utc
awards$source_note <- "FanGraphs season leaderboard via BaseballR; award score is a transparent performance index, not a ballot forecast."

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path(workspace, "data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(compact$hitters, file.path(output_dir, "fangraphs-season-hitters.csv"), row.names = FALSE, na = "")
utils::write.csv(compact$pitchers, file.path(output_dir, "fangraphs-season-pitchers.csv"), row.names = FALSE, na = "")
utils::write.csv(awards, file.path(output_dir, "award-race-board.csv"), row.names = FALSE, na = "")
utils::write.csv(positional_war, file.path(output_dir, "team-positional-war.csv"), row.names = FALSE, na = "")
cat("Built", nrow(compact$hitters), "FanGraphs hitter rows,", nrow(compact$pitchers), "pitcher rows,",
  nrow(awards), "award-watch rows, and", nrow(positional_war), "team-position rows.\n")
