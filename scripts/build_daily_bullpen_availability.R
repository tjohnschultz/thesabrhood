suppressPackageStartupMessages(library(sabrhoodR))

season <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
pbp_path <- Sys.getenv(
  "SABRHOOD_PBP_PATH",
  unset = file.path(".private-data", "pbp", as.character(season), "current.rds")
)
if (!file.exists(pbp_path)) stop("Private PBP cache is missing: ", pbp_path, call. = FALSE)
pbp <- readRDS(pbp_path)
date_column <- if ("game_date" %in% names(pbp)) "game_date" else "game_date_date"
as_of_date <- max(as.Date(pbp[[date_column]]), na.rm = TRUE)
pitch <- build_pitch_view(pbp)
appearances <- build_pitcher_appearance_view(pitch)
registry <- build_pitcher_registry(appearances)
availability <- build_bullpen_availability(appearances, registry, as_of_date = as_of_date)
output_path <- file.path("data", "derived", "bullpen-availability.csv")
utils::write.csv(availability, output_path, row.names = FALSE, na = "")
cat("Built", nrow(availability), "bullpen availability rows through", as.character(as_of_date), "\n")
