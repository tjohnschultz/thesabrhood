workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- file.path(workspace, "data", "derived")
read_product <- function(name) utils::read.csv(file.path(output_dir, name), stringsAsFactors = FALSE, check.names = FALSE)

hitters <- read_product("fangraphs-season-hitters.csv")
pitchers <- read_product("fangraphs-season-pitchers.csv")
lineups <- read_product("daily-batting-orders.csv")
probables <- read_product("daily-probable-starters.csv")
board <- build_player_probability_board(hitters, pitchers, lineups, probables)
board$source_note <- "FanGraphs season rates via BaseballR joined to posted batting orders and probable starters. Opponent, park, weather, and lineup-slot adjustments are not yet fitted."
utils::write.csv(board, file.path(output_dir, "daily-player-probabilities.csv"), row.names = FALSE, na = "")
cat("Built", nrow(board), "daily player probability rows across", length(unique(board$metric_id)), "leaderboards.\n")
