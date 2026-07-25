workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- file.path(workspace, "data", "derived")
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing matchup-model input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

games <- read_product("daily-game-inputs.csv")
lineups <- read_product("daily-batting-orders.csv")
hitters <- read_product("hitter-performance-summary.csv")
pitchers <- read_product("pitcher-performance-summary.csv")
hitter_platoon <- read_product("hitter-platoon-summary.csv")
pitcher_platoon <- read_product("pitcher-platoon-summary.csv")
hitter_form <- read_product("hitter-recent-form.csv")
pitcher_form <- read_product("pitcher-recent-form.csv")

if (!nrow(games)) stop("The daily game input table is empty.", call. = FALSE)
if (!nrow(lineups)) stop("Posted batting orders are required for daily matchup probabilities.", call. = FALSE)

matchups <- lapply(seq_len(nrow(lineups)), function(index) {
  lineup <- lineups[index, , drop = FALSE]
  game <- games[as.character(games$game_id) == as.character(lineup$game_id[[1L]]), , drop = FALSE]
  if (!nrow(game)) stop("No daily game input found for game ", lineup$game_id[[1L]], ".", call. = FALSE)
  game <- game[1L, , drop = FALSE]
  is_away <- tolower(as.character(lineup$team_side[[1L]])) == "away"
  data.frame(
    game_id = as.character(lineup$game_id[[1L]]),
    game_date = as.character(game$game_date[[1L]]),
    team_side = as.character(lineup$team_side[[1L]]),
    batting_order = suppressWarnings(as.integer(lineup$batting_order[[1L]])),
    batter_id = as.character(lineup$player_id[[1L]]),
    batter_name = as.character(lineup$player_name[[1L]]),
    batter_team = as.character(lineup$team_name[[1L]]),
    pitcher_id = as.character(if (is_away) game$home_starter_id[[1L]] else game$away_starter_id[[1L]]),
    pitcher_name = as.character(if (is_away) game$home_starter_name[[1L]] else game$away_starter_name[[1L]]),
    pitcher_team = as.character(if (is_away) game$home_team[[1L]] else game$away_team[[1L]]),
    stringsAsFactors = FALSE
  )
})
matchups <- do.call(rbind, matchups)

probabilities <- build_matchup_event_probabilities(
  matchups = matchups,
  hitters = hitters,
  pitchers = pitchers,
  hitter_platoon = hitter_platoon,
  pitcher_platoon = pitcher_platoon,
  hitter_form = hitter_form,
  pitcher_form = pitcher_form,
  hitter_prior_pa = 200,
  pitcher_prior_pa = 250,
  split_prior_pa = 125,
  form_strength = 1
)
league_rates <- attr(probabilities, "league_event_rates")
probability_columns <- c("p_BB", "p_HBP", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
probabilities$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

event_map <- c(
  BB = "p_BB", HBP = "p_HBP", K = "p_K", X1B = "p_1B",
  X2B = "p_2B", X3B = "p_3B", HR = "p_HR", OUT = "p_OUT"
)
diagnostics <- do.call(rbind, lapply(names(event_map), function(event) {
  values <- suppressWarnings(as.numeric(probabilities[[event_map[[event]]]]))
  data.frame(
    game_date = as.character(games$game_date[[1L]]),
    event = event,
    league_rate = unname(league_rates[[event]]),
    slate_mean = mean(values, na.rm = TRUE),
    slate_p10 = unname(stats::quantile(values, 0.10, na.rm = TRUE)),
    slate_p90 = unname(stats::quantile(values, 0.90, na.rm = TRUE)),
    slate_min = min(values, na.rm = TRUE),
    slate_max = max(values, na.rm = TRUE),
    model_version = "multinomial_log5_platoon_form_v1",
    stringsAsFactors = FALSE
  )
}))

model_card <- data.frame(
  game_date = as.character(games$game_date[[1L]]),
  scheduled_games = length(unique(probabilities$game_id)),
  matchup_rows = nrow(probabilities),
  confirmed_lineup_rows = nrow(lineups),
  overall_player_match_rate = mean(!grepl("fallback", probabilities$input_status)),
  complete_platoon_match_rate = mean(probabilities$input_status == "overall_and_platoon_complete"),
  mean_matchup_reliability = mean(probabilities$matchup_reliability),
  max_probability_sum_error = max(abs(rowSums(probabilities[probability_columns]) - 1)),
  hitter_prior_pa = 200,
  pitcher_prior_pa = 250,
  split_prior_pa = 125,
  form_strength = 1,
  model_version = "multinomial_log5_platoon_form_v1",
  publication_status = "shadow development; not yet driving the public game probabilities",
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

utils::write.csv(
  probabilities,
  file.path(output_dir, "daily-matchup-event-probabilities.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  diagnostics,
  file.path(output_dir, "daily-matchup-event-diagnostics.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  model_card,
  file.path(output_dir, "daily-matchup-event-model-card.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  "Built", nrow(probabilities), "batter-starter matchup distributions across",
  length(unique(probabilities$game_id)), "games.\n"
)
cat(
  "Maximum probability-sum error:",
  format(model_card$max_probability_sum_error, scientific = TRUE), "\n"
)
