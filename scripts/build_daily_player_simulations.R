workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- file.path(workspace, "data", "derived")
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing player simulation input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
num <- function(value) suppressWarnings(as.numeric(value))
clamp <- function(value, lower, upper) pmin(pmax(value, lower), upper)

games <- read_product("daily-game-inputs.csv")
lineups <- read_product("daily-batting-orders.csv")
hitters <- read_product("fangraphs-season-hitters.csv")
pitchers <- read_product("fangraphs-season-pitchers.csv")
components <- read_product("daily-projection-components-live.csv")
game_projections <- read_product("daily-projections-live.csv")

lineups$game_id <- as.character(lineups$game_id)
lineups$player_id <- as.character(lineups$player_id)
games$game_id <- as.character(games$game_id)
hitters$player_id <- as.character(hitters$player_id)
pitchers$player_id <- as.character(pitchers$player_id)
components$game_id <- as.character(components$game_id)
game_projections$game_id <- as.character(game_projections$game_id)

weighted_mean_safe <- function(value, weight, fallback) {
  value <- num(value); weight <- num(weight)
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(keep)) return(fallback)
  stats::weighted.mean(value[keep], weight[keep])
}
league_avg <- weighted_mean_safe(hitters$avg, hitters$ab, .245)
league_slg <- weighted_mean_safe(hitters$slg, hitters$ab, .405)
league_bb <- sum(num(hitters$walks), na.rm = TRUE) / sum(num(hitters$pa), na.rm = TRUE)
league_k <- sum(num(hitters$strikeouts), na.rm = TRUE) / sum(num(hitters$pa), na.rm = TRUE)
league_hr <- sum(num(hitters$home_runs), na.rm = TRUE) / sum(num(hitters$pa), na.rm = TRUE)
league_fip <- weighted_mean_safe(pitchers$fip, pitchers$innings_outs, 4.20)
league_pitcher_k <- weighted_mean_safe(pitchers$strikeout_rate, pitchers$batters_faced, .225)
league_pitcher_bb <- weighted_mean_safe(pitchers$walk_rate, pitchers$batters_faced, .082)
n_sims <- as.integer(Sys.getenv("SABRHOOD_PLAYER_N_SIMS", unset = "2000"))
if (!is.finite(n_sims) || n_sims < 500L) n_sims <- 2000L

fallback_hitter <- list(pa = 100, ab = 90, hits = league_avg * 90, home_runs = league_hr * 100,
  total_bases = league_slg * 90, avg = league_avg, slg = league_slg, walks = league_bb * 100,
  strikeouts = league_k * 100)

build_lineup <- function(game_id, team_name, starter_id, starter_fip, bullpen_fip, environment_multiplier) {
  order_rows <- lineups[lineups$game_id == game_id & lineups$team_name == team_name, , drop = FALSE]
  order_rows <- order_rows[order(num(order_rows$batting_order)), , drop = FALSE]
  if (nrow(order_rows) < 9L) return(NULL)
  match_index <- match(order_rows$player_id, hitters$player_id)
  rows <- hitters[match_index, , drop = FALSE]
  missing <- is.na(match_index)
  total_bases <- num(rows$singles) + 2 * num(rows$doubles) + 3 * num(rows$triples) + 4 * num(rows$home_runs)
  pa <- num(rows$pa); ab <- num(rows$ab); hits <- num(rows$hits); home_runs <- num(rows$home_runs)
  avg <- num(rows$avg); slg <- num(rows$slg); walks <- num(rows$walks); strikeouts <- num(rows$strikeouts)
  pa[missing | !is.finite(pa)] <- fallback_hitter$pa
  ab[missing | !is.finite(ab)] <- fallback_hitter$ab
  hits[missing | !is.finite(hits)] <- fallback_hitter$hits
  home_runs[missing | !is.finite(home_runs)] <- fallback_hitter$home_runs
  total_bases[missing | !is.finite(total_bases)] <- fallback_hitter$total_bases
  avg[missing | !is.finite(avg)] <- fallback_hitter$avg
  slg[missing | !is.finite(slg)] <- fallback_hitter$slg
  walks[missing | !is.finite(walks)] <- fallback_hitter$walks
  strikeouts[missing | !is.finite(strikeouts)] <- fallback_hitter$strikeouts

  starter <- pitchers[pitchers$player_id == as.character(starter_id), , drop = FALSE]
  starter_k <- if (nrow(starter)) num(starter$strikeout_rate[[1L]]) else league_pitcher_k
  starter_bb <- if (nrow(starter)) num(starter$walk_rate[[1L]]) else league_pitcher_bb
  if (!is.finite(starter_k)) starter_k <- league_pitcher_k
  if (!is.finite(starter_bb)) starter_bb <- league_pitcher_bb
  starter_quality <- clamp(sqrt(starter_fip / league_fip), .82, 1.22)
  bullpen_quality <- clamp(sqrt(bullpen_fip / league_fip), .84, 1.18)
  starter_k_factor <- clamp(sqrt(starter_k / league_pitcher_k), .78, 1.28)
  starter_bb_factor <- clamp(sqrt(starter_bb / league_pitcher_bb), .75, 1.30)
  environment <- clamp(environment_multiplier, .94, 1.08)

  base <- data.frame(
    game_pk = game_id,
    batting_side = ifelse(order_rows$team_side == "away", "away", "home"),
    opposing_pitcher = if (nrow(starter)) starter$player_name[[1L]] else "Starter fallback",
    opposing_pitcher_hand = if (nrow(starter)) starter$throws[[1L]] else "U",
    player_id = order_rows$player_id,
    player = ifelse(is.na(rows$player_name), order_rows$player_name, rows$player_name),
    team = team_name,
    PA = pa, H = hits, HR = home_runs, TB = total_bases,
    AVG = avg, SLG = slg, BB_pct = walks / pmax(pa, 1), K_pct = strikeouts / pmax(pa, 1),
    shrunk_AVG = (hits + 100 * league_avg) / (ab + 100),
    shrunk_SLG = (total_bases + 100 * league_slg) / (ab + 100),
    shrunk_BB_pct = (walks + 75 * league_bb) / (pa + 75),
    shrunk_K_pct = (strikeouts + 75 * league_k) / (pa + 75),
    stringsAsFactors = FALSE
  )
  starter_lineup <- base
  starter_lineup$shrunk_AVG <- clamp(starter_lineup$shrunk_AVG * starter_quality, .150, .360)
  starter_lineup$shrunk_SLG <- clamp(starter_lineup$shrunk_SLG * starter_quality * environment, .220, .700)
  starter_lineup$shrunk_K_pct <- clamp(starter_lineup$shrunk_K_pct * starter_k_factor, .08, .42)
  starter_lineup$shrunk_BB_pct <- clamp(starter_lineup$shrunk_BB_pct * starter_bb_factor, .03, .20)
  starter_lineup$HR <- starter_lineup$HR * starter_quality * environment
  bullpen_lineup <- base
  bullpen_lineup$shrunk_AVG <- clamp(bullpen_lineup$shrunk_AVG * bullpen_quality, .150, .360)
  bullpen_lineup$shrunk_SLG <- clamp(bullpen_lineup$shrunk_SLG * bullpen_quality * environment, .220, .700)
  bullpen_lineup$HR <- bullpen_lineup$HR * bullpen_quality * environment
  list(starter = starter_lineup, bullpen = bullpen_lineup)
}

batter_rows <- list()
starter_rows <- list()
team_rows <- list()
skipped <- list()
for (game_index in seq_len(nrow(games))) {
  game <- games[game_index, , drop = FALSE]
  game_id <- game$game_id[[1L]]
  component <- components[components$game_id == game_id, , drop = FALSE]
  projection <- game_projections[game_projections$game_id == game_id, , drop = FALSE]
  if (!nrow(component) || !nrow(projection)) next
  sides <- list(
    away = list(team = game$away_team[[1L]], starter_id = game$home_starter_id[[1L]], starter_name = game$home_starter_name[[1L]],
      starter_fip = num(component$home_starter_fip_blend[[1L]]), bullpen_fip = num(component$home_bullpen_fip_blend[[1L]]),
      expected_runs = num(projection$away_expected_runs_input[[1L]])),
    home = list(team = game$home_team[[1L]], starter_id = game$away_starter_id[[1L]], starter_name = game$away_starter_name[[1L]],
      starter_fip = num(component$away_starter_fip_blend[[1L]]), bullpen_fip = num(component$away_bullpen_fip_blend[[1L]]),
      expected_runs = num(projection$home_expected_runs_input[[1L]]))
  )
  for (side_index in seq_along(sides)) {
    side_name <- names(sides)[[side_index]]
    side <- sides[[side_index]]
    lineup <- build_lineup(game_id, side$team, side$starter_id, side$starter_fip, side$bullpen_fip, num(component$weather_multiplier[[1L]]))
    if (is.null(lineup)) {
      skipped[[length(skipped) + 1L]] <- data.frame(game_id = game_id, team = side$team, reason = "confirmed nine-player order unavailable", stringsAsFactors = FALSE)
      next
    }
    pitcher <- pitchers[pitchers$player_id == as.character(side$starter_id), , drop = FALSE]
    starter_bf <- if (nrow(pitcher) && num(pitcher$starts[[1L]]) > 0) num(pitcher$batters_faced[[1L]]) / num(pitcher$starts[[1L]]) else 22
    starter_bf <- clamp(starter_bf, 16, 28)
    total_bf <- clamp(36.5 + .85 * (side$expected_runs - 4.4), 32, 43)
    seed <- 721000L + game_index * 10L + side_index
    full <- simulate_lineup_game(
      lineup$starter, lineup$bullpen, n_sims = n_sims, starter_bf = starter_bf, total_bf = total_bf,
      player_col = "player", team_col = "team", seed = seed
    )
    starter_probabilities <- prepare_lineup_event_probs(lineup$starter, player_col = "player", team_col = "team", use_features = TRUE)
    set.seed(seed + 50000L)
    starter_bf_draw <- pmin(pmax(round(stats::rnorm(n_sims, starter_bf, 2.5)), 9L), 32L)
    lineup_weight <- c(1.10, 1.07, 1.04, 1.02, 1, .97, .94, .91, .88)
    probability_mean <- function(column) stats::weighted.mean(num(starter_probabilities[[column]]), lineup_weight)
    starter_k_draw <- stats::rbinom(n_sims, starter_bf_draw, probability_mean("p_K"))
    starter_h_draw <- stats::rbinom(n_sims, starter_bf_draw, probability_mean("p_1B") + probability_mean("p_2B") + probability_mean("p_3B") + probability_mean("p_HR"))
    starter_bb_draw <- stats::rbinom(n_sims, starter_bf_draw, probability_mean("p_BB"))
    starter_hr_draw <- stats::rbinom(n_sims, starter_bf_draw, probability_mean("p_HR"))
    batter <- as.data.frame(full$hitter_summary, stringsAsFactors = FALSE)
    player_index <- match(batter$player, lineup$starter$player)
    batter$player_id <- lineup$starter$player_id[player_index]
    batter$game_id <- game_id
    batter$game_date <- game$game_date[[1L]]
    batter$opponent <- if (side_name == "away") game$home_team[[1L]] else game$away_team[[1L]]
    batter$opposing_starter_id <- side$starter_id
    batter$opposing_starter <- side$starter_name
    batter$opposing_starter_hand <- lineup$starter$opposing_pitcher_hand[[1L]]
    batter$expected_team_runs <- side$expected_runs
    batter$model_version <- "starter_bullpen_player_sim_v1"
    batter$publication_status <- "development_uncalibrated"
    batter_rows[[length(batter_rows) + 1L]] <- batter

    starter_summary <- data.frame(
      sims = n_sims,
      avg_H = mean(starter_h_draw), median_H = stats::median(starter_h_draw), H_p10 = unname(stats::quantile(starter_h_draw, .10)), H_p90 = unname(stats::quantile(starter_h_draw, .90)),
      avg_BB = mean(starter_bb_draw), median_BB = stats::median(starter_bb_draw), BB_p10 = unname(stats::quantile(starter_bb_draw, .10)), BB_p90 = unname(stats::quantile(starter_bb_draw, .90)),
      avg_K = mean(starter_k_draw), median_K = stats::median(starter_k_draw), K_p10 = unname(stats::quantile(starter_k_draw, .10)), K_p90 = unname(stats::quantile(starter_k_draw, .90)),
      avg_HR = mean(starter_hr_draw), median_HR = stats::median(starter_hr_draw), HR_p10 = unname(stats::quantile(starter_hr_draw, .10)), HR_p90 = unname(stats::quantile(starter_hr_draw, .90)),
      p_k_5plus = mean(starter_k_draw >= 5), p_k_7plus = mean(starter_k_draw >= 7),
      stringsAsFactors = FALSE
    )
    starter_summary$game_id <- game_id
    starter_summary$game_date <- game$game_date[[1L]]
    starter_summary$player_id <- side$starter_id
    starter_summary$player_name <- side$starter_name
    starter_summary$team <- if (side_name == "away") game$home_team[[1L]] else game$away_team[[1L]]
    starter_summary$opponent <- side$team
    starter_summary$expected_batters_faced <- starter_bf
    starter_summary$model_version <- "starter_bullpen_player_sim_v1"
    starter_summary$publication_status <- "development_uncalibrated"
    starter_rows[[length(starter_rows) + 1L]] <- starter_summary

    team_summary <- as.data.frame(full$team_summary, stringsAsFactors = FALSE)
    team_summary$game_id <- game_id
    team_summary$game_date <- game$game_date[[1L]]
    team_summary$team <- side$team
    team_summary$opponent <- batter$opponent[[1L]]
    team_summary$model_version <- "starter_bullpen_player_sim_v1"
    team_rows[[length(team_rows) + 1L]] <- team_summary
  }
}

batters <- if (length(batter_rows)) do.call(rbind, batter_rows) else data.frame()
starters <- if (length(starter_rows)) do.call(rbind, starter_rows) else data.frame()
team_simulations <- if (length(team_rows)) do.call(rbind, team_rows) else data.frame()
skipped <- if (length(skipped)) do.call(rbind, skipped) else data.frame(game_id = character(), team = character(), reason = character())

long_rows <- list()
add_metric <- function(metric_id, metric_label, data, probability_col, expected_col, role) {
  if (!nrow(data)) return(NULL)
  data.frame(
    metric_id = metric_id, metric_label = metric_label, game_id = data$game_id,
    player_id = data$player_id, player_name = if (role == "hitter") data$player else data$player_name,
    team = data$team, opponent = data$opponent, role = role,
    probability = num(data[[probability_col]]), expected_value = num(data[[expected_col]]),
    simulations = n_sims, model_version = "starter_bullpen_player_sim_v1",
    publication_status = "development_uncalibrated", stringsAsFactors = FALSE
  )
}
long_rows[[1L]] <- add_metric("batter_hit_1plus", "1+ hit", batters, "p_1_plus_H", "avg_H", "hitter")
long_rows[[2L]] <- add_metric("batter_hit_2plus", "2+ hits", batters, "p_2_plus_H", "avg_H", "hitter")
long_rows[[3L]] <- add_metric("batter_hr_1plus", "1+ home run", batters, "p_1_plus_HR", "avg_HR", "hitter")
long_rows[[4L]] <- add_metric("batter_xbh_1plus", "1+ extra-base hit", batters, "p_1_plus_XBH", "avg_XBH", "hitter")
long_rows[[5L]] <- add_metric("batter_tb_2plus", "2+ total bases", batters, "p_2_plus_TB", "avg_TB", "hitter")
long_rows[[6L]] <- add_metric("batter_tb_3plus", "3+ total bases", batters, "p_3_plus_TB", "avg_TB", "hitter")
if (nrow(starters)) {
  long_rows[[7L]] <- add_metric("pitcher_k_5plus", "5+ strikeouts", starters, "p_k_5plus", "avg_K", "pitcher")
  long_rows[[8L]] <- add_metric("pitcher_k_7plus", "7+ strikeouts", starters, "p_k_7plus", "avg_K", "pitcher")
}
player_board <- do.call(rbind, long_rows[!vapply(long_rows, is.null, logical(1))])
player_board$metric_rank <- ave(-player_board$probability, player_board$metric_id, FUN = function(value) rank(value, ties.method = "first"))
player_board <- player_board[order(player_board$metric_id, player_board$metric_rank), , drop = FALSE]

model_card <- data.frame(
  game_date = if (nrow(games)) games$game_date[[1L]] else NA,
  scheduled_games = nrow(games), simulated_lineups = length(batter_rows), skipped_lineups = nrow(skipped),
  hitter_rows = nrow(batters), starter_rows = nrow(starters), simulations_per_lineup = n_sims,
  starter_phase = "probable-starter season rates, FIP quality, K/BB rates, and expected batters faced",
  bullpen_phase = "active-roster bullpen FIP blend; batting order continues from the starter phase",
  hitter_inputs = "FanGraphs AVG/SLG/BB/K/HR with empirical-Bayes shrinkage and posted batting-order opportunity",
  environment_inputs = "team expected runs and temperature multiplier; empirical park factors pending",
  calibration_status = "prediction snapshot ledger begins now; individual-event calibration requires settled games",
  model_version = "starter_bullpen_player_sim_v1", publication_status = "development_uncalibrated",
  stringsAsFactors = FALSE
)

utils::write.csv(batters, file.path(output_dir, "daily-batter-simulation-summary.csv"), row.names = FALSE, na = "")
utils::write.csv(starters, file.path(output_dir, "daily-starter-simulation-summary.csv"), row.names = FALSE, na = "")
utils::write.csv(team_simulations, file.path(output_dir, "daily-lineup-simulation-summary.csv"), row.names = FALSE, na = "")
utils::write.csv(player_board, file.path(output_dir, "daily-player-simulations.csv"), row.names = FALSE, na = "")
utils::write.csv(skipped, file.path(output_dir, "daily-player-simulation-skips.csv"), row.names = FALSE, na = "")
utils::write.csv(model_card, file.path(output_dir, "daily-player-simulation-model-card.csv"), row.names = FALSE, na = "")
cat("Simulated", nrow(batters), "posted hitters and", nrow(starters), "probable starters across", length(batter_rows), "lineups.\n")
