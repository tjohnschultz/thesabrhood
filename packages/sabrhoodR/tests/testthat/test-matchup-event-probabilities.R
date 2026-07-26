matchup_summary_row <- function(
    player_id,
    hand = "R",
    pa = 500,
    walks = 45,
    hit_by_pitch = 5,
    strikeouts = 110,
    singles = 80,
    doubles = 25,
    triples = 3,
    home_runs = 22) {
  data.frame(
    player_id = as.character(player_id),
    player_name = paste("Player", player_id),
    team = "TST",
    hand = hand,
    pa = pa,
    walks = walks,
    hit_by_pitch = hit_by_pitch,
    strikeouts = strikeouts,
    singles = singles,
    doubles = doubles,
    triples = triples,
    home_runs = home_runs,
    stringsAsFactors = FALSE
  )
}

test_that("matchup probabilities are mutually exclusive and normalized", {
  hitters <- rbind(
    matchup_summary_row("h1"),
    matchup_summary_row("h2", hand = "L", home_runs = 30, strikeouts = 125)
  )
  pitchers <- matchup_summary_row("p1", hand = "R", pa = 600, home_runs = 24)
  matchup <- data.frame(
    game_id = "g1", batting_order = 1L, batter_id = "h1", pitcher_id = "p1",
    batter_name = "Hitter One", pitcher_name = "Pitcher One"
  )
  result <- build_matchup_event_probabilities(matchup, hitters, pitchers)
  probability_columns <- c("p_BB", "p_HBP", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  expect_equal(rowSums(result[probability_columns]), 1, tolerance = 1e-10)
  expect_true(all(unlist(result[probability_columns]) >= 0))
  expect_true(all(unlist(result[probability_columns]) <= 1))
  expect_equal(result$model_version, "multinomial_log5_platoon_form_v1")
})

test_that("league-neutral players reproduce the league event distribution", {
  hitters <- matchup_summary_row("h1")
  pitchers <- matchup_summary_row("p1")
  matchup <- data.frame(batter_id = "h1", pitcher_id = "p1")
  result <- build_matchup_event_probabilities(
    matchup, hitters, pitchers,
    hitter_prior_pa = 0, pitcher_prior_pa = 0, split_prior_pa = 0,
    form_strength = 0
  )
  league <- attr(result, "league_event_rates")
  expect_equal(result$p_BB, unname(league[["BB"]]), tolerance = 1e-10)
  expect_equal(result$p_K, unname(league[["K"]]), tolerance = 1e-10)
  expect_equal(result$p_HR, unname(league[["HR"]]), tolerance = 1e-10)
})

test_that("power hitter versus homer-prone pitcher raises home-run probability", {
  hitters <- rbind(
    matchup_summary_row("power", home_runs = 55, singles = 60),
    matchup_summary_row("average", home_runs = 18, singles = 90)
  )
  pitchers <- rbind(
    matchup_summary_row("prone", hand = "R", pa = 600, home_runs = 45),
    matchup_summary_row("average_pitcher", hand = "L", pa = 600, home_runs = 20)
  )
  matchup <- data.frame(batter_id = "power", pitcher_id = "prone")
  result <- build_matchup_event_probabilities(
    matchup, hitters, pitchers,
    hitter_prior_pa = 100, pitcher_prior_pa = 100, form_strength = 0
  )
  league <- attr(result, "league_event_rates")
  expect_gt(result$p_HR, unname(league[["HR"]]))
})

test_that("platoon splits are shrunk but still move the matchup", {
  hitters <- rbind(
    matchup_summary_row("h1", hand = "L"),
    matchup_summary_row("h2", hand = "R")
  )
  pitchers <- matchup_summary_row("p1", hand = "R", pa = 650)
  hitter_split <- matchup_summary_row(
    "h1", hand = "L", pa = 180, home_runs = 25, singles = 24,
    walks = 20, hit_by_pitch = 2, strikeouts = 38, doubles = 9, triples = 1
  )
  hitter_split$opponent_hand <- "R"
  matchup <- data.frame(batter_id = "h1", pitcher_id = "p1")
  unsplit <- build_matchup_event_probabilities(
    matchup, hitters, pitchers, form_strength = 0
  )
  split <- build_matchup_event_probabilities(
    matchup, hitters, pitchers, hitter_platoon = hitter_split,
    split_prior_pa = 100, form_strength = 0
  )
  expect_gt(split$p_HR, unsplit$p_HR)
  expect_equal(split$batter_split_pa, 180)
})

test_that("missing player summaries fall back without invalid probabilities", {
  hitters <- matchup_summary_row("h1")
  pitchers <- matchup_summary_row("p1")
  matchup <- data.frame(batter_id = "unknown", pitcher_id = "missing")
  result <- build_matchup_event_probabilities(matchup, hitters, pitchers)
  expect_equal(result$input_status, "league_fallback")
  expect_equal(result$probability_sum, 1, tolerance = 1e-10)
  expect_equal(result$matchup_reliability, 0)
})
