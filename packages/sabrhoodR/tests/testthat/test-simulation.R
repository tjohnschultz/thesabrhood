test_that("event probabilities are valid", {
  probabilities <- prepare_lineup_event_probs(example_lineup())

  expect_equal(nrow(probabilities), 9)
  expect_equal(probabilities$final_prob_sum, rep(1, 9), tolerance = 1e-10)
  expect_true(all(probabilities$p_OUT >= 0))
  expect_true(all(probabilities$p_HR <= 1))
})

test_that("starter simulations are deterministic and return derived outputs", {
  first <- simulate_starter_matchup(example_lineup(), n_sims = 25, starter_bf = 18, seed = 42)
  second <- simulate_starter_matchup(example_lineup(), n_sims = 25, starter_bf = 18, seed = 42)

  expect_equal(first$simulation_results, second$simulation_results)
  expect_equal(nrow(first$simulation_results), 25)
  expect_equal(first$pitcher_summary$sims, 25)
  expect_equal(nrow(first$hitter_summary), 9)
  expect_false("event_log" %in% names(first))
})

test_that("invalid lineups fail clearly", {
  bad <- example_lineup()
  bad$SLG <- NULL

  expect_error(validate_lineup_df(bad), "SLG")
})

test_that("game projections are deterministic and resolve every draw", {
  first <- simulate_game_projection(4.2, 4.6, n_sims = 500, seed = 99, away_team = "A", home_team = "H")
  second <- simulate_game_projection(4.2, 4.6, n_sims = 500, seed = 99, away_team = "A", home_team = "H")

  expect_equal(first$draws, second$draws)
  expect_equal(nrow(first$draws), 500)
  expect_true(all(first$draws$away_runs != first$draws$home_runs))
  expect_equal(first$projection_summary$away_win_probability + first$projection_summary$home_win_probability, 1)
  expect_equal(sum(first$margin_distribution$probability), 1)
})

test_that("game projection inputs fail clearly", {
  expect_error(simulate_game_projection(0, 4.4), "away_expected_runs")
  expect_error(simulate_game_projection(4.4, NA_real_), "home_expected_runs")
  expect_error(simulate_game_projection(4.4, 4.4, n_sims = 1.5), "whole number")
})
