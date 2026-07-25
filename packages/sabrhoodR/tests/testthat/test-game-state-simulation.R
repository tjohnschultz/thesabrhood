make_lineup_probabilities <- function(out_probability = 0.70) {
  data.frame(
    batting_order = 1:9,
    batter_id = 101:109,
    batter_name = paste("Hitter", 1:9),
    p_BB = 0.08,
    p_HBP = 0.01,
    p_K = 0.20,
    p_1B = 0.14,
    p_2B = 0.04,
    p_3B = 0.01,
    p_HR = 0.03,
    p_OUT = out_probability
  )
}

test_that("game-state simulation produces coherent game and player outputs", {
  lineup <- make_lineup_probabilities()
  result <- simulate_game_state(
    away_starter_probs = lineup,
    home_starter_probs = lineup,
    away_bullpen_probs = lineup,
    home_bullpen_probs = lineup,
    n_sims = 80,
    seed = 42
  )

  expect_equal(nrow(result$game_results), 80)
  expect_equal(nrow(result$hitter_summary), 18)
  expect_equal(
    result$game_summary$away_win_probability +
      result$game_summary$home_win_probability +
      result$game_summary$tie_probability,
    1
  )
  expect_true(all(result$game_results$away_runs >= 0))
  expect_true(all(result$game_results$home_runs >= 0))
  expect_true(all(result$game_results$innings >= 9))
  expect_true(all(result$hitter_summary$expected_pa > 0))
  expect_true(result$game_summary$bullpen_pa_share > 0)
})

test_that("game-state simulation is reproducible and normalizes input rows", {
  lineup <- make_lineup_probabilities()
  first <- simulate_game_state(lineup, lineup, lineup, lineup, n_sims = 25, seed = 8)
  second <- simulate_game_state(lineup, lineup, lineup, lineup, n_sims = 25, seed = 8)

  expect_equal(first$game_results, second$game_results)
  expect_equal(first$hitter_summary, second$hitter_summary)
})

test_that("game-state simulation rejects incomplete lineups", {
  lineup <- make_lineup_probabilities()
  expect_error(
    simulate_game_state(lineup[-1, ], lineup, lineup, lineup, n_sims = 2),
    "exactly nine"
  )
})
