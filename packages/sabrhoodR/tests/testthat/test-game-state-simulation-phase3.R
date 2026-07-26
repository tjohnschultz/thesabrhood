make_phase3_lineup <- function() {
  data.frame(
    batting_order = 1:9,
    batter_id = 101:109,
    batter_name = paste("Hitter", 1:9),
    p_BB = 0.08, p_HBP = 0.01, p_K = 0.20, p_1B = 0.14,
    p_2B = 0.04, p_3B = 0.01, p_HR = 0.03, p_OUT = 0.49
  )
}

make_phase3_pool <- function() {
  pitchers <- data.frame(
    pitcher_id = c("201", "202", "203"),
    pitcher_name = c("Reliever A", "Reliever B", "Reliever C"),
    throws = c("R", "L", "R"),
    pitcher_role = c("high_leverage", "middle", "closer"),
    availability_score = c(0.95, 0.75, 0.85)
  )
  do.call(rbind, lapply(seq_len(nrow(pitchers)), function(i) {
    cbind(pitchers[rep(i, 9), , drop = FALSE], make_phase3_lineup())
  }))
}

test_that("Phase 3 simulates detailed events and named relievers", {
  lineup <- make_phase3_lineup()
  pool <- make_phase3_pool()
  second_pool <- make_phase3_pool()
  second_pool$pitcher_id <- as.character(as.integer(second_pool$pitcher_id) + 100L)
  second_pool$pitcher_name <- paste(second_pool$pitcher_name, "Second")
  result <- simulate_game_state_phase3(
    lineup, lineup, lineup, lineup,
    away_reliever_probs = pool,
    home_reliever_probs = second_pool,
    n_sims = 60,
    seed = 33
  )

  expect_equal(nrow(result$game_results), 60)
  expect_equal(nrow(result$hitter_summary), 18)
  expect_equal(nrow(result$reliever_summary), 6)
  expect_true(all(c(
    "expected_runs", "expected_rbi", "expected_sb", "expected_sf", "expected_gdp"
  ) %in% names(result$hitter_summary)))
  expect_true(result$game_summary$mean_relievers_used > 0)
  expect_true(sum(result$reliever_summary$selection_count) > 0)
  expect_true(all(result$reliever_summary$appearance_probability >= 0))
  expect_equal(
    result$game_summary$away_win_probability +
      result$game_summary$home_win_probability +
      result$game_summary$tie_probability,
    1
  )
})

test_that("Phase 3 park multipliers affect advancement without invalid states", {
  lineup <- make_phase3_lineup()
  fast_park <- simulate_game_state_phase3(
    lineup, lineup, lineup, lineup,
    n_sims = 30,
    seed = 9,
    park_baserunning = list(
      single_second_score = 1.15,
      single_first_to_third = 1.15,
      double_first_score = 1.10,
      steal_attempt = 1.10
    )
  )
  expect_true(all(fast_park$game_results$away_runs >= 0))
  expect_true(all(fast_park$game_results$home_runs >= 0))
  expect_true(all(fast_park$hitter_summary$expected_pa > 0))
})

test_that("Phase 4 activates empirical runner profiles", {
  lineup <- make_phase3_lineup()
  runner_profiles <- do.call(rbind, lapply(lineup$batter_id, function(id) {
    data.frame(
      runner_id = as.character(id),
      opportunity_type = c("steal_second", "steal_third"),
      shrunk_rate = 0,
      shrunk_success_rate = 0.78
    )
  }))
  result <- simulate_game_state_phase4(
    lineup, lineup, lineup, lineup,
    n_sims = 25,
    seed = 51,
    away_runner_profiles = runner_profiles,
    home_runner_profiles = runner_profiles
  )

  expect_equal(
    result$game_summary$model_version,
    "plate_appearance_state_machine_phase4_baserunning_v1"
  )
  expect_equal(result$game_summary$mean_steal_attempts, 0)
})
