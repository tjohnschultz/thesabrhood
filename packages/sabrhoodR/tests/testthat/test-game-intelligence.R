test_that("hook scenario board ranks representative contexts", {
  n <- 500L
  index <- seq_len(n)
  set.seed(42)
  pitches <- sample(20:115, n, replace = TRUE)
  role <- sample(c("starter", "reliever"), n, replace = TRUE)
  probability <- stats::plogis(-4 + 0.045 * pitches + 0.7 * (role == "reliever"))
  decisions <- tibble::tibble(
    pitches_in_appearance = pitches,
    batters_faced_in_appearance = pmax(3, round(pitches / sample(seq(3.2, 4.5, .1), n, replace = TRUE))),
    times_through_order_proxy = sample(1:4, n, replace = TRUE),
    inning = sample(1:9, n, replace = TRUE),
    fielding_score_diff = sample(-6:6, n, replace = TRUE),
    event_key = sample(c("field_out", "single", "walk", "strikeout"), n, replace = TRUE),
    pitcher_role = role,
    pitcher_pulled_after_pa = stats::runif(n) < probability
  )
  model <- fit_manager_hook_model(decisions)
  scenarios <- build_hook_scenario_board(model)
  expect_equal(nrow(scenarios), 6)
  expect_true(all(scenarios$hook_probability >= 0 & scenarios$hook_probability <= 1))
  expect_identical(sort(scenarios$hook_probability_rank), 1:6)
  expect_true(all(grepl("not causal", scenarios$causal_warning)))
})

test_that("bullpen matchup board combines handedness, workload, and performance", {
  availability <- tibble::tibble(
    pitcher_id = c("1", "2", "3"), pitcher_name = c("Left Arm", "Right Arm", "Tired Arm"),
    team = "Test Club", throws = c("L", "R", "L"), days_rest = c(2, 2, 1),
    pitcher_role = "reliever", availability_score = c(.9, .9, .2),
    availability_status = c("available", "available", "limited"),
    pitches_last_3d = c(10, 10, 55)
  )
  summary <- tibble::tibble(
    player_id = c("1", "2", "3"), woba_estimate = c(.280, .270, .250),
    ops = c(.620, .600, .560), strikeout_rate = c(.28, .30, .35),
    hard_hit_rate = c(.32, .30, .25), pa_reliability = c(.8, .8, .8)
  )
  board <- build_bullpen_matchup_board(availability, summary, top_n = 2)
  vs_left <- board[board$upcoming_batter_side == "L", ]
  vs_right <- board[board$upcoming_batter_side == "R", ]
  expect_identical(vs_left$pitcher_name[[1]], "Left Arm")
  expect_identical(vs_right$pitcher_name[[1]], "Right Arm")
  expect_true(all(board$identity_match_method == "exact_mlbam_id"))
})
