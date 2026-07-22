test_that("hook model produces bounded probabilities", {
  n <- 160L
  decisions <- tibble::tibble(
    pitches_in_appearance = rep(seq(35, 110, length.out = 40), 4),
    batters_faced_in_appearance = rep(seq(10, 31, length.out = 40), 4),
    times_through_order_proxy = rep(c(1, 2, 3, 4), each = 40),
    inning = rep(1:8, length.out = n),
    fielding_score_diff = rep(-5:4, length.out = n),
    event_key = rep(c("field_out", "single", "walk", "strikeout"), length.out = n),
    pitcher_role = rep(c("starter", "reliever"), length.out = n),
    pitcher_pulled_after_pa = seq_len(n) %% 5L == 0L |
      (seq_len(n) > 120L & seq_len(n) %% 2L == 0L)
  )

  model <- fit_manager_hook_model(decisions)
  scored <- predict_manager_hook_probability(model, decisions[1:12, ])

  expect_s3_class(model, "sabrhood_hook_model")
  expect_equal(nrow(scored), 12L)
  expect_true(all(scored$hook_probability >= 0 & scored$hook_probability <= 1))
  expect_identical(unique(scored$hook_model_method), "sabrhood_pooled_hook_logit_v1")
})

test_that("reliever selector values availability and platoon fit", {
  candidates <- tibble::tibble(
    pitcher_id = c("left", "right", "tired"),
    pitcher_name = c("Left Arm", "Right Arm", "Tired Left"),
    throws = c("L", "R", "L"),
    availability_score = c(0.90, 0.90, 0.10),
    performance_score = c(0.60, 0.60, 1.00)
  )

  ranked <- rank_reliever_options(candidates, "L", leverage_index = 1.5)

  expect_identical(ranked$pitcher_id[[1L]], "left")
  expect_identical(ranked$selection_rank, 1:3)
  expect_true(all(ranked$selection_score >= 0 & ranked$selection_score <= 100))
})

test_that("hook validation reserves the latest dates and reports calibration", {
  n <- 360L
  index <- seq_len(n)
  decisions <- tibble::tibble(
    game_date = as.Date("2026-04-01") + (index - 1L) %/% 12L,
    pitches_in_appearance = 30 + index %% 85,
    batters_faced_in_appearance = 8 + index %% 28,
    times_through_order_proxy = 1 + index %% 4,
    inning = 1 + index %% 9,
    fielding_score_diff = -5 + index %% 11,
    event_key = rep(c("field_out", "single", "walk", "strikeout"), length.out = n),
    pitcher_role = rep(c("starter", "starter", "reliever"), length.out = n),
    pitcher_pulled_after_pa = index %% 7L == 0L | (index > 280L & index %% 4L == 0L)
  )

  validation <- validate_manager_hook_model(decisions, validation_fraction = 0.2, calibration_bins = 5L)

  expect_s3_class(validation, "sabrhood_hook_validation")
  expect_equal(nrow(validation$metrics), 1L)
  expect_true(validation$metrics$validation_rows >= 25L)
  expect_true(validation$metrics$roc_auc >= 0 && validation$metrics$roc_auc <= 1)
  expect_equal(nrow(validation$calibration), 5L)
  expect_true(all(validation$validation_predictions$game_date > as.Date(validation$metrics$train_through)))
})
