test_that("player change profiles select the largest direction-aware change", {
  form <- tibble::tibble(
    player_id = c(1L, 2L, 3L), player_name = c("A", "B", "C"),
    team = "Test Club", hand = "R", perspective = "batter",
    recent_pa = 30L, baseline_pa = 100L, form_score = c(80, 50, 20),
    form_score_confidence = 0.3, form_label = c("surging", "stable", "slumping"),
    recent_start = as.Date("2026-06-01"), as_of_date = as.Date("2026-06-14"),
    recent_ops = c(1.2, .7, .6), baseline_ops = c(.7, .7, .8), ops_delta = c(.5, 0, -.2),
    recent_woba_estimate = c(.45, .32, .25), baseline_woba_estimate = .32,
    woba_estimate_delta = c(.13, 0, -.07),
    recent_strikeout_rate = c(.1, .2, .35), baseline_strikeout_rate = .2,
    strikeout_rate_delta = c(-.1, 0, .15),
    recent_walk_rate = c(.2, .08, .04), baseline_walk_rate = .08,
    walk_rate_delta = c(.12, 0, -.04),
    recent_hard_hit_rate = c(.7, .4, .3), baseline_hard_hit_rate = .4,
    hard_hit_rate_delta = c(.3, 0, -.1),
    recent_run_value_per_pa = c(.3, 0, -.1), baseline_run_value_per_pa = 0,
    run_value_per_pa_delta = c(.3, 0, -.1)
  )
  season <- tibble::tibble(
    player_id = 1:3, perspective = "batter", pa = 200L,
    ops = c(.95, .75, .6), woba_estimate = c(.4, .32, .25),
    strikeout_rate = c(.15, .2, .3), walk_rate = c(.14, .08, .04),
    hard_hit_rate = c(.6, .4, .2), run_value_per_pa = c(.2, 0, -.1),
    pa_reliability = .7, sample_label = "strong"
  )
  output <- build_player_change_profiles(form, season)
  expect_equal(nrow(output), 3L)
  expect_true(all(output$identity_match_method == "exact_mlbam_player_id"))
  expect_true("perspective" %in% names(output))
  expect_false(any(c("perspective.x", "perspective.y") %in% names(output)))
  expect_true(all(output$season_ops_percentile >= 0 & output$season_ops_percentile <= 100))
  expect_true(all(output$change_signal_score >= 0 & output$change_signal_score <= 100))
  expect_gt(output$ops_change_z[output$player_id == 1L], 0)
  expect_lt(output$strikeout_rate_change_z[output$player_id == 3L], 0)
  expect_true(all(nzchar(output$change_context)))
})

test_that("pitcher changes treat lower results allowed as improvement", {
  form <- tibble::tibble(
    player_id = 1:3, player_name = c("A", "B", "C"), team = "Test Club", hand = "R",
    perspective = "pitcher", recent_pa = 20L, baseline_pa = 80L,
    form_score = c(80, 50, 20), form_score_confidence = .2,
    form_label = c("surging", "stable", "slumping"),
    recent_start = as.Date("2026-06-01"), as_of_date = as.Date("2026-06-14"),
    recent_ops = c(.4, .7, 1.1), baseline_ops = .7, ops_delta = c(-.3, 0, .4),
    recent_woba_estimate = c(.2, .32, .5), baseline_woba_estimate = .32,
    woba_estimate_delta = c(-.12, 0, .18),
    recent_strikeout_rate = c(.4, .2, .1), baseline_strikeout_rate = .2,
    strikeout_rate_delta = c(.2, 0, -.1),
    recent_walk_rate = c(.02, .08, .18), baseline_walk_rate = .08,
    walk_rate_delta = c(-.06, 0, .1),
    recent_hard_hit_rate = c(.2, .4, .7), baseline_hard_hit_rate = .4,
    hard_hit_rate_delta = c(-.2, 0, .3),
    recent_run_value_per_pa = c(-.2, 0, .3), baseline_run_value_per_pa = 0,
    run_value_per_pa_delta = c(-.2, 0, .3)
  )
  season <- tibble::tibble(
    player_id = 1:3, perspective = "pitcher", pa = 200L,
    ops = c(.5, .7, .9), woba_estimate = c(.25, .32, .4),
    strikeout_rate = c(.35, .2, .1), walk_rate = c(.04, .08, .14),
    hard_hit_rate = c(.25, .4, .6), run_value_per_pa = c(-.1, 0, .2),
    pa_reliability = .7, sample_label = "strong"
  )
  output <- build_player_change_profiles(form, season)
  expect_gt(output$ops_change_z[output$player_id == 1L], 0)
  expect_equal(output$season_ops_percentile[output$player_id == 1L], 100)
  expect_equal(output$season_ops_percentile[output$player_id == 3L], 0)
})
