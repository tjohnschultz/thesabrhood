test_that("canonical pitches expose disciplined sequence features", {
  pitches <- build_pitch_view(example_raw_pbp())
  sequenced <- add_pitch_sequence_features(pitches)

  expect_true(all(c(
    "is_swing", "is_whiff", "is_zone", "is_chase", "count_key",
    "previous_pitch_type", "velocity_change", "location_separation", "sequence_key"
  ) %in% names(sequenced)))
  expect_equal(sum(pitches$is_whiff), 2L)
  expect_equal(sum(is.na(sequenced$previous_pitch_type)), 8L)
  expect_true(any(sequenced$pitch_type_changed %in% TRUE))
})

test_that("run expectancy and game context are auditable", {
  pa <- build_plate_appearance_view(example_raw_pbp())
  re24 <- build_run_expectancy_table(pa, minimum_pa = 1L)
  enriched <- add_game_context(add_run_expectancy(pa, re24))

  expect_equal(nrow(re24), 24L)
  expect_true(all(c("expected_runs", "sample_reliability", "run_expectancy_method") %in% names(re24)))
  expect_true(all(c("run_value", "leverage_proxy", "pressure_index") %in% names(enriched)))
  expect_identical(unique(enriched$leverage_method), "sabrhood_context_proxy_v1")
})

test_that("player and pitch-type summaries carry reliability", {
  pitches <- build_pitch_view(example_raw_pbp())
  pa_base <- build_plate_appearance_view(pitches)
  re24 <- build_run_expectancy_table(pa_base, minimum_pa = 1L)
  pa <- add_game_context(add_run_expectancy(pa_base, re24))
  hitters <- summarize_hitters(pa)
  pitchers <- summarize_pitchers(pa, group_by = "opponent_hand")
  arsenals <- summarize_pitch_types(pitches, minimum_pitches = 1L)

  expect_equal(nrow(hitters), 4L)
  expect_true(all(c("ops", "woba_estimate", "pa_reliability", "sample_label") %in% names(hitters)))
  expect_true("opponent_hand" %in% names(pitchers))
  expect_true(all(c("usage_rate", "whiff_rate", "pitch_change_rate", "pitch_reliability") %in% names(arsenals)))
})

test_that("recent form compares non-overlapping windows", {
  raw <- dplyr::bind_rows(
    dplyr::mutate(example_raw_pbp(), game_date = "2026-06-01"),
    dplyr::mutate(example_raw_pbp(), game_pk = game_pk + 10L, game_date = "2026-07-02")
  )
  pa_base <- build_plate_appearance_view(raw)
  re24 <- build_run_expectancy_table(pa_base, minimum_pa = 1L)
  pa <- add_run_expectancy(pa_base, re24)
  form <- compare_recent_player_form(
    pa,
    perspective = "batter",
    recent_days = 7L,
    minimum_recent_pa = 1L,
    minimum_baseline_pa = 1L,
    as_of_date = "2026-07-02"
  )

  expect_true(nrow(form) > 0L)
  expect_true(all(c("form_score", "form_label", "recent_start", "form_score_confidence") %in% names(form)))
  expect_true(all(form$form_score >= 0 & form$form_score <= 100))
})
