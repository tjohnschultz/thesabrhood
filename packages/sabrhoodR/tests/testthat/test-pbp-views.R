test_that("canonical PBP views retain stable grains", {
  pitches <- build_pitch_view(example_raw_pbp())
  plate_appearances <- build_plate_appearance_view(pitches)
  batted_balls <- build_batted_ball_view(plate_appearances)
  appearances <- build_pitcher_appearance_view(pitches)

  expect_equal(nrow(pitches), 10)
  expect_equal(nrow(plate_appearances), 8)
  expect_equal(nrow(batted_balls), 6)
  expect_equal(nrow(appearances), 4)
  expect_equal(sum(appearances$is_starter), 2)
  expect_equal(sum(appearances$pitches), 10)
})

test_that("pitcher registry classifies observed starter and relief roles", {
  appearances <- build_pitcher_appearance_view(example_raw_pbp())
  registry <- build_pitcher_registry(appearances, as_of_date = "2026-07-02")

  expect_equal(registry$pitcher_role[registry$pitcher_id == "10"], "opener")
  expect_equal(registry$pitcher_role[registry$pitcher_id == "20"], "reliever")
  expect_equal(registry$throws[registry$pitcher_id == "20"], "L")
  expect_equal(classify_pitcher_role(20, 18, 22, NA), "starter")
})

test_that("bullpen availability and manager decisions are derived", {
  appearances <- build_pitcher_appearance_view(example_raw_pbp())
  registry <- build_pitcher_registry(appearances, as_of_date = "2026-07-02")
  availability <- build_bullpen_availability(appearances, registry, as_of_date = "2026-07-03")
  decisions <- build_manager_pitching_decisions(build_plate_appearance_view(example_raw_pbp()), registry)

  expect_true("availability_score" %in% names(availability))
  expect_true(all(availability$availability_score >= 0 & availability$availability_score <= 1))
  expect_equal(sum(decisions$pitcher_pulled_after_pa, na.rm = TRUE), 2)
  expect_true(all(c("next_pitcher_id", "next_batter_side", "pitcher_role") %in% names(decisions)))
})
