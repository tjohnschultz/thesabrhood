test_that("projection input contract exposes every publication gate", {
  games <- data.frame(
    game_id = c("1", "2"), game_date = c("2026-07-20", "2026-07-20"),
    away_team = c("A", "C"), home_team = c("B", "D"),
    away_starter_id = c("10", ""), home_starter_id = c("20", "40"),
    away_lineup_status = c("projected", "missing"), home_lineup_status = c("confirmed", "confirmed"),
    park_factor = c(1.01, NA), weather_status = c("available", "missing"),
    away_roster_status = c("confirmed", "confirmed"), home_roster_status = c("confirmed", "missing")
  )
  result <- validate_projection_game_inputs(games)

  expect_true(result$projection_ready[[1]])
  expect_identical(result$readiness_label[[1]], "ready")
  expect_false(result$projection_ready[[2]])
  expect_match(result$missing_inputs[[2]], "starters")
  expect_match(result$missing_inputs[[2]], "lineups")
  expect_match(result$missing_inputs[[2]], "park")
})

test_that("bullpen chain updates workload and preserves alternatives", {
  candidates <- tibble::tibble(
    pitcher_id = c("1", "2", "3"), pitcher_name = c("Left", "Right A", "Right B"),
    availability_score = c(1, 0.95, 0.90), throws = c("L", "R", "R"),
    performance_score = c(0.7, 0.8, 0.6), pitcher_role = "reliever",
    days_rest = c(2, 2, 2)
  )
  pockets <- tibble::tibble(
    pocket_id = c("seven", "eight", "nine"), inning = 7:9,
    upcoming_batter_side = c("L", "R", "R"), leverage_index = c(1.3, 1.7, 2.2),
    batters_in_pocket = c(3L, 3L, 3L)
  )
  chain <- plan_bullpen_chain(candidates, pockets, max_pitches_per_reliever = 20L)

  expect_equal(nrow(chain), 3L)
  expect_identical(chain$pitcher_name[[1]], "Left")
  expect_true(all(chain$selection_score >= 0 & chain$selection_score <= 100))
  expect_true(all(chain$cumulative_planned_pitches <= 24L))
  expect_true(all(nzchar(chain$alternatives)))
})

test_that("projection input and bullpen chain failures are explicit", {
  expect_error(validate_projection_game_inputs(data.frame()), "non-empty")
  expect_error(plan_bullpen_chain(data.frame(), data.frame()), "candidates")
})
