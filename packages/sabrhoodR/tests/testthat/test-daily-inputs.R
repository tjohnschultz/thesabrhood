test_that("BaseballR rosters create an active pitcher contract", {
  raw <- data.frame(
    person_id = c(10, 20, 30),
    person_full_name = c("Starter", "Reliever", "Shortstop"),
    team_id = 111,
    position_abbreviation = c("SP", "RP", "SS"),
    position_type = c("Pitcher", "Pitcher", "Infielder"),
    status_description = "Active",
    roster_type = "active"
  )
  roster <- standardize_baseballr_rosters(raw, "2026-07-20")
  expect_equal(nrow(roster), 3L)
  expect_true(all(roster$is_active))
  expect_identical(roster$roster_pitcher_role, c("starter", "reliever", "position_player"))

  availability <- data.frame(
    pitcher_id = c("10", "20", "99"), pitcher_name = c("Starter", "Reliever", "Depth"),
    team = "Club", availability_score = c(1, 0.8, 0.9)
  )
  bullpen <- filter_active_roster_bullpen(availability, roster, probable_starter_ids = "10")
  expect_identical(bullpen$pitcher_id, "20")
  expect_true(bullpen$active_roster_verified)
})

test_that("schedule, probables, and posted batting orders assemble by MLBAM ID", {
  schedule <- data.frame(
    game_pk = 9001, official_date = "2026-07-20", game_date = "2026-07-20T23:10:00Z",
    teams_away_team_id = 111, teams_away_team_name = "Away",
    teams_home_team_id = 222, teams_home_team_name = "Home", venue_name = "Park"
  )
  probables <- data.frame(
    game_pk = 9001, id = c(10, 20), fullName = c("Away Arm", "Home Arm"),
    team_id = c(111, 222), team = c("Away", "Home")
  )
  orders <- data.frame(
    game_pk = 9001,
    team = rep(c("away", "home"), each = 9),
    teamID = rep(c(111, 222), each = 9),
    teamName = rep(c("Away", "Home"), each = 9),
    batting_order = rep(1:9, 2), id = 1:18,
    fullName = paste("Player", 1:18), abbreviation = "DH"
  )
  inputs <- assemble_baseballr_game_inputs(schedule, probables, orders)
  expect_identical(inputs$games$away_starter_name, "Away Arm")
  expect_identical(inputs$games$home_starter_name, "Home Arm")
  expect_identical(inputs$games$away_lineup_status, "confirmed")
  expect_equal(nrow(inputs$lineups), 18L)
})

test_that("Open-Meteo URL preserves the auditable park forecast contract", {
  url <- build_open_meteo_url(42.3467, -71.0972, "2026-07-20")
  expect_match(url, "latitude=42.3467", fixed = TRUE)
  expect_match(url, "longitude=-71.0972", fixed = TRUE)
  expect_match(url, "precipitation_probability", fixed = TRUE)
  expect_match(url, "start_date=2026-07-20", fixed = TRUE)
  expect_error(build_open_meteo_url(100, 0, "2026-07-20"), "latitude")
})

