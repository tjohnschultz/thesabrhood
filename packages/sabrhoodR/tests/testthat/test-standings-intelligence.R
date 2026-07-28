test_that("standings movement treats rank gains as positive", {
  current <- tibble::tibble(
    snapshot_date = as.Date("2026-07-25"),
    team_id = c("1", "2"),
    team = c("Alpha", "Beta"),
    winning_percentage = c(.600, .500),
    run_differential = c(40, 5),
    games_back = c(0, 4),
    division_rank = c(1, 2),
    league_rank = c(2, 8)
  )
  prior <- tibble::tibble(
    snapshot_date = as.Date("2026-07-18"),
    team_id = c("1", "2"),
    team = c("Alpha", "Beta"),
    winning_percentage = c(.550, .520),
    run_differential = c(28, 10),
    games_back = c(2, 3),
    division_rank = c(2, 1),
    league_rank = c(5, 6)
  )

  movement <- build_standings_movement(current, prior)
  alpha <- movement[movement$team_id == "1", ]

  expect_equal(alpha$division_rank_change, 1)
  expect_equal(alpha$games_back_change, 2)
  expect_gt(alpha$movement_score, 0)
  expect_equal(alpha$movement_label, "surging")
})

test_that("Triple-A team rankings combine record and player pipelines", {
  standings <- tibble::tibble(
    team_id = c("1", "2"),
    team = c("Red Wings", "Bisons"),
    winning_percentage = c(.600, .500),
    run_differential = c(40, 0),
    wins = c(60, 50),
    losses = c(40, 50)
  )
  movement <- tibble::tibble(
    team_id = c("1", "2"),
    movement_score = c(2, -1),
    movement_label = c("surging", "steady"),
    division_rank_change = c(1, 0),
    run_differential_change = c(10, -2)
  )
  hitters <- tibble::tibble(
    team = c("Rochester Red Wings", "Buffalo Bisons"),
    performance_score = c(80, 55),
    age = c(22, 25)
  )
  pitchers <- tibble::tibble(
    team = c("Rochester Red Wings", "Buffalo Bisons"),
    performance_score = c(75, 50),
    age = c(23, 26)
  )
  callups <- tibble::tibble(
    team = "Rochester Red Wings",
    callup_score = 75
  )

  rankings <- build_aaa_team_rankings(standings, movement, hitters, pitchers, callups)

  expect_equal(rankings$full_team[[1]], "Rochester Red Wings")
  expect_equal(rankings$team_rank[[1]], 1)
  expect_gt(rankings$team_strength_score[[1]], rankings$team_strength_score[[2]])
})
