test_that("tracking leaderboards retain 100 mph counts and opportunity rates", {
  pitches <- tibble::tibble(
    pitcher_id = c("1", "1", "2"),
    pitcher_name = c("Pitcher A", "Pitcher A", "Pitcher B"),
    fielding_team = c("AAA", "AAA", "BBB"),
    start_speed = c(100.4, 99.1, 101.2),
    batter_id = c("10", "10", "20"),
    batter_name = c("Hitter A", "Hitter A", "Hitter B"),
    batting_team = c("BBB", "BBB", "AAA")
  )
  pa <- tibble::tibble(
    batter_id = c("10", "10", "20"),
    batter_name = c("Hitter A", "Hitter A", "Hitter B"),
    batting_team = c("BBB", "BBB", "AAA"),
    pitcher_id = c("1", "1", "2"),
    pitcher_name = c("Pitcher A", "Pitcher A", "Pitcher B"),
    fielding_team = c("AAA", "AAA", "BBB"),
    is_batted_ball = c(TRUE, TRUE, TRUE),
    launch_speed = c(100.1, 98.0, 103.0)
  )

  board <- build_tracking_event_leaderboards(pitches, pa)

  expect_equal(board$pitchers$pitches_100_plus[board$pitchers$pitcher_id == "1"], 1)
  expect_equal(board$hitters$batted_balls_100_plus[board$hitters$batter_id == "10"], 1)
  expect_equal(board$hitters$batted_balls_100_plus_rate[board$hitters$batter_id == "10"], 0.5)
  expect_equal(sum(board$teams$pitches_100_plus), 2)
})
