test_that("Triple-A watch separates young-player lens from performance ranking", {
  hitting <- data.frame(
    player_id = 1:4, player_full_name = paste("Hitter", 1:4), age = c(21, 23, 26, 28),
    team_name = "AAA Club", position_abbreviation = "OF", games_played = 60,
    plate_appearances = c(220, 210, 205, 200), home_runs = c(15, 10, 20, 5),
    stolen_bases = c(12, 4, 1, 2), strike_outs = c(45, 35, 70, 30),
    base_on_balls = c(30, 25, 15, 40), avg = c(.280, .270, .260, .250),
    obp = c(.370, .350, .310, .390), slg = c(.510, .450, .520, .390), ops = c(.880, .800, .830, .780)
  )
  pitching <- data.frame(
    player_id = 11:14, player_full_name = paste("Pitcher", 1:4), age = c(22, 24, 26, 29),
    team_name = "AAA Club", position_abbreviation = "P", games_played = 15,
    innings_pitched = c(60, 55, 50, 45), era = c(2.5, 3.0, 3.4, 4.0), whip = c(1.0, 1.1, 1.2, 1.3),
    strike_outs = c(75, 65, 55, 40), base_on_balls = c(15, 18, 20, 22),
    batters_faced = c(240, 225, 210, 195), home_runs = c(5, 6, 7, 8)
  )
  watch <- build_aaa_performance_watch(hitting, pitching, minimum_pa = 100, minimum_ip = 20)
  expect_equal(nrow(watch$hitters), 4L)
  expect_equal(nrow(watch$pitchers), 4L)
  expect_true(any(watch$hitters$age_lens == "age-qualified watch"))
  expect_true(all(watch$hitters$performance_score >= 0 & watch$hitters$performance_score <= 100))
})
