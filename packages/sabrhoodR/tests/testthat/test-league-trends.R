test_that("rolling league trends preserve usage shares and production rates", {
  pitches <- data.frame(
    game_date = as.Date(c("2026-04-01", "2026-04-01", "2026-04-02", "2026-04-02")),
    pitch_type = c("FF", "FF", "SL", "FF"), pitch_name = c("Four-Seam", "Four-Seam", "Slider", "Four-Seam")
  )
  pa <- data.frame(
    game_date = as.Date(c("2026-04-01", "2026-04-02")), is_at_bat = c(TRUE, TRUE),
    is_hit = c(TRUE, FALSE), is_home_run = c(FALSE, TRUE), is_walk = c(FALSE, FALSE),
    is_strikeout = c(FALSE, TRUE), total_bases = c(1, 4), is_batted_ball = c(TRUE, TRUE), is_hard_hit = c(FALSE, TRUE)
  )
  result <- build_rolling_league_trends(pitches, pa, rolling_days = 2L)
  final_usage <- result$pitch_usage[result$pitch_usage$date == as.Date("2026-04-02"), ]
  expect_equal(sum(final_usage$usage_rate_rolling), 1)
  expect_equal(result$production$slugging_percentage[[2]], 2.5)
  expect_equal(result$production$strikeout_rate[[2]], 0.5)
})
