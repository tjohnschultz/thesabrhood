test_that("full-game lineup simulation carries hitters from starter to bullpen", {
  lineup <- data.frame(
    player = paste("Hitter", 1:9), team = "BOS", PA = rep(300, 9), H = rep(75, 9), HR = rep(12, 9),
    TB = rep(120, 9), AVG = rep(.250, 9), SLG = rep(.400, 9), BB_pct = rep(.09, 9), K_pct = rep(.22, 9),
    stringsAsFactors = FALSE
  )
  bullpen <- lineup
  bullpen$shrunk_K_pct <- .28
  bullpen$shrunk_AVG <- .235
  bullpen$shrunk_SLG <- .375
  bullpen$shrunk_BB_pct <- .09
  result <- simulate_lineup_game(lineup, bullpen, n_sims = 100L, starter_bf = 21, total_bf = 38, seed = 55)
  expect_equal(nrow(result$hitter_summary), 9)
  expect_true(all(result$hitter_summary$avg_PA > 3))
  expect_true(all(result$hitter_summary$avg_bullpen_PA > 0))
  expect_true(all(result$hitter_summary$p_1_plus_HR >= 0 & result$hitter_summary$p_1_plus_HR <= 1))
  expect_equal(result$team_summary$sims, 100)
})
