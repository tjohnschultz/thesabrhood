test_that("player probability board ranks posted hitters and probable starters", {
  hitters <- data.frame(
    player_id = c("1", "2"), player_name = c("Power Bat", "Contact Bat"), team = c("NYY", "BOS"),
    pa = c(300, 300), hits = c(80, 100), singles = c(45, 80), doubles = c(15, 15), triples = c(0, 2),
    home_runs = c(20, 3), stringsAsFactors = FALSE
  )
  pitchers <- data.frame(
    player_id = "10", player_name = "Strikeout Arm", team = "NYY", starts = 15,
    batters_faced = 330, strikeout_rate = .30, innings_outs = 270, innings_display = 90,
    stringsAsFactors = FALSE
  )
  lineups <- data.frame(game_id = "g1", player_id = c("1", "2"), batting_order = c(3, 1))
  probables <- data.frame(game_id = "g1", starter_id = "10")
  board <- build_player_probability_board(hitters, pitchers, lineups, probables)
  expect_true(all(board$probability >= 0 & board$probability <= 1))
  expect_true(all(c("batter_hr_1plus", "batter_hit_1plus", "batter_tb_2plus", "pitcher_k_5plus", "pitcher_k_7plus") %in% board$metric_id))
  expect_equal(board$player_name[board$metric_id == "batter_hr_1plus" & board$metric_rank == 1], "Power Bat")
  expect_true(board$probability[board$metric_id == "pitcher_k_5plus"] > board$probability[board$metric_id == "pitcher_k_7plus"])
})
