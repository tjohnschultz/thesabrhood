test_that("team positional WAR separates strengths and pitching roles", {
  hitters <- data.frame(
    team = c("BOS", "BOS", "BOS", "NYY", "NYY"), league = "AL", player_id = 1:5,
    player_name = c("C One", "C Backup", "OF One", "C Two", "OF Two"), position = c("C", "C", "OF", "C", "OF"),
    pa = 100, war = c(3, 0.5, 2, 1, 4), stringsAsFactors = FALSE
  )
  pitchers <- data.frame(
    team = c("BOS", "BOS", "NYY", "NYY"), league = "AL", player_id = 11:14,
    player_name = c("Starter A", "Reliever A", "Starter B", "Reliever B"), games = c(10, 30, 10, 30),
    starts = c(10, 0, 10, 0), innings_outs = c(180, 90, 180, 90), war = c(2, 1, 3, 0.5),
    stringsAsFactors = FALSE
  )
  board <- build_team_positional_war(hitters, pitchers)
  expect_true(all(c("C", "OF", "SP", "RP") %in% board$position))
  expect_equal(board$team[board$position == "C" & board$mlb_rank == 1], "BOS")
  expect_equal(board$war[board$team == "BOS" & board$position == "C"], 3.5)
  expect_equal(board$top_player_war[board$team == "BOS" & board$position == "C"], 3)
  expect_equal(board$top_player[board$team == "NYY" & board$position == "SP"], "Starter B")
})
