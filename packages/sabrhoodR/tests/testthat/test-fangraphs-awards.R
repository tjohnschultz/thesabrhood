test_that("FanGraphs season rows standardize and produce league award boards", {
  hitters <- data.frame(
    Season = 2026, team_name = c("NYY", "BOS", "LAD", "SFG"), xMLBAMID = 1:4,
    PlayerName = c("AL One", "AL Two", "NL One", "NL Two"), Age = c(24, 29, 25, 31),
    position = "OF", G = 100, PA = c(420, 390, 430, 380), AB = 350, H = 100,
    HR = c(30, 20, 28, 18), R = 70, RBI = 80, BB = 50, SO = 90, SB = 10,
    AVG = .285, OBP = .370, SLG = .520, OPS = .890, wOBA = .375,
    wRC_plus = c(160, 130, 155, 125), Offense = c(35, 20, 32, 18),
    Defense = c(8, 3, 6, 2), BaseRunning = c(4, 1, 3, 0), WAR = c(6, 3, 5.5, 2.5), WPA = c(3, 1, 2.5, .8),
    check.names = FALSE
  )
  pitchers <- data.frame(
    Season = 2026, team_name = c("NYY", "BOS", "LAD", "SFG"), xMLBAMID = 11:14,
    PlayerName = c("AL Arm", "AL Arm Two", "NL Arm", "NL Arm Two"), Age = c(25, 30, 24, 32), Throws = "R",
    G = 24, GS = 24, W = 12, L = 5, SV = 0, IP = c(150.1, 130.2, 145.0, 125.1), TBF = 600,
    ERA = c(2.2, 3.4, 2.4, 3.5), WHIP = c(.95, 1.2, 1.0, 1.25), FIP = c(2.5, 3.6, 2.6, 3.7),
    xFIP = c(2.7, 3.7, 2.8, 3.8), SO = 170, BB = 40, K_pct = .28, BB_pct = .07,
    `K-BB_pct` = c(.24, .15, .22, .14), WAR = c(5, 2.5, 4.7, 2.2), WPA = 2,
    check.names = FALSE
  )
  compact <- standardize_fangraphs_season(hitters, pitchers, 2026)
  expect_equal(sort(unique(compact$hitters$league)), c("AL", "NL"))
  expect_equal(compact$pitchers$innings_outs[[1]], 451)
  board <- build_award_race_boards(compact$hitters, compact$pitchers, minimum_pa = 1, minimum_outs = 1)
  expect_true(all(c("MVP", "Cy Young", "ROTY watch") %in% board$award))
  expect_equal(board$player_name[board$award == "MVP" & board$league == "AL"][[1]], "AL One")
  expect_true(all(board$eligibility_status[board$award == "ROTY watch"] == "provisional age screen; MLB rookie service not verified"))
})

test_that("MVP combines two-way WAR and relievers receive their own board", {
  hitters <- data.frame(
    player_id = c("1", "2"), player_name = c("Two Way", "Slugger"), team = c("LAD", "SFG"), league = "NL",
    age = 28, pa = c(400, 400), home_runs = c(25, 40), rbi = c(70, 90), runs = c(75, 85),
    wrc_plus = c(150, 155), offense = c(30, 32), defense = c(-5, 2), baserunning = c(2, 1),
    war = c(3, 5), wpa = c(2, 2.5), ab = c(350, 350), stringsAsFactors = FALSE
  )
  pitchers <- data.frame(
    player_id = c("1", "3", "4"), player_name = c("Two Way", "Closer", "Starter"), team = c("LAD", "LAD", "SFG"), league = "NL",
    age = 28, games = c(15, 45, 25), starts = c(15, 0, 25), wins = 5, losses = 2, saves = c(0, 30, 0),
    innings_display = c(80, 45, 150), innings_outs = c(240, 135, 450), batters_faced = c(320, 180, 600),
    era = c(2, 1.8, 2.5), whip = c(1, .9, 1.05), fip = c(2.5, 2.1, 2.7), xfip = c(3, 2.5, 3),
    strikeouts = c(90, 65, 170), walks = c(25, 15, 40), strikeout_rate = c(.28, .36, .28),
    walk_rate = c(.08, .08, .07), k_minus_bb_rate = c(.20, .28, .21), war = c(3, 2, 5), wpa = c(2, 3, 2),
    stringsAsFactors = FALSE
  )
  board <- build_award_race_boards(hitters, pitchers, minimum_pa = 1, minimum_outs = 1)
  two_way <- board[board$award == "MVP" & board$player_id == "1", ]
  expect_equal(two_way$war, 6)
  expect_true("Reliever of the Year" %in% board$award)
  expect_equal(board$player_name[board$award == "Reliever of the Year"], "Closer")
})
