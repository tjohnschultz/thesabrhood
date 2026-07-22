award_history_hitters <- function(multiplier = 1, flip = FALSE) {
  names <- c("AL Alpha", "AL Beta", "AL Gamma", "NL Alpha", "NL Beta", "NL Gamma")
  teams <- c("NYY", "BOS", "BAL", "LAD", "SFG", "NYM")
  base <- c(10, 8, 5, 11, 7, 4) * multiplier
  if (flip) base[c(1, 2)] <- base[c(2, 1)] + c(0, 5)
  data.frame(
    season = 2026, player_id = as.character(1:6), player_name = names, team = teams,
    league = rep(c("AL", "NL"), each = 3), age = 28, position = "OF", games = 30,
    pa = pmax(80, 90 * multiplier), ab = 75, hits = base * 2, singles = base,
    doubles = base / 2, triples = 1, home_runs = base, runs = base * 2, rbi = base * 2,
    walks = 15, strikeouts = 20, stolen_bases = 3, avg = .280, obp = .360, slg = .510,
    ops = .870, woba = .370, wrc_plus = 100 + base * 3, offense = base * 1.5,
    defense = base / 3, baserunning = 1, war = base / 4, wpa = base / 6,
    stringsAsFactors = FALSE
  )
}

award_history_pitchers <- function() {
  data.frame(
    season = 2026, player_id = c("21", "22"), player_name = c("AL Arm", "NL Arm"),
    team = c("NYY", "LAD"), league = c("AL", "NL"), age = 28, throws = "R", games = 10,
    starts = 10, wins = 5, losses = 2, saves = 0, innings_display = 60,
    innings_outs = 180, batters_faced = 240, era = 3, whip = 1.1, fip = 3.2, xfip = 3.3,
    strikeouts = 70, walks = 18, strikeout_rate = .29, walk_rate = .075,
    k_minus_bb_rate = .215, war = 2, wpa = 1, stringsAsFactors = FALSE
  )
}

test_that("award history traces current leaders without leaking later scores backward", {
  checkpoints <- list(
    list(date = as.Date("2026-04-12"), hitters = award_history_hitters(1, FALSE), pitchers = award_history_pitchers()),
    list(date = as.Date("2026-04-19"), hitters = award_history_hitters(1.5, TRUE), pitchers = award_history_pitchers())
  )
  result <- build_award_race_history(checkpoints, award = "MVP", top_n = 2, opening_date = as.Date("2026-03-26"))
  expect_equal(length(unique(result$history$checkpoint_date)), 2)
  expect_true(all(result$display$player_id %in% result$leaders$player_id))
  expect_true(any(result$events$event_type == "new_leader"))
  expect_true(all(result$history$race_rating >= 0 & result$history$race_rating <= 100))
})
