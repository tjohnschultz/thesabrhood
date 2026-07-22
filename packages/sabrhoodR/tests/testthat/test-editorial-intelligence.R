editorial_summary <- function(names, teams, pa, ops, woba, hard_hit, strikeout, walk, run_value, reliability = 0.8) {
  tibble::tibble(
    player_id = seq_along(names), player_name = names, team = teams,
    pa = pa, hits = round(pa * 0.22), home_runs = round(pa * 0.04),
    strikeouts = round(pa * strikeout), batted_balls = round(pa * 0.7),
    ops = ops, woba_estimate = woba, hard_hit_rate = hard_hit,
    strikeout_rate = strikeout, walk_rate = walk,
    run_value_per_pa = run_value, pa_reliability = reliability
  )
}

test_that("active milestone watch adds current season to an unambiguous career", {
  hitters <- editorial_summary("Active Star", "Boston Red Sox", 300, .900, .390, .48, .20, .12, .08)
  hitters$hits <- 62
  hitters$home_runs <- 18
  pitchers <- editorial_summary("Other Pitcher", "Boston Red Sox", 120, .650, .290, .35, .30, .06, -.04)
  profiles <- tibble::tibble(
    playerID = c("star01", "other01"), player_name = c("Active Star", "Other Pitcher"),
    player_name_raw = c("Active Star", "Other Pitcher"), player_name_key = c("active star", "other pitcher"),
    career_H = c(1930, 0), career_HR = c(275, 0), career_SO_pitch = c(0, 920),
    career_significance_score = c(75, 50), recognition_tier = c("star", "notable")
  )

  watch <- build_active_milestone_watch(hitters, pitchers, profiles, 2026, 2025)

  hits <- watch[watch$player_name == "Active Star" & watch$milestone_stat == "hits", ]
  expect_equal(hits$career_to_date_value, 1992)
  expect_equal(hits$distance_to_milestone, 8)
  expect_identical(hits$identity_match_method, "unique_normalized_name_v1")
  expect_error(build_active_milestone_watch(hitters, pitchers, profiles, 2026, 2026), "double counting")
})

test_that("duplicate normalized historical identities are not auto-matched", {
  hitters <- editorial_summary("Shared Name", "A", 300, .900, .390, .48, .20, .12, .08)
  pitchers <- hitters[0, ]
  profiles <- tibble::tibble(
    playerID = c("one01", "two01"), player_name = c("Shared Name (born 1980)", "Shared Name (born 2000)"),
    player_name_raw = c("Shared Name", "Shared Name"), player_name_key = c("shared name", "shared name"),
    career_H = c(1950, 950), career_HR = c(295, 95), career_SO_pitch = 0,
    career_significance_score = c(70, 40), recognition_tier = c("star", "notable")
  )
  expect_equal(nrow(build_active_milestone_watch(hitters, pitchers, profiles, 2026, 2025)), 0)
})

test_that("league race boards rank descriptive performance", {
  hitters <- editorial_summary(c("Best Bat", "Other Bat"), c("A", "B"), c(300, 300), c(1.000, .700), c(.420, .300), c(.55, .30), c(.18, .28), c(.14, .06), c(.10, -.03))
  pitchers <- editorial_summary(c("Best Arm", "Other Arm"), c("A", "B"), c(250, 250), c(.550, .820), c(.250, .360), c(.25, .48), c(.34, .17), c(.05, .11), c(-.08, .04))
  boards <- build_league_race_boards(hitters, pitchers)
  expect_identical(boards$offense$player_name[[1]], "Best Bat")
  expect_identical(boards$run_prevention$player_name[[1]], "Best Arm")
  expect_match(boards$offense$race_label[[1]], "not_award_prediction")
})

test_that("team intelligence combines components and writes a reporting lead", {
  hitters <- editorial_summary(c("A Bat", "B Bat"), c("A", "B"), c(300, 300), c(.950, .700), c(.400, .300), c(.50, .30), c(.18, .28), c(.13, .06), c(.08, -.03))
  pitchers <- editorial_summary(c("A Arm", "B Arm"), c("A", "B"), c(250, 250), c(.580, .800), c(.260, .350), c(.28, .46), c(.32, .18), c(.05, .11), c(-.07, .03))
  hitter_form <- tibble::tibble(player_name = c("A Bat", "B Bat"), team = c("A", "B"), form_score = c(80, 30))
  pitcher_form <- tibble::tibble(player_name = c("A Arm", "B Arm"), team = c("A", "B"), form_score = c(75, 35))
  bullpen <- tibble::tibble(
    pitcher_name = c("A Reliever", "B Reliever"), team = c("A", "B"),
    availability_score = c(.9, .3), availability_status = c("available", "limited"),
    pitches_last_3d = c(8, 65)
  )
  teams <- summarize_team_intelligence(hitters, pitchers, hitter_form, pitcher_form, bullpen)
  expect_identical(teams$team[[1]], "A")
  expect_equal(teams$offense_rank[teams$team == "A"], 1)
  expect_match(teams$team_story[teams$team == "A"], "1st in offensive quality")
  expect_identical(teams$bullpen_health[teams$team == "B"], "taxed")
})
