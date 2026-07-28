run_game_pbp_fixture <- function() {
  tibble::tibble(
    game_pk = 9001L,
    game_date = "2026-07-01",
    isPitch = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    pitchNumber = c(1L, 1L, 2L, NA, 1L),
    about.atBatIndex = c(0L, 1L, 1L, 1L, 2L),
    about.inning = 1L,
    about.halfInning = "top",
    about.isTopInning = TRUE,
    batting_team = "Away",
    fielding_team = "Home",
    home_team = "Home",
    away_team = "Away",
    matchup.pitcher.id = 500L,
    matchup.pitcher.fullName = "Test Pitcher",
    matchup.pitchHand.code = "R",
    matchup.batter.id = c(101L, 102L, 102L, 102L, 103L),
    matchup.batter.fullName = c("Runner One", "Batter Two", "Batter Two", "Batter Two", "Batter Three"),
    matchup.batSide.code = "R",
    count.balls.start = c(0L, 0L, 1L, 1L, 0L),
    count.strikes.start = c(0L, 0L, 0L, 0L, 0L),
    count.outs.start = 0L,
    count.balls.end = c(0L, 1L, 1L, 1L, 0L),
    count.strikes.end = c(1L, 0L, 1L, 1L, 1L),
    count.outs.end = 0L,
    result.eventType = c("single", "strikeout", "strikeout", NA, "field_out"),
    result.event = c("Single", "Strikeout", "Strikeout", NA, "Field Out"),
    details.eventType = c(NA, NA, NA, "stolen_base_2b", NA),
    details.event = c(NA, NA, NA, "Stolen Base 2B", NA),
    details.isInPlay = c(TRUE, FALSE, FALSE, FALSE, TRUE),
    details.isStrike = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    details.isBall = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    details.call.description = c("In play, no out", "Ball", "Called Strike", NA, "In play, out(s)"),
    details.type.code = c("FF", "CH", "SL", NA, "FF"),
    details.type.description = c("Four-Seam Fastball", "Changeup", "Slider", NA, "Four-Seam Fastball"),
    pitchData.plateTime = c(.40, .46, .44, NA, .40),
    pitchData.coordinates.pX = c(0, .2, -.1, NA, .3),
    pitchData.coordinates.pZ = c(2.5, 2.3, 2.6, NA, 2.2),
    pitchData.strikeZoneTop = 3.5,
    pitchData.strikeZoneBottom = 1.5,
    umpire.id = 77L,
    last.pitch.of.ab = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    matchup.postOnFirst.id = c(101L, NA, NA, NA, NA),
    matchup.postOnSecond.id = c(NA, 101L, 101L, 101L, 101L),
    matchup.postOnThird.id = NA_integer_,
    details.homeScore = 0L,
    details.awayScore = 0L,
    result.homeScore = 0L,
    result.awayScore = 0L,
    result.rbi = 0L,
    player.id = c(NA, NA, NA, 101L, NA)
  )
}

test_that("pitch-level run windows attach the attempt to the preceding pitch", {
  starts <- data.frame(
    game_pk = "9001",
    fielding_team = "Home",
    catcher_id = "700",
    catcher_name = "Test Catcher"
  )
  opportunities <- build_run_game_pitch_opportunities(
    run_game_pbp_fixture(),
    starting_catchers = starts
  )
  second <- opportunities[opportunities$target_base == 2L, ]

  expect_equal(nrow(second), 2L)
  expect_identical(second$count_key, c("0-0", "1-0"))
  expect_identical(second$attempted, c(FALSE, TRUE))
  expect_true(second$success[[2L]])
  expect_true(all(second$runner_id == "101"))
  expect_true(all(second$catcher_id == "700"))
  expect_identical(second$pitch_family, c("offspeed", "breaking"))
})

test_that("active catcher changes at the substitution boundary", {
  starts <- data.frame(
    game_pk = "1", fielding_team = "Home",
    catcher_id = "10", catcher_name = "Starter"
  )
  raw <- tibble::tibble(
    game_pk = c(1L, 1L),
    about.atBatIndex = c(5L, 5L),
    fielding_team = "Home",
    batting_team = "Away",
    details.eventType = c("defensive_substitution", NA),
    position.name = c("Catcher", NA),
    player.id = c(20L, NA),
    details.description = c(
      "Defensive Substitution: Backup replaces Starter, batting 9th, playing catcher.",
      NA
    )
  )
  stints <- build_catcher_stints(raw, starting_catchers = starts)
  assigned <- assign_active_catcher(
    data.frame(
      game_pk = "1", fielding_team = "Home",
      at_bat_index = c(4L, 5L)
    ),
    stints
  )

  expect_identical(assigned$catcher_id, c("10", "20"))
  expect_identical(assigned$catcher_name, c("Starter", "Backup"))
})

test_that("adjusted run-game ratings separate pitchers and catchers", {
  set.seed(42)
  n <- 480L
  opportunities <- tibble::tibble(
    runner_id = rep(c("r1", "r2"), each = n / 2),
    runner_name = rep(c("Fast Runner", "Other Runner"), each = n / 2),
    batting_team = "Away",
    pitcher_id = rep(c("p1", "p2"), length.out = n),
    pitcher_name = rep(c("Quick Pitcher", "Slow Pitcher"), length.out = n),
    catcher_id = rep(c("c1", "c2"), each = n / 2),
    catcher_name = rep(c("Strong Catcher", "Weak Catcher"), each = n / 2),
    fielding_team = "Home",
    target_base = 2L,
    count_key = rep(c("0-0", "1-0", "1-1"), length.out = n),
    disengagement_count = rep(0:2, length.out = n),
    pitch_family = rep(c("fastball", "offspeed"), length.out = n)
  )
  attempt_probability <- ifelse(opportunities$runner_id == "r1", .09, .03)
  attempt_probability <- attempt_probability *
    ifelse(opportunities$pitcher_id == "p2", 1.5, .7)
  opportunities$attempted <- stats::runif(n) < attempt_probability
  success_probability <- .72 +
    ifelse(opportunities$pitcher_id == "p2", .12, -.08) +
    ifelse(opportunities$catcher_id == "c2", .08, -.08)
  opportunities$success <- ifelse(
    opportunities$attempted,
    stats::runif(n) < success_probability,
    NA
  )

  ratings <- build_run_game_ratings(opportunities)
  expect_true(all(c("runner", "pitcher", "catcher", "battery", "model_card") %in% names(ratings)))
  expect_equal(nrow(ratings$pitcher), 2L)
  expect_equal(nrow(ratings$catcher), 2L)
  expect_true(all(is.finite(ratings$pitcher$attempt_index)))
  expect_identical(ratings$model_card$model_version, "run_game_shrunk_logit_v1")
})

test_that("framing and ABS products expose stable public contracts", {
  set.seed(7)
  called <- tibble::tibble(
    called_strike = c(stats::runif(300) < .58, stats::runif(300) < .42),
    location_bin = rep(c("10:10", "11:10", "10:11"), length.out = 600),
    catcher_id = rep(c("1", "2"), each = 300),
    catcher_name = rep(c("Receiver A", "Receiver B"), each = 300),
    fielding_team = rep(c("A", "B"), each = 300),
    umpire_id = rep(c("u1", "u2"), length.out = 600)
  )
  framing <- score_catcher_framing(called)
  expect_equal(nrow(framing$catcher), 2L)
  expect_true(all(c("framing_score", "framing_runs_estimate") %in% names(framing$catcher)))

  abs_raw <- data.frame(
    year = 2026,
    id = 1,
    player_name = "Receiver A",
    team_abbr = "A",
    n_total_sample = 100,
    n_challenges = 12,
    n_overturns = 8,
    n_fails = 4,
    exp_chal = 10,
    exp_chal_gained = 6,
    net_net_chal = 2,
    n_chal_runs = 1.2,
    net_net_runs = .4,
    rate_challenges = .12,
    rate_overturns = 8 / 12
  )
  abs <- standardize_abs_challenge_leaderboard(abs_raw, "catcher")
  expect_identical(abs$challenge_type, "catcher")
  expect_equal(abs$overturn_rate, 8 / 12)
  expect_equal(abs$runs_vs_expected, .4)
})
