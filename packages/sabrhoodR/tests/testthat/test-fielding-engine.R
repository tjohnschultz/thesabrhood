fielding_fixture <- function() {
  tibble::tibble(
    game_pk = 77L,
    game_date = "2026-07-01",
    isPitch = TRUE,
    pitchNumber = 1L,
    playId = paste0("play-", 0:5),
    about.atBatIndex = 0:5,
    about.inning = 1L,
    about.halfInning = "top",
    about.isTopInning = TRUE,
    batting_team = "Away",
    fielding_team = "Home",
    home_team = "Home",
    away_team = "Away",
    matchup.pitcher.id = 500L,
    matchup.pitcher.fullName = "Pitcher",
    matchup.pitchHand.code = "R",
    matchup.batter.id = 101:106,
    matchup.batter.fullName = paste("Batter", 1:6),
    matchup.batSide.code = "R",
    count.balls.start = 0L,
    count.strikes.start = 0L,
    count.outs.start = c(0L, 0L, 0L, 0L, 1L, 2L),
    count.balls.end = 0L,
    count.strikes.end = 0L,
    count.outs.end = c(0L, 0L, 0L, 1L, 2L, 3L),
    result.eventType = c(
      "single", "single", "single", "field_out", "double", "field_out"
    ),
    result.event = c(
      "Single", "Single", "Single", "Field Out", "Double", "Field Out"
    ),
    result.description = c(
      "Batter 1 singles on a line drive to right fielder Arm One.",
      "Batter 2 singles on a ground ball to center fielder Range Two. Batter 1 to 2nd.",
      "Batter 3 singles on a line drive to center fielder Range Two. Batter 1 scores. Batter 2 to 3rd.",
      "Batter 4 flies out to left fielder Glove Three.",
      "Batter 5 doubles on a line drive to right fielder Arm One. Batter 2 scores.",
      "Batter 6 grounds out, shortstop Hands Four to first baseman First Five."
    ),
    details.description = "In play",
    details.isInPlay = TRUE,
    details.isStrike = TRUE,
    details.isBall = FALSE,
    details.call.description = "In play, out(s)",
    last.pitch.of.ab = TRUE,
    hitData.trajectory = c(
      "line_drive", "ground_ball", "line_drive",
      "fly_ball", "line_drive", "ground_ball"
    ),
    hitData.location = c(9L, 8L, 8L, 7L, 9L, 6L),
    hitData.launchSpeed = c(98, 91, 103, 88, 101, 84),
    hitData.launchAngle = c(14, -3, 18, 31, 21, -8),
    hitData.totalDistance = c(245, 160, 275, 330, 305, 95),
    matchup.postOnFirst.id = c(101L, 102L, 103L, 103L, 105L, NA),
    matchup.postOnSecond.id = c(NA, 101L, NA, NA, NA, NA),
    matchup.postOnThird.id = c(NA, NA, 102L, 102L, NA, NA),
    details.homeScore = 0L,
    details.awayScore = c(0L, 0L, 1L, 1L, 2L, 2L),
    result.homeScore = 0L,
    result.awayScore = c(0L, 0L, 1L, 1L, 2L, 2L),
    result.rbi = c(0L, 0L, 1L, 0L, 1L, 0L)
  )
}

fielding_reference <- function() {
  tibble::tibble(
    player_id = as.character(901:905),
    player_name = c(
      "Arm One", "Range Two", "Glove Three", "Hands Four", "First Five"
    ),
    team = "Home",
    position = c("RF", "CF", "LF", "SS", "1B")
  )
}

test_that("fielding credits resolve the first primary fielder", {
  credit <- extract_fielding_credit(c(
    "A flies out to center fielder Range Two.",
    "B grounds out, shortstop Hands Four to first baseman First Five."
  ))
  expect_identical(credit$fielder_name, c("Range Two", "Hands Four"))
  expect_identical(credit$position, c("CF", "SS"))
})

test_that("fielding opportunities and ratings retain player identity", {
  opportunities <- build_fielding_opportunity_view(
    fielding_fixture(),
    fielding_reference()
  )
  ratings <- build_fielding_ratings(opportunities, prior_opportunities = 5)

  expect_equal(nrow(opportunities), 6L)
  expect_true(all(!is.na(opportunities$fielder_id)))
  expect_true(all(opportunities$expected_out_probability > 0))
  expect_true(all(opportunities$expected_out_probability < 1))
  expect_true(nrow(ratings$player) >= 4L)
  expect_true(all(is.finite(ratings$player$fielding_score)))
})

test_that("runner advancement defense is separated from runner quality", {
  fielding <- build_fielding_opportunity_view(
    fielding_fixture(),
    fielding_reference()
  )
  advancement <- build_runner_advancement_fielding(
    fielding_fixture(),
    fielding,
    fielding_reference()
  )

  expect_true(nrow(advancement$opportunities) >= 3L)
  expect_true(all(c(
    "expected_advancement_rate", "advancement_credit",
    "advancement_runs_saved"
  ) %in% names(advancement$opportunities)))
  expect_true(nrow(advancement$player) >= 1L)
  expect_true(all(is.finite(
    advancement$player$advancement_prevention_score
  )))
})

test_that("official fielding data feed the Gold Glove Watch", {
  official_raw <- data.frame(
    id = c(1, 2, 3, 4),
    name = c("One, Arm", "Two, Range", "Three, Glove", "Four, Hands"),
    team_id = c(135, 135, 2, 2),
    team_name = c("Padres", "Padres", "Away", "Away"),
    total_runs = c(5, 3, 4, -1),
    inf_of_runs = c(5, 3, 4, -1),
    range_runs = c(3, 3, 4, -1),
    arm_runs = c(2, 0, 0, 0),
    dp_runs = c(NA, NA, 0, 0),
    catching_runs = NA,
    framing_runs = NA,
    throwing_runs = NA,
    blocking_runs = NA,
    outs_total = c(900, 850, 800, 700),
    tot_pa = c(300, 290, 280, 260),
    outs_2 = 0,
    outs_3 = 0,
    outs_4 = 0,
    outs_5 = 0,
    outs_6 = c(0, 0, 800, 700),
    outs_7 = 0,
    outs_8 = c(900, 850, 0, 0),
    outs_9 = 0
  )
  official <- standardize_fielding_run_value(official_raw, 2026)
  official$league <- c("NL", "AL", "NL", "AL")
  watch <- build_gold_glove_watch(official, minimum_innings = 100)

  expect_identical(official$player_name[[1L]], "Arm One")
  expect_identical(official$team[[1L]], "San Diego Padres")
  expect_setequal(official$primary_position, c("CF", "SS"))
  expect_equal(sum(watch$position_rank == 1L), 4L)
  expect_true(all(c("AL", "NL") %in% watch$league))
  expect_true(all(is.finite(watch$gold_glove_score)))
})

test_that("Play of the Day selects one development play per date", {
  fielding <- build_fielding_opportunity_view(
    fielding_fixture(),
    fielding_reference()
  )
  advancement <- build_runner_advancement_fielding(
    fielding_fixture(),
    fielding,
    fielding_reference()
  )
  plays <- build_fielding_play_of_day(
    fielding,
    advancement$opportunities
  )
  expect_equal(nrow(plays), 1L)
  expect_match(plays$publication_status, "not official")
  expect_true(is.finite(plays$play_runs_saved))
})
