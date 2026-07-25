baserunning_fixture <- function() {
  tibble::tibble(
    game_pk = 99L,
    game_date = "2026-07-01",
    isPitch = TRUE,
    pitchNumber = 1L,
    about.atBatIndex = 0:4,
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
    matchup.batter.id = 101:105,
    matchup.batter.fullName = paste("Batter", 1:5),
    matchup.batSide.code = "R",
    count.balls.start = 0L,
    count.strikes.start = 0L,
    count.outs.start = c(0L, 0L, 0L, 0L, 1L),
    count.balls.end = 0L,
    count.strikes.end = 0L,
    count.outs.end = c(0L, 0L, 0L, 1L, 1L),
    result.eventType = c("single", "single", "single", "field_out", "double"),
    result.event = c("Single", "Single", "Single", "Groundout", "Double"),
    details.isInPlay = TRUE,
    details.isStrike = TRUE,
    details.isBall = FALSE,
    details.call.description = "In play, no out",
    last.pitch.of.ab = TRUE,
    hitData.trajectory = c("line_drive", "line_drive", "line_drive", "ground_ball", "line_drive"),
    matchup.postOnFirst.id = c(101L, 102L, 103L, 103L, NA),
    matchup.postOnSecond.id = c(NA, 101L, NA, NA, 105L),
    matchup.postOnThird.id = c(NA, NA, 102L, 102L, NA),
    details.homeScore = 0L,
    details.awayScore = c(0L, 0L, 1L, 1L, 3L),
    result.homeScore = 0L,
    result.awayScore = c(0L, 0L, 1L, 1L, 3L),
    result.rbi = c(0L, 0L, 1L, 0L, 2L)
  )
}

test_that("baserunning opportunities preserve runner identity", {
  opportunities <- build_baserunning_opportunity_view(
    baserunning_fixture(),
    data.frame(game_pk = "99", venue_id = "1", venue_name = "Test Park")
  )

  expect_true(all(c(
    "single_first_to_third", "single_second_scores",
    "double_first_scores", "ground_ball_double_play",
    "steal_second", "steal_third"
  ) %in% opportunities$opportunity_type))
  expect_true(any(
    opportunities$opportunity_type == "single_first_to_third" &
      opportunities$runner_id == "102" &
      opportunities$success
  ))
  expect_true(any(
    opportunities$opportunity_type == "single_second_scores" &
      opportunities$runner_id == "101" &
      opportunities$success
  ))
  expect_true(all(opportunities$venue_name == "Test Park"))
})
