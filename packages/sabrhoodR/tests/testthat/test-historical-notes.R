test_that("historical anniversary notes are date-matched and scored", {
  people <- tibble::tibble(
    playerID = c("alpha01", "beta01", "gamma01"),
    nameFirst = c("Alex", "Bo", "Casey"),
    nameLast = c("Alpha", "Beta", "Gamma"),
    debut = as.Date(c("2001-07-20", "2010-07-19", "2020-07-20")),
    finalGame = as.Date(c("2012-07-20", "2018-07-20", NA))
  )

  notes <- build_historical_anniversary_notes(people, "2026-07-20")

  expect_equal(nrow(notes), 4)
  expect_true(all(notes$story_score > 0 & notes$story_score <= 100))
  expect_true(all(notes$report_date == as.Date("2026-07-20")))
  expect_match(notes$body[[1]], "years ago today")
})

test_that("story scores reject unsupported component ranges", {
  candidate <- tibble::tibble(
    rarity = 1.2, magnitude = .5, timeliness = 1,
    relevance = .5, confidence = 1, novelty = .5
  )
  expect_error(score_story_candidates(candidate), "between zero and one")
})

test_that("career profiles distinguish icons from short-career players", {
  people <- tibble::tibble(
    playerID = c("icon01", "brief01"),
    nameFirst = c("Historic", "Brief"),
    nameLast = c("Star", "Player"),
    debut = as.Date(c("1980-07-20", "2020-07-20")),
    finalGame = as.Date(c("2000-07-20", "2020-07-20")),
    bats = c("R", "R"), throws = c("R", "R")
  )
  batting <- tibble::tibble(
    playerID = c("icon01", "brief01"), yearID = c(1980, 2020),
    G = c(2800, 3), AB = c(10500, 5), R = c(1800, 0), H = c(3200, 1),
    X2B = c(600, 0), X3B = c(50, 0), HR = c(520, 0), RBI = c(1700, 0),
    SB = c(250, 0), BB = c(1300, 0), SO = c(1400, 2)
  )
  pitching <- tibble::tibble(playerID = character(), G = numeric())
  awards <- tibble::tibble(
    playerID = c("icon01", "icon01"), awardID = c("Most Valuable Player", "Gold Glove"), yearID = c(1988, 1989)
  )
  allstar <- tibble::tibble(playerID = rep("icon01", 8), yearID = 1985:1992)
  hall <- tibble::tibble(playerID = "icon01", inducted = "Y")

  profiles <- build_player_career_profiles(people, batting, pitching, awards, allstar, hall)

  expect_gt(profiles$career_significance_score[profiles$playerID == "icon01"], 80)
  expect_lt(profiles$career_significance_score[profiles$playerID == "brief01"], 20)
  expect_identical(profiles$recognition_tier[profiles$playerID == "icon01"], "icon")
  expect_match(profiles$career_summary[profiles$playerID == "icon01"], "Hall of Famer")
})

test_that("recognition changes historical anniversary ordering", {
  people <- tibble::tibble(
    playerID = c("brief01", "icon01"),
    nameFirst = c("Brief", "Historic"), nameLast = c("Player", "Star"),
    debut = as.Date(c("1990-07-20", "1990-07-20")),
    finalGame = as.Date(c(NA, NA))
  )
  profiles <- tibble::tibble(
    playerID = c("brief01", "icon01"),
    player_name = c("Brief Player", "Historic Star"),
    career_significance_score = c(5, 95),
    recognition_tier = c("limited", "icon"),
    career_summary = c("Brief Player appeared in MLB.", "Historic Star was a Hall of Famer.")
  )

  notes <- build_debut_anniversary_notes(people, "2026-07-20", career_profiles = profiles)

  expect_identical(notes$subject_id[[1]], "icon01")
  expect_gt(notes$story_score[[1]], notes$story_score[[2]])
  expect_identical(unique(notes$score_method), "sabrhood_historical_story_score_v2")
})

test_that("career milestone notes keep the highest achieved threshold", {
  profiles <- tibble::tibble(
    playerID = "icon01", player_name = "Historic Star",
    career_significance_score = 90, recognition_tier = "icon",
    career_H = 3200, career_HR = 520, career_RBI = 1700, career_SB = 250,
    career_W = 0, career_SO_pitch = 0, career_SV = 0,
    rank_career_H = 8, rank_career_HR = 20, rank_career_W = 20000,
    rank_career_SO_pitch = 20000, rank_career_SV = 20000,
    career_summary = "Historic Star finished with 3,200 hits and 520 home runs."
  )

  notes <- build_career_milestone_notes(profiles)

  expect_true(any(notes$milestone_stat == "hits" & notes$milestone_value == 3000))
  expect_true(any(notes$milestone_stat == "home runs" & notes$milestone_value == 500))
  expect_true(any(notes$record_flag))
  expect_identical(unique(notes$score_method), "sabrhood_historical_story_score_v2")
})
