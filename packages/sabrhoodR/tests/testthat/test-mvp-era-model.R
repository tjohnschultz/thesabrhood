test_that("MVP era profiles use league-season percentiles and normalized modern weights", {
  batting <- tibble::tibble(
    playerID = c("a", "b", "c", "d", "e", "f"),
    yearID = c(2020, 2020, 2020, 2021, 2021, 2021),
    lgID = "AL",
    G = 100,
    AB = c(350, 360, 340, 350, 360, 340),
    R = c(80, 50, 40, 85, 55, 42),
    H = c(120, 100, 90, 125, 105, 92),
    X2B = c(25, 20, 15, 27, 21, 16),
    X3B = c(3, 2, 1, 4, 2, 1),
    HR = c(30, 18, 10, 32, 19, 11),
    RBI = c(90, 60, 45, 95, 62, 46),
    SB = c(12, 8, 5, 13, 9, 5),
    BB = c(55, 40, 30, 58, 42, 31),
    HBP = 0,
    SF = 5
  )
  awards <- tibble::tibble(
    playerID = c("a", "d", "b"),
    awardID = c("Most Valuable Player", "Most Valuable Player", "Other MVP"),
    yearID = c(2020, 2021, 2020),
    lgID = "AL"
  )

  result <- build_mvp_era_profiles(batting, awards, minimum_pa = 100)
  expect_equal(nrow(result$winners), 2L)
  expect_equal(unique(result$profiles$decade), 2020)
  expect_equal(sum(result$weights$model_weight), 1, tolerance = 1e-10)
  expect_equal(result$weights$model_weight[result$weights$metric == "war"], 0.35)
  expect_false(any(result$winners$playerID == "b"))
})
