test_that("pitch usage changes compare recent games with a non-overlapping baseline", {
  games <- rep(1:10, each = 20)
  pitch_type <- unlist(lapply(1:10, function(game) {
    if (game <= 5) c(rep("FF", 14), rep("SL", 6)) else c(rep("FF", 8), rep("SL", 12))
  }))
  pitches <- tibble::tibble(
    game_pk = as.character(games), game_date = as.Date("2026-04-01") + games,
    pitcher_id = "1", pitcher_name = "Pitcher", fielding_team = "Club",
    pitch_type = pitch_type, pitch_name = ifelse(pitch_type == "FF", "Four-Seam Fastball", "Slider"),
    start_speed = ifelse(pitch_type == "FF", 95, 86), horizontal_break = ifelse(pitch_type == "FF", 8, -5),
    induced_vertical_break = ifelse(pitch_type == "FF", 16, 2),
    is_whiff = FALSE, is_swing = TRUE, is_chase = FALSE
  )
  board <- build_pitch_usage_change_board(
    pitches, recent_games = 5, minimum_recent_pitches = 20, minimum_baseline_pitches = 20
  )
  slider <- board[board$pitch_type == "SL", ]
  expect_equal(slider$baseline_usage, 0.30)
  expect_equal(slider$recent_usage, 0.60)
  expect_equal(slider$usage_delta_pp, 30)
  expect_identical(slider$direction, "usage up")
})

