test_that("insane awards create transparent ranked boards", {
  hitters <- data.frame(
    player_id = c("1", "2", "3"), player_name = c("A", "B", "C"), team = c("X", "Y", "Z"),
    pa = c(200, 200, 200), average_exit_velocity = c(95, 90, 85), ground_ball_rate = c(.60, .45, .30)
  )
  pitchers <- hitters
  make_pa <- function(index, batter, pitcher, count_type = "full", event = "strikeout") {
    data.frame(
      game_pk = paste0("g", index), at_bat_index = index, pitch_in_pa = if (count_type == "first") 1L else 6L,
      is_terminal_pitch = TRUE, balls_before = if (count_type == "full") 3L else 0L,
      strikes_before = if (count_type == "full") 2L else 0L, batter_id = batter,
      batter_name = paste0("Batter ", batter), batting_team = paste0("T", batter), pitcher_id = pitcher,
      pitcher_name = paste0("Pitcher ", pitcher), fielding_team = paste0("P", pitcher), event_key = event,
      stringsAsFactors = FALSE
    )
  }
  full_rows <- do.call(rbind, lapply(seq_len(20), function(i) make_pa(i, ifelse(i <= 16, "1", "2"), "9", "full", ifelse(i <= 12, "strikeout", "walk"))))
  first_rows <- do.call(rbind, lapply(21:40, function(i) make_pa(i, "3", "8", "first", ifelse(i %% 3 == 0, "home_run", "single"))))
  awards <- build_insane_baseball_awards(hitters, pitchers, rbind(full_rows, first_rows), minimum_pa = 100L, minimum_split_pa = 3L)
  expect_true(all(c("earthworm_killer", "full_count_executioner", "ambush_artist") %in% awards$award_id))
  expect_equal(awards$player_name[awards$award_id == "earthworm_killer" & awards$rank == 1], "A")
  expect_true(any(awards$featured))
  expect_true(all(awards$showcase_score >= 0 & awards$showcase_score <= 100))
})
