test_that("platoon edge boards require both hands and rank the split gap", {
  splits <- tibble::tibble(
    player_id = c(1, 1, 2, 2), player_name = c("Split Star", "Split Star", "Even Bat", "Even Bat"),
    team = c("A", "A", "B", "B"), hand = "R", opponent_hand = c("L", "R", "L", "R"),
    pa = c(80, 140, 90, 120), ops = c(1.050, .650, .800, .790),
    woba_estimate = c(.450, .290, .350, .345), pa_reliability = c(.7, .8, .7, .8)
  )
  boards <- build_platoon_edge_boards(splits, splits)
  expect_identical(boards$hitters$player_name[[1]], "Split Star")
  expect_identical(boards$hitters$stronger_opponent_hand[[1]], "L")
  expect_identical(boards$pitchers$stronger_opponent_hand[[1]], "R")
})

test_that("signature pitch board rewards bat-missing and contact suppression", {
  pitches <- tibble::tibble(
    player_id = 1:3, player_name = c("Whiff Arm", "Contact Arm", "Tiny Sample"), team = "A",
    pitch_type = c("SL", "FF", "CH"), pitch_name = c("Slider", "Four-Seam Fastball", "Changeup"),
    pitches = c(400, 400, 50), swings = c(200, 200, 20), usage_rate = c(.35, .35, .20),
    whiff_rate = c(.48, .15, .60), chase_rate = c(.40, .20, .50), hard_hit_rate = c(.20, .50, .10),
    putaway_rate = c(.30, .10, .40), pitch_reliability = c(.9, .9, .3), average_velocity = c(87, 96, 85),
    average_horizontal_break = c(14, 5, 12), average_induced_vertical_break = c(2, 17, 5)
  )
  board <- build_signature_pitch_board(pitches)
  expect_equal(nrow(board), 2)
  expect_identical(board$player_name[[1]], "Whiff Arm")
  expect_identical(board$pitch_family[[1]], "Breaking")
})

test_that("daily story queue diversifies its shortlist", {
  hitter_form <- tibble::tibble(player_id = 1, player_name = "Hot Bat", team = "A", recent_ops = 1.1, ops_delta = .3, form_score = 90, form_score_confidence = .8)
  pitcher_form <- tibble::tibble(player_id = 2, player_name = "Hot Arm", team = "B", recent_ops = .5, ops_delta = -.2, form_score = 88, form_score_confidence = .8)
  offense <- tibble::tibble(player_id = 3, player_name = "Race Bat", team = "C", ops = 1, race_score = 95, pa_reliability = .9)
  prevention <- tibble::tibble(player_id = 4, player_name = "Race Arm", team = "D", ops = .55, race_score = 94, pa_reliability = .9)
  milestone <- tibble::tibble(player_id = 5, player_name = "Veteran", team = "E", headline = "Veteran reached 2,000 hits", career_to_date_value = 2000, story_score = 92, identity_match_confidence = .75)
  history <- tibble::tibble(note_id = "h1", subject_name = "Legend", headline = "Legend debuted today", career_summary = "A Hall of Famer.", story_score = 90, confidence = 1)
  teams <- tibble::tibble(team = "A", team_story = "A ranks first.", team_index = 85)
  pitch <- tibble::tibble(player_id = 6, pitch_type = "SL", player_name = "Pitch Arm", team = "F", identity_headline = "Pitch Arm's Slider", pitch_quality_score = 91, pitch_reliability = .9)
  queue <- build_daily_story_queue(hitter_form, pitcher_form, offense, prevention, milestone, history, teams, pitch)
  expect_equal(nrow(queue), 8)
  expect_true(all(queue$daily_shortlist))
  expect_identical(queue$story_score_method[[1]], "cross_product_editorial_queue_v1")
})

test_that("team broadcast notes produce identity, form, and a research hook", {
  teams <- tibble::tibble(team = "Test Club", team_story = "Test Club ranks first in offense.", team_index = 82)
  hitter_form <- tibble::tibble(player_name = "Hot Bat", team = "Test Club", form_score = 90, recent_ops = 1.1, baseline_ops = .75)
  pitcher_form <- tibble::tibble(player_name = "Hot Arm", team = "Test Club", form_score = 70, recent_ops = .55, baseline_ops = .70)
  pitches <- tibble::tibble(player_name = "Pitch Arm", team = "Test Club", pitch_quality_score = 88, identity_headline = "Pitch Arm's slider", usage_rate = .4)
  matchups <- tibble::tibble(player_name = "Split Bat", team = "Test Club", matchup_edge_score = 80, headline = "Split Bat crushes lefties", evidence = "A large split.")
  milestones <- tibble::tibble(player_name = "Veteran", team = "Test Club", story_score = 85, headline = "Veteran reached 1,500 hits", career_to_date_value = 1500)
  notes <- build_team_broadcast_notes(teams, hitter_form, pitcher_form, pitches, matchups, matchups[0, ], milestones)
  expect_equal(nrow(notes), 3)
  expect_identical(notes$note_order, 1:3)
  expect_identical(notes$subject[[2]], "Hot Bat")
  expect_true(notes$note_category[[3]] %in% c("signature_pitch", "milestone", "matchup_edge"))
})
