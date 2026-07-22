example_lineup <- function() {
  tibble::tibble(
    player = paste("Hitter", seq_len(9)),
    team = "BOS",
    PA = rep(300, 9),
    H = c(80, 75, 70, 68, 65, 63, 61, 58, 55),
    HR = c(20, 18, 16, 14, 12, 10, 8, 6, 4),
    TB = c(150, 140, 130, 125, 120, 115, 110, 100, 90),
    AVG = c(.285, .275, .270, .265, .260, .255, .250, .245, .240),
    SLG = c(.500, .480, .455, .440, .425, .410, .395, .375, .355),
    BB_pct = rep(.09, 9),
    K_pct = rep(.22, 9),
    game_pk = 123456,
    batting_side = "away",
    opposing_pitcher = "Example Pitcher",
    opposing_pitcher_hand = "R"
  )
}
