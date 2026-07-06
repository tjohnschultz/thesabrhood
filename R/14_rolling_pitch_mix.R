make_hitter_pitchtype_rolling <- function(data,
                                          hitter,
                                          pitch_types = NULL,
                                          top_n = 5,
                                          batter_col = "matchup.batter.fullName",
                                          pitch_col = "details.type.description",
                                          date_col = "game_date",
                                          game_col = NULL,
                                          window = 15,
                                          complete_window = FALSE,
                                          rate_name = "rolling_pitch_pct") {
  library(dplyr)
  library(tidyr)
  library(slider)
  
  # 1. Filter to hitter and create clean internal columns
  hitter_pitches <- data %>%
    filter(.data[[batter_col]] == hitter) %>%
    filter(!is.na(.data[[pitch_col]])) %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      .pitch_type = .data[[pitch_col]]
    )
  
  if (nrow(hitter_pitches) == 0) {
    stop("No pitches found for this hitter. Check hitter name or column names.")
  }
  
  # 2. Create game id
  if (!is.null(game_col) && game_col %in% names(hitter_pitches)) {
    hitter_pitches <- hitter_pitches %>%
      mutate(.game_id = paste(.date, .data[[game_col]], sep = "_"))
  } else {
    hitter_pitches <- hitter_pitches %>%
      mutate(.game_id = as.character(.date))
  }
  
  # 3. Pick pitch types if none are supplied
  if (is.null(pitch_types)) {
    pitch_types <- hitter_pitches %>%
      count(.pitch_type, sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.pitch_type)
  }
  
  # 4. Total pitches seen by game
  game_totals <- hitter_pitches %>%
    count(.game_id, .date, name = "game_total_pitches") %>%
    arrange(.date, .game_id) %>%
    mutate(game_number = row_number())
  
  # 5. Pitches seen by type and game
  pitch_counts <- hitter_pitches %>%
    filter(.pitch_type %in% pitch_types) %>%
    count(.game_id, .date, .pitch_type, name = "game_pitch_count")
  
  # 6. Create every game x pitch type combo, filling missing pitch types with 0
  per_game_pitch_mix <- game_totals %>%
    crossing(.pitch_type = pitch_types) %>%
    left_join(
      pitch_counts,
      by = c(".game_id", ".date", ".pitch_type")
    ) %>%
    mutate(
      game_pitch_count = replace_na(game_pitch_count, 0L)
    ) %>%
    arrange(.pitch_type, game_number)
  
  # 7. Rolling pitch-type share over last N games
  rolling_tbl <- per_game_pitch_mix %>%
    group_by(.pitch_type) %>%
    arrange(game_number, .by_group = TRUE) %>%
    mutate(
      rolling_pitch_count = slide_dbl(
        game_pitch_count,
        sum,
        .before = window - 1,
        .complete = complete_window
      ),
      rolling_total_pitches = slide_dbl(
        game_total_pitches,
        sum,
        .before = window - 1,
        .complete = complete_window
      ),
      !!rate_name := if_else(
        rolling_total_pitches > 0,
        rolling_pitch_count / rolling_total_pitches,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    transmute(
      hitter = hitter,
      game_date = .date,
      game_number,
      pitch_type = .pitch_type,
      game_pitch_count,
      game_total_pitches,
      rolling_pitch_count,
      rolling_total_pitches,
      !!rate_name := .data[[rate_name]]
    )
  
  rolling_tbl
}

plot_single_rolling_stat <- function(data,
                                     y_col,
                                     title = NULL,
                                     subtitle = NULL,
                                     y_label = NULL,
                                     y_is_pct = NULL,
                                     caption = NULL) {
  
  if (is.null(y_is_pct)) {
    y_is_pct <- grepl("%|pct|rate", y_col, ignore.case = TRUE)
  }
  
  plot_rolling_trend(
    data = data,
    x_col = "game_date",
    y_col = y_col,
    color_col = NULL,
    title = title,
    subtitle = subtitle,
    y_label = y_label,
    caption = caption,
    y_is_pct = y_is_pct
  )
}

plot_rolling_pitchtype_trend <- function(data,
                                         y_col = "rolling_pitch_pct",
                                         pitch_col = "pitch_type",
                                         title = NULL,
                                         subtitle = NULL,
                                         y_label = "Pitch Type Share",
                                         caption = NULL) {
  
  plot_rolling_trend(
    data = data,
    x_col = "game_date",
    y_col = y_col,
    color_col = pitch_col,
    title = title,
    subtitle = subtitle,
    y_label = y_label,
    color_label = "Pitch Type",
    caption = caption,
    y_is_pct = TRUE
  )
}