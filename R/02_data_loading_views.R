#Function 1.1
#First step in analyzing the raw data. Enhance adds the encoding of essential binary columns to summarize 
#data with.
enhance_pbp <- function(data) {
  library(dplyr)
  library(stringr)
  library(lubridate)
  
  swing_events <- c(
    "Swinging Strike", "Foul", "In play, out(s)", "In play, run(s)",
    "In play, no out", "Foul Tip", "Swinging Strike (Blocked)",
    "Foul Bunt", "Missed Bunt"
  )
  
  whiff_events <- c(
    "Swinging Strike", "Swinging Strike (Blocked)", "Missed Bunt"
  )
  
  non_pa_only <- c(
    "pickoff_1b", "pickoff_2b", "pickoff_3b", "pickoff_error_1b",
    "caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home",
    "stolen_base_2b", "wild_pitch", "pickoff_caught_stealing_2b",
    "pickoff_caught_stealing_3b", "game_advisory"
  )
  
  pa_end_events <- c(
    "single", "double", "triple", "home_run",
    "field_out", "force_out", "field_error",
    "double_play", "grounded_into_double_play",
    "fielders_choice", "fielders_choice_out",
    "strikeout", "strikeout_double_play",
    "walk", "intent_walk", "hit_by_pitch",
    "sac_fly", "sac_bunt", "sac_fly_double_play",
    "sac_bunt_double_play", "catcher_interf", "other_out"
  )
  
  pa_only_events <- c(
    "walk", "intent_walk", "hit_by_pitch",
    "sac_fly", "sac_bunt", "sac_fly_double_play",
    "sac_bunt_double_play", "catcher_interf"
  )
  
  hit_events <- c("single", "double", "triple", "home_run")
  
  data %>%
    mutate(game_date = as.Date(game_date)) %>%
    group_by(game_pk, atBatIndex) %>%
    mutate(
      game_date = as.Date(game_date),
      month_name = lubridate::month(game_date, label = TRUE),
      
      last_pitch_num = ifelse(
        any(isPitch %in% TRUE & !is.na(pitchNumber)),
        max(pitchNumber[isPitch %in% TRUE], na.rm = TRUE),
        NA_real_
      ),
      
      is_last_pitch = as.integer(
        isPitch %in% TRUE &
          !is.na(pitchNumber) &
          pitchNumber == last_pitch_num
      ),
      
      pa_end_row = as.integer(
        row_number() == dplyr::n() &
          result.eventType %in% pa_end_events
      ),
      
      is_swing = as.integer(details.call.description %in% swing_events),
      is_whiff = as.integer(details.call.description %in% whiff_events),
      is_contact = as.integer(is_swing == 1 & is_whiff == 0),
      
      is_pa = pa_end_row,
      
      is_at_bat = as.integer(
        pa_end_row == 1 & !(result.eventType %in% pa_only_events)
      ),
      
      is_bb = as.integer(
        pa_end_row == 1 & result.eventType %in% c("walk", "intent_walk")
      ),
      
      is_k = as.integer(
        pa_end_row == 1 & str_detect(result.eventType, "^strikeout")
      ),
      
      total_bases = case_when(
        pa_end_row != 1 ~ NA_real_,
        result.eventType == "single" ~ 1,
        result.eventType == "double" ~ 2,
        result.eventType == "triple" ~ 3,
        result.eventType == "home_run" ~ 4,
        is_at_bat == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      
      is_hit = as.integer(
        pa_end_row == 1 & result.eventType %in% hit_events
      ),
      
      is_hr = as.integer(
        pa_end_row == 1 & result.eventType == "home_run"
      ),
      
      is_bbe = as.integer(
        is_last_pitch == 1 &
          isPitch %in% TRUE &
          ((details.isInPlay %in% TRUE) | !is.na(hitData.launchSpeed))
      ),
      
      ev = suppressWarnings(as.numeric(hitData.launchSpeed)),
      la = suppressWarnings(as.numeric(hitData.launchAngle)),
      hit_location = suppressWarnings(as.numeric(hitData.location)),
      
      is_hard_hit = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 95),
      
      is_barrel = as.integer(
        is_bbe == 1 & !is.na(ev) & ev >= 98 & la >= 24 & la <= 33
      ),
      
      is_fb = as.integer(
        is_bbe == 1 & !is.na(ev) & la >= 25 & la <= 50
      ),
      
      contact_quality = case_when(
        is_bbe != 1 | is.na(ev) ~ NA_character_,
        ev < 84.4 ~ "Weak",
        ev < 97.2 ~ "Average",
        TRUE ~ "Hard"
      ),
      
      is_gb = as.integer(
        is_bbe == 1 & hitData.trajectory == "ground_ball"
      ),
      
      bbe_type = case_when(
        is_bbe != 1 ~ NA_character_,
        hitData.trajectory %in% c("ground_ball", "bunt_grounder") ~ "Grounder",
        hitData.trajectory %in% c("line_drive", "bunt_line_drive") ~ "LineDrive",
        hitData.trajectory %in% c("fly_ball", "popup", "bunt_popup") ~ "FlyBall",
        TRUE ~ NA_character_
      ),
      
      spray_direction = case_when(
        is_bbe != 1 ~ NA_character_,
        
        matchup.batSide.description == "Right" & hit_location %in% c(5, 6, 7) ~ "Pull",
        matchup.batSide.description == "Right" & hit_location %in% c(8, 78, 89) ~ "Middle",
        matchup.batSide.description == "Right" & hit_location %in% c(9, 4, 3) ~ "Oppo",
        
        matchup.batSide.description == "Left" & hit_location %in% c(9, 4, 3) ~ "Pull",
        matchup.batSide.description == "Left" & hit_location %in% c(8, 78, 89) ~ "Middle",
        matchup.batSide.description == "Left" & hit_location %in% c(5, 6, 7) ~ "Oppo",
        
        TRUE ~ NA_character_
      ),
      
      is_pull = as.integer(spray_direction == "Pull"),
      is_fbhr = as.integer(is_fb == 1 & is_hr == 1),
      is_pull_fb = as.integer(is_fb == 1 & is_pull == 1),
      
      zone = suppressWarnings(as.integer(pitchData.zone)),
      is_zone  = as.integer(isPitch %in% TRUE & !is.na(zone) & zone >= 1 & zone <= 9),
      is_ozone = as.integer(isPitch %in% TRUE & !is.na(zone) & zone > 9),
      is_heart = as.integer(isPitch %in% TRUE & !is.na(zone) & zone == 5),
      
      is_chase = as.integer(is_swing == 1 & is_ozone == 1),
      is_z_swing = as.integer(is_swing == 1 & is_zone == 1),
      is_o_contact = as.integer(is_contact == 1 & is_ozone == 1),
      is_z_contact = as.integer(is_contact == 1 & is_zone == 1),
      
      count = paste(count.balls.start, "-", count.strikes.start),
      
      lineup_spot = suppressWarnings(as.integer(battingOrder)),
      lineup_bucket = case_when(
        lineup_spot %in% 1:3 ~ "Front",
        lineup_spot %in% 4:6 ~ "Middle",
        lineup_spot %in% 7:9 ~ "Back",
        TRUE ~ NA_character_
      ),
      
      is_RISP = as.integer(
        !is.na(matchup.postOnThird.fullName) |
          !is.na(matchup.postOnSecond.fullName)
      ),
      
      bbe_type = factor(bbe_type, levels = c("Grounder", "LineDrive", "FlyBall")),
      spray_direction = factor(spray_direction, levels = c("Pull", "Middle", "Oppo")),
      contact_quality = factor(contact_quality, levels = c("Weak", "Average", "Hard")),
      
      player_name_key = case_when(
        matchup.batter.fullName == "Max Muncy" & batting_team == "Athletics" ~ "Max Muncy_ATH",
        matchup.batter.fullName == "Max Muncy" & batting_team == "Los Angeles Dodgers" ~ "Max Muncy_LAD",
        TRUE ~ matchup.batter.fullName
      )
    ) %>%
    ungroup()
}

#Function 1.2
#Quick function to update pbp data.
update_pbp <- function(og_data,
                       szn_type = "Regular Season",
                       start_date = "2026-02-20",
                       end_date = Sys.Date(),
                       level_ids = 1) {
  library(dplyr)
  library(purrr)
  library(baseballr)
  
  if (missing(og_data) || is.null(og_data)) {
    stop("Provide an existing pbp object as og_data.")
  }
  
  if (!"game_pk" %in% names(og_data)) {
    stop("og_data must include a game_pk column.")
  }
  
  dates <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "day")
  
  scheds <- purrr::map_dfr(
    dates,
    ~ baseballr::get_game_pks_mlb(date = .x, level_ids = level_ids)
  )
  
  if (nrow(scheds) == 0) {
    message("No schedule rows returned. Returning original data.")
    return(og_data)
  }
  
  scheds <- scheds %>%
    dplyr::filter(seriesDescription == szn_type, status.codedGameState == "F")
  
  og_pks <- unique(as.vector(og_data$game_pk))
  new_pks <- unique(as.vector(scheds$game_pk))
  pks_diff <- setdiff(new_pks, og_pks)
  
  if (length(pks_diff) == 0) {
    message("No new completed games found. Returning original data.")
    return(og_data)
  }
  
  message("Pulling ", length(pks_diff), " new completed games...")
  
  new_data <- purrr::map_dfr(
    pks_diff,
    ~ baseballr::get_pbp_mlb(game_pk = .x)
  )
  
  dplyr::bind_rows(og_data, new_data) %>%
    dplyr::distinct(game_pk, atBatIndex, pitchNumber, result.eventType, .keep_all = TRUE)
}

#Function 1.3
#Prepares a list of tables in order to use in summary functions.
prepare_pbp_views <- function(pbp, enhanced = FALSE) {
  if (enhanced == FALSE) {
    enh <- enhance_pbp(pbp)
  } else if (enhanced == TRUE) {
    enh <- pbp
  } else {}
  
  list(
    pbp   = enh,
    pitch = enh %>% filter(isPitch %in% TRUE),
    pa    = enh %>% filter(is_pa == 1),
    bbe   = enh %>% filter(is_bbe == 1)
  )
}