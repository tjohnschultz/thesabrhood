plot_pitcher_heatmaps <- function(data, pitcher_name) {
  sz_box <- data.frame(
    x = c(-0.85, 0.85, 0.85, -0.85, -0.85),
    y = c(1.6, 1.6, 3.5, 3.5, 1.6)
  )
  
  data %>%
    filter(isPitch == TRUE) %>%
    ggplot(aes(x = pitchData.coordinates.pX, y = pitchData.coordinates.pZ)) +
    geom_density_2d_filled(
      aes(fill = after_stat(level)), 
      contour_var = "ndensity",
      bins = 8, 
      alpha = 0.6
    ) +
    geom_density_2d(color = "white", alpha = 0.3, size = 0.2) +
    geom_path(data = sz_box, aes(x, y), color = "black", size = 1.2) +
    scale_fill_viridis_d(option = "rocket", direction = -1) +
    facet_wrap(~details.type.description) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none"
    ) +
    labs(
      title = paste("Arsenal Heatmap:", pitcher_name),
      subtitle = "Densities normalized per pitch type | Catcher's Perspective",
      x = "Horizontal (ft)", y = "Height (ft)"
    )
}

library(dplyr); library(slider)

#Enhcancement of PBP used for both methods
enhance_pbp <- function(data) {
  library(dplyr)
  library(stringr)
  
  baserunning_events <- c(
    "pickoff_1b", "pickoff_2b", "pickoff_3b", "pickoff_error_1b",
    "caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home",
    "stolen_base_2b", "wild_pitch", "pickoff_caught_stealing_2b",
    "pickoff_caught_stealing_3b", "game_advisory"
  )
  
  swing_events <- c(
    "Swinging Strike", "Foul", "In play, out(s)", "In play, run(s)",
    "In play, no out", "Foul Tip", "Swinging Strike (Blocked)",
    "Foul Bunt", "Missed Bunt"
  )
  
  whiff_events <- c(
    "Swinging Strike", "Swinging Strike (Blocked)", "Missed Bunt"
  )
  
  non_ab_events <- c(
    "walk", "intent_walk", "hit_by_pitch",
    "sac_fly", "sac_bunt", "sac_fly_double_play", "sac_bunt_double_play",
    "catcher_interf"
  )
  
  non_pa_events <- c(
    "pickoff_1b", "pickoff_2b", "pickoff_3b", "pickoff_error_1b",
    "caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home",
    "stolen_base_2b", "wild_pitch", "pickoff_caught_stealing_2b", 
    "pickoff_caught_stealing_3b", "game_advisory"
  )
  
  pa_only_events <- c(
    "walk", "intent_walk", "hit_by_pitch", "sac_fly", 
    "sac_bunt", "sac_fly_double_play", "catcher_interf"
  )
  
  data %>%
    mutate(game_date = as.Date(game_date)) %>%
    group_by(game_pk, atBatIndex) %>%
    mutate(
      game_date = as.Date(game_date), 
      month_name = month(as.Date(game_date), label = TRUE), 
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
      is_swing = as.integer(details.call.description %in% swing_events),
      is_whiff = as.integer(details.call.description %in% whiff_events),
      is_contact = as.integer(is_swing == 1 & is_whiff == 0),
      is_pa = as.integer(is_last_pitch == 1 & !(result.eventType %in% non_pa_events)),
      is_at_bat = as.integer(is_pa == 1 & !(result.eventType %in% pa_only_events)),
      is_bb = as.integer(is_pa == 1 & (result.eventType %in% c("walk", "intent_walk"))),
      is_k  = as.integer(is_pa == 1 & str_detect(result.eventType, "^strikeout")),
      total_bases = case_when(
        is_last_pitch != 1 ~ NA_real_,
        result.eventType == "single" ~ 1,
        result.eventType == "double" ~ 2,
        result.eventType == "triple" ~ 3,
        result.eventType == "home_run" ~ 4,
        is_at_bat == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      is_hit = as.integer(
        is_last_pitch == 1 &
          result.eventType %in% c("single", "double", "triple", "home_run")), 
      is_bbe = as.integer(
        is_last_pitch == 1 &
          isPitch %in% TRUE &
          ((details.isInPlay %in% TRUE) | !is.na(hitData.launchSpeed))
      ),
      ev = suppressWarnings(as.numeric(hitData.launchSpeed)),
      la = suppressWarnings(as.numeric(hitData.launchAngle)),
      hit_location = suppressWarnings(as.numeric(hitData.location)),
      is_hard_hit = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 95),
      is_barrel = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 98 & la >= 24 & la <= 33), 
      is_fb = as.integer(is_bbe == 1 & !is.na(ev) & la >= 25 & la <= 50), 
      contact_quality = case_when(
        is_bbe != 1 | is.na(ev) ~ NA_character_,
        ev < 84.4 ~ "Weak",
        ev < 97.2 ~ "Average",
        TRUE ~ "Hard"
      ),
      is_gb = as.integer(is_bbe == 1 & hitData.trajectory == "ground_ball"), 
      is_hr = as.integer(is_last_pitch == 1 & result.eventType == "home_run"), 
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
      is_chase   = as.integer(is_swing == 1 & is_ozone == 1),
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
      is_RISP = as.integer(!is.na(matchup.postOnThird.fullName) | !is.na(matchup.postOnSecond.fullName)), 
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
#Prepare with inside enhancement
prepare_pbp_views <- function(pbp) {
  enh <- enhance_pbp(pbp)
  
  list(
    pbp   = enh,
    pitch = enh %>% filter(isPitch %in% TRUE),
    pa    = enh %>% filter(is_pa == 1),
    bbe   = enh %>% filter(is_bbe == 1)
  )
}

rate <- function(num, den) ifelse(den > 0, num / den, NA_real_)

apply_split_filters <- function(tbl,
                                group = "none",
                                bat_hand = "none",
                                pit_hand = "none", 
                                start_date = NULL,
                                risp = 'all',
                                end_date = NULL) {
  library(dplyr)
  
  fastballs <- c("Four-Seam Fastball", "Cutter", "Sinker", "Fastball")
  breaking  <- c("Knuckle Curve", "Slider", "Sweeper", "Curveball", "Slurve")
  offspeed  <- c("Changeup", "Splitter", "Forkball")
  
  if (group == "fastballs") {
    tbl <- tbl %>% filter(details.type.description %in% fastballs)
  } else if (group == "breaking") {
    tbl <- tbl %>% filter(details.type.description %in% breaking)
  } else if (group == "offspeed") {
    tbl <- tbl %>% filter(details.type.description %in% offspeed)
  }
  
  if (bat_hand %in% c("LHH", "LHB")) {
    tbl <- tbl %>% filter(matchup.batSide.description == "Left")
  } else if (bat_hand %in% c("RHH", "RHB")) {
    tbl <- tbl %>% filter(matchup.batSide.description == "Right")
  }
  
  if (pit_hand == "LHP") {
    tbl <- tbl %>% filter(matchup.pitchHand.description == "Left")
  } else if (pit_hand == "RHP") {
    tbl <- tbl %>% filter(matchup.pitchHand.description == "Right")
  }
  
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    tbl <- tbl %>% filter(game_date >= start_date)
  }
  
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    tbl <- tbl %>% filter(game_date <= end_date)
  }
  
  if (risp == "yes") {
    pitch_tbl <- pitch_tbl %>% filter(is_RISP == TRUE)
    bbe_tbl   <- bbe_tbl %>% filter(is_RISP == TRUE)
    pa_tbl    <- pa_tbl %>% filter(is_RISP == TRUE)
  } else if (risp == "no") {
    pitch_tbl <- pitch_tbl %>% filter(is_RISP == FALSE)
    bbe_tbl   <- bbe_tbl %>% filter(is_RISP == FALSE)
    pa_tbl    <- pa_tbl %>% filter(is_RISP == FALSE)
  }
  
  tbl
}

mask_below_threshold <- function(df, count_col, pct_threshold = NULL, keep_cols = "name") {
  library(dplyr)
  
  if (is.null(pct_threshold) || !(count_col %in% names(df)) || nrow(df) == 0) {
    return(df)
  }
  
  max_count <- suppressWarnings(max(df[[count_col]], na.rm = TRUE))
  
  if (!is.finite(max_count)) {
    return(df)
  }
  
  cutoff <- max_count * pct_threshold
  metric_cols <- setdiff(names(df), c(keep_cols, count_col))
  
  df %>%
    mutate(qualifies_threshold = .data[[count_col]] >= cutoff) %>%
    mutate(
      across(
        all_of(metric_cols),
        ~ ifelse(qualifies_threshold, ., NA)
      )
    ) %>%
    select(-qualifies_threshold)
}

## Summarize pichers functions
summarize_pa_pitcher <- function(pa_tbl, level = "player", group.by = NULL, team_col = "fielding_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  if (level != "team") {
    pa_tbl <- pa_tbl %>% filter(!is.na(matchup.pitcher.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  pa_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      AVG = round(rate(H, AB), 3),
      K_pct = round(rate(SO, PA), 2),
      BB_pct = round(rate(BB, PA), 2),
      SLG = round(rate(TB, AB), 3),
      OBP = round(rate(H + BB, PA), 3),
      OPS = round(SLG + OBP, 3),
      .groups = "drop"
    )
}

summarize_disc_pitcher <- function(pitch_tbl, level = "player", group.by = NULL, team_col = "fielding_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  if (level != "team") {
    pitch_tbl <- pitch_tbl %>% filter(!is.na(matchup.pitcher.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  pitch_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      Pitches = n(),
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact = sum(is_o_contact, na.rm = TRUE),
      ZContact = sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE),
      Swing_pct = round(rate(Swings, Pitches), 2),
      Whiff_pct = round(rate(Whiffs, Swings), 2),
      Contact_pct = round(rate(Contact, Swings), 2),
      Zone_pct = round(rate(ZoneP, Pitches), 2),
      Chase_pct = round(rate(Chase, OZoneP), 2),
      ZSwing_pct = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct = round(rate(Heart, Pitches), 2),
      Shadow_pct = round(Zone_pct - Heart_pct, 2),
      .groups = "drop"
    )
}

summarize_bbe_pitcher <- function(bbe_tbl, level = "player", group.by = NULL, team_col = "fielding_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  if (level != "team") {
    bbe_tbl <- bbe_tbl %>% filter(!is.na(matchup.pitcher.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  bbe_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      pull_fb = sum(is_pull_fb, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2),
      barrel_pct = round(rate(barrels, BBE), 2),
      pull_fbpct = round(rate(pull_fb, BBE), 2),
      GB_pct = round(rate(GB, BBE), 2),
      HRFB = round(rate(FBHR, FB), 2),
      EV = round(mean(ev, na.rm = TRUE), 0),
      LA = round(mean(la, na.rm = TRUE), 0),
      .groups = "drop"
    )
}

summarize_overall_pitcher <- function(views,
                                      group = "none",
                                      bat_hand = "none",
                                      pit_hand = "none",
                                      level = "player",
                                      start_date = NULL,
                                      end_date = NULL,
                                      group.by = NULL,
                                      team_col = "fielding_team",
                                      min_pitch_pct = NULL,
                                      min_bbe_pct = NULL,
                                      min_pa_pct = NULL,
                                      risp = 'all',
                                      group_on_pa = TRUE) {
  library(dplyr)
  
  pitch_tbl <- apply_split_filters(
    views$pitch,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp
  )
  
  bbe_tbl <- apply_split_filters(
    views$bbe,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp
  )
  
  pa_tbl <- apply_split_filters(
    views$pa,
    group = if (group_on_pa) group else "none",
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp
  )
  
  pa_sum <- summarize_pa_pitcher(
    pa_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  disc_sum <- summarize_disc_pitcher(
    pitch_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  bbe_sum <- summarize_bbe_pitcher(
    bbe_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  id_keep <- if (level == "team") team_col else "matchup.pitcher.id"
  keep_base <- c(id_keep, "name", group.by)
  
  pa_sum <- mask_below_threshold(
    pa_sum,
    "PA",
    min_pa_pct,
    keep_cols = keep_base
  )
  
  disc_sum <- mask_below_threshold(
    disc_sum,
    "Pitches",
    min_pitch_pct,
    keep_cols = keep_base
  )
  
  bbe_sum <- mask_below_threshold(
    bbe_sum,
    "BBE",
    min_bbe_pct,
    keep_cols = keep_base
  )
  
  pa_sum <- add_metric_ranks(
    pa_sum,
    high_good = c("PA", "AB", "SO", "K_pct", "GB", "GB_pct"),
    low_good  = c("H", "BB", "HR", "TB", "AVG", "BB_pct", "SLG", "OBP", "OPS"),
    by_group  = group.by
  )
  
  disc_sum <- add_metric_ranks(
    disc_sum,
    high_good = c("Pitches", "Whiffs", "Whiff_pct", "Chase", "Chase_pct"),
    low_good  = c("Contact", "Contact_pct", "OContact", "OContact_pct", "ZContact", "ZContact_pct"),
    by_group  = group.by
  )
  
  bbe_sum <- add_metric_ranks(
    bbe_sum,
    high_good = c("BBE", "GB", "GB_pct"),
    low_good  = c("HardHit", "HardHit_pct", "barrels", "barrel_pct",
                  "pull_fb", "pull_fbpct", "FBHR", "HRFB", "EV", "LA"),
    by_group  = group.by
  )
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  join_cols <- c(id_col, group.by)
  
  pa_sum %>%
    full_join(disc_sum, by = join_cols) %>%
    full_join(bbe_sum, by = join_cols) %>%
    arrange(desc(PA), desc(Pitches), desc(BBE))
}

##Pitcher by pitch type and shape

summarize_pitch_shape_pitcher <- function(pitch_tbl,
                                          level = "player",
                                          group.by = NULL,
                                          team_col = "fielding_team") {
  library(dplyr)
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  name_source <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  pitch_tbl2 <- pitch_tbl %>%
    mutate(
      name = .data[[name_source]],
      pitch_type = details.type.description,
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate,
      hz_movement = pitchData.breaks.breakHorizontal,
      ivb_movement = pitchData.breaks.breakVerticalInduced,
      vy0 = pitchData.coordinates.vY0,
      vx0 = pitchData.coordinates.vX0,
      vz0 = pitchData.coordinates.vZ0,
      ay = pitchData.coordinates.aY,
      az = pitchData.coordinates.aZ,
      ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t),
      vz_f = vz0 + (az * t),
      vx_f = vx0 + (ax * t),
      VAA = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      HAA = atan(vx_f / vy_f) * (180 / pi)
    )
  
  group_vars <- c(id_col, "name", group.by, "pitch_type")
  
  sum_base <- pitch_tbl2 %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      Pitches = n(),
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0),
      VAA = round(mean(VAA, na.rm = TRUE), 2),
      HAA = round(mean(HAA, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  parent_group <- c(id_col, "name", group.by)
  
  final_sum <- sum_base %>%
    group_by(across(all_of(parent_group))) %>%
    mutate(
      usage = round(Pitches / sum(Pitches, na.rm = TRUE), 3)
    ) %>%
    ungroup()
  
  final_sum <- final_sum %>%
    group_by(across(all_of(parent_group))) %>%
    group_modify(~{
      df <- .x
      df %>%
        rowwise() %>%
        mutate(
          move_diversity = sum(
            sqrt((VAA - df$VAA)^2 + (HAA - df$HAA)^2) * df$usage,
            na.rm = TRUE
          )
        ) %>%
        ungroup()
    }) %>%
    ungroup() %>%
    mutate(
      move_diversity = round(move_diversity, 2)
    ) %>%
    arrange(desc(Pitches))
  
  return(final_sum)
}

summarize_disc_by_pitchtype_pitcher <- function(pitch_tbl,
                                                level = "player",
                                                group.by = NULL,
                                                team_col = "fielding_team") {
  library(dplyr)
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  pitch_tbl %>%
    mutate(
      pitch_type = details.type.description
    ) %>%
    group_by(
      across(all_of(c(id_col, name_col, group.by, "pitch_type")))
    ) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      Pitches = n(),
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact = sum(is_o_contact, na.rm = TRUE),
      ZContact = sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE),
      Swing_pct = round(rate(Swings, Pitches), 2),
      Whiff_pct = round(rate(Whiffs, Swings), 2),
      Contact_pct = round(rate(Contact, Swings), 2),
      Zone_pct = round(rate(ZoneP, Pitches), 2),
      Chase_pct = round(rate(Chase, OZoneP), 2),
      ZSwing_pct = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct = round(rate(Heart, Pitches), 2),
      Shadow_pct = round(Zone_pct - Heart_pct, 2),
      .groups = "drop"
    )
}

summarize_bbe_by_pitchtype_pitcher <- function(bbe_tbl,
                                               level = "player",
                                               group.by = NULL,
                                               team_col = "fielding_team") {
  library(dplyr)
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  bbe_tbl %>%
    mutate(
      pitch_type = details.type.description
    ) %>%
    group_by(
      across(all_of(c(id_col, name_col, group.by, "pitch_type")))
    ) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      pull_fb = sum(is_pull_fb, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2),
      barrel_pct = round(rate(barrels, BBE), 2),
      pull_fbpct = round(rate(pull_fb, BBE), 2),
      GB_pct = round(rate(GB, BBE), 2),
      HRFB = round(rate(FBHR, FB), 2),
      EV = round(mean(ev, na.rm = TRUE), 0),
      LA = round(mean(la, na.rm = TRUE), 0),
      .groups = "drop"
    )
}

summarize_overall_pitcher_by_pitchtype <- function(views,
                                                   group = "none",
                                                   bat_hand = "none",
                                                   pit_hand = "none",
                                                   level = "player",
                                                   start_date = NULL,
                                                   end_date = NULL,
                                                   risp = 'all',
                                                   group.by = NULL,
                                                   team_col = "fielding_team",
                                                   min_pitch_pct = NULL,
                                                   min_bbe_pct = NULL) {
  library(dplyr)
  
  pitch_tbl <- apply_split_filters(
    views$pitch,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp
  )
  
  bbe_tbl <- apply_split_filters(
    views$bbe,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp
  )
  
  disc_sum <- summarize_disc_by_pitchtype_pitcher(
    pitch_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  shape_sum <- summarize_pitch_shape_pitcher(
    pitch_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  bbe_sum <- summarize_bbe_by_pitchtype_pitcher(
    bbe_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  id_keep <- if (level == "team") team_col else "matchup.pitcher.id"
  keep_base <- c(id_keep, "name", group.by, "pitch_type")
  
  disc_sum <- mask_below_threshold(
    disc_sum,
    "Pitches",
    min_pitch_pct,
    keep_cols = keep_base
  )
  
  bbe_sum <- mask_below_threshold(
    bbe_sum,
    "BBE",
    min_bbe_pct,
    keep_cols = keep_base
  )
  
  disc_sum <- add_metric_ranks(
    disc_sum,
    high_good = c("Pitches", "Swing", "Whiffs", "Whiff_pct", "Chase", "Chase_pct"),
    low_good  = c("Contact", "Contact_pct", "OContact", "OContact_pct", "ZContact", "ZContact_pct"),
    by_group  = group.by
  )
  
  bbe_sum <- add_metric_ranks(
    bbe_sum,
    high_good = c("BBE", "GB", "GB_pct"),
    low_good  = c("HardHit", "HardHit_pct", "barrels", "barrel_pct",
                  "pull_fb", "pull_fbpct", "FBHR", "HRFB", "EV", "LA"),
    by_group  = group.by
  )
  
  join_cols <- c(id_keep, group.by, "pitch_type")
  
  disc_sum %>%
    full_join(shape_sum, by = join_cols) %>%
    full_join(bbe_sum, by = join_cols) %>% 
    mutate(pitch = pitch_type)
}

####
##SUmmarize batters
summarize_pa_batter <- function(pa_tbl, level = "player", group.by = NULL, team_col = "batting_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"
  
  if (level != "team") {
    pa_tbl <- pa_tbl %>% filter(!is.na(matchup.batter.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  pa_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      AVG = round(rate(H, AB), 3),
      K_pct = round(rate(SO, PA), 2),
      BB_pct = round(rate(BB, PA), 2),
      SLG = round(rate(TB, AB), 3),
      OBP = round(rate(H + BB, PA), 3),
      OPS = round(SLG + OBP, 3),
      .groups = "drop"
    )
}

summarize_disc_batter <- function(pitch_tbl, level = "player", group.by = NULL, team_col = "batting_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"
  
  if (level != "team") {
    pitch_tbl <- pitch_tbl %>% filter(!is.na(matchup.batter.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  pitch_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      Pitches = n(),
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact = sum(is_o_contact, na.rm = TRUE),
      ZContact = sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE),
      Swing_pct = round(rate(Swings, Pitches), 2),
      Whiff_pct = round(rate(Whiffs, Swings), 2),
      Contact_pct = round(rate(Contact, Swings), 2),
      Zone_pct = round(rate(ZoneP, Pitches), 2),
      Chase_pct = round(rate(Chase, OZoneP), 2),
      ZSwing_pct = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct = round(rate(Heart, Pitches), 2),
      Shadow_pct = round(Zone_pct - Heart_pct, 2),
      .groups = "drop"
    )
}

summarize_bbe_batter <- function(bbe_tbl, level = "player", group.by = NULL, team_col = "batting_team") {
  library(dplyr)
  
  id_col   <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"
  
  if (level != "team") {
    bbe_tbl <- bbe_tbl %>% filter(!is.na(matchup.batter.id))
  }
  
  grouping_cols <- c(id_col, group.by)
  
  bbe_tbl %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      pull_fb = sum(is_pull_fb, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2),
      barrel_pct = round(rate(barrels, BBE), 2),
      pull_fbpct = round(rate(pull_fb, BBE), 2),
      GB_pct = round(rate(GB, BBE), 2),
      HRFB = round(rate(FBHR, FB), 2),
      EV = round(mean(ev, na.rm = TRUE), 0),
      LA = round(mean(la, na.rm = TRUE), 0),
      .groups = "drop"
    )
}

summarize_overall_batter <- function(views,
                                     group = "none",
                                     bat_hand = "none",
                                     pit_hand = "none",
                                     level = "player", 
                                     start_date = NULL,
                                     end_date = NULL,
                                     group.by = NULL,
                                     team_col = "batting_team",
                                     min_pitch_pct = NULL,
                                     min_bbe_pct = NULL,
                                     min_pa_pct = NULL,
                                     group_on_pa = TRUE) {
  library(dplyr)
  
  pitch_tbl <- apply_split_filters(
    views$pitch,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand, 
    start_date = start_date, 
    end_date = end_date
  )
  
  bbe_tbl <- apply_split_filters(
    views$bbe,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand, 
    start_date = start_date, 
    end_date = end_date
  )
  
  pa_tbl <- apply_split_filters(
    views$pa,
    group = if (group_on_pa) group else "none",
    bat_hand = bat_hand,
    pit_hand = pit_hand, 
    start_date = start_date, 
    end_date = end_date
  )
  
  pa_sum <- summarize_pa_batter(
    pa_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  disc_sum <- summarize_disc_batter(
    pitch_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  bbe_sum <- summarize_bbe_batter(
    bbe_tbl,
    level = level,
    group.by = group.by,
    team_col = team_col
  )
  
  id_keep <- if (level == "team") team_col else "matchup.batter.id"
  keep_base <- c(id_keep, "name", group.by)
  
  pa_sum <- mask_below_threshold(
    pa_sum,
    "PA",
    min_pa_pct,
    keep_cols = keep_base
  )
  
  disc_sum <- mask_below_threshold(
    disc_sum,
    "Pitches",
    min_pitch_pct,
    keep_cols = keep_base
  )
  
  bbe_sum <- mask_below_threshold(
    bbe_sum,
    "BBE",
    min_bbe_pct,
    keep_cols = keep_base
  )
  
  pa_sum <- add_metric_ranks(
    pa_sum,
    high_good = c("PA", "AB", "H", "BB", "HR", "TB", "AVG", "BB_pct", "SLG", "OBP", "OPS"),
    low_good  = c("SO", "K_pct"),
    by_group  = group.by
  )
  
  disc_sum <- add_metric_ranks(
    disc_sum,
    high_good = c("Pitches", "Swings", "Contact", "Contact_pct",
                  "OContact", "OContact_pct", "ZContact", "ZContact_pct"),
    low_good  = c("Whiffs", "Whiff_pct", "Chase", "Chase_pct"),
    by_group  = group.by
  )
  
  bbe_sum <- add_metric_ranks(
    bbe_sum,
    high_good = c("BBE", "HardHit", "HardHit_pct", "barrels", "barrel_pct",
                  "pull_fb", "pull_fbpct", "FBHR", "HRFB", "EV"),
    low_good  = c("GB", "GB_pct"),
    by_group  = group.by
  )
  
  id_col <- if (level == "team") team_col else "matchup.batter.id"
  join_cols <- c(id_col, group.by)
  
  pa_sum %>%
    full_join(disc_sum, by = join_cols) %>%
    full_join(bbe_sum, by = join_cols) %>%
    arrange(desc(PA), desc(Pitches), desc(BBE))
}

add_metric_ranks <- function(df, high_good = NULL, low_good = NULL, by_group = NULL) {
  library(dplyr)
  
  high_good <- intersect(high_good %||% character(), names(df))
  low_good  <- intersect(low_good  %||% character(), names(df))
  
  if (length(c(high_good, low_good)) == 0) {
    return(df)
  }
  
  rank_one <- function(x, descending = TRUE) {
    out <- rep(NA_integer_, length(x))
    good <- !is.na(x)
    
    if (any(good)) {
      if (descending) {
        out[good] <- min_rank(desc(x[good]))
      } else {
        out[good] <- min_rank(x[good])
      }
    }
    out
  }
  
  if (!is.null(by_group) && length(by_group) > 0) {
    by_group <- intersect(by_group, names(df))
    
    df <- df %>%
      group_by(across(all_of(by_group))) %>%
      mutate(
        across(
          all_of(high_good),
          ~ rank_one(., descending = TRUE),
          .names = "{.col}_rank"
        ),
        across(
          all_of(low_good),
          ~ rank_one(., descending = FALSE),
          .names = "{.col}_rank"
        )
      ) %>%
      ungroup()
  } else {
    df <- df %>%
      mutate(
        across(
          all_of(high_good),
          ~ rank_one(., descending = TRUE),
          .names = "{.col}_rank"
        ),
        across(
          all_of(low_good),
          ~ rank_one(., descending = FALSE),
          .names = "{.col}_rank"
        )
      )
  }
  
  df
}
`%||%` <- function(x, y) if (is.null(x)) y else x

####
#function
rolling_daily_rate <- function(data,
                               num_col,
                               den_col,
                               date_col = "game_date",
                               group_cols = NULL,
                               window = 7,
                               rate_name = "rolling_rate") {
  library(dplyr)
  library(slider)
  
  grouping <- c(group_cols, date_col)
  
  daily_tbl <- data %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      numerator   = sum(.data[[num_col]], na.rm = TRUE),
      denominator = sum(.data[[den_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(c(group_cols, date_col))))
  
  if (!is.null(group_cols) && length(group_cols) > 0) {
    daily_tbl <- daily_tbl %>%
      group_by(across(all_of(group_cols))) %>%
      mutate(
        rolling_num = slide_dbl(numerator, sum, .before = window - 1, .complete = TRUE),
        rolling_den = slide_dbl(denominator, sum, .before = window - 1, .complete = TRUE),
        !!rate_name := ifelse(rolling_den > 0, rolling_num / rolling_den, NA_real_)
      ) %>%
      ungroup()
  } else {
    daily_tbl <- daily_tbl %>%
      mutate(
        rolling_num = slide_dbl(numerator, sum, .before = window - 1, .complete = TRUE),
        rolling_den = slide_dbl(denominator, sum, .before = window - 1, .complete = TRUE),
        !!rate_name := ifelse(rolling_den > 0, rolling_num / rolling_den, NA_real_)
      )
  }
  
  daily_tbl
}
#plot function
plot_rolling_trend <- function(data, x_col = "game_date", y_col, color_col = NULL, title = NULL) {
  library(ggplot2)
  
  p <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]]))
  
  if (!is.null(color_col)) {
    p <- p + geom_line(aes(color = .data[[color_col]]), linewidth = 1)
  } else {
    p <- p + geom_line(linewidth = 1)
  }
  
  p +
    labs(
      title = title,
      x = "Date",
      y = y_col
    ) +
    theme_minimal()
}

####
build_contact_matrix <- function(enhanced_pbp, group_type = "player") {
  library(dplyr)
  
  tbl <- enhanced_pbp %>%
    filter(
      is_bbe == 1,
      !is.na(contact_quality),
      !is.na(bbe_type),
      !is.na(spray_direction)
    )
  
  if (group_type == "player") {
    out <- tbl %>%
      group_by(
        matchup.batter.fullName,
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarize(
        bbe = n(),
        hits = sum(is_hit, na.rm = TRUE),
        TB = sum(total_bases, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if (group_type == "team") {
    out <- tbl %>%
      group_by(
        batting_team,
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarize(
        bbe = n(),
        hits = sum(is_hit, na.rm = TRUE),
        TB = sum(total_bases, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if (group_type == "league") {
    out <- tbl %>%
      group_by(
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarize(
        bbe = n(),
        hits = sum(is_hit, na.rm = TRUE),
        TB = sum(total_bases, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    stop("group_type must be 'player', 'team', or 'league'")
  }
  
  out
}

load_contact_matrix <- function(path = "contact_matrix.csv") {
  library(dplyr)
  
  read.csv(path) %>%
    select(contact_quality, bbe_type, spray_direction, batting_avg, SLG_pct) %>%
    rename(
      xBA_bucket = batting_avg,
      xSLG_bucket = SLG_pct
    )
}

expected_contact_detail <- function(pbp, contact_matrix, group_type = "player") {
  library(dplyr)
  
  build_contact_matrix(pbp, group_type = group_type) %>%
    left_join(
      contact_matrix,
      by = c("contact_quality", "bbe_type", "spray_direction")
    ) %>%
    mutate(
      xH_contrib = bbe * xBA_bucket,
      xTB_contrib = bbe * xSLG_bucket
    )
}

calc_expected_contact_stats <- function(pbp, contact_matrix, group_type = "player") {
  library(dplyr)
  
  detail_tbl <- expected_contact_detail(
    pbp = pbp,
    contact_matrix = contact_matrix,
    group_type = group_type
  )
  
  if (group_type == "player") {
    out <- detail_tbl %>%
      group_by(matchup.batter.fullName) %>%
      summarize(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(rate(H, BBE), 3),
        SLG_on_contact = round(rate(TB, BBE), 3),
        xBA = round(rate(xH, BBE), 3),
        xSLG = round(rate(xTB, BBE), 3),
        BA_v_exp = (BA_on_contact - xBA), 
        SLG_v_exp = (SLG_on_contact - xSLG), 
        .groups = "drop"
      )
    
  } else if (group_type == "team") {
    out <- detail_tbl %>%
      group_by(batting_team) %>%
      summarize(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(rate(H, BBE), 3),
        SLG_on_contact = round(rate(TB, BBE), 3),
        xBA = round(rate(xH, BBE), 3),
        xSLG = round(rate(xTB, BBE), 3),
        BA_v_exp = (BA_on_contact - xBA), 
        SLG_v_exp = (SLG_on_contact - xSLG), 
        .groups = "drop"
      )
    
  } else if (group_type == "league") {
    out <- detail_tbl %>%
      summarize(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(rate(H, BBE), 3),
        SLG_on_contact = round(rate(TB, BBE), 3),
        xBA = round(rate(xH, BBE), 3),
        xSLG = round(rate(xTB, BBE), 3), 
        BA_v_exp = (BA_on_contact - xBA), 
        SLG_v_exp = (SLG_on_contact - xSLG)
      )
    
  } else {
    stop("group_type must be 'player', 'team', or 'league'")
  }
  
  out
}