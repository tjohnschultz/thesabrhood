#Function 2.2.1
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
      team = dplyr::first(.data[[team_col]]), 
      hand = case_when(
        all(.data[["matchup.pitchHand.code"]] == "L", na.rm = TRUE) ~ "L",
        all(.data[["matchup.pitchHand.code"]] == "R", na.rm = TRUE) ~ "R",
        .default = "S"
      ),
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

#Function 2.2.2
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
      velocity = mean(pitchData.startSpeed, na.rm = TRUE), 
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

#Function 2.2.3
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

#Function 2.1.1
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
  library(dplyr); library(purrr)
  
  group.by <- if (length(group.by) == 1 && is.na(group.by)) NULL else group.by
  
  required_views <- c("pitch", "bbe", "pa")
  
  missing_views <- required_views[!required_views %in% names(views)]
  
  if (length(missing_views) > 0) {
    stop(
      "summarize_overall_pitcher() is missing required views tables: ",
      paste(missing_views, collapse = ", "),
      call. = FALSE
    )
  }
  
  null_views <- required_views[purrr::map_lgl(views[required_views], is.null)]
  
  if (length(null_views) > 0) {
    stop(
      "summarize_overall_pitcher() received NULL tables: ",
      paste(null_views, collapse = ", "),
      call. = FALSE
    )
  }
  
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
    high_good = c("Pitches", "Whiffs", "Whiff_pct", "Chase", "Chase_pct", "Zone_pct"),
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

#Function 2.2.4
summarize_pitch_shape_pitcher <- function(pitch_tbl,
                                          level = "player",
                                          group.by = NULL,
                                          team_col = "fielding_team") {
  library(dplyr)
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  name_source <- if (level == "team") team_col else "matchup.pitcher.fullName"
  
  pitch_tbl2 <- pitch_tbl %>%
    mutate(
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
  
  group_vars <- c(id_col, group.by, "pitch_type")
  
  sum_base <- pitch_tbl2 %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      name = dplyr::first(.data[[name_source]]),
      team = dplyr::first(.data[[team_col]]),
      Shape_Pitches = n(),
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0),
      VAA = round(mean(VAA, na.rm = TRUE), 2),
      HAA = round(mean(HAA, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  parent_group <- c(id_col, group.by)
  
  final_sum <- sum_base %>%
    group_by(across(all_of(parent_group))) %>%
    mutate(
      usage = round(Shape_Pitches / sum(Shape_Pitches, na.rm = TRUE), 3)
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
    arrange(desc(Shape_Pitches))
  
  return(final_sum)
}

#Function 2.2.5
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
      across(all_of(c(id_col, group.by, "pitch_type")))
    ) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
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

#Function 2.2.6
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
      across(all_of(c(id_col, group.by, "pitch_type")))
    ) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
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

#Function 2.1.2
summarize_overall_pitcher_by_pitchtype <- function(views,
                                                   group = "none",
                                                   bat_hand = "none",
                                                   pit_hand = "none",
                                                   level = "player",
                                                   start_date = NULL,
                                                   end_date = NULL,
                                                   risp = "all",
                                                   group.by = NULL,
                                                   team_col = "fielding_team",
                                                   pitch_type = NA, 
                                                   min_pitch_pct = NULL,
                                                   min_bbe_pct = NULL) {
  library(dplyr)
  
  if (!level %in% c("player", "team", "league")) {
    stop("level must be 'player', 'team', or 'league'")
  }
  
  summary_level <- if (level == "league") "team" else level
  summary_team_col <- if (level == "league") ".league" else team_col
  
  pitch_tbl <- apply_split_filters(
    views$pitch,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp, 
    pitch_type = pitch_type
  )
  
  bbe_tbl <- apply_split_filters(
    views$bbe,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date, 
    risp = risp, 
    pitch_type = pitch_type
  )
  
  if (level == "league") {
    pitch_tbl <- pitch_tbl %>% mutate(.league = "League")
    bbe_tbl   <- bbe_tbl %>% mutate(.league = "League")
  }
  
  disc_sum <- summarize_disc_by_pitchtype_pitcher(
    pitch_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  shape_sum <- summarize_pitch_shape_pitcher(
    pitch_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  bbe_sum <- summarize_bbe_by_pitchtype_pitcher(
    bbe_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  id_keep <- if (level == "team") {
    team_col
  } else if (level == "league") {
    ".league"
  } else {
    "matchup.pitcher.id"
  }
  
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
  
  shape_sum <- add_metric_ranks(
    shape_sum,
    high_good = c("velo", "spin", "HAA", "VAA"),
    low_good  = c(),
    by_group  = group.by
  )
  
  disc_sum <- add_metric_ranks(
    disc_sum,
    high_good = c(
      "Pitches",
      "Swings",
      "Whiffs",
      "Whiff_pct",
      "Chase",
      "Chase_pct",
      "Zone_pct"
    ),
    low_good  = c(
      "Contact",
      "Contact_pct",
      "OContact",
      "OContact_pct",
      "ZContact",
      "ZContact_pct"
    ),
    by_group  = group.by
  )
  
  bbe_sum <- add_metric_ranks(
    bbe_sum,
    high_good = c("BBE", "GB", "GB_pct"),
    low_good  = c(
      "HardHit",
      "HardHit_pct",
      "barrels",
      "barrel_pct",
      "pull_fb",
      "pull_fbpct",
      "FBHR",
      "HRFB",
      "EV",
      "LA"
    ),
    by_group  = group.by
  )
  
  join_cols <- c(id_keep, group.by, "pitch_type")
  
  disc_dupes <- disc_sum %>%
    count(across(all_of(join_cols))) %>%
    filter(n > 1)
  
  shape_dupes <- shape_sum %>%
    count(across(all_of(join_cols))) %>%
    filter(n > 1)
  
  bbe_dupes <- bbe_sum %>%
    count(across(all_of(join_cols))) %>%
    filter(n > 1)
  
  if (nrow(disc_dupes) > 0) {
    stop("disc_sum has duplicate join keys. Check disc_dupes.")
  }
  
  if (nrow(shape_dupes) > 0) {
    stop("shape_sum has duplicate join keys. Check shape_dupes.")
  }
  
  if (nrow(bbe_dupes) > 0) {
    stop("bbe_sum has duplicate join keys. Check bbe_dupes.")
  }
  
  final_sum <- disc_sum %>%
    full_join(
      shape_sum %>% select(-any_of(c("name", "team"))),
      by = join_cols,
      relationship = "one-to-one"
    ) %>%
    full_join(
      bbe_sum %>% select(-any_of(c("name", "team"))),
      by = join_cols,
      relationship = "one-to-one"
    )
  
  if (level == "league") {
    final_sum <- final_sum %>%
      mutate(name = "League") %>%
      select(-any_of(".league"))
  }
  
  final_sum <- final_sum %>%
    mutate(
      pitch = pitch_type,
      
      name_ref_1 = name,
      pitch_ref_1 = pitch_type,
      
      name_ref_2 = name,
      pitch_ref_2 = pitch_type,
      
      name_ref_3 = name,
      pitch_ref_3 = pitch_type,
      
      name_ref_4 = name,
      pitch_ref_4 = pitch_type
    ) %>%
    relocate(name_ref_1, pitch_ref_1, .after = name) %>%
    relocate(name_ref_2, pitch_ref_2, .after = HAA) %>%
    relocate(name_ref_3, pitch_ref_3, .after = Shadow_pct) %>%
    relocate(name_ref_4, pitch_ref_4, .after = LA)
  
  final_sum
}

#Function 2.2.7
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
      team = dplyr::first(.data[[team_col]]), 
      hand = case_when(
        all(.data[["matchup.batSide.code"]] == "L", na.rm = TRUE) ~ "L",
        all(.data[["matchup.batSide.code"]] == "R", na.rm = TRUE) ~ "R",
        .default = "S"
      ),
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

#Function 2.2.8
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

#Function 2.2.9
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

#Function 2.1.3
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
  
  if (!level %in% c("player", "team", "league")) {
    stop("level must be 'player', 'team', or 'league'")
  }
  
  summary_level <- if (level == "league") "team" else level
  summary_team_col <- if (level == "league") ".league" else team_col
  
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
  
  if (level == "league") {
    pitch_tbl <- pitch_tbl %>% mutate(.league = "League")
    bbe_tbl   <- bbe_tbl %>% mutate(.league = "League")
    pa_tbl    <- pa_tbl %>% mutate(.league = "League")
  }
  
  pa_sum <- summarize_pa_batter(
    pa_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  disc_sum <- summarize_disc_batter(
    pitch_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  bbe_sum <- summarize_bbe_batter(
    bbe_tbl,
    level = summary_level,
    group.by = group.by,
    team_col = summary_team_col
  )
  
  id_keep <- if (level == "team") {
    team_col
  } else if (level == "league") {
    ".league"
  } else {
    "matchup.batter.id"
  }
  
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
    high_good = c("PA", "AB", "H", "BB", "HR", "TB", "AVG", "BB_pct", "SLG", "OBP", "OPS", "SO", "K_pct"),
    low_good  = c(),
    by_group  = group.by
  )
  
  disc_sum <- add_metric_ranks(
    disc_sum,
    high_good = c("Pitches", "Swings", "Contact", "Contact_pct",
                  "OContact", "OContact_pct", "ZContact", "ZContact_pct", 
                  "Whiffs", "Whiff_pct", "Chase", "Chase_pct"),
    low_good  = c(),
    by_group  = group.by
  )
  
  bbe_sum <- add_metric_ranks(
    bbe_sum,
    high_good = c("BBE", "HardHit", "HardHit_pct", "barrels", "barrel_pct",
                  "pull_fb", "pull_fbpct", "FBHR", "HRFB", "EV", "GB", "GB_pct"),
    low_good  = c(),
    by_group  = group.by
  )
  
  join_cols <- c(id_keep, group.by)
  
  if (level == "league") {
    disc_sum <- disc_sum %>% select(-any_of("name"))
    bbe_sum  <- bbe_sum %>% select(-any_of("name"))
  }
  
  final_sum <- pa_sum %>%
    full_join(disc_sum, by = join_cols) %>%
    full_join(bbe_sum, by = join_cols)
  
  if (level == "league") {
    final_sum <- final_sum %>%
      mutate(
        name = "League",
        name_ref_1 = "League",
        name_ref_2 = "League",
        name_ref_3 = "League",
        name_ref_4 = "League"
      ) %>%
      select(-any_of(".league"))
  }
  
  final_sum %>%
    arrange(desc(PA), desc(Pitches), desc(BBE)) %>%
    relocate(any_of("name_ref_1"), .after = any_of("name")) %>%
    relocate(any_of("name_ref_2"), .after = any_of("OPS")) %>%
    relocate(any_of("name_ref_3"), .after = any_of("Shadow_pct")) %>%
    relocate(any_of("name_ref_4"), .after = any_of("LA"))
}