# =========================================================
# METRICS SCRIPT FOR QUARTO WEBPAGE
# Reorganized by section.
# Existing function bodies are kept as-is except where a
# summarize / summarize leaderboard counterpart was missing
# a metric already present in the other version.
# =========================================================

# =========================================================
# 1) CORE PLAY-BY-PLAY ENHANCEMENT + HELPERS
# =========================================================

enhance_pbp <- function(data) {
  library(dplyr)
  library(stringr)
  
  non_pa_events <- c(
    "walk", "intent_walk", "hit_by_pitch", "sac_fly", "sac_bunt",
    "sac_fly_double_play", "catcher_interf"
  )
  
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
  
  data %>%
    mutate(game_date = as.Date(game_date)) %>%
    group_by(game_pk, atBatIndex) %>%
    mutate(
      is_last_pitch = as.integer(pitchNumber == max(pitchNumber, na.rm = TRUE)),
      is_swing = as.integer(details.call.description %in% swing_events),
      is_whiff = as.integer(details.call.description %in% whiff_events),
      is_contact = as.integer(is_swing == 1 & is_whiff == 0),
      is_pa = as.integer(is_last_pitch == 1 & !(result.eventType %in% baserunning_events)),
      is_at_bat = as.integer(is_last_pitch == 1 &
                               !(result.eventType %in% non_pa_events) &
                               !(result.eventType %in% baserunning_events)),
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
      is_hit = as.integer(!is.na(total_bases) & total_bases > 0),
      is_bbe = as.integer(isPitch %in% TRUE & ((details.isInPlay %in% TRUE) | !is.na(hitData.launchSpeed))),
      ev = suppressWarnings(as.numeric(hitData.launchSpeed)),
      la = suppressWarnings(as.numeric(hitData.launchAngle)),
      is_hard_hit = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 95),
      is_barrel = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 98 & la >= 24 & la <= 33), 
      is_fb = as.integer(is_bbe == 1 & !is.na(ev) & la >= 25 & la <= 50), 
      is_gb = as.integer(hitData.trajectory == "ground_ball"), 
      is_hr = as.integer(!is.na(total_bases) & total_bases == 4), 
      is_pull = case_when(
        matchup.batSide.description == "Left" & hitData.location %in% c(3, 4, 9) ~ 1,
        matchup.batSide.description == "Right" & hitData.location %in% c(5, 6, 7) ~ 1,
        TRUE ~ 0
      ), 
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
      count = paste(count.balls.start, "-", count.strikes.start)
    ) %>%
    ungroup()
}

update_pbp <- function(og_data, szn_type) {
  og_pks <- as.vector(og_data$game_pk)
  dates <- seq(from = as.Date("2026-02-20"), to = Sys.Date(), by = "day")
  scheds <- map_dfr(dates, ~get_game_pks_mlb(date = .x, level_ids = 1)) %>% 
    filter(seriesDescription == szn_type & status.detailedState == "Final")
  new_pks <- as.vector(scheds$game_pk)
  pks_diff <- setdiff(new_pks, og_pks)
  new_data <- map_dfr(pks_diff, ~get_pbp_mlb(game_pk = .x))
  updated <- bind_rows(og_data, new_data)
  return(updated)
}

make_tables <- function(pbp) {
  enh <- enhance_pbp(pbp)
  tbl_pitch <- enh %>%
    filter(isPitch %in% TRUE)
  
  tbl_pa <- enh %>%
    filter(is_pa %in% TRUE)
  
  tbl_bip <- enh %>%
    filter(isPitch %in% TRUE) %>%
    filter((details.isInPlay %in% TRUE) | (!is.na(hitData.launchSpeed)))
  
  list(
    pbp = enh,
    pitch = tbl_pitch,
    pa = tbl_pa,
    bip = tbl_bip
  )
}

rate <- function(num, den) ifelse(den > 0, num / den, NA_real_)


# =========================================================
# 2) INDIVIDUAL PITCHER SUMMARIES
# =========================================================

pitch_summary <- function(data, playername) {
  sum_base <- data %>% 
    filter(matchup.pitcher.fullName == playername & isPitch == TRUE) %>%
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), vy_f = vy0 + (ay * t), vz_f = vz0 + (az * t), vx_f = vx0 + (ax * t),
      pitch_vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      pitch_haa = atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0), 
      VAA = round(mean(pitch_vaa, na.rm = TRUE), 2),
      HAA = round(mean(pitch_haa, na.rm = TRUE), 2)
    ) %>%
    mutate(usage = pitches / sum(pitches))
  
  final_sum <- sum_base %>%
    rowwise() %>%
    mutate(
      move_diversity = sum(
        sqrt((VAA - sum_base$VAA)^2 + (HAA - sum_base$HAA)^2) * sum_base$usage
      )
    ) %>%
    ungroup() %>%
    mutate(move_diversity = round(move_diversity, 2)) %>%
    arrange(desc(pitches))
  
  return(final_sum)
}

pitch_summary_avg <- function(data) {
  sum_base <- data %>% 
    filter(isPitch == TRUE) %>%
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), vz_f = vz0 + (az * t), vx_f = vx0 + (ax * t),
      pitch_vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      pitch_haa = atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0), 
      VAA = round(mean(pitch_vaa, na.rm = TRUE), 2),
      HAA = round(mean(pitch_haa, na.rm = TRUE), 2)
    ) %>%
    mutate(usage = pitches / sum(pitches))
  
  final_sum <- sum_base %>%
    rowwise() %>%
    mutate(
      move_diversity = sum(
        sqrt((VAA - sum_base$VAA)^2 + (HAA - sum_base$HAA)^2) * sum_base$usage
      )
    ) %>%
    ungroup() %>%
    mutate(move_diversity = round(move_diversity, 2)) %>%
    arrange(desc(pitches))
  
  return(final_sum)
}

summarize_bbe_quality_pit_hand <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(details.type.description, matchup.batSide.description) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      GB = sum(is_gb, na.rm = TRUE), 
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      GB_pct = round(rate(GB, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      pull_fb = sum(is_pull_fb, na.rm = TRUE), 
      pull_fbpct = round(rate(pull_fb, BBE), 2), 
      .groups="drop"
    )
  final_sum <- sum[,-c(4, 5, 8, 12)]
  return(final_sum)
}

# THE BIG SIX IN CREATE PITCH SUMS

summarize_pa_results <- function(pa_tbl) {
  sum <- pa_tbl %>%
    summarise(
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      
      AVG = round(rate(H, AB), 3), 
      K_pct  = round(rate(SO, PA), 2), 
      BB_pct = round(rate(BB, PA), 2), 
      .groups = "drop"
    )
  return(sum)
}

summarize_pitch_discipline <- function(pitch_tbl) {
  sum <- pitch_tbl %>%
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      Heart   = sum(is_heart, na.rm = TRUE),
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2), 
      Contact_pct  = round(rate(Contact, Swings), 2), 
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2), 
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct    = round(rate(Heart, Pitches), 2),
      Shadow_pct   = round((Zone_pct - Heart_pct), 2),
      .groups = "drop"
    )
  final_sum <- sum[,c(1, 13:21)]
  return(final_sum)
}

summarize_pitch_discipline_gb <- function(pitch_tbl) {
  pitch_tbl <- enhance_pbp(pitch_tbl)
  sum <- pitch_tbl %>%
    group_by(details.type.description, matchup.pitcher.fullName) %>% 
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      Heart   = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2),
      Contact_pct  = round(rate(Contact, Swings), 2),
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2),
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct    = round(rate(Heart, Pitches), 2),
      Shadow_pct   = round((Zone_pct - Heart_pct), 2),
      .groups = "drop"
    )
  final_sum <- sum[,c(1, 3, 14:23)]
  final_sum <- final_sum %>% 
    arrange(desc(Pitches))
  return(final_sum)
}

summarize_pitch_shape <- function(pitch_tbl) {
  sum_base <- pitch_tbl %>%
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), vz_f = vz0 + (az * t), vx_f = vx0 + (ax * t),
      pitch_vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      pitch_haa = atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0), 
      VAA = round(mean(pitch_vaa, na.rm = TRUE), 2),
      HAA = round(mean(pitch_haa, na.rm = TRUE), 2)
    ) %>%
    mutate(usage = round(pitches / sum(pitches), 2))
  
  final_sum <- sum_base %>%
    rowwise() %>%
    mutate(
      move_diversity = sum(
        sqrt((VAA - sum_base$VAA)^2 + (HAA - sum_base$HAA)^2) * sum_base$usage
      )
    ) %>%
    ungroup() %>%
    mutate(move_diversity = round(move_diversity, 2)) %>%
    arrange(desc(pitches))
  
  return(final_sum)
}

summarize_bbe_quality <- function(bip_tbl) {
  sum <- bip_tbl %>%
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      pull_fb = sum(is_pull_fb, na.rm = TRUE), 
      pull_fbpct = round(rate(pull_fb, BBE), 2), 
      FB = sum(is_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HRFB = round(rate(FBHR, FB), 2),
      .groups="drop"
    )
  final_sum <- sum[,c(3, 5, 7, 9, 12)]
  return(final_sum)
}

summarize_bbe_quality_pit <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(details.type.description) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      GB = sum(is_gb, na.rm = TRUE), 
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      GB_pct = round(rate(GB, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      pull_fb = sum(is_pull_fb, na.rm = TRUE), 
      pull_fbpct = round(rate(pull_fb, BBE), 2), 
      .groups="drop"
    )
  final_sum <- sum[,-c(3, 4, 7, 11)]
  final_sum <- final_sum %>% 
    arrange(desc(BBE))
  return(final_sum)
}

create_pitch_sums <- function(data) {
  enh <- enhance_pbp(data)
  tabs <- make_tables(enh)
  pa_sum <- summarize_pa_results(tabs$pa)
  disc_sum <- summarize_pitch_discipline(tabs$pitch)
  shape_sum <- summarize_pitch_shape(tabs$pitch)
  bbe_sum <- summarize_bbe_quality(tabs$bip)
  disc_pit_sum <- summarize_pitch_discipline_gb(tabs$pitch)
  bbe_pit_sum <- summarize_bbe_quality_pit(tabs$bip)
  
  list(
    pa_sum = pa_sum, 
    disc_sum = disc_sum, 
    shape_sum = shape_sum, 
    bbe_sum = bbe_sum, 
    bbe_pit_sum = bbe_pit_sum, 
    disc_pit_sum = disc_pit_sum
  )
}


# =========================================================
# 3) QUANTILES
# =========================================================

quantiles_aps <- function(aps_indy) {
  sum <- aps_indy %>% 
    group_by(pitch_type) %>% 
    summarise(
      quantvelo = quantile(velo), 
      quantIVB = quantile(IVB), 
      quantileHB = quantile(HB), 
      quantspin = quantile(spin), 
      quantVAA = quantile(VAA), 
      quantHAA = quantile(HAA), 
      .groups = "drop"
    )
  return(sum)
}

quantiles_bbe <- function(bbe) {
  sum <- bbe %>% 
    group_by(details.type.description) %>% 
    summarise(
      quanthard_hit = quantile(HardHit_pct), 
      quantbrl_pct = quantile(barrel_pct), 
      quantileEV = quantile(EV), 
      quantLA = quantile(LA),  
      .groups = "drop"
    )
  return(sum)
}

quantiles_disc <- function(disc_quant_base) {
  sum <- disc_quant_base %>% 
    group_by(details.type.description) %>% 
    summarise(
      quantswing_pct = quantile(Swing_pct), 
      quantwhiff_pct = quantile(Whiff_pct), 
      quantileContact_pct = quantile(Contact_pct), 
      quantZone_pct = quantile(Zone_pct), 
      quantChase_pct = quantile(Chase_pct), 
      quantzswing_pct = quantile(ZSwing_pct), 
      quantileOContact_pct = quantile(OContact_pct), 
      quantileZContact_pct = quantile(ZContact_pct), 
      .groups = "drop"
    )
  return(sum)
}


# =========================================================
# 4) SPLIT / TIME / STADIUM SUMMARIES
# =========================================================

pitch_summary_date <- function(data, playername) {
  data <- data %>% 
    filter(matchup.pitcher.fullName == playername & isPitch == TRUE) 
  data <- data %>% 
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      release_extension = pitchData.extension, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      pitch_hand = matchup.pitchHand.description, 
      spin_tilt = pitchData.breaks.spinDirection
    ) %>% 
    mutate(month_name = month(as.Date(game_date), label = TRUE, abbr = FALSE))
  clean <- data %>% 
    select(
      pitcher_name,  
      pitch_type,
      release_velo, 
      release_spin, 
      release_extension, 
      hz_movement, 
      ivb_movement,
      pitch_hand, 
      spin_tilt, 
      game_date, 
      month_name
    )
  sum <- clean %>% 
    group_by(pitch_type, month_name, "month") %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, rm.na = TRUE), 
      IVB = mean(ivb_movement, rm.na = TRUE), 
      HB = mean(hz_movement, rm.na = TRUE), 
      spin = mean(release_spin, rm.na = TRUE)
      
    )
  return(sum)
}

pitch_summary_stadiums <- function(data) {
  data <- data %>% 
    filter(isPitch == TRUE) 
  data <- data %>% 
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      release_extension = pitchData.extension, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      pitch_hand = matchup.pitchHand.description, 
      spin_tilt = pitchData.breaks.spinDirection
    )
  clean <- data %>% 
    select(
      pitcher_name,  
      pitch_type,
      release_velo, 
      release_spin, 
      release_extension, 
      hz_movement, 
      ivb_movement,
      pitch_hand, 
      spin_tilt,
      home_team
    )
  sum <- clean %>% 
    group_by(pitch_type, home_team) %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, na.rm = TRUE), 
      IVB = mean(ivb_movement, na.rm = TRUE), 
      HB = mean(abs(hz_movement), na.rm = TRUE), 
      spin = mean(release_spin, na.rm = TRUE)
    )
  return(sum)
}

pitch_summary_stadiums_pitcher <- function(data, playername) {
  data <- data %>% 
    filter(isPitch == TRUE & matchup.pitcher.fullName == playername) 
  data <- data %>% 
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      release_extension = pitchData.extension, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      pitch_hand = matchup.pitchHand.description, 
      spin_tilt = pitchData.breaks.spinDirection
    )
  clean <- data %>% 
    select(
      pitcher_name,  
      pitch_type,
      release_velo, 
      release_spin, 
      release_extension, 
      hz_movement, 
      ivb_movement,
      pitch_hand, 
      spin_tilt,
      home_team
    )
  sum <- clean %>% 
    group_by(pitch_type, home_team) %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, na.rm = TRUE), 
      IVB = mean(ivb_movement, na.rm = TRUE), 
      HB = mean(abs(hz_movement), na.rm = TRUE), 
      spin = mean(release_spin, na.rm = TRUE)
    )
  return(sum)
}


# =========================================================
# 5) VISUALS + SMALL HELPERS / WIP
# =========================================================

swinging_plot <- function(data, playername, year) {
  plot_title <- paste("Pitches swung at by", playername, "in", year)
  values_to_remove <- c("Hit By Pitch", "Foul Bunt", "Missed Bunt", "Called Strike", "Ball", "Ball In Dirt")
  data <- data %>% 
    filter(matchup.batter.fullName == playername & isPitch == TRUE) %>% 
    filter(!(details.call.description %in% values_to_remove)) %>% 
    mutate(
      whiff = as.factor(case_when(
        details.call.description == "Swinging Strike" ~ 1,
        details.call.description == "Swinging Strike (Blocked)" ~ 1,
        TRUE ~ 0
      ))
    )
  
  plate_width <- 17 + 2 * (9 / pi)
  k_zone_plot <- ggplot(data = data, aes(x = pitchData.coordinates.pX, y = pitchData.coordinates.pZ, color = whiff) ) + 
    geom_point() + 
    geom_rect(
      xmin = -(plate_width/2)/12, 
      xmax = (plate_width/2)/12, 
      ymin = mean(data$pitchData.strikeZoneBottom), 
      ymax = mean(data$pitchData.strikeZoneTop), color = "blue", alpha = 0
    ) +
    coord_equal() + 
    scale_x_continuous(
      "Horizontal location (ft.)", 
      limits = c(-2, 2)
    ) + 
    scale_y_continuous(
      "Vertical location (ft.)", 
      limits = c(0, 5)
    ) + 
    labs(
      title = plot_title,
      subtitle = "From Catcher's Perspective.",
      caption = "Data supplied by baseballR. Plot by Tyler Schultz."
    )
  k_zone_plot
}

rolling_batting_vectors <- function(pbp_data) {
  
}

filter_by <- function(pitcher = NA, batter = NA, team = NA){
  
}

odd_nums <- function(data){
  odds <- c()
  for (i in 1:length(data)) {
    if((data[i] %% 2) == 1) {
      odds <- c(odds, data[i])
    }
    else {
    }
  }
  return(odds)
}

even_nums <- function(data){
  evens <- c()
  for (i in 1:length(data)) {
    if((data[i] %% 2) != 1) {
      evens <- c(evens, data[i])
    }
    else {
    }
  }
  return(evens)
}


# =========================================================
# 6) BATTER LEADERBOARD SUMMARIES
# =========================================================

summarize_pa_results_bat_lead <- function(pa_tbl) {
  sum <- pa_tbl %>% 
    group_by(matchup.batter.fullName) %>% 
    summarise(
      total_bases = sum(total_bases, na.rm = TRUE), 
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE), 
      HR = sum(is_hr, na.rm = TRUE), 
      
      AVG = round(rate(H, AB), 3), 
      K_pct  = round(rate(SO, PA), 2), 
      BB_pct = round(rate(BB, PA), 2), 
      SLG = round(rate(total_bases, AB), 3), 
      OBP = round((AVG + BB_pct), 3), 
      OPS = round((SLG + OBP), 3), 
      .groups = "drop"
    )  %>% 
    filter(PA >= (max(PA) * 0.25))
  return(sum[,-c(2)])
}

summarize_bbe_quality_bat_lead <- function(bip_tbl) {
  sum <- bip_tbl %>% 
    group_by(matchup.batter.fullName) %>% 
    summarise(
      BBE = n(),
      FB = sum(is_fb, na.rm = TRUE), 
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      HR = sum(is_hr, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE), 
      pull_fb = sum(is_pull_fb, na.rm = TRUE), 
      GB_pct = round(rate(GB, BBE), 2), 
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      HRFB = round(rate(HR, FB), 2), 
      pull_fbpct = round(rate(pull_fb, BBE), 2), 
      .groups="drop"
    ) %>% 
    filter(BBE >= (max(BBE) * 0.15))
  final_sum <- sum[,-c(3:8)]
  return(final_sum)
}

summarize_bbe_quality_pit_leader_heart <- function (bip_tbl) {
  bip_tbl <- bip_tbl %>% 
    filter(is_heart == 1)
  sum <- bip_tbl %>%
    group_by(matchup.batter.fullName) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      GB = sum(is_gb, na.rm = TRUE), 
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      GB_pct = round(rate(GB, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      .groups="drop"
    ) %>% 
    filter(BBE >= (max(BBE) * 0.15))
  final_sum <- sum[,-c(3,4,7)]
  final_sum <- final_sum
  return(final_sum)
}

summarize_pitch_discipline_bat_lead <- function(pitch_tbl) {
  sum <- pitch_tbl %>% 
    group_by(matchup.batter.fullName) %>% 
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE), 
      Heart = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2), 
      Contact_pct  = round(rate(Contact, Swings), 2), 
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2), 
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2), 
      barrel_swing = round(rate(barrels, Swings), 2), 
      Heart_pct = round(rate(Heart, Pitches), 2), 
      .groups = "drop"
    ) %>% 
    filter(Pitches >= (max(Pitches) * 0.25))
  final_sum <- sum[,-c(3:13)]
  return(final_sum)
}

summarize_pitch_hand_discipline_bat_lead <- function(pitch_tbl) {
  sum <- pitch_tbl %>% 
    group_by(matchup.batter.fullName, matchup.pitchHand.description) %>% 
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE), 
      Heart = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2), 
      Contact_pct  = round(rate(Contact, Swings), 2), 
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2), 
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2), 
      barrel_swing = round(rate(barrels, Swings), 2), 
      Heart_pct = round(rate(Heart, Pitches), 2), 
      .groups = "drop"
    ) %>% 
    filter(Pitches >= (max(Pitches) * 0.1))
  final_sum <- sum[,-c(4:14)]
  return(final_sum)
}

create_bat_sums_leader <- function(data) {
  enh <- enhance_pbp(data) 
  tabs <- make_tables(enh) 
  pa_sum <- summarize_pa_results_bat_lead(tabs$pa)
  bbe_qual <- summarize_bbe_quality_bat_lead(tabs$bip)
  disc <- summarize_pitch_discipline_bat_lead(tabs$pitch)
  disc_hand <- summarize_pitch_hand_discipline_bat_lead(tabs$pitch)
  bbe_heart <- summarize_bbe_quality_pit_leader_heart(tabs$bip)
  
  list(
    pa_sum = pa_sum,
    bbe_qual = bbe_qual,
    bbe_heart = bbe_heart, 
    disc = disc, 
    disc_hand = disc_hand
  )
}


# =========================================================
# 7) PITCHER LEADERBOARD SUMMARIES
# =========================================================

summarize_pa_results_leader <- function(pa_tbl) {
  sum <- pa_tbl %>%
    group_by(matchup.pitcher.fullName) %>% 
    summarise(
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      
      AVG = round(rate(H, AB), 3), 
      K_pct  = round(rate(SO, PA), 2), 
      BB_pct = round(rate(BB, PA), 2), 
      .groups = "drop"
    )
  return(sum)
}

summarize_pitch_discipline_gb <- function(pitch_tbl) {
  pitch_tbl <- enhance_pbp(pitch_tbl)
  sum <- pitch_tbl %>%
    group_by(matchup.pitcher.fullName, details.type.description) %>% 
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2),
      Contact_pct  = round(rate(Contact, Swings), 2),
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2),
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct = round(rate(Heart, Pitches), 2), 
      Shadow_pct = round((Zone_pct - Heart_pct), 2), 
      .groups = "drop"
    )
  final_sum <- sum[,-c(4:13)]
  final_sum <- final_sum %>% 
    arrange(desc(Pitches))
  return(final_sum)
}

summarize_pitch_discipline <- function(pitch_tbl) {
  pitch_tbl <- enhance_pbp(pitch_tbl)
  sum <- pitch_tbl %>%
    group_by(matchup.pitcher.fullName) %>% 
    summarise(
      Pitches = n(),
      
      Swings  = sum(is_swing, na.rm = TRUE),
      Whiffs  = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      
      ZoneP   = sum(is_zone, na.rm = TRUE),
      OZoneP  = sum(is_ozone, na.rm = TRUE),
      Chase   = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact= sum(is_o_contact, na.rm = TRUE),
      ZContact= sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2),
      Contact_pct  = round(rate(Contact, Swings), 2),
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2),
      Chase_pct    = round(rate(Chase, OZoneP), 2),
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),
      OContact_pct = round(rate(OContact, Chase), 2),
      ZContact_pct = round(rate(ZContact, ZSwings), 2),
      Heart_pct = round(rate(Heart, Pitches), 2), 
      Shadow_pct = round((Zone_pct - Heart_pct), 2), 
      .groups = "drop"
    )
  final_sum <- sum[,-c(3:13)]
  final_sum <- final_sum %>% 
    arrange(desc(Pitches))
  return(final_sum)
}

summarize_pitch_shape_leader <- function(pitch_tbl) {
  sum_base <- pitch_tbl %>%
    rename(
      pitcher_name = matchup.pitcher.fullName, 
      release_velo = pitchData.startSpeed,
      release_spin = pitchData.breaks.spinRate, 
      hz_movement = pitchData.breaks.breakHorizontal, 
      ivb_movement = pitchData.breaks.breakVerticalInduced, 
      pitch_type = details.type.description, 
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), vz_f = vz0 + (az * t), vx_f = vx0 + (ax * t),
      pitch_vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      pitch_haa = atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    group_by(pitch_type, pitcher_name) %>% 
    summarize(
      pitches = n(), 
      velo = round(mean(release_velo, na.rm = TRUE), 1),
      IVB = round(mean(ivb_movement, na.rm = TRUE), 1),
      HB = round(mean(hz_movement, na.rm = TRUE), 1),
      spin = round(mean(release_spin, na.rm = TRUE), 0), 
      VAA = round(mean(pitch_vaa, na.rm = TRUE), 2),
      HAA = round(mean(pitch_haa, na.rm = TRUE), 2)
    ) %>%
    mutate(usage = round(pitches / sum(pitches), 2))
  
  final_sum <- sum_base %>%
    rowwise() %>%
    mutate(
      move_diversity = sum(
        sqrt((VAA - sum_base$VAA)^2 + (HAA - sum_base$HAA)^2) * sum_base$usage
      )
    ) %>%
    ungroup() %>%
    mutate(move_diversity = round(move_diversity, 2)) %>%
    arrange(desc(pitches))
  
  return(final_sum)
}

summarize_bbe_quality_leader <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(matchup.pitcher.fullName) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      pull_fb = sum(is_pull_fb, na.rm = TRUE), 
      pull_fbpct = round(rate(pull_fb, BBE), 2), 
      FB = sum(is_fb, na.rm = TRUE), 
      HR = sum(is_hr, na.rm = TRUE),
      HRFB = round(rate(HR, FB), 2), 
      .groups="drop"
    )
  final_sum <- sum[,-c(3, 5, 9, 11, 12)]
  return(final_sum)
}

summarize_bbe_quality_pit_leader <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(matchup.pitcher.fullName, details.type.description) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      GB = sum(is_gb, na.rm = TRUE), 
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      GB_pct = round(rate(GB, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      FB = sum(is_fb, na.rm = TRUE), 
      HR = sum(is_hr, na.rm = TRUE),
      HRFB = round(rate(HR, FB), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      .groups="drop"
    )
  final_sum <- sum[,-c(4, 5, 8, 10, 11)]
  final_sum <- final_sum
  return(final_sum)
}

create_pitch_sums_leader <- function(data) {
  enh <- enhance_pbp(data)
  tabs <- make_tables(enh)
  pa_sum <- summarize_pa_results_leader(tabs$pa)
  shape_sum <- summarize_pitch_shape_leader(tabs$pitch)
  bbe_sum <- summarize_bbe_quality_leader(tabs$bip)
  disc_pit_sum <- summarize_pitch_discipline_gb(tabs$pitch)
  disc_sum <- summarize_pitch_discipline(tabs$pitch)
  bbe_pit_sum <- summarize_bbe_quality_pit_leader(tabs$bip)
  
  list(
    pa_sum = pa_sum, 
    shape_sum = shape_sum, 
    bbe_sum = bbe_sum, 
    bbe_pit_sum = bbe_pit_sum, 
    disc_pit_sum = disc_pit_sum, 
    disc_sum = disc_sum
  )
}


# =========================================================
# 8) LEADERBOARDS / MODEL-TYPE FUNCTIONS / PLOTTING
# =========================================================

small_date_pbp_pull <- function(dates, team, name, level_code) {
  game_ids <- map_dfr(dates, ~get_game_pks_mlb(date = .x, level_ids = level_code))
  game_ids <- game_ids %>% 
    filter(teams.home.team.name == team | teams.away.team.name == team)
  game_ids <- as.vector(game_ids$game_pk)
  pbp_data <- map_dfr(game_ids, get_pbp_mlb)
  pitches <- pbp_data %>% 
    filter(matchup.pitcher.fullName == name)
  return(pitches)
}

percentilef <- function(name, summary, col, pitch_type, value) {
  df <- get(name)[[summary]]
  if(summary == "pa_sum") {
    filter_sum <- df %>% 
      filter(PA >= 20)
  } else if(summary == "shape_sum") {
    filter_sum <- df %>% 
      filter(pitches >= 10 & pitch_type == pitch_type)
  } else if(summary == "disc_pit_sum") {
    filter_sum <- df %>% 
      filter(Pitches >= 10 & details.type.description == pitch_type)
  } else if(summary == "bbe_sum") {
    filter_sum <- df %>% 
      filter(BBE >= 15)
  } else if(summary == "bbe_pit_sum") {
    filter_sum <- df %>% 
      filter(BBE >= 15 & details.type.description == pitch_type)
  } else if(summary == "disc_sum") {
    filter_sum <- df %>% 
      filter(Pitches >= 15)
  }
  vector <- filter_sum[[col]]
  ecdf_function <- ecdf(vector)
  percentile_rank <- ecdf(vector)(value) * 100
  return(percentile_rank)
}

calculate_pitcher_diversity <- function(data, min_pitches = 100, min_usage = 0.05) {
  pitch_level <- data %>%
    filter(isPitch == TRUE) %>%
    group_by(matchup.pitcher.fullName) %>%
    filter(n() >= min_pitches) %>%
    ungroup() %>%
    rename(
      pitcher_name = matchup.pitcher.fullName,
      pitch_type = details.type.description,
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), 
      vz_f = vz0 + (az * t), 
      vx_f = vx0 + (ax * t),
      vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      haa = atan(vx_f / vy_f) * (180 / pi)
    )
  
  pitcher_arsenal <- pitch_level %>%
    group_by(pitcher_name, pitch_type) %>%
    summarize(
      n = n(),
      avg_vaa = mean(vaa, na.rm = TRUE),
      avg_haa = mean(haa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(pitcher_name) %>%
    mutate(usage = n / sum(n)) %>%
    filter(usage >= min_usage) %>%
    mutate(usage = n / sum(n)) %>% 
    ungroup()
  
  diversity_results <- pitcher_arsenal %>%
    group_by(pitcher_name) %>%
    nest() %>%
    mutate(
      total_diversity = map_dbl(data, function(df) {
        if(nrow(df) < 2) return(0)
        pairs <- df %>% 
          cross_join(df) %>%
          filter(pitch_type.x != pitch_type.y) %>%
          mutate(
            dist = sqrt((avg_vaa.x - avg_vaa.y)^2 + (avg_haa.x - avg_haa.y)^2),
            weight = usage.x * usage.y
          )
        sum(pairs$dist * pairs$weight) / sum(pairs$weight)
      })
    ) %>%
    select(pitcher_name, total_diversity) %>%
    arrange(desc(total_diversity))
  
  return(diversity_results)
}

calculate_pitcher_leaderboard <- function(data, 
                                          min_pitches = 200, 
                                          min_usage = 0.05, 
                                          w_div = 0.50, 
                                          w_cmd = 0.25, 
                                          w_vel = 0.25) {
  
  data <- enhance_pbp(data)
  pitch_level <- data %>%
    filter(isPitch == TRUE) %>%
    group_by(matchup.pitcher.fullName) %>%
    filter(n() >= min_pitches) %>%
    ungroup() %>%
    rename(
      pitcher_name = matchup.pitcher.fullName,
      pitch_type = details.type.description,
      velo = pitchData.startSpeed,
      vy0 = pitchData.coordinates.vY0, vx0 = pitchData.coordinates.vX0, vz0 = pitchData.coordinates.vZ0, 
      ay = pitchData.coordinates.aY, az = pitchData.coordinates.aZ, ax = pitchData.coordinates.aX
    ) %>%
    mutate(
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), vz_f = vz0 + (az * t), vx_f = vx0 + (ax * t),
      vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      haa = atan(vx_f / vy_f) * (180 / pi)
    )
  
  pitcher_core_stats <- data %>%
    filter(matchup.pitcher.fullName %in% unique(pitch_level$pitcher_name)) %>%
    group_by(matchup.pitcher.fullName) %>%
    summarise(
      total_pitches = n(),
      avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
      PA = sum(is_pa, na.rm = TRUE),
      BB = sum(result.eventType %in% c("walk", "intent_walk") & is_last_pitch, na.rm = TRUE),
      bb_pct = round((BB / PA) * 100, 1),
      .groups = "drop"
    ) %>%
    rename(pitcher_name = matchup.pitcher.fullName)
  
  pitcher_arsenal <- pitch_level %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(
      n = n(),
      avg_vaa = mean(vaa, na.rm = TRUE),
      avg_haa = mean(haa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(pitcher_name) %>%
    mutate(usage = n / sum(n)) %>%
    filter(usage >= min_usage) %>%
    mutate(usage = n / sum(n)) %>% 
    ungroup()
  
  leaderboard <- pitcher_arsenal %>%
    group_by(pitcher_name) %>%
    nest() %>%
    mutate(
      move_diversity = map_dbl(data, function(df) {
        if(nrow(df) < 2) return(0)
        pairs <- df %>% cross_join(df) %>% filter(pitch_type.x != pitch_type.y) %>%
          mutate(dist = sqrt((avg_vaa.x - avg_vaa.y)^2 + (avg_haa.x - avg_haa.y)^2),
                 weight = usage.x * usage.y)
        sum(pairs$dist * pairs$weight) / sum(pairs$weight)
      })
    ) %>%
    left_join(pitcher_core_stats, by = "pitcher_name") %>%
    ungroup() %>%
    mutate(
      diversity_pctl = round(percent_rank(move_diversity) * 100, 1),
      command_pctl   = round(percent_rank(desc(bb_pct)) * 100, 1),
      velo_pctl      = round(percent_rank(avg_velo) * 100, 1),
      overall_score  = round((diversity_pctl * w_div) + 
                               (command_pctl * w_cmd) + 
                               (velo_pctl * w_vel), 1)
    ) %>%
    select(
      pitcher_name, 
      overall_score, 
      move_diversity, diversity_pctl, 
      bb_pct, command_pctl, 
      avg_velo, velo_pctl
    ) %>%
    arrange(desc(overall_score))
  
  return(leaderboard)
}

plot_pitcher_outliers <- function(leaderboard_data) {
  library(ggplot2)
  library(ggrepel)
  
  ggplot(leaderboard_data, aes(x = bb_pct, y = avg_velo)) +
    geom_point(aes(color = overall_score, size = move_diversity), alpha = 0.7) +
    scale_color_viridis_c(option = "plasma", name = "Overall Grade") +
    scale_size_continuous(name = "Diversity") +
    scale_x_reverse() + 
    geom_text_repel(data = filter(leaderboard_data, overall_score > quantile(overall_score, 0.90)),
                    aes(label = pitcher_name), size = 3.5, fontface = "bold", box.padding = 0.5) +
    geom_vline(xintercept = mean(leaderboard_data$bb_pct), linetype = "dashed", alpha = 0.4) +
    geom_hline(yintercept = mean(leaderboard_data$avg_velo), linetype = "dashed", alpha = 0.4) +
    theme_minimal() +
    labs(
      title = "Pitcher Strategic Profile: Power vs. Command",
      subtitle = "Top-Right: High Velo & Low Walks | Color = Weighted Overall Grade",
      x = "Walk Rate (BB%) - Reversed",
      y = "Average Velocity (MPH)"
    )
}

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