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
      # find the last pitches
      is_last_pitch = as.integer(pitchNumber == max(pitchNumber, na.rm = TRUE)),
      
      # is swing / is whiff
      is_swing = as.integer(details.call.description %in% swing_events),
      is_whiff = as.integer(details.call.description %in% whiff_events),
      
      # contact is a swing that isn't a whiff
      is_contact = as.integer(is_swing == 1 & is_whiff == 0),
      
      # identifier for plate appearences and at bats
      is_pa = as.integer(is_last_pitch == 1 & !(result.eventType %in% baserunning_events)),
      is_at_bat = as.integer(is_last_pitch == 1 &
                               !(result.eventType %in% non_pa_events) &
                               !(result.eventType %in% baserunning_events)),
      
      # outcomes on PA rows
      is_bb = as.integer(is_pa == 1 & (result.eventType %in% c("walk", "intent_walk"))),
      is_k  = as.integer(is_pa == 1 & str_detect(result.eventType, "^strikeout")),
      
      # total bases / hits on PA rows
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
      
      # batted-ball event (BBE) / EV / hard-hit
      is_bbe = as.integer(isPitch %in% TRUE & ((details.isInPlay %in% TRUE) | !is.na(hitData.launchSpeed))),
      ev = suppressWarnings(as.numeric(hitData.launchSpeed)),
      la = suppressWarnings(as.numeric(hitData.launchAngle)),
      is_hard_hit = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 95),
      is_barrel = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 98 & la >= 24 & la <= 33), 
      is_fb = as.integer(is_bbe == 1 & !is.na(ev) & la >= 25 & la <= 50),
      is_hr = as.integer(!is.na(total_bases) & total_bases == 4), 
      
      # zone / discipline
      zone = suppressWarnings(as.integer(pitchData.zone)),
      is_zone  = as.integer(isPitch %in% TRUE & !is.na(zone) & zone >= 1 & zone <= 9),
      is_ozone = as.integer(isPitch %in% TRUE & !is.na(zone) & zone > 9),
      
      # chase and zone swing
      is_chase   = as.integer(is_swing == 1 & is_ozone == 1),  # O-Swing
      is_z_swing = as.integer(is_swing == 1 & is_zone == 1),   # Z-Swing
      
      # contact split by zone
      is_o_contact = as.integer(is_contact == 1 & is_ozone == 1),
      is_z_contact = as.integer(is_contact == 1 & is_zone == 1)
    ) %>%
    ungroup()
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

pitch_summary <- function(data, playername) {
  # 1. Initial Processing and Summarization
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
    mutate(usage = pitches / sum(pitches)) # Added usage for weighting
  
  # 2. Calculate Diversity Metric (Distance from this pitch to all others)
  # We use a nested map or a cross-join logic to find weighted distance
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
  # 1. Initial Processing and Summarization
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
    mutate(usage = pitches / sum(pitches)) # Added usage for weighting
  
  # 2. Calculate Diversity Metric (Distance from this pitch to all others)
  # We use a nested map or a cross-join logic to find weighted distance
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
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2), 
      Contact_pct  = round(rate(Contact, Swings), 2), 
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2), 
      Chase_pct    = round(rate(Chase, OZoneP), 2),       # O-Swing%
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),      # Z-Swing%
      OContact_pct = round(rate(OContact, Chase), 2),     # O-Contact% conditional on chases
      ZContact_pct = round(rate(ZContact, ZSwings), 2),   # Z-Contact%
      .groups = "drop"
    )
  final_sum <- sum[,c(1, 12:18)]
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
      
      Swing_pct    = round(rate(Swings, Pitches), 02), 
      Whiff_pct    = round(rate(Whiffs, Swings), 02),
      Contact_pct  = round(rate(Contact, Swings), 02),
      
      Zone_pct     = round(rate(ZoneP, Pitches), 02),
      Chase_pct    = round(rate(Chase, OZoneP), 02),      # O-Swing%
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 02),     # Z-Swing%
      OContact_pct = round(rate(OContact, Chase), 02),    # O-Contact% conditional on chases
      ZContact_pct = round(rate(ZContact, ZSwings), 02),  # Z-Contact%
      .groups = "drop"
    )
  final_sum <- sum[,c(1, 3, 13:20)]
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
    mutate(usage = round(pitches / sum(pitches), 2)) # Added usage for weighting
  
  # 2. Calculate Diversity Metric (Distance from this pitch to all others)
  # We use a nested map or a cross-join logic to find weighted distance
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
      .groups="drop"
    )
  final_sum <- sum[,c(3, 5, 7)]
  return(final_sum)
}

summarize_bbe_quality_pit <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(details.type.description) %>% 
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      .groups="drop"
    )
  final_sum <- sum[,-c(3, 5)]
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

#quantiles for aps_indy
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

#PITCH SUMMARY BY MONTH
pitch_summary_date <- function(data, playername) {
  # filter by specific pitcher & only pitches
  data <- data %>% 
    filter(matchup.pitcher.fullName == playername & isPitch == TRUE) 
  #rename necessary columns
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
  # select necessary data
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
  # create summary
  sum <- clean %>% 
    group_by(pitch_type, month_name, "month") %>% 
  summarize(
    pitches = n(), 
    velo = mean(release_velo, rm.na = TRUE), 
    IVB = mean(ivb_movement, rm.na = TRUE), 
    HB = mean(hz_movement, rm.na = TRUE), 
    spin = mean(release_spin, rm.na = TRUE)
    
  )
#return summary
return(sum)
}

# SPLIT DATA INTO STADIUM BY STADIUM GROUP
pitch_summary_stadiums <- function(data) {
  # filter by specific pitcher & only pitches
  data <- data %>% 
    filter(isPitch == TRUE) 
  #rename necessary columns
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
  # select necessary data
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
  # create summary
  sum <- clean %>% 
    group_by(pitch_type, home_team) %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, na.rm = TRUE), 
      IVB = mean(ivb_movement, na.rm = TRUE), 
      HB = mean(abs(hz_movement), na.rm = TRUE), 
      spin = mean(release_spin, na.rm = TRUE)
    )
  #return summary
  return(sum)
}

# SPLIT DATA INTO STADIUM BY STADIUM GROUP / PITCHER
pitch_summary_stadiums_pitcher <- function(data, playername) {
  # filter by specific pitcher & only pitches
  data <- data %>% 
    filter(isPitch == TRUE & matchup.pitcher.fullName == playername) 
  #rename necessary columns
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
  # select necessary data
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
  # create summary
  sum <- clean %>% 
    group_by(pitch_type, home_team) %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, na.rm = TRUE), 
      IVB = mean(ivb_movement, na.rm = TRUE), 
      HB = mean(abs(hz_movement), na.rm = TRUE), 
      spin = mean(release_spin, na.rm = TRUE)
    )
  #return summary
  return(sum)
}

#swung at pitches plot by whiff and no whiff
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

# create vector of rolling batting average from pbp data from ENHANCED DATA
rolling_batting_vectors <- function(pbp_data) {

}

# filter by function
filter_by <- function(pitcher = NA, batter = NA, team = NA){
  
}

# pitcher arsenal approach angle diversity
calculate_pitcher_diversity <- function(data, min_pitches = 100, min_usage = 0.05) {
  
  # 1. INITIAL CLEANING & KINEMATICS
  # We process everything at the pitch level first
  pitch_level <- data %>%
    filter(isPitch == TRUE) %>%
    # Filter for volume to remove position players/small samples
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
      # Kinematics for Approach Angles
      t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
      vy_f = vy0 + (ay * t), 
      vz_f = vz0 + (az * t), 
      vx_f = vx0 + (ax * t),
      vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
      haa = atan(vx_f / vy_f) * (180 / pi)
    )
  
  # 2. SUMMARIZE BY PITCH TYPE PER PITCHER
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
    # Filter out "junk" pitches (e.g., throwaway pitches < 5% usage)
    filter(usage >= min_usage) %>%
    # Recalculate usage after filtering junk
    mutate(usage = n / sum(n)) %>% 
    ungroup()
  
  # 3. CALCULATE ARSENAL DIVERSITY (Pairwise Distance)
  # We nest by pitcher to run the distance math on each unique arsenal
  diversity_results <- pitcher_arsenal %>%
    group_by(pitcher_name) %>%
    nest() %>%
    mutate(
      total_diversity = map_dbl(data, function(df) {
        # Need at least 2 pitch types to have "diversity"
        if(nrow(df) < 2) return(0)
        
        # Cross join the pitcher's pitches to create pairs (i and j)
        pairs <- df %>% 
          cross_join(df) %>%
          filter(pitch_type.x != pitch_type.y) %>%
          mutate(
            # Euclidean distance between HAA and VAA coordinates
            dist = sqrt((avg_vaa.x - avg_vaa.y)^2 + (avg_haa.x - avg_haa.y)^2),
            # Weight is the product of how often both pitches are thrown
            weight = usage.x * usage.y
          )
        
        # Final weighted average distance
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
  
  # 1. PITCH-LEVEL KINEMATICS & DATA CLEANING
  # Filter for volume and calculate approach angles
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
  
  # 2. PITCHER-LEVEL SUMMARY (Velo & BB%)
  # We calculate the "Core Stats" per pitcher here
  pitcher_core_stats <- data %>%
    filter(matchup.pitcher.fullName %in% unique(pitch_level$pitcher_name)) %>%
    group_by(matchup.pitcher.fullName) %>%
    summarise(
      total_pitches = n(),
      avg_velo = mean(pitchData.startSpeed, na.rm = TRUE),
      # BB% calculation (Walks / Total Plate Appearances)
      PA = sum(is_pa, na.rm = TRUE),
      BB = sum(result.eventType %in% c("walk", "intent_walk") & is_last_pitch, na.rm = TRUE),
      bb_pct = round((BB / PA) * 100, 1),
      .groups = "drop"
    ) %>%
    rename(pitcher_name = matchup.pitcher.fullName)
  
  # 3. ARSENAL-LEVEL SUMMARY (For Diversity Math)
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
    mutate(usage = n / sum(n)) %>% # Re-weight after filter
    ungroup()
  
  # Final processing block
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
      
      # Calculate Overall Score using the arguments
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
    # Color mapped to Overall Grade; size can be fixed or mapped to Diversity
    geom_point(aes(color = overall_score, size = move_diversity), alpha = 0.7) +
    scale_color_viridis_c(option = "plasma", name = "Overall Grade") +
    scale_size_continuous(name = "Diversity") +
    # REVERSE X-AXIS: Best command (lowest BB%) on the right
    scale_x_reverse() + 
    # Label the top 10% Overall Score
    geom_text_repel(data = filter(leaderboard_data, overall_score > quantile(overall_score, 0.90)),
                    aes(label = pitcher_name), size = 3.5, fontface = "bold", box.padding = 0.5) +
    # Average lines to create Quadrants
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
  # Define Strike Zone Box
  sz_box <- data.frame(
    x = c(-0.85, 0.85, 0.85, -0.85, -0.85),
    y = c(1.6, 1.6, 3.5, 3.5, 1.6)
  )
  
  data %>%
    filter(isPitch == TRUE) %>%
    ggplot(aes(x = pitchData.coordinates.pX, y = pitchData.coordinates.pZ)) +
    # 1. Filled Density with Normalization
    geom_density_2d_filled(
      aes(fill = after_stat(level)), 
      contour_var = "ndensity", # Normalizes each facet's colors
      bins = 8, 
      alpha = 0.6
    ) +
    # 2. Add White Contour Lines for "Topographic" look
    geom_density_2d(color = "white", alpha = 0.3, size = 0.2) +
    # 3. Enhanced Strike Zone
    geom_path(data = sz_box, aes(x, y), color = "black", size = 1.2) +
    # 4. Professional Styling
    scale_fill_viridis_d(option = "rocket", direction = -1) + # "Rocket" is a great heatmap palette
    facet_wrap(~details.type.description) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none" # Hide legend for cleaner multi-facet look
    ) +
    labs(
      title = paste("Arsenal Heatmap:", pitcher_name),
      subtitle = "Densities normalized per pitch type | Catcher's Perspective",
      x = "Horizontal (ft)", y = "Height (ft)"
    )
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

#batter leaderboard
summarize_pa_results_bat_lead <- function(pa_tbl) {
  sum <- pa_tbl %>% 
    group_by(matchup.batter.fullName) %>% 
    summarise(
      #hidden data
      total_bases = sum(total_bases, na.rm = TRUE), 
      #end hidden data
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
      #hidden data
      BBE = n(),
      FB = sum(is_fb, na.rm = TRUE), 
      HardHit = sum(is_hard_hit, na.rm = TRUE), 
      barrels = sum(is_barrel, na.rm = TRUE), 
      HR = sum(is_hr, na.rm = TRUE),
      #end hidden data
      HardHit_pct = round(rate(HardHit, BBE), 2), 
      barrel_pct = round(rate(barrels, BBE), 2), 
      EV = round(mean(ev, na.rm = TRUE), 0), 
      LA = round(mean(la, na.rm = TRUE), 0), 
      HRFB = round(rate(HR, FB), 2),
      .groups="drop"
    ) %>% 
    filter(BBE >= (max(BBE) * 0.15))
  final_sum <- sum[,-c(2:6)]
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
      
      Swing_pct    = round(rate(Swings, Pitches), 2), 
      Whiff_pct    = round(rate(Whiffs, Swings), 2), 
      Contact_pct  = round(rate(Contact, Swings), 2), 
      
      Zone_pct     = round(rate(ZoneP, Pitches), 2), 
      Chase_pct    = round(rate(Chase, OZoneP), 2),       # O-Swing%
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 2),      # Z-Swing%
      OContact_pct = round(rate(OContact, Chase), 2),     # O-Contact% conditional on chases
      ZContact_pct = round(rate(ZContact, ZSwings), 2),   # Z-Contact% 
      barrel_swing = round(rate(barrels, Swings), 2), 
      .groups = "drop"
    ) %>% 
    filter(Pitches >= (max(Pitches) * 0.25))
  final_sum <- sum[,-c(3:12)]
  return(final_sum)
}

batter_leaders <- function(data) {
  enh <- enhance_pbp(data)
  tabs <- make_tables(enh)
  pa_sum <- summarize_pa_results_bat_lead(tabs$pa)
  disc_sum <- summarize_pitch_discipline_bat_lead(tabs$pitch)
  bbe_sum <- summarize_bbe_quality_bat_lead(tabs$bip)
  #  disc_pit_sum <- summarize_pitch_discipline_gb(tabs$pitch)
  #  bbe_pit_sum <- summarize_bbe_quality_pit(tabs$bip)
  
  list(
    pa_sum = pa_sum, 
    disc_sum = disc_sum, 
    #   shape_sum = shape_sum, 
    bbe_sum = bbe_sum 
    #   bbe_pit_sum = bbe_pit_sum, 
    #   disc_pit_sum = disc_pit_sum
  )
}
