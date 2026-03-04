pitch_summary <- function(data, playername) {
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
      spin_tilt
    )
  # create summary
  sum <- clean %>% 
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      velo = mean(release_velo, na.rm = TRUE), 
      IVB = mean(ivb_movement, na.rm = TRUE), 
      HB = mean(hz_movement, na.rm = TRUE), 
      spin = mean(release_spin, na.rm = TRUE)
      
    )
  #return summary
  return(sum)
}

#create pitcher summary
pitcher_sum <- function(data, playername) {
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
      spin_tilt = pitchData.breaks.spinDirection, 
      exit_velo = hitData.launchSpeed,
      call = details.call.description
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
      exit_velo, 
      call
    )
  #mutate column for whiff coding
  clean <- clean %>% 
    mutate(
      whiff = case_when(
        call == "Swinging Strike" ~ 1,
        call == "Swinging Strike (Blocked)" ~ 1, 
        TRUE ~ 0
      ), 
      swing = case_when(
        call == "Ball" ~ 0,
        call == "Called Strike" ~ 0, 
        call == "Ball in Dirt" ~ 0, 
        call == "Hit By Pitch" ~ 0, 
        call == "Automatic Ball - Pitcher Pitch Timer Violation" ~ 0,
        call == "Automatic Ball - Intentional" ~ 0,
        call == "Foul Bunt" ~ 0,
        call == "Automatic Strike - Batter Pitch Timer Violation" ~ 0,
        call == "Missed Bunt" ~ 0, 
        call == "Pitchout" ~ 0, 
        call == "Automatic Ball" ~ 0, 
        call == "Automatic Ball - Catcher Pitch Timer Violation"  ~ 0, 
        call == "Automatic Strike - Batter Timeout Violation" ~0, 
        TRUE ~ 1
      )
    )
  # create summary
  sum <- clean %>% 
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      hard_hit = sum(exit_velo >= 95, na.rm = TRUE),
      BBE = sum(!is.na(exit_velo)),
      hard_hit_perc = round(hard_hit / BBE, 2) * 100, 
      whiff_cnt = sum(whiff), 
      swing_cnt = sum(swing), 
      whiff_perc = round(whiff_cnt / swing_cnt, 2) * 100
    )
  #return summary
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

pitch_summary_avg <- function(data) {
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
      spin_tilt
    )
  # create summary
  sum <- clean %>% 
    group_by(pitch_type) %>% 
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

#create pitcher summary
pitcher_sum_avg <- function(data) {
  # filter by only pitches
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
      spin_tilt = pitchData.breaks.spinDirection, 
      exit_velo = hitData.launchSpeed,
      call = details.call.description
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
      exit_velo, 
      call
    )
  #mutate column for whiff coding
  clean <- clean %>% 
    mutate(
      whiff = case_when(
        call == "Swinging Strike" ~ 1,
        call == "Swinging Strike (Blocked)" ~ 1, 
        TRUE ~ 0
      ), 
      swing = case_when(
        call == "Ball" ~ 0,
        call == "Called Strike" ~ 0, 
        call == "Ball in Dirt" ~ 0, 
        call == "Hit By Pitch" ~ 0, 
        call == "Automatic Ball - Pitcher Pitch Timer Violation" ~ 0,
        call == "Automatic Ball - Intentional" ~ 0,
        call == "Foul Bunt" ~ 0,
        call == "Automatic Strike - Batter Pitch Timer Violation" ~ 0,
        call == "Missed Bunt" ~ 0, 
        call == "Pitchout" ~ 0, 
        call == "Automatic Ball" ~ 0, 
        call == "Automatic Ball - Catcher Pitch Timer Violation"  ~ 0, 
        call == "Automatic Strike - Batter Timeout Violation" ~0, 
        TRUE ~ 1
      )
    )
  # create summary
  sum <- clean %>% 
    group_by(pitch_type) %>% 
    summarize(
      pitches = n(), 
      hard_hit = sum(exit_velo >= 95, na.rm = TRUE),
      BBE = sum(!is.na(exit_velo)),
      hard_hit_perc = round(hard_hit / BBE, 2) * 100, 
      whiff_cnt = sum(whiff), 
      swing_cnt = sum(swing), 
      whiff_perc = round(whiff_cnt / swing_cnt, 2) * 100
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

#2026 mlb game pks
library(baseballr)
st_dates <- seq(as.Date("2026-02-14"), as.Date("2026-03-24"), by = "days")

st_sch <- mlb_schedule(season = 2026) %>% 
  filter(series_description == "Spring Training" & status_detailed_state == "Final")
st_pks_final <- st_sch$game_pk
st_pbp_2026 <- map_dfr(st_pks_final, ~get_pbp_mlb(game_pk = .x))
