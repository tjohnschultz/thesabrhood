ritchie <- read.csv("ritchie_logs.csv")

twhite_dates <- as.Date(c("2025-09-06", "2025-09-17", "2025-09-24"))


game_ids <- map_dfr(twhite_dates, ~get_game_pks_mlb(date = .x, level_ids = 11))
game_ids <- game_ids %>% 
  filter(teams.home.team.name == "Jacksonville Jumbo Shrimp" | teams.away.team.name == "Jacksonville Jumbo Shrimp")
game_ids <- as.vector(game_ids[,1])
# 3. Use map_dfr to get PBP data for every game ID found
pbp_data <- map_dfr(game_ids[["game_pk"]], get_pbp_mlb)

twhite_pitches <- pbp_data %>% 
  filter(matchup.pitcher.fullName == "Thomas White")

rich_sums <- create_pitch_sums(twhite_pitches)
twhite_sums <- pitch_summary(twhite_pitches, "Thomas White")

##
mlb_tbs <- make_tables(mlb_pbp_2025)
sum <- mlb_tbs$bip %>%
  group_by(matchup.pitcher.fullName, details.type.description) %>% 
  summarise(
    BBE = n(),
    HardHit = sum(is_hard_hit, na.rm = TRUE),
    HardHit_pct = round(rate(HardHit, BBE), 2), 
    barrels = sum(is_barrel, na.rm = TRUE), 
    GB = sum(is_gb, na.rm = TRUE), 
    GB_pct = round(rate(GB, BBE), 2), 
    barrel_pct = round(rate(barrels, BBE), 2), 
    EV = round(mean(ev, na.rm = TRUE), 0), 
    LA = round(mean(la, na.rm = TRUE), 0), 
    .groups="drop"
  )
final_sum <- sum[,-c(4, 6:7)] %>% 
  filter(BBE >= 30)
final_curve <- final_sum %>% 
  filter(details.type.description == "Knuckle Curve")

final_sink <- final_sum %>% 
  filter(details.type.description == "Sinker")

sum_base <- mlb_tbs$pitch %>%
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
  mutate(usage = round(pitches / sum(pitches), 2)) # Added usage for weighting

# 2. Calculate Diversity Metric (Distance from this pitch to all others)
# We use a nested map or a cross-join logic to find weighted distance
psasum <- sum_base %>%
  rowwise() %>%
  mutate(
    move_diversity = sum(
      sqrt((VAA - sum_base$VAA)^2 + (HAA - sum_base$HAA)^2) * sum_base$usage
    )
  ) %>%
  ungroup() %>%
  mutate(move_diversity = round(move_diversity, 2)) %>%
  arrange(desc(pitches))

psasum <- psasum %>% 
  filter(pitches >= 40)
psa_curve <- psasum %>% 
  filter(pitch_type == "Knuckle Curve")




## upda4te st pbp
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
#st_26 <- update_pbp(st_0311, "Spring Training")
#write_rds(st_26, "st_26.rds")

##
soriano_dates <- as.Date(c("2026-02-21", "2026-02-26", "2026-03-09"))
game_ids <- map_dfr(lewis_dates, ~get_game_pks_mlb(date = .x, level_ids = 11))
game_ids <- game_ids %>% 
  filter(teams.home.team.name == "St. Paul Saints" | teams.away.team.name == "St. Paul Saints")
game_ids <- as.vector(game_ids[,1])
# 3. Use map_dfr to get PBP data for every game ID found
pbp_data <- map_dfr(game_ids[["game_pk"]], get_pbp_mlb)

lewis_pitches <- pbp_data %>% 
  filter(matchup.pitcher.fullName == "Cory Lewis")

lewis_sums <- pitch_summary(twhite_pitches, "Cory Lewis")

##small vector function for specific player pbp
soriano_dates <- as.Date(c("2026-02-21", "2026-02-26", "2026-03-09", "2026-03-20"))

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
sorianospring <- small_date_pbp_pull(dates = soriano_dates, 
                                     team = "Los Angeles Angels", name = "José Soriano", level_code = 1)
sospringsums <- create_pitch_sums(sorianospring)
plot_pitcher_heatmaps(sorianospring, "José Soriano")

sorianospring <- st_0311 %>% 
  filter(matchup.pitcher.fullName == "José Soriano")

##cory lewis
lewis_dates <- read.csv("corylewisdates.csv")
lewis_dates <- lewis_dates[-4,2]
lewis_dates <- as.Date(lewis_dates, format = "%m/%d/%Y")

cory <- small_date_pbp_pull(dates = lewis_dates, 
                                     team = "St. Paul Saints", name = "Cory Lewis", level_code = c(11))


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
      Heart = sum(is_heart, na.rm = TRUE), 
      
      Swing_pct    = round(rate(Swings, Pitches), 02), 
      Whiff_pct    = round(rate(Whiffs, Swings), 02),
      Contact_pct  = round(rate(Contact, Swings), 02),
      
      Zone_pct     = round(rate(ZoneP, Pitches), 02),
      Chase_pct    = round(rate(Chase, OZoneP), 02),      # O-Swing%
      ZSwing_pct   = round(rate(ZSwings, ZoneP), 02),     # Z-Swing%
      OContact_pct = round(rate(OContact, Chase), 02),    # O-Contact% conditional on chases
      ZContact_pct = round(rate(ZContact, ZSwings), 02),  # Z-Contact%
      Heart_pct = round(rate(Heart, Pitches), 2), 
      .groups = "drop"
    )
  final_sum <- sum[,c(1, 3, 14:22)]
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
      .groups="drop"
    )
  final_sum <- sum[,c(3, 5, 7, 9)]
  return(final_sum)
}

summarize_bbe_quality_pit_leader <- function(bip_tbl) {
  sum <- bip_tbl %>%
    group_by(details.type.description, matchup.pitcher.fullName) %>% 
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
    )
  final_sum <- sum[,-c(3, 5)]
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
  bbe_pit_sum <- summarize_bbe_quality_pit_leader(tabs$bip)
  
  list(
    pa_sum = pa_sum, 
    shape_sum = shape_sum, 
    bbe_sum = bbe_sum, 
    bbe_pit_sum = bbe_pit_sum, 
    disc_pit_sum = disc_pit_sum
  )
}

redsox_spring_pitch <- st_0319 %>% 
  filter(fielding_team == "Boston Red Sox")

redsox_leaders <- create_pitch_sums_leader(redsox_spring_pitch)
view(redsox_leaders$disc_sum)


reidis_sum <- redsox_leaders$shape_sum %>% 
  filter(pitcher_name == "Reidis Sena")
reidis_pit <- redsox_spring_pitch %>% 
  filter(matchup.pitcher.fullName == "Reidis Sena")
plot_pitcher_heatmaps(reidis_pit, "Reidis Sena")

keller_pit <- redsox_spring_pitch %>% 
  filter(matchup.pitcher.fullName == "Kyle Keller")
plot_pitcher_heatmaps(keller_pit, "Kyle Keller")



redsox_spring_bat <- st_0319 %>% 
  filter(batting_team == "Boston Red Sox")
redsox_bat <- create_bat_sums_leader(redsox_spring_bat)
view(redsox_bat$pa_sum)
view(redsox_bat$bbe_qual)
view(redsox_bat$disc)
view(redsox_bat$bbe_heart)


mlb_bat_leaders <- create_bat_sums_leader(mlb_pbp_2025)
redsox25batdata <- mlb_pbp_2025 %>% 
  filter(batting_team == "Boston Red Sox")
redsox25batleaders <- create_bat_sums_leader(redsox25batdata)


mlbsum_pit <- create_pitch_sums_leader(mlb_pbp_2025)
#view(mlbsum_pit$pa_sum)
view(mlbsum_pit$pa_sum[mlbsum_pit$pa_sum$PA >= 20,])
#view(mlbsum_pit$shape_sum)
view(mlbsum_pit$shape_sum[mlbsum_pit$shape_sum$pitches >= 20,])
view(mlbsum_pit$bbe_sum)
view(mlbsum_pit$bbe_pit_sum)
view(mlbsum_pit$disc_pit_sum)
view(mlbsum_pit$disc_sum)

#write_rds(mlbsum_pit$pa_sum, "mlbpa_sum.rds")
#write_rds(mlbsum_pit$shape_sum, "mlbshape_sum.rds")
#write_rds(mlbsum_pit$bbe_sum, "mlbbbe_sum.rds")
#write_rds(mlbsum_pit$bbe_pit_sum, "mlbbbe_pit_sum.rds")
#write_rds(mlbsum_pit$disc_pit_sum, "mlbdisc_pit_sum.rds")
#write_rds(mlbsum_pit$disc_sum, "mlbdisc_sum.rds")


#####====Make a Percentile ranking function====####

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


percentilef("mlbsum_pit", "shape_sum", "VAA", "Four-Seam Fastball", -5.86)
percentilef("mlbsum_pit", "bbe_sum", "pull_fbpct", "", .2)
percentilef("mlbsum_pit", "bbe_pit_sum", "pull_fbpct", "Sinker", .22)
percentilef("mlbsum_pit", "disc_pit_sum", "Zone_pct", "Cutter", .63)
percentilef("mlbsum_pit", "disc_pit_sum", "Chase_pct", "Sweeper", .33)
percentilef("mlbsum_pit", "disc_pit_sum", "Heart_pct", "Curveball", .13)
so_sums_left$pa_sum
so_sums_right$pa_sum
so_sums_left$bbe_sum
so_sums_right$bbe_sum

#change hr to fb rate calculation, numerator should be HR on FB not all HRs
#needs enhance is fbhr = as.integer(is_fb == 1 & is_hr == 1)

#add to enhance inside mutate
#count = paste(countstartballs, "-", countstartstrikes)

usage_by_count<- function(pitch_tbl) {
  pitch_tbl <- enhance_pbp(pitch_tbl)
  pitch_tbl <- pitch_tbl %>% 
    filter(count.balls.start < 4 & count.strikes.start < 3)
  sum <- pitch_tbl %>%
    group_by(count, details.type.description) %>%
    summarise(
      pitches = n(),
      .groups = "drop"
    ) %>%
    group_by(count) %>%
    mutate(
      usage = round(pitches / sum(pitches), 2)
    ) %>%
    ungroup() %>%
    arrange(count, desc(usage))
  
  return(sum)
}
headpbp <- head(mlb_tbs$pitch, 20000)
view(usage_by_count(headpbp))


#st_rj <- st_26 %>% 
#  filter(matchup.pitcher.fullName == "Ryan Johnson")
#write_rds(st_rj,file = file.path(dl_path, "st_rj.rds"))
#rj_25 <- mlb_pbp_2025 %>% 
#  filter(matchup.pitcher.fullName == "Ryan Johnson")
#write_rds(rj_25,file = file.path(dl_path, "rj_25.rds"))

rj_sums25 <- create_pitch_sums(rj_25)
rjstsums <- create_pitch_sums(st_rj)

rj_sums25$pa_sum
rj_sums25$disc_sum
rj_sums25$shape_sum
rj_sums25$bbe_sum
rj_sums25$bbe_pit_sum
rj_sums25$disc_pit_sum


#grod <- st_0318 %>% 
# filter(matchup.pitcher.fullName == "Grayson Rodriguez" & isPitch == TRUE)
#write_rds(grod,file = file.path(dl_path, "grod25.rds"))
grod <- read_rds(here("data", "grod25.rds"))
grod_sums <- create_pitch_sums(grod)
grod_sums$disc_pit_sum
grod_sums$bbe_sum
grod_sums$shape_sum
plot_pitcher_heatmaps(grod, "Grayson Rodriguez")