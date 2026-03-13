# pitch avgs
write_rds(pit_sum, "pitch_type_avg_basic.rds")

# count number of each pitch thrown
pitch_totals <- enhance %>% 
  group_by(details.type.description) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(desc(n))
pitch_totals <- pitch_totals %>% 
  mutate(
    N = sum(n), 
    perc = round((n / N), 2)
  ) %>% 
  ungroup()

sum <- enhance %>% 
  filter(isPitch == TRUE) %>% 
  rename(
    release_velo = pitchData.startSpeed,
    release_spin = pitchData.breaks.spinRate, 
    hz_movement = pitchData.breaks.breakHorizontal, 
    ivb_movement = pitchData.breaks.breakVerticalInduced, 
    pitch_type = details.type.description,
    vy0 = pitchData.coordinates.vY0,
    vx0 = pitchData.coordinates.vX0,
    vz0 = pitchData.coordinates.vZ0, 
    ay = pitchData.coordinates.aY, 
    az = pitchData.coordinates.aZ, 
    ax = pitchData.coordinates.aX
  ) %>%
  mutate(
    # 1. Kinematics for pitch-by-pitch angles
    t = (-vy0 - sqrt(vy0^2 - 2 * ay * 50)) / ay,
    vy_f = vy0 + (ay * t),
    vz_f = vz0 + (az * t),
    vx_f = vx0 + (ax * t),
    # 2. Calculate approach angles
    pitch_vaa = atan(vz_f / sqrt(vx_f^2 + vy_f^2)) * (180 / pi),
    pitch_haa = atan(vx_f / vy_f) * (180 / pi)
  )

# standard deviation for approach angles
four_sum <- filter(sum, pitch_type == "Four-Seam Fastball")


calculate_pitcher_diversity <- function(data, min_pitches = 100, min_usage = 0.05) {
  
  # 1. INITIAL CLEANING & KINEMATICS
  # process everything at the pitch level first
  pitch_level <- data %>%
    filter(isPitch == TRUE) %>%
    # filter sample size
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
      # Calculating approach angles
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
    # filter out small usage pitches
    filter(usage >= min_usage) %>%
    mutate(usage = n / sum(n)) %>% 
    ungroup()
  
  # 3. CALCULATE ARSENAL DIVERSITY (Pairwise Distance)
  # We nest by pitcher to run the distance math on each unique arsenal
  diversity_results <- pitcher_arsenal %>%
    group_by(pitcher_name) %>%
    nest() %>%
    mutate(
      total_diversity = map_dbl(data, function(df) {
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

vdiverr <- calculate_pitcher_leaderboard(enhance)
vdiverr <- vdiverr %>%
  filter(!is.na(total_diversity) & total_diversity > 0) %>%
  ungroup() %>% 
  mutate(
    diversity_percentile = round(percent_rank(total_diversity) * 100, 0)
  )

target_pitchers <- c("Garrett Crochet", "Alek Jacob", "Sonny Gray", 
                     "Ranger Suárez", "Payton Tolle", "Brayan Bello", 
                     "Joahn Ovideo", "Connelly Early")
filtered_df <- vdiverr %>%
  filter(pitcher_name %in% target_pitchers)

plot_pitcher_outliers(filtered_df)
plot_pitcher_heatmaps(enhance, "Paul Skenes")


library(gt)

vdiverr %>%
  select(pitcher_name, overall_score, move_diversity, bb_pct, avg_velo) %>%
  head(20) %>%
  gt() %>%
  tab_header(title = "MLB Pitcher Arsenal Leaderboard") %>%
  # Color scale the Overall Score column
  data_color(
    columns = overall_score,
    colors = scales::col_numeric(palette = c("blue", "white", "red"), domain = c(0, 100))
  ) %>%
  cols_label(
    pitcher_name = "Pitcher",
    overall_score = "Grade",
    move_diversity = "Diversity",
    bb_pct = "BB%",
    avg_velo = "Velo"
  ) %>%
  fmt_number(columns = c(move_diversity, avg_velo), decimals = 1)
