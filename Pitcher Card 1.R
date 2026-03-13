make_tables <- function(pbp_enh) {
  # 1) all pitch rows
  tbl_pitch <- pbp_enh %>%
    dplyr::filter(isPitch %in% TRUE)
  
  # 2) one row per PA (your is_pa / is_pa_end flags should be TRUE only on last pitch)
  tbl_pa <- pbp_enh %>%
    dplyr::filter(is_pa %in% TRUE)
  
  # 3) optional batted-ball table
  tbl_bip <- pbp_enh %>%
    dplyr::filter(isPitch %in% TRUE) %>%
    dplyr::filter((details.isInPlay %in% TRUE) | (!is.na(hitData.launchSpeed)))
  
  list(
    pbp = pbp_enh,
    pitch = tbl_pitch,
    pa = tbl_pa,
    bip = tbl_bip
  )
}

tabs <- make_tables(enhance)

rate <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# usage within the current group (e.g., within pitcher, or pitcher+hand)
add_usage <- function(df, group_cols, n_col = "Pitches") {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(Usage = .data[[n_col]] / sum(.data[[n_col]], na.rm = TRUE)) %>%
    ungroup()
}

summarize_pa_results <- function(pa_tbl, group_vars) {
  pa_tbl %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H  = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      
      AVG = rate(H, AB),
      K_pct  = rate(SO, PA),
      BB_pct = rate(BB, PA),
      .groups = "drop"
    )
}

summarize_pitch_discipline <- function(pitch_tbl, group_vars) {
  pitch_tbl %>%
    group_by(across(all_of(group_vars))) %>%
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
      
      Swing_pct    = rate(Swings, Pitches),
      Whiff_pct    = rate(Whiffs, Swings),
      Contact_pct  = rate(Contact, Swings),
      
      Zone_pct     = rate(ZoneP, Pitches),
      Chase_pct    = rate(Chase, OZoneP),      # O-Swing%
      ZSwing_pct   = rate(ZSwings, ZoneP),     # Z-Swing%
      OContact_pct = rate(OContact, Chase),    # O-Contact% conditional on chases
      ZContact_pct = rate(ZContact, ZSwings),  # Z-Contact%
      .groups = "drop"
    )
}

summarize_pitch_shape <- function(pitch_tbl, group_vars) {
  pitch_tbl %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      Pitches = n(),
      Velo = mean(pitchData.startSpeed, na.rm = TRUE),
      Extension = mean(pitchData.extension, na.rm = TRUE),
      Spin = mean(pitchData.breaks.spinRate, na.rm = TRUE),
      IVB = mean(pitchData.breaks.breakVerticalInduced, na.rm = TRUE),
      HB  = mean(pitchData.breaks.breakHorizontal, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_bbe_quality <- function(bip_tbl, group_vars) {
  bip_tbl %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      HardHit_pct = rate(HardHit, BBE),
      EV = mean(ev, na.rm = TRUE),
      LA = mean(la, na.rm = TRUE),
      .groups="drop"
    )
}

build_groups <- function(base_groups,
                         by_pitch_type = FALSE,
                         by_bat_hand = FALSE,
                         pitch_type_col = "details.type.description",
                         bat_hand_col = "matchup.batSide.code") {
  g <- base_groups
  if (by_pitch_type) g <- c(g, pitch_type_col)
  if (by_bat_hand)   g <- c(g, bat_hand_col)
  g
}

make_all_summaries <- function(pa_tbl, pitch_tbl, bip_tbl = NULL,
                               # choose grouping for pitcher analyses by default:
                               who = c("pitcher","batter"),
                               by_pitch_type = TRUE,
                               by_bat_hand = FALSE) {
  who <- match.arg(who)
  
  if (who == "pitcher") {
    base_pa_groups    <- c("matchup.pitcher.fullName")
    base_pitch_groups <- c("matchup.pitcher.fullName")
  } else {
    base_pa_groups    <- c("matchup.batter.fullName")
    base_pitch_groups <- c("matchup.batter.fullName")
  }
  
  g_pitch <- build_groups(base_pitch_groups, by_pitch_type, by_bat_hand)
  g_pa    <- if (by_bat_hand) c(base_pa_groups, "matchup.batSide.code") else base_pa_groups
  
  # results (PA grain)
  results <- summarize_pa_results(pa_tbl, g_pa)
  
  # discipline + shape (pitch grain)
  discipline <- summarize_pitch_discipline(pitch_tbl, g_pitch)
  shape <- summarize_pitch_shape(pitch_tbl, g_pitch)
  
  # usage (computed on pitch+grouping, often you want usage within pitcher or pitcher+hand)
  usage_group_cols <- base_pitch_groups
  if (by_bat_hand) usage_group_cols <- c(usage_group_cols, "matchup.batSide.code")
  if (by_pitch_type) {
    # usage by pitch type within pitcher/(hand)
    usage <- discipline %>%
      select(any_of(c(usage_group_cols, "details.type.description", "Pitches"))) %>%
      distinct()
    usage <- add_usage(usage, group_cols = usage_group_cols, n_col = "Pitches")
  } else {
    usage <- discipline %>%
      select(any_of(c(usage_group_cols, "Pitches"))) %>%
      distinct() %>%
      mutate(Usage = 1)
  }
  
  # bbe quality (BBE grain)
  if (is.null(bip_tbl)) {
    bip_tbl <- pitch_tbl %>% filter(is_bbe == 1)
  }
  bbe <- summarize_bbe_quality(bip_tbl, g_pitch)
  
  # optional: merged “one table” view for pitch-type rows (nice for dashboards)
  # left_join keeps discipline rows and appends shape + bbe
  pitch_profile <- discipline %>%
    left_join(shape, by = intersect(names(discipline), names(shape))) %>%
    left_join(bbe, by = intersect(names(discipline), names(bbe))) %>%
    left_join(usage, by = intersect(names(discipline), names(usage)))
  
  list(
    results = results,
    discipline = discipline,
    shape = shape,
    bbe = bbe,
    usage = usage,
    pitch_profile = pitch_profile
  )
}


tabs <- make_tables(enhance)
pa_tbl <- tabs$pa      # should already be is_pa==1 from your make_tables()
pitch_tbl <- tabs$pitch
bip_tbl <- tabs$bip

# pitcher summaries by pitch type
SB <- make_all_summaries(
  pa_tbl, pitch_tbl, bip_tbl,
  who = "batter",
  by_pitch_type = FALSE,
  by_bat_hand = FALSE
)

# now you have:
S_results <- S$results
S$pitch_profile
S_discipline <- S$discipline
S_shape <- S$shape
S$bbe
S$usage

SB_results <- SB$results
SB$pitch_profile
SB_discipline <- SB$discipline %>% 
  filter(Pitches >= 100)
SB_shape <- SB$shape
SB$bbe
SB$usage


S_hand <- make_all_summaries(
  pa_tbl, pitch_tbl, bip_tbl,
  who = "pitcher",
  by_pitch_type = TRUE,
  by_bat_hand = TRUE
)

S_hand$pitch_profile

contour_plot <- function(data, person_name) {
  # Define Strike Zone Box
  sz_box <- data.frame(
    x = c(-0.85, 0.85, 0.85, -0.85, -0.85),
    y = c(1.6, 1.6, 3.5, 3.5, 1.6)
  )
  
  data %>%
    filter(isPitch == TRUE & matchup.pitcher.fullName == person_name) %>%
    ggplot(aes(x = pitchData.coordinates.pX, y = pitchData.coordinates.pZ)) +
    # 1. Filled Density with Normalization
    geom_density_2d_filled(
      aes(fill = after_stat(level)), 
      contour_var = "ndensity", # Normalizes each facet's colors
      bins = 10, 
      h = c(0.5, 0.5), 
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
      legend.position = "none", # Hide legend for cleaner multi-facet look
      plot.title = element_text(family = "Courier New", face = "bold")
    ) +
    labs(
      title = paste("Arsenal Heatmap:", person_name),
      subtitle = "Densities normalized per pitch type | Catcher's Perspective",
      x = "Horizontal (ft)", y = "Height (ft)"
    )
}

plot_tilt <- function(data, person_name) {
  data %>%
    filter(matchup.pitcher.fullName == person_name & isPitch == TRUE) %>%
    # Convert spinDirection to radians if it's in degrees
    mutate(tilt_rad = pitchData.breaks.spinDirection * pi / 180) %>%
    ggplot(aes(x = tilt_rad, fill = details.type.description)) +
    geom_histogram(binwidth = 0.2, alpha = 0.8) +
    coord_polar(start = 0) +
    theme_minimal() +
    scale_x_continuous(
      # Create exactly 12 breaks from 0 to (almost) 2*pi
      breaks = seq(0, 2*pi, length.out = 13)[-13], 
      # Ensure exactly 12 labels match those breaks
      labels = c("6:00", "5:00", "4:00", "3:00", "2:00", "1:00", 
                 "12:00", "11:00", "10:00", "9:00", "8:00", "7:00")
    ) +
    labs(title = "Spin Tilt Distribution", x = "", y = "", fill = "Pitch")
}

build_player_card <- function(player_pbp,
                              who = c("pitcher","batter"),
                              by_pitch_type = TRUE,
                              by_bat_hand = FALSE,
                              min_pitches = 25,
                              contour_fun, 
                              person_name) {
  who <- match.arg(who)
  
  # enhance + route into grain-correct tables
  enhanced <- my_enhance(player_pbp)
  tabs <- make_tables(enhanced)
  
  # all summary tables at once
  S <- make_all_summaries(
    pa_tbl = tabs$pa,
    pitch_tbl = tabs$pitch,
    bip_tbl = tabs$bip,
    who = who,
    by_pitch_type = by_pitch_type,
    by_bat_hand = by_bat_hand
  )
  
  # display name
  player_name <- if (who == "pitcher") unique(enhanced$matchup.pitcher.fullName)[1]
  else unique(enhanced$matchup.batter.fullName)[1]
  
  # a filtered pitch profile (reduce noise)
  pitch_profile <- S$pitch_profile
  if (by_pitch_type) pitch_profile <- pitch_profile %>% filter(Pitches >= min_pitches)
  
  # plots
  p_contours <- contour_plot(tabs$pitch, person_name)
  
  p_mix <- pitch_profile %>%
    ggplot(aes(x = reorder(`details.type.description`, Usage), y = Usage)) +
    geom_col() +
    coord_flip() +
    labs(title = "Pitch Mix (Usage)", x = NULL, y = "Usage") + 
    theme(
      plot.title = element_text( size = 16, color = "red", 
                                 face = "bold", family = "COurier New"), 
      axis.title.x = element_text( size = 8, color = "red", 
                                   face = "bold", family = "COurier New"), 
      axis.title.y = element_text( size = 8, color = "red", 
                                   face = "bold", family = "COurier New"), 
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "#380"), 
      axis.text.y = element_text( size = 8, color = "red", 
                                  face = "bold", family = "COurier New"),
      axis.text.x = element_text( size = 8, color = "red", 
                                  face = "bold", family = "COurier New")
    )
  
  p_tilt <- plot_tilt(data = player_pbp, person_name = person_name)
  
  list(
    player_name = player_name,
    who = who,
    tabs = tabs,          # raw tables (pa/pitch/bip)
    summaries = S,        # all summary tables
    pitch_profile = pitch_profile,
    plots = list(contours = p_contours, mix = p_mix, tilt = p_tilt)
  )
}

render_player_card <- function(card, digits = 3) {
  library(knitr)
  library(kableExtra)
  
  cat(sprintf("## %s\n\n", card$player_name))
  
  # 1) PA results table
  card$summaries$results %>%
    kable(format = "html", digits = digits, caption = "Results (PA-grain)") %>%
    kable_styling(full_width = FALSE) %>%
    print()
  
  cat("\n")
  
  # 2) Pitch profile table (discipline+shape+bbe+usage)
  card$pitch_profile %>%
    arrange(desc(Pitches)) %>%
    kable(format = "html", digits = digits, caption = "Pitch Profile (Pitch-grain)") %>%
    kable_styling(full_width = FALSE) %>%
    scroll_box(width = "100%", height = "350px") %>%
    print()
  
  cat("\n")
  
  # 3) Plots
  print(card$plots$mix)
  print(card$plots$contours)
  print(card$plots$tilt)
  
  invisible(card)
}

# 1) filter to ONE player first
pbp_one <- enhance %>% filter(matchup.pitcher.fullName == "Paul Skenes")

# 2) build
card <- build_player_card(
  pbp_one,
  who = "pitcher",
  by_pitch_type = TRUE,
  by_bat_hand = FALSE,
  contour_fun = your_contour_function, 
  person_name = "Paul Skenes"
)

# 3) render (best in Quarto/Rmd HTML)
render_player_card(card)
