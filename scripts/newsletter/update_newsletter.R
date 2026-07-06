# scripts/newsletter/update_newsletter.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(glue)
  library(lubridate)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(readr)
  library(scales)
  library(forcats)
  library(baseballr)
})

# =========================================================
# 0. Paths and setup
# =========================================================

newsletter_dir <- file.path("data", "newsletter_data")
archive_dir    <- file.path(newsletter_dir, "archive")

img_root               <- file.path("images", "newsletter")
chart_dir              <- file.path(img_root, "charts")
hitter_trend_plot_dir  <- file.path(img_root, "hitter_trends")
pitcher_trend_plot_dir <- file.path(img_root, "pitcher_trends")

dir.create(newsletter_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(hitter_trend_plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pitcher_trend_plot_dir, recursive = TRUE, showWarnings = FALSE)

mlb_pbp_path          <- file.path(newsletter_dir, "mlb_pbp.rds")
mlb_pbp_enhanced_path <- file.path(newsletter_dir, "mlb_pbp_enhanced.rds")
daily_path            <- file.path(newsletter_dir, "newsletter_today.rds")

today <- Sys.Date()

# =========================================================
# 1. Source only the core R functions needed here
# =========================================================

source_project_functions <- function() {
  core_files <- c(
    "00_setup_packages.R",
    "01_core_helpers.R",
    "02_data_loading_views.R",
    "03_player_summaries.R",
    "04_expected_contact.R",
    "05_rolling_trends.R",
    "06_split_comparisons.R"
  )
  
  existing_core <- file.path("R", core_files)
  existing_core <- existing_core[file.exists(existing_core)]
  
  if (length(existing_core) == 0) {
    stop("No core R files found in R/. Check your R function folder.")
  }
  
  invisible(lapply(existing_core, source))
}

source_project_functions()

# =========================================================
# 2. Utility functions
# =========================================================

slugify <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "-") %>%
    stringr::str_replace_all("(^-|-$)", "")
}

save_newsletter_plot <- function(plot_obj, filename, folder, width = 7, height = 4, dpi = 180) {
  if (is.null(plot_obj)) {
    return(NA_character_)
  }
  
  abs_path <- file.path(folder, filename)
  
  ggsave(
    filename = abs_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  
  gsub("\\\\", "/", abs_path)
}

pick_first_col <- function(data, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(data)][1]
  
  if (is.na(hit) && required) {
    stop("None of these columns were found: ", paste(candidates, collapse = ", "))
  }
  
  hit
}

coalesce_existing_cols <- function(data, candidates, default = NA_character_) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(default, nrow(data)))
  }
  
  out <- rep(NA_character_, nrow(data))
  
  for (col in existing) {
    value <- as.character(data[[col]])
    fill <- is.na(out) | out == ""
    out[fill] <- value[fill]
  }
  
  out
}

normalize_event_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "_")
}

roll_sum <- function(x, n) {
  x <- tidyr::replace_na(as.numeric(x), 0)
  out <- numeric(length(x))
  
  for (i in seq_along(x)) {
    start_i <- max(1, i - n + 1)
    out[i] <- sum(x[start_i:i], na.rm = TRUE)
  }
  
  out
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

outs_to_ip <- function(outs) {
  outs <- suppressWarnings(as.integer(outs))
  
  ifelse(
    is.na(outs),
    NA_real_,
    floor(outs / 3) + (outs %% 3) / 10
  )
}

format_newsletter_value <- function(x, stat) {
  if (length(x) == 0 || is.na(x)) return("NA")
  
  pct_stats <- c(
    "K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
    "Contact_pct", "HardHit_pct", "barrel_pct",
    "Swing_pct", "Zone_pct", "recent_share", "base_share"
  )
  
  if (stat %in% pct_stats) {
    if (abs(x) <= 1) {
      return(scales::percent(x, accuracy = 0.1))
    } else {
      return(paste0(round(x, 1), "%"))
    }
  }
  
  as.character(round(x, 3))
}

make_trender_blurb <- function(row) {
  movement_word <- ifelse(row$z_change >= 0, "upward", "downward")
  
  glue(
    "{row$player} has one of the biggest {movement_word} movements in {row$stat} on today’s board. ",
    "His recent mark is {format_newsletter_value(row$recent_value, row$stat)} compared with a baseline of ",
    "{format_newsletter_value(row$baseline_value, row$stat)}, good for a {round(row$z_change, 2)} z-score change across the qualified hitter pool."
  )
}

make_leaderboard <- function(summary_tbl, arrange_stat, keep_cols, min_pa = 50, n = 15) {
  if (!arrange_stat %in% names(summary_tbl)) {
    return(tibble(note = glue("Missing column: {arrange_stat}")))
  }
  
  summary_tbl %>%
    filter(PA >= min_pa) %>%
    arrange(desc(.data[[arrange_stat]])) %>%
    select(any_of(keep_cols)) %>%
    slice_head(n = n)
}

# =========================================================
# 3. Efficient raw/enhanced PBP update
# =========================================================

update_raw_and_enhanced_pbp_files <- function(
    raw_path,
    enhanced_path,
    szn_type = "Regular Season",
    start_date = "2026-02-20",
    end_date = Sys.Date(),
    level_ids = 1,
    schedule_lookback_days = 7
) {
  if (!file.exists(raw_path)) {
    stop("Missing raw PBP file: ", raw_path)
  }
  
  raw_pbp <- readRDS(raw_path)
  
  if (!"game_pk" %in% names(raw_pbp)) {
    stop("Raw PBP must include game_pk.")
  }
  
  if (file.exists(enhanced_path)) {
    message("Reading enhanced PBP: ", enhanced_path)
    enhanced_pbp <- readRDS(enhanced_path)
  } else {
    message("Enhanced PBP does not exist yet. Creating it once from full raw PBP...")
    enhanced_pbp <- enhance_pbp(raw_pbp)
    saveRDS(enhanced_pbp, enhanced_path)
  }
  
  existing_pks <- unique(as.vector(enhanced_pbp$game_pk))
  
  latest_existing_date <- if ("game_date" %in% names(enhanced_pbp)) {
    suppressWarnings(max(as.Date(enhanced_pbp$game_date), na.rm = TRUE))
  } else {
    as.Date(start_date)
  }
  
  if (is.infinite(latest_existing_date) || is.na(latest_existing_date)) {
    latest_existing_date <- as.Date(start_date)
  }
  
  schedule_start <- max(
    as.Date(start_date),
    latest_existing_date - schedule_lookback_days
  )
  
  message("Checking schedule from ", schedule_start, " to ", as.Date(end_date), "...")
  
  dates <- seq(
    from = as.Date(schedule_start),
    to = as.Date(end_date),
    by = "day"
  )
  
  scheds <- purrr::map_dfr(
    dates,
    ~ baseballr::get_game_pks_mlb(date = .x, level_ids = level_ids)
  )
  
  if (nrow(scheds) == 0) {
    message("No schedule rows returned. Using existing enhanced PBP.")
    return(enhanced_pbp)
  }
  
  scheds <- scheds %>%
    filter(
      seriesDescription == szn_type,
      status.codedGameState == "F"
    )
  
  new_pks <- unique(as.vector(scheds$game_pk))
  pks_diff <- setdiff(new_pks, existing_pks)
  
  if (length(pks_diff) == 0) {
    message("No new completed games found. Using existing enhanced PBP.")
    return(enhanced_pbp)
  }
  
  message("Pulling ", length(pks_diff), " new completed games...")
  
  new_raw <- purrr::map_dfr(
    pks_diff,
    ~ baseballr::get_pbp_mlb(game_pk = .x)
  )
  
  if (nrow(new_raw) == 0) {
    message("No new PBP returned. Using existing enhanced PBP.")
    return(enhanced_pbp)
  }
  
  message("Enhancing only new PBP rows...")
  
  new_enhanced <- enhance_pbp(new_raw)
  
  raw_updated <- bind_rows(raw_pbp, new_raw) %>%
    distinct(game_pk, atBatIndex, pitchNumber, result.eventType, .keep_all = TRUE)
  
  enhanced_updated <- bind_rows(enhanced_pbp, new_enhanced) %>%
    distinct(game_pk, atBatIndex, pitchNumber, result.eventType, .keep_all = TRUE)
  
  saveRDS(raw_updated, raw_path)
  saveRDS(enhanced_updated, enhanced_path)
  
  message("Saved updated raw PBP: ", raw_path)
  message("Saved updated enhanced PBP: ", enhanced_path)
  
  enhanced_updated
}

message("Updating raw/enhanced MLB PBP workflow...")

pbp_data <- update_raw_and_enhanced_pbp_files(
  raw_path = mlb_pbp_path,
  enhanced_path = mlb_pbp_enhanced_path,
  szn_type = "Regular Season",
  start_date = "2026-02-20",
  end_date = today,
  level_ids = 1,
  schedule_lookback_days = 7
)

report_date <- if ("game_date" %in% names(pbp_data)) {
  max(as.Date(pbp_data$game_date), na.rm = TRUE)
} else {
  today
}

weekday_name <- weekdays(report_date)

message("Report date: ", report_date)

message("Preparing PBP views from enhanced data...")

views <- prepare_pbp_views(
  pbp = pbp_data,
  enhanced = TRUE
)

# =========================================================
# 4. Hitter summaries and leaderboards
# =========================================================

message("Building hitter summaries...")

mlb_hitter_summary <- summarize_overall_batter(
  views = views,
  level = "player",
  team_col = "batting_team"
)

ops_leaderboard <- make_leaderboard(
  summary_tbl = mlb_hitter_summary,
  arrange_stat = "OPS",
  keep_cols = c("name", "team", "PA", "AVG", "OBP", "SLG", "OPS", "HR", "K_pct", "BB_pct"),
  min_pa = 50,
  n = 15
)

power_leaderboard <- make_leaderboard(
  summary_tbl = mlb_hitter_summary,
  arrange_stat = "SLG",
  keep_cols = c("name", "team", "PA", "SLG", "OPS", "HR", "HardHit_pct", "barrel_pct"),
  min_pa = 50,
  n = 10
)

discipline_leaderboard <- make_leaderboard(
  summary_tbl = mlb_hitter_summary,
  arrange_stat = "BB_pct",
  keep_cols = c("name", "team", "PA", "BB_pct", "K_pct", "OBP", "OPS"),
  min_pa = 50,
  n = 10
)

# =========================================================
# 5. Hitter movement board
# =========================================================

calc_hitter_change_zscores <- function(
    views,
    report_date,
    season_start = "2026-02-20",
    recent_days = 7,
    stats = c(
      "OPS", "SLG", "OBP",
      "K_pct", "BB_pct",
      "Whiff_pct", "Chase_pct",
      "HardHit_pct", "barrel_pct"
    ),
    min_recent_pa = 10,
    min_base_pa = 30
) {
  recent_start <- as.Date(report_date) - recent_days + 1
  baseline_start <- as.Date(season_start)
  baseline_end <- recent_start - 1
  
  recent_raw <- summarize_overall_batter(
    views = views,
    level = "player",
    team_col = "batting_team",
    start_date = recent_start,
    end_date = report_date
  )
  
  baseline_raw <- summarize_overall_batter(
    views = views,
    level = "player",
    team_col = "batting_team",
    start_date = baseline_start,
    end_date = baseline_end
  )
  
  id_col <- if ("matchup.batter.id" %in% names(recent_raw)) {
    "matchup.batter.id"
  } else {
    stop("Could not find matchup.batter.id in hitter summary.")
  }
  
  available_stats <- stats %>%
    keep(~ .x %in% names(recent_raw) && .x %in% names(baseline_raw))
  
  if (length(available_stats) == 0) {
    stop("None of the requested hitter trender stats are available.")
  }
  
  recent <- recent_raw %>%
    filter(PA >= min_recent_pa) %>%
    select(
      player_id = all_of(id_col),
      name,
      team,
      PA_recent = PA,
      all_of(available_stats)
    ) %>%
    rename_with(~ paste0(.x, "_recent"), all_of(available_stats))
  
  baseline <- baseline_raw %>%
    filter(PA >= min_base_pa) %>%
    select(
      player_id = all_of(id_col),
      name,
      team,
      PA_base = PA,
      all_of(available_stats)
    ) %>%
    rename_with(~ paste0(.x, "_base"), all_of(available_stats))
  
  joined <- recent %>%
    inner_join(
      baseline,
      by = c("player_id", "name", "team")
    )
  
  map_dfr(available_stats, function(stat) {
    recent_col <- paste0(stat, "_recent")
    base_col   <- paste0(stat, "_base")
    
    joined %>%
      transmute(
        player_id,
        player = name,
        team,
        stat,
        PA_recent,
        PA_base,
        recent_value = .data[[recent_col]],
        baseline_value = .data[[base_col]],
        change = recent_value - baseline_value
      )
  }) %>%
    filter(!is.na(change), is.finite(change)) %>%
    group_by(stat) %>%
    mutate(
      league_change_mean = mean(change, na.rm = TRUE),
      league_change_sd = sd(change, na.rm = TRUE),
      z_change = if_else(
        league_change_sd > 0,
        (change - league_change_mean) / league_change_sd,
        NA_real_
      ),
      direction = case_when(
        z_change > 0 ~ "Riser",
        z_change < 0 ~ "Faller",
        TRUE ~ "Neutral"
      )
    ) %>%
    ungroup() %>%
    arrange(desc(abs(z_change)))
}

message("Calculating hitter movement z-scores...")

mlb_hitter_trenders <- calc_hitter_change_zscores(
  views = views,
  report_date = report_date,
  season_start = "2026-02-20",
  recent_days = 7
)

top_riser_row <- mlb_hitter_trenders %>%
  filter(direction == "Riser") %>%
  arrange(desc(z_change)) %>%
  slice(1)

top_faller_row <- mlb_hitter_trenders %>%
  filter(direction == "Faller") %>%
  arrange(z_change) %>%
  slice(1)

top_riser <- list(
  name = top_riser_row$player[[1]],
  team = top_riser_row$team[[1]],
  stat = top_riser_row$stat[[1]],
  z_change = top_riser_row$z_change[[1]],
  blurb = make_trender_blurb(top_riser_row[1, ])
)

top_faller <- list(
  name = top_faller_row$player[[1]],
  team = top_faller_row$team[[1]],
  stat = top_faller_row$stat[[1]],
  z_change = top_faller_row$z_change[[1]],
  blurb = make_trender_blurb(top_faller_row[1, ])
)

quick_hit_rows <- mlb_hitter_trenders %>%
  arrange(desc(abs(z_change))) %>%
  slice_head(n = 6)

quick_hits <- map(seq_len(nrow(quick_hit_rows)), function(i) {
  row <- quick_hit_rows[i, ]
  
  list(
    tag = row$direction,
    title = glue("{row$player}: {row$stat} movement"),
    blurb = make_trender_blurb(row)
  )
})

# =========================================================
# 6. Chart of the day
# =========================================================

message("Saving chart of the day...")

chart_stat <- if ("OPS" %in% unique(mlb_hitter_trenders$stat)) {
  "OPS"
} else {
  unique(mlb_hitter_trenders$stat)[1]
}

chart_tbl <- mlb_hitter_trenders %>%
  filter(stat == chart_stat) %>%
  arrange(desc(abs(z_change))) %>%
  slice_head(n = 10) %>%
  mutate(player = forcats::fct_reorder(player, z_change))

chart_of_day_plot <- if (nrow(chart_tbl) > 0) {
  ggplot(chart_tbl, aes(x = player, y = z_change)) +
    geom_col() +
    coord_flip() +
    labs(
      title = glue("Top {chart_stat} Movers"),
      subtitle = "Recent window vs earlier season baseline",
      x = NULL,
      y = "Z-score change"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
} else {
  NULL
}

chart_of_day_path <- save_newsletter_plot(
  plot_obj = chart_of_day_plot,
  filename = paste0("chart_of_day_", slugify(chart_stat), ".png"),
  folder = chart_dir,
  width = 8,
  height = 5
)

chart_of_day <- list(
  title = glue("Top {chart_stat} Movers"),
  note = glue("Biggest 7-day movement in {chart_stat} relative to each hitter’s earlier season baseline."),
  path = chart_of_day_path
)

# =========================================================
# 7. Hitter rolling plots and featured hitter
# =========================================================

build_hitter_rolling_stats <- function(pbp_data, player_name, window = 7) {
  pbp_clean <- pbp_data %>%
    mutate(
      game_date = as.Date(game_date),
      player_id = as.character(matchup.batter.id),
      player = matchup.batter.fullName
    )
  
  pbp_clean$pa_event <- coalesce_existing_cols(
    pbp_clean,
    c("result.event", "details.event", "result.eventType")
  )
  
  pbp_clean <- pbp_clean %>%
    filter(player == player_name, !is.na(atBatIndex))
  
  if (nrow(pbp_clean) == 0) return(NULL)
  
  pa_rows <- pbp_clean %>%
    arrange(game_pk, atBatIndex, pitchNumber) %>%
    group_by(game_pk, atBatIndex) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(!is.na(pa_event)) %>%
    mutate(
      event_key = normalize_event_key(pa_event),
      
      is_hit = event_key %in% c("single", "double", "triple", "home_run"),
      is_bb = event_key %in% c("walk", "intent_walk", "intentional_walk"),
      is_so = event_key %in% c("strikeout", "strikeout_double_play"),
      is_hbp = event_key %in% c("hit_by_pitch"),
      is_sf = event_key %in% c("sac_fly", "sac_fly_double_play"),
      
      is_ab = !event_key %in% c(
        "walk", "intent_walk", "intentional_walk",
        "hit_by_pitch", "sac_fly", "sac_bunt",
        "catcher_interf", "catcher_interference"
      ),
      
      total_bases = case_when(
        event_key == "single" ~ 1,
        event_key == "double" ~ 2,
        event_key == "triple" ~ 3,
        event_key == "home_run" ~ 4,
        TRUE ~ 0
      )
    ) %>%
    group_by(player, game_date, game_pk) %>%
    summarise(
      PA = n(),
      AB = sum(is_ab, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      SO = sum(is_so, na.rm = TRUE),
      HBP = sum(is_hbp, na.rm = TRUE),
      SF = sum(is_sf, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(game_date, game_pk) %>%
    mutate(
      roll_PA = roll_sum(PA, window),
      roll_AB = roll_sum(AB, window),
      roll_H = roll_sum(H, window),
      roll_BB = roll_sum(BB, window),
      roll_SO = roll_sum(SO, window),
      roll_HBP = roll_sum(HBP, window),
      roll_SF = roll_sum(SF, window),
      roll_TB = roll_sum(TB, window),
      
      AVG = safe_divide(roll_H, roll_AB),
      OBP = safe_divide(roll_H + roll_BB + roll_HBP, roll_AB + roll_BB + roll_HBP + roll_SF),
      SLG = safe_divide(roll_TB, roll_AB),
      OPS = OBP + SLG,
      BB_pct = safe_divide(roll_BB, roll_PA),
      K_pct = safe_divide(roll_SO, roll_PA)
    )
  
  pa_rows
}

plot_hitter_trend <- function(rolling_tbl, player_name, stat = "OPS", window = 7) {
  if (is.null(rolling_tbl) || !stat %in% names(rolling_tbl)) return(NULL)
  
  plot_tbl <- rolling_tbl %>%
    filter(!is.na(.data[[stat]]))
  
  if (nrow(plot_tbl) < 3) return(NULL)
  
  mean_val <- mean(plot_tbl[[stat]], na.rm = TRUE)
  sd_val <- sd(plot_tbl[[stat]], na.rm = TRUE)
  
  ggplot(plot_tbl, aes(x = game_date, y = .data[[stat]])) +
    geom_ribbon(
      aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
      alpha = 0.12
    ) +
    geom_hline(yintercept = mean_val, linetype = "dashed", linewidth = 0.7) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.6) +
    geom_point(
      data = plot_tbl %>% slice_tail(n = 1),
      aes(x = game_date, y = .data[[stat]]),
      size = 3
    ) +
    labs(
      title = glue("{player_name}: Rolling {stat}"),
      subtitle = glue("{window}-game rolling trend"),
      x = NULL,
      y = stat
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_featured_hitter_card <- function(top_riser_row, mlb_hitter_summary, pbp_data) {
  if (nrow(top_riser_row) == 0) {
    return(NULL)
  }
  
  row <- top_riser_row[1, ]
  player_name <- row$player[[1]]
  
  season_row <- mlb_hitter_summary %>%
    filter(name == player_name) %>%
    slice(1)
  
  rolling_tbl <- build_hitter_rolling_stats(
    pbp_data = pbp_data,
    player_name = player_name,
    window = 7
  )
  
  chart_stat <- if (!is.null(rolling_tbl) && row$stat[[1]] %in% names(rolling_tbl)) {
    row$stat[[1]]
  } else {
    "OPS"
  }
  
  chart_path <- NA_character_
  
  if (!is.null(rolling_tbl) && chart_stat %in% names(rolling_tbl)) {
    p <- plot_hitter_trend(
      rolling_tbl = rolling_tbl,
      player_name = player_name,
      stat = chart_stat,
      window = 7
    )
    
    chart_path <- save_newsletter_plot(
      plot_obj = p,
      filename = paste0(slugify(player_name), "_featured_", slugify(chart_stat), ".png"),
      folder = hitter_trend_plot_dir,
      width = 7,
      height = 4
    )
  }
  
  season_line <- if (nrow(season_row) > 0) {
    glue(
      "Season line: {sprintf('%.3f', season_row$AVG)} / ",
      "{sprintf('%.3f', season_row$OBP)} / ",
      "{sprintf('%.3f', season_row$SLG)} with {season_row$HR} HR."
    )
  } else {
    "Season line unavailable."
  }
  
  evidence <- list(
    list(label = "Trend", value = row$stat[[1]]),
    list(label = "Signal", value = paste0(round(row$z_change[[1]], 2), " z")),
    list(label = "Recent", value = format_newsletter_value(row$recent_value[[1]], row$stat[[1]]))
  )
  
  list(
    player = player_name,
    team = row$team[[1]],
    tag = "Hot Bat",
    headline = glue("{player_name} leads the hitter movement board"),
    summary = make_trender_blurb(row),
    season_line = season_line,
    trend_stat = row$stat[[1]],
    z_change = row$z_change[[1]],
    evidence = evidence,
    chart_path = chart_path
  )
}

featured_hitter_card <- build_featured_hitter_card(
  top_riser_row = top_riser_row,
  mlb_hitter_summary = mlb_hitter_summary,
  pbp_data = pbp_data
)

# =========================================================
# 8. Last night's top performers
# =========================================================

build_last_night_hitter_scores <- function(pbp_data, target_date, n = 10) {
  pbp_clean <- pbp_data %>%
    mutate(
      game_date = as.Date(game_date),
      batter_id = as.character(matchup.batter.id),
      batter = matchup.batter.fullName,
      team = batting_team
    )
  
  pbp_clean$pa_event <- coalesce_existing_cols(
    pbp_clean,
    c("result.event", "details.event", "result.eventType")
  )
  
  if ("result.rbi" %in% names(pbp_clean)) {
    pbp_clean$rbi <- suppressWarnings(as.numeric(pbp_clean[["result.rbi"]]))
    pbp_clean$rbi <- tidyr::replace_na(pbp_clean$rbi, 0)
  } else {
    pbp_clean$rbi <- 0
  }
  
  pa_rows <- pbp_clean %>%
    filter(game_date == as.Date(target_date), !is.na(atBatIndex)) %>%
    arrange(game_pk, atBatIndex, pitchNumber) %>%
    group_by(game_pk, atBatIndex) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(!is.na(batter_id), !is.na(batter), !is.na(pa_event)) %>%
    mutate(
      event_key = normalize_event_key(pa_event),
      
      is_hit = event_key %in% c("single", "double", "triple", "home_run"),
      is_2b = event_key == "double",
      is_3b = event_key == "triple",
      is_hr = event_key == "home_run",
      is_bb = event_key %in% c("walk", "intent_walk", "intentional_walk"),
      is_hbp = event_key == "hit_by_pitch",
      is_so = event_key %in% c("strikeout", "strikeout_double_play"),
      
      total_bases = case_when(
        event_key == "single" ~ 1,
        event_key == "double" ~ 2,
        event_key == "triple" ~ 3,
        event_key == "home_run" ~ 4,
        TRUE ~ 0
      )
    )
  
  if (nrow(pa_rows) == 0) {
    return(tibble())
  }
  
  pa_rows %>%
    group_by(batter_id, player = batter, team, game_pk, game_date) %>%
    summarise(
      PA = n(),
      H = sum(is_hit, na.rm = TRUE),
      doubles = sum(is_2b, na.rm = TRUE),
      triples = sum(is_3b, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HBP = sum(is_hbp, na.rm = TRUE),
      SO = sum(is_so, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      RBI = sum(rbi, na.rm = TRUE),
      hitter_game_score =
        H * 3 +
        doubles * 2 +
        triples * 3 +
        HR * 5 +
        BB * 1 +
        HBP * 1 +
        TB * 1 +
        RBI * 1 -
        SO * 1,
      .groups = "drop"
    ) %>%
    arrange(desc(hitter_game_score), desc(TB), desc(HR), desc(H)) %>%
    slice_head(n = n)
}

calc_outs_on_play <- function(pa_rows) {
  event_key <- pa_rows$event_key
  
  fallback_outs <- case_when(
    event_key %in% c(
      "strikeout", "field_out", "force_out", "sac_fly",
      "sac_bunt", "fielders_choice_out", "other_out"
    ) ~ 1,
    event_key %in% c(
      "double_play", "grounded_into_double_play",
      "strikeout_double_play", "sac_fly_double_play",
      "sac_bunt_double_play"
    ) ~ 2,
    event_key %in% c("triple_play") ~ 3,
    TRUE ~ 0
  )
  
  has_count_outs <- all(c("count.outs.start", "count.outs.end") %in% names(pa_rows))
  
  if (!has_count_outs) {
    return(fallback_outs)
  }
  
  outs_start <- suppressWarnings(as.numeric(pa_rows[["count.outs.start"]]))
  outs_end   <- suppressWarnings(as.numeric(pa_rows[["count.outs.end"]]))
  
  outs_from_count <- outs_end - outs_start
  
  outs_from_count <- ifelse(
    is.na(outs_from_count) | outs_from_count < 0 | outs_from_count > 3,
    NA_real_,
    outs_from_count
  )
  
  ifelse(is.na(outs_from_count), fallback_outs, outs_from_count)
}

build_last_night_pitcher_scores <- function(pbp_data, target_date, n = 10) {
  pbp_clean <- pbp_data %>%
    mutate(
      game_date = as.Date(game_date),
      pitcher_id = as.character(matchup.pitcher.id),
      pitcher = matchup.pitcher.fullName,
      team = fielding_team
    )
  
  pbp_clean$pa_event <- coalesce_existing_cols(
    pbp_clean,
    c("result.event", "details.event", "result.eventType")
  )
  
  pa_rows <- pbp_clean %>%
    filter(game_date == as.Date(target_date), !is.na(atBatIndex)) %>%
    arrange(game_pk, atBatIndex, pitchNumber) %>%
    group_by(game_pk, atBatIndex) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(!is.na(pitcher_id), !is.na(pitcher), !is.na(pa_event)) %>%
    mutate(
      event_key = normalize_event_key(pa_event),
      
      is_hit = event_key %in% c("single", "double", "triple", "home_run"),
      is_hr = event_key == "home_run",
      is_bb = event_key %in% c("walk", "intent_walk", "intentional_walk"),
      is_so = event_key %in% c("strikeout", "strikeout_double_play"),
      
      total_bases = case_when(
        event_key == "single" ~ 1,
        event_key == "double" ~ 2,
        event_key == "triple" ~ 3,
        event_key == "home_run" ~ 4,
        TRUE ~ 0
      )
    )
  
  if (nrow(pa_rows) == 0) {
    return(tibble())
  }
  
  pa_rows$outs_on_play <- calc_outs_on_play(pa_rows)
  
  pitch_rows <- pbp_clean %>%
    filter(game_date == as.Date(target_date), isPitch %in% TRUE)
  
  pitch_rows$call_desc <- coalesce_existing_cols(
    pitch_rows,
    c("details.call.description", "details.description")
  )
  
  pitch_rows <- pitch_rows %>%
    mutate(
      call_desc = stringr::str_to_lower(call_desc),
      is_whiff_pitch = stringr::str_detect(call_desc, "swinging strike|missed bunt")
    )
  
  whiff_tbl <- pitch_rows %>%
    group_by(pitcher_id, pitcher, team, game_pk, game_date) %>%
    summarise(
      pitches = n(),
      whiffs = sum(is_whiff_pitch, na.rm = TRUE),
      .groups = "drop"
    )
  
  pa_rows %>%
    group_by(pitcher_id, pitcher, team, game_pk, game_date) %>%
    summarise(
      outs_recorded = sum(outs_on_play, na.rm = TRUE),
      H_allowed = sum(is_hit, na.rm = TRUE),
      HR_allowed = sum(is_hr, na.rm = TRUE),
      BB_allowed = sum(is_bb, na.rm = TRUE),
      SO = sum(is_so, na.rm = TRUE),
      TB_allowed = sum(total_bases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      whiff_tbl,
      by = c("pitcher_id", "pitcher", "team", "game_pk", "game_date")
    ) %>%
    mutate(
      pitches = replace_na(pitches, 0L),
      whiffs = replace_na(whiffs, 0L),
      IP = outs_to_ip(outs_recorded),
      
      pitcher_game_score =
        outs_recorded * 2 +
        SO * 3 +
        whiffs * 0.5 -
        H_allowed * 2 -
        BB_allowed * 2 -
        HR_allowed * 4 -
        TB_allowed * 1
    ) %>%
    filter(outs_recorded >= 3) %>%
    arrange(desc(pitcher_game_score), desc(SO), desc(whiffs), desc(outs_recorded)) %>%
    slice_head(n = n)
}

last_night_hitters <- build_last_night_hitter_scores(
  pbp_data = pbp_data,
  target_date = report_date,
  n = 10
)

last_night_pitchers <- build_last_night_pitcher_scores(
  pbp_data = pbp_data,
  target_date = report_date,
  n = 10
)

last_night <- list(
  date = report_date,
  hitters = last_night_hitters,
  pitchers = last_night_pitchers
)

# =========================================================
# 9. Pitcher hot board
# =========================================================

calc_pitcher_hot_board <- function(
    views,
    report_date,
    season_start = "2026-02-20",
    recent_days = 14,
    min_recent_pa = 20,
    min_base_pa = 60
) {
  recent_start <- as.Date(report_date) - recent_days + 1
  baseline_start <- as.Date(season_start)
  baseline_end <- recent_start - 1
  
  recent_raw <- summarize_overall_pitcher(
    views = views,
    level = "player",
    team_col = "fielding_team",
    start_date = recent_start,
    end_date = report_date
  )
  
  baseline_raw <- summarize_overall_pitcher(
    views = views,
    level = "player",
    team_col = "fielding_team",
    start_date = baseline_start,
    end_date = baseline_end
  )
  
  id_col <- if ("matchup.pitcher.id" %in% names(recent_raw)) {
    "matchup.pitcher.id"
  } else {
    stop("Could not find matchup.pitcher.id in pitcher summary.")
  }
  
  stat_direction <- tibble::tribble(
    ~stat,          ~higher_is_good, ~category,
    "K_pct",        TRUE,            "Strikeouts",
    "Whiff_pct",    TRUE,            "Miss bats",
    "Chase_pct",    TRUE,            "Command / chase",
    "Zone_pct",     TRUE,            "Command / zone",
    "BB_pct",       FALSE,           "Command / walks",
    "OPS",          FALSE,           "Run prevention",
    "HardHit_pct",  FALSE,           "Contact quality",
    "barrel_pct",   FALSE,           "Contact quality"
  )
  
  available_stats <- stat_direction %>%
    filter(stat %in% names(recent_raw), stat %in% names(baseline_raw))
  
  if (nrow(available_stats) == 0) {
    return(list(board = tibble(), detail = tibble()))
  }
  
  recent <- recent_raw %>%
    filter(PA >= min_recent_pa) %>%
    select(
      pitcher_id = all_of(id_col),
      name,
      team,
      PA_recent = PA,
      any_of(available_stats$stat)
    ) %>%
    rename_with(~ paste0(.x, "_recent"), all_of(available_stats$stat))
  
  baseline <- baseline_raw %>%
    filter(PA >= min_base_pa) %>%
    select(
      pitcher_id = all_of(id_col),
      name,
      team,
      PA_base = PA,
      any_of(available_stats$stat)
    ) %>%
    rename_with(~ paste0(.x, "_base"), all_of(available_stats$stat))
  
  joined <- recent %>%
    inner_join(
      baseline,
      by = c("pitcher_id", "name", "team")
    )
  
  detail <- purrr::map_dfr(seq_len(nrow(available_stats)), function(i) {
    stat_i <- available_stats$stat[[i]]
    higher_good_i <- available_stats$higher_is_good[[i]]
    category_i <- available_stats$category[[i]]
    
    recent_col <- paste0(stat_i, "_recent")
    base_col <- paste0(stat_i, "_base")
    
    joined %>%
      transmute(
        pitcher_id,
        pitcher = name,
        team,
        PA_recent,
        PA_base,
        stat = stat_i,
        category = category_i,
        higher_is_good = higher_good_i,
        recent_value = .data[[recent_col]],
        baseline_value = .data[[base_col]],
        raw_change = recent_value - baseline_value,
        baseball_change = if (isTRUE(higher_good_i)) {
          recent_value - baseline_value
        } else {
          baseline_value - recent_value
        }
      )
  }) %>%
    filter(!is.na(baseball_change), is.finite(baseball_change)) %>%
    group_by(stat) %>%
    mutate(
      stat_change_mean = mean(baseball_change, na.rm = TRUE),
      stat_change_sd = sd(baseball_change, na.rm = TRUE),
      z_change = if_else(
        stat_change_sd > 0,
        (baseball_change - stat_change_mean) / stat_change_sd,
        NA_real_
      )
    ) %>%
    ungroup()
  
  board <- detail %>%
    filter(!is.na(z_change), is.finite(z_change)) %>%
    group_by(pitcher_id, pitcher, team) %>%
    arrange(desc(z_change), .by_group = TRUE) %>%
    summarise(
      hot_score = mean(z_change, na.rm = TRUE),
      max_signal = max(z_change, na.rm = TRUE),
      best_category = first(category),
      best_stat = first(stat),
      best_z = first(z_change),
      PA_recent = max(PA_recent, na.rm = TRUE),
      PA_base = max(PA_base, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(hot_score), is.finite(hot_score)) %>%
    arrange(desc(hot_score), desc(max_signal))
  
  list(board = board, detail = detail)
}

pitcher_hot <- calc_pitcher_hot_board(
  views = views,
  report_date = report_date,
  season_start = "2026-02-20",
  recent_days = 14
)

pitcher_hot_board <- pitcher_hot$board
pitcher_hot_detail <- pitcher_hot$detail

# =========================================================
# 10. Pitch diagnostics, featured pitcher, pitch of day
# =========================================================

classify_pitch_family_newsletter <- function(pitch_type) {
  pitch_type_lower <- stringr::str_to_lower(pitch_type)
  
  case_when(
    str_detect(pitch_type_lower, "four-seam|4-seam|fastball|sinker|cutter|cut fastball") ~ "Fastball",
    str_detect(pitch_type_lower, "slider|sweeper|curve|knuckle curve|slurve") ~ "Breaking",
    str_detect(pitch_type_lower, "changeup|splitter|split-finger|forkball") ~ "Offspeed",
    TRUE ~ "Other"
  )
}

build_pitcher_pitch_diagnostics <- function(
    pitch_tbl,
    pitcher_name,
    report_date,
    recent_days = 14,
    pitch_col = NULL,
    min_recent_pitches = 10,
    min_base_pitches = 25
) {
  if (is.null(pitch_col)) {
    pitch_col <- pick_first_col(
      pitch_tbl,
      c("details.type.description", "pitch_type", "pitch_name"),
      required = TRUE
    )
  }
  
  recent_start <- as.Date(report_date) - recent_days + 1
  
  usable <- pitch_tbl %>%
    mutate(
      game_date = as.Date(game_date),
      pitcher = matchup.pitcher.fullName,
      pitch_type = .data[[pitch_col]],
      pitch_family = classify_pitch_family_newsletter(pitch_type),
      is_swing_bool = is_swing == 1,
      is_whiff_bool = is_whiff == 1,
      is_zone_bool = is_zone == 1,
      is_chase_bool = is_chase == 1,
      velo = suppressWarnings(as.numeric(pitchData.startSpeed))
    ) %>%
    filter(
      pitcher == pitcher_name,
      !is.na(pitch_type)
    )
  
  if (nrow(usable) == 0) {
    return(tibble())
  }
  
  make_window_summary <- function(data, window_name) {
    total_pitches <- nrow(data)
    
    data %>%
      group_by(pitch_type, pitch_family) %>%
      summarise(
        pitches = n(),
        usage = pitches / total_pitches,
        swings = sum(is_swing_bool, na.rm = TRUE),
        whiffs = sum(is_whiff_bool, na.rm = TRUE),
        zones = sum(is_zone_bool, na.rm = TRUE),
        chase = sum(is_chase_bool, na.rm = TRUE),
        velo = mean(velo, na.rm = TRUE),
        Whiff_pct = safe_divide(whiffs, swings),
        Zone_pct = safe_divide(zones, pitches),
        Chase_pct = safe_divide(chase, pitches),
        .groups = "drop"
      ) %>%
      rename_with(
        ~ paste0(.x, "_", window_name),
        c(pitches, usage, swings, whiffs, zones, chase, velo, Whiff_pct, Zone_pct, Chase_pct)
      )
  }
  
  recent <- usable %>%
    filter(game_date >= recent_start) %>%
    make_window_summary("recent")
  
  baseline <- usable %>%
    filter(game_date < recent_start) %>%
    make_window_summary("base")
  
  recent %>%
    full_join(
      baseline,
      by = c("pitch_type", "pitch_family")
    ) %>%
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 0)),
      
      usage_change = usage_recent - usage_base,
      whiff_change = Whiff_pct_recent - Whiff_pct_base,
      zone_change = Zone_pct_recent - Zone_pct_base,
      chase_change = Chase_pct_recent - Chase_pct_base,
      velo_change = velo_recent - velo_base,
      
      usage_signal = abs(usage_change),
      miss_bat_signal = whiff_change + usage_change,
      command_signal = if_else(
        pitch_family == "Fastball",
        zone_change + usage_change,
        zone_change
      ),
      
      primary_signal = case_when(
        pitch_family == "Fastball" & command_signal >= miss_bat_signal ~ "Command / zone",
        miss_bat_signal >= usage_signal ~ "Miss bats",
        TRUE ~ "Usage"
      )
    ) %>%
    filter(
      pitches_recent >= min_recent_pitches,
      pitches_base >= min_base_pitches
    ) %>%
    arrange(desc(pmax(abs(usage_change), abs(whiff_change), abs(zone_change), na.rm = TRUE)))
}

make_pitcher_pitchtype_rolling <- function(data,
                                           pitcher,
                                           pitch_types = NULL,
                                           top_n = 5,
                                           pitcher_col = "matchup.pitcher.fullName",
                                           pitch_col = NULL,
                                           date_col = "game_date",
                                           game_col = "game_pk",
                                           window = 5) {
  if (is.null(pitch_col)) {
    pitch_col <- pick_first_col(
      data,
      c("details.type.description", "pitch_type", "pitch_name"),
      required = TRUE
    )
  }
  
  pitcher_pitches <- data %>%
    filter(.data[[pitcher_col]] == pitcher) %>%
    filter(!is.na(.data[[pitch_col]])) %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      .pitch_type = .data[[pitch_col]],
      .game_id = paste(.data[[date_col]], .data[[game_col]], sep = "_")
    )
  
  if (nrow(pitcher_pitches) == 0) return(NULL)
  
  if (is.null(pitch_types)) {
    pitch_types <- pitcher_pitches %>%
      count(.pitch_type, sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.pitch_type)
  }
  
  game_totals <- pitcher_pitches %>%
    count(.game_id, .date, name = "game_total_pitches") %>%
    arrange(.date, .game_id) %>%
    mutate(game_number = row_number())
  
  pitch_counts <- pitcher_pitches %>%
    filter(.pitch_type %in% pitch_types) %>%
    count(.game_id, .date, .pitch_type, name = "game_pitch_count")
  
  per_game_pitch_mix <- game_totals %>%
    tidyr::crossing(.pitch_type = pitch_types) %>%
    left_join(
      pitch_counts,
      by = c(".game_id", ".date", ".pitch_type")
    ) %>%
    mutate(
      game_pitch_count = replace_na(game_pitch_count, 0L)
    ) %>%
    arrange(.pitch_type, game_number)
  
  per_game_pitch_mix %>%
    group_by(.pitch_type) %>%
    arrange(game_number, .by_group = TRUE) %>%
    mutate(
      rolling_pitch_count = roll_sum(game_pitch_count, window),
      rolling_total_pitches = roll_sum(game_total_pitches, window),
      rolling_pitch_pct = if_else(
        rolling_total_pitches > 0,
        rolling_pitch_count / rolling_total_pitches,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    transmute(
      pitcher = pitcher,
      game_date = .date,
      game_number,
      pitch_type = .pitch_type,
      game_pitch_count,
      game_total_pitches,
      rolling_pitch_count,
      rolling_total_pitches,
      rolling_pitch_pct
    )
}

plot_pitcher_pitch_mix_trend <- function(rolling_tbl, pitcher_name) {
  if (is.null(rolling_tbl) || nrow(rolling_tbl) == 0) return(NULL)
  
  ggplot(
    rolling_tbl,
    aes(x = game_date, y = rolling_pitch_pct, color = pitch_type, group = pitch_type)
  ) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = glue("{pitcher_name}: Rolling Pitch Mix"),
      subtitle = "Rolling share of pitches thrown by type",
      x = "Date",
      y = "Pitch Type Share",
      color = "Pitch Type"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_featured_pitcher_card <- function(
    pitcher_hot_board,
    pitcher_hot_detail,
    views,
    report_date
) {
  if (nrow(pitcher_hot_board) == 0) {
    return(NULL)
  }
  
  featured <- pitcher_hot_board %>%
    slice(1)
  
  pitcher_name <- featured$pitcher[[1]]
  
  detail_rows <- pitcher_hot_detail %>%
    filter(pitcher == pitcher_name) %>%
    arrange(desc(z_change))
  
  k_row <- detail_rows %>% filter(stat == "K_pct") %>% slice(1)
  whiff_row <- detail_rows %>% filter(stat == "Whiff_pct") %>% slice(1)
  chase_row <- detail_rows %>% filter(stat == "Chase_pct") %>% slice(1)
  ops_row <- detail_rows %>% filter(stat == "OPS") %>% slice(1)
  
  pitch_diagnostics <- build_pitcher_pitch_diagnostics(
    pitch_tbl = views$pitch,
    pitcher_name = pitcher_name,
    report_date = report_date,
    recent_days = 14
  )
  
  top_pitch_note <- if (nrow(pitch_diagnostics) > 0) {
    note_row <- pitch_diagnostics %>%
      arrange(desc(pmax(abs(usage_change), abs(whiff_change), abs(zone_change), na.rm = TRUE))) %>%
      slice(1)
    
    glue(
      "The main arsenal signal is the {note_row$pitch_type}: usage moved from ",
      "{scales::percent(note_row$usage_base, accuracy = 0.1)} to ",
      "{scales::percent(note_row$usage_recent, accuracy = 0.1)}, with whiff rate moving from ",
      "{scales::percent(note_row$Whiff_pct_base, accuracy = 0.1)} to ",
      "{scales::percent(note_row$Whiff_pct_recent, accuracy = 0.1)}."
    )
  } else {
    "No pitch-level diagnostic met the minimum pitch thresholds."
  }
  
  rolling_tbl <- make_pitcher_pitchtype_rolling(
    data = views$pitch,
    pitcher = pitcher_name,
    window = 5
  )
  
  chart_path <- NA_character_
  
  if (!is.null(rolling_tbl)) {
    p <- plot_pitcher_pitch_mix_trend(
      rolling_tbl = rolling_tbl,
      pitcher_name = pitcher_name
    )
    
    chart_path <- save_newsletter_plot(
      plot_obj = p,
      filename = paste0(slugify(pitcher_name), "_featured_pitch_mix.png"),
      folder = pitcher_trend_plot_dir,
      width = 7,
      height = 4
    )
  }
  
  result_line <- if (nrow(k_row) > 0) {
    glue(
      "K% is at {format_newsletter_value(k_row$recent_value, 'K_pct')} recently, ",
      "up from {format_newsletter_value(k_row$baseline_value, 'K_pct')}."
    )
  } else {
    "Strikeout trend did not qualify for this card."
  }
  
  whiff_line <- if (nrow(whiff_row) > 0) {
    glue(
      "Whiff% is at {format_newsletter_value(whiff_row$recent_value, 'Whiff_pct')} recently, ",
      "compared with {format_newsletter_value(whiff_row$baseline_value, 'Whiff_pct')} before the window."
    )
  } else {
    "Whiff trend did not qualify for this card."
  }
  
  chase_line <- if (nrow(chase_row) > 0) {
    glue(
      "Chase% is at {format_newsletter_value(chase_row$recent_value, 'Chase_pct')} recently, ",
      "compared with {format_newsletter_value(chase_row$baseline_value, 'Chase_pct')} before the window."
    )
  } else {
    "Chase trend did not qualify for this card."
  }
  
  ops_line <- if (nrow(ops_row) > 0) {
    glue(
      "OPS allowed is {format_newsletter_value(ops_row$recent_value, 'OPS')} recently, ",
      "from a baseline of {format_newsletter_value(ops_row$baseline_value, 'OPS')}."
    )
  } else {
    "OPS allowed trend did not qualify for this card."
  }
  
  pitcher_tag <- case_when(
    nrow(whiff_row) > 0 && whiff_row$z_change[[1]] >= 1.25 ~ "Whiff Riser",
    nrow(chase_row) > 0 && chase_row$z_change[[1]] >= 1.25 ~ "Chase Riser",
    nrow(k_row) > 0 && k_row$z_change[[1]] >= 1.25 ~ "Strikeout Riser",
    featured$best_category[[1]] == "Command / zone" ~ "Command Riser",
    TRUE ~ "Hot Arm"
  )
  
  evidence <- list(
    list(
      label = "K%",
      value = if (nrow(k_row) > 0) format_newsletter_value(k_row$recent_value[[1]], "K_pct") else "—"
    ),
    list(
      label = "Whiff%",
      value = if (nrow(whiff_row) > 0) format_newsletter_value(whiff_row$recent_value[[1]], "Whiff_pct") else "—"
    ),
    list(
      label = "Best signal",
      value = featured$best_stat[[1]]
    )
  )
  
  list(
    pitcher = pitcher_name,
    team = featured$team[[1]],
    tag = pitcher_tag,
    headline = glue("{pitcher_name} is trending behind {featured$best_category[[1]]} gains"),
    summary = glue(
      "{pitcher_name} grades as the top pitcher on today’s hot board, led by ",
      "{featured$best_stat[[1]]} with a {round(featured$best_z[[1]], 2)} z-score signal."
    ),
    result_line = result_line,
    whiff_line = whiff_line,
    chase_line = chase_line,
    prevention_line = ops_line,
    pitchmix_note = top_pitch_note,
    pitch_diagnostics = pitch_diagnostics,
    evidence = evidence,
    chart_path = chart_path
  )
}

featured_pitcher_card <- build_featured_pitcher_card(
  pitcher_hot_board = pitcher_hot_board,
  pitcher_hot_detail = pitcher_hot_detail,
  views = views,
  report_date = report_date
)

build_pitch_of_day <- function(featured_pitcher_card) {
  if (is.null(featured_pitcher_card)) {
    return(NULL)
  }
  
  diag <- featured_pitcher_card$pitch_diagnostics
  
  if (is.null(diag) || nrow(diag) == 0) {
    return(NULL)
  }
  
  pitch_row <- diag %>%
    mutate(
      whiff_score = replace_na(whiff_change, 0),
      zone_score = replace_na(zone_change, 0),
      usage_score = replace_na(abs(usage_change), 0),
      
      story_type = case_when(
        pitch_family == "Fastball" & zone_score >= whiff_score & zone_score > 0 ~ "Fastball command riser",
        whiff_score > 0.05 ~ "Whiff riser",
        usage_change > 0.07 ~ "Usage riser",
        TRUE ~ primary_signal
      ),
      
      story_score = case_when(
        story_type == "Fastball command riser" ~ zone_score + usage_score,
        story_type == "Whiff riser" ~ whiff_score + usage_score,
        story_type == "Usage riser" ~ usage_score,
        TRUE ~ pmax(abs(usage_change), abs(whiff_change), abs(zone_change), na.rm = TRUE)
      )
    ) %>%
    arrange(desc(story_score)) %>%
    slice(1)
  
  list(
    title = glue("Pitch of the Day: {pitch_row$pitch_type}"),
    pitcher = featured_pitcher_card$pitcher,
    team = featured_pitcher_card$team,
    tag = pitch_row$story_type,
    note = glue(
      "{featured_pitcher_card$pitcher}'s {pitch_row$pitch_type} is the clearest pitch-level hook in today's card. ",
      "Usage moved from {scales::percent(pitch_row$usage_base, accuracy = 0.1)} to ",
      "{scales::percent(pitch_row$usage_recent, accuracy = 0.1)}, while whiff rate moved from ",
      "{scales::percent(pitch_row$Whiff_pct_base, accuracy = 0.1)} to ",
      "{scales::percent(pitch_row$Whiff_pct_recent, accuracy = 0.1)}."
    ),
    evidence = list(
      list(label = "Usage", value = glue("{scales::percent(pitch_row$usage_base, accuracy = 0.1)} → {scales::percent(pitch_row$usage_recent, accuracy = 0.1)}")),
      list(label = "Whiff", value = glue("{scales::percent(pitch_row$Whiff_pct_base, accuracy = 0.1)} → {scales::percent(pitch_row$Whiff_pct_recent, accuracy = 0.1)}")),
      list(label = "Zone", value = glue("{scales::percent(pitch_row$Zone_pct_base, accuracy = 0.1)} → {scales::percent(pitch_row$Zone_pct_recent, accuracy = 0.1)}"))
    )
  )
}

pitch_of_day <- build_pitch_of_day(featured_pitcher_card)

# =========================================================
# 11. Rolling OPS comparison chart
# =========================================================

build_multi_hitter_rolling_ops <- function(pbp_data, players, window = 7) {
  pbp_clean <- pbp_data %>%
    mutate(
      game_date = as.Date(game_date),
      player_id = as.character(matchup.batter.id),
      player = matchup.batter.fullName
    )
  
  pbp_clean$pa_event <- coalesce_existing_cols(
    pbp_clean,
    c("result.event", "details.event", "result.eventType")
  )
  
  pbp_clean <- pbp_clean %>%
    filter(player %in% players, !is.na(atBatIndex))
  
  if (nrow(pbp_clean) == 0) {
    return(tibble())
  }
  
  pa_rows <- pbp_clean %>%
    arrange(game_pk, atBatIndex, pitchNumber) %>%
    group_by(game_pk, atBatIndex) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(!is.na(pa_event)) %>%
    mutate(
      event_key = normalize_event_key(pa_event),
      
      is_hit = event_key %in% c("single", "double", "triple", "home_run"),
      is_bb = event_key %in% c("walk", "intent_walk", "intentional_walk"),
      is_hbp = event_key == "hit_by_pitch",
      is_sf = event_key %in% c("sac_fly", "sac_fly_double_play"),
      is_ab = !event_key %in% c(
        "walk", "intent_walk", "intentional_walk",
        "hit_by_pitch", "sac_fly", "sac_bunt",
        "catcher_interf", "catcher_interference"
      ),
      
      total_bases = case_when(
        event_key == "single" ~ 1,
        event_key == "double" ~ 2,
        event_key == "triple" ~ 3,
        event_key == "home_run" ~ 4,
        TRUE ~ 0
      )
    ) %>%
    group_by(player, game_date, game_pk) %>%
    summarise(
      PA = n(),
      AB = sum(is_ab, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HBP = sum(is_hbp, na.rm = TRUE),
      SF = sum(is_sf, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(player, game_date, game_pk) %>%
    group_by(player) %>%
    mutate(
      roll_PA = roll_sum(PA, window),
      roll_AB = roll_sum(AB, window),
      roll_H = roll_sum(H, window),
      roll_BB = roll_sum(BB, window),
      roll_HBP = roll_sum(HBP, window),
      roll_SF = roll_sum(SF, window),
      roll_TB = roll_sum(TB, window),
      
      AVG = safe_divide(roll_H, roll_AB),
      OBP = safe_divide(roll_H + roll_BB + roll_HBP, roll_AB + roll_BB + roll_HBP + roll_SF),
      SLG = safe_divide(roll_TB, roll_AB),
      OPS = OBP + SLG
    ) %>%
    ungroup()
  
  pa_rows
}

plot_multi_hitter_rolling_ops <- function(rolling_ops_tbl, title = "Rolling OPS: Six Hottest Bats") {
  if (nrow(rolling_ops_tbl) == 0) {
    return(NULL)
  }
  
  last_points <- rolling_ops_tbl %>%
    group_by(player) %>%
    arrange(game_date, .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  ggplot(
    rolling_ops_tbl,
    aes(x = game_date, y = OPS, color = player, group = player)
  ) +
    geom_line(linewidth = 1.0, alpha = 0.9) +
    geom_point(size = 1.1, alpha = 0.9) +
    geom_text(
      data = last_points,
      aes(label = player),
      hjust = -0.05,
      size = 3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.18))) +
    labs(
      title = title,
      subtitle = "Seven-game rolling OPS for the strongest OPS risers on today’s board",
      x = NULL,
      y = "Rolling OPS"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

ops_comparison_players <- mlb_hitter_trenders %>%
  filter(stat == "OPS", direction == "Riser") %>%
  arrange(desc(z_change)) %>%
  distinct(player, .keep_all = TRUE) %>%
  slice_head(n = 6) %>%
  pull(player) %>%
  unique()

rolling_ops_comparison_tbl <- build_multi_hitter_rolling_ops(
  pbp_data = pbp_data,
  players = ops_comparison_players,
  window = 7
)

rolling_ops_comparison_plot <- plot_multi_hitter_rolling_ops(
  rolling_ops_tbl = rolling_ops_comparison_tbl
)

rolling_ops_comparison_path <- save_newsletter_plot(
  plot_obj = rolling_ops_comparison_plot,
  filename = "rolling_ops_top_movers.png",
  folder = chart_dir,
  width = 8.5,
  height = 5
)

rolling_ops_comparison <- list(
  title = "Rolling OPS: Six Hottest Bats",
  note = "Seven-game rolling OPS for the six strongest OPS risers on today’s movement board.",
  players = ops_comparison_players,
  path = rolling_ops_comparison_path
)

# =========================================================
# 12. Split spotlight
# =========================================================

summarise_hitter_hand_split <- function(views, player_name, min_pa_each = 5) {
  pa <- views$pa %>%
    filter(matchup.batter.fullName == player_name) %>%
    mutate(
      split = matchup.pitchHand.code
    ) %>%
    filter(split %in% c("L", "R"))
  
  if (nrow(pa) == 0) return(NULL)
  
  split_tbl <- pa %>%
    group_by(split) %>%
    summarise(
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      AVG = safe_divide(H, AB),
      OBP = safe_divide(H + BB, PA),
      SLG = safe_divide(TB, AB),
      OPS = OBP + SLG,
      K_pct = safe_divide(SO, PA),
      BB_pct = safe_divide(BB, PA),
      .groups = "drop"
    ) %>%
    filter(PA >= min_pa_each)
  
  if (!all(c("L", "R") %in% split_tbl$split)) return(NULL)
  
  split_tbl
}

summarise_pitcher_hand_split <- function(views, pitcher_name, min_pa_each = 5) {
  pa <- views$pa %>%
    filter(matchup.pitcher.fullName == pitcher_name) %>%
    mutate(
      split = matchup.batSide.code
    ) %>%
    filter(split %in% c("L", "R"))
  
  if (nrow(pa) == 0) return(NULL)
  
  split_tbl <- pa %>%
    group_by(split) %>%
    summarise(
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      AVG = safe_divide(H, AB),
      OBP = safe_divide(H + BB, PA),
      SLG = safe_divide(TB, AB),
      OPS = OBP + SLG,
      K_pct = safe_divide(SO, PA),
      BB_pct = safe_divide(BB, PA),
      .groups = "drop"
    ) %>%
    filter(PA >= min_pa_each)
  
  if (!all(c("L", "R") %in% split_tbl$split)) return(NULL)
  
  split_tbl
}

make_split_item <- function(split_tbl, player_name, type, comparison) {
  if (is.null(split_tbl) || nrow(split_tbl) == 0) return(NULL)
  
  left <- split_tbl %>% filter(split == "L") %>% slice(1)
  right <- split_tbl %>% filter(split == "R") %>% slice(1)
  
  stats <- c("OPS", "SLG", "OBP", "K_pct", "BB_pct")
  
  diffs <- tibble(
    stat = stats,
    left_value = map_dbl(stats, ~ left[[.x]][[1]]),
    right_value = map_dbl(stats, ~ right[[.x]][[1]]),
    diff = left_value - right_value
  ) %>%
    filter(!is.na(diff), is.finite(diff)) %>%
    arrange(desc(abs(diff)))
  
  if (nrow(diffs) == 0) return(NULL)
  
  best <- diffs %>% slice(1)
  
  list(
    player = player_name,
    type = type,
    comparison = comparison,
    stat = best$stat[[1]],
    diff = best$diff[[1]],
    note = glue(
      "The largest handedness split is {best$stat[[1]]}, with a raw L/R gap of {round(best$diff[[1]], 3)}."
    )
  )
}

build_split_spotlight <- function(views, featured_hitter_card, featured_pitcher_card) {
  hitter_split <- NULL
  pitcher_split <- NULL
  
  if (!is.null(featured_hitter_card)) {
    hitter_split_tbl <- summarise_hitter_hand_split(
      views = views,
      player_name = featured_hitter_card$player,
      min_pa_each = 5
    )
    
    hitter_split <- make_split_item(
      split_tbl = hitter_split_tbl,
      player_name = featured_hitter_card$player,
      type = "Hitter handedness split",
      comparison = "vs LHP compared with vs RHP"
    )
  }
  
  if (!is.null(featured_pitcher_card)) {
    pitcher_split_tbl <- summarise_pitcher_hand_split(
      views = views,
      pitcher_name = featured_pitcher_card$pitcher,
      min_pa_each = 5
    )
    
    pitcher_split <- make_split_item(
      split_tbl = pitcher_split_tbl,
      player_name = featured_pitcher_card$pitcher,
      type = "Pitcher handedness split",
      comparison = "vs LHH compared with vs RHH"
    )
  }
  
  list(
    hitter = hitter_split,
    pitcher = pitcher_split
  )
}

split_spotlight <- build_split_spotlight(
  views = views,
  featured_hitter_card = featured_hitter_card,
  featured_pitcher_card = featured_pitcher_card
)

# =========================================================
# 13. Watch list and trend legend
# =========================================================

build_watch_list <- function(mlb_hitter_trenders, pitcher_hot_board, n_hitters = 3, n_pitchers = 3) {
  hitter_watch <- mlb_hitter_trenders %>%
    arrange(desc(abs(z_change))) %>%
    distinct(player, .keep_all = TRUE) %>%
    slice_head(n = n_hitters) %>%
    transmute(
      type = "Hitter",
      name = player,
      team,
      tag = paste(direction, stat),
      reason = glue("{stat} moved by {round(z_change, 2)} z-score points on the hitter board.")
    )
  
  pitcher_watch <- pitcher_hot_board %>%
    slice_head(n = n_pitchers) %>%
    transmute(
      type = "Pitcher",
      name = pitcher,
      team,
      tag = best_category,
      reason = glue("{best_stat} is the top signal with a {round(best_z, 2)} z-score mark.")
    )
  
  bind_rows(hitter_watch, pitcher_watch)
}

watch_list <- build_watch_list(
  mlb_hitter_trenders = mlb_hitter_trenders,
  pitcher_hot_board = pitcher_hot_board
)

trend_legend <- tibble::tribble(
  ~tag, ~meaning,
  "Whiff Riser", "A pitcher is missing more bats than his baseline.",
  "Command Riser", "A pitcher is finding the zone or limiting walks more effectively.",
  "Usage Riser", "A pitch is taking up more of the arsenal than usual.",
  "Fastball Command Riser", "A fastball-type pitch is gaining zone presence or setting up the arsenal better.",
  "Hot Bat", "A hitter has one of the strongest recent-vs-baseline offensive changes.",
  "Split Lens", "A quick check for whether handedness changes the player’s profile."
)

# =========================================================
# 14. Weekday edition type
# =========================================================

get_edition_type <- function(weekday_name) {
  case_when(
    weekday_name == "Monday"    ~ "Weekend Market Movers",
    weekday_name == "Tuesday"   ~ "Pitch Shape + Plate Discipline",
    weekday_name == "Wednesday" ~ "Fantasy Ranking Movement",
    weekday_name == "Thursday"  ~ "AAA / Minor League Feature",
    weekday_name == "Friday"    ~ "Weekend Edge Preview",
    weekday_name == "Saturday"  ~ "Weekend Slate + Simulation Notes",
    weekday_name == "Sunday"    ~ "Weekly Leaderboard Reset",
    TRUE                        ~ "Daily Baseball Notes"
  )
}

edition_type <- get_edition_type(weekday_name)

# =========================================================
# 15. Final daily object
# =========================================================

daily <- list(
  date = report_date,
  generated_at = Sys.time(),
  weekday = weekday_name,
  edition_type = edition_type,
  
  edition_label = glue("{format(report_date, '%B %d, %Y')} • {edition_type}"),
  
  headline = glue("{edition_type}: {top_riser$name} Leads Today’s Movement Board"),
  
  deck = glue(
    "A daily baseball notebook generated from updated play-by-play data, ",
    "leaderboards, recent form changes, pitcher usage changes, split notes, and SABRhood trend boards."
  ),
  
  lead_blurb = glue(
    "Today’s board is led by {top_riser$name}, whose biggest movement came in {top_riser$stat}. ",
    "On the other side, {top_faller$name} shows the sharpest negative movement in {top_faller$stat}."
  ),
  
  top_riser = top_riser,
  top_faller = top_faller,
  quick_hits = quick_hits,
  
  trenders = mlb_hitter_trenders,
  
  leaderboards = list(
    ops = ops_leaderboard,
    power = power_leaderboard,
    discipline = discipline_leaderboard
  ),
  
  last_night = last_night,
  
  featured_hitter_card = featured_hitter_card,
  featured_pitcher_card = featured_pitcher_card,
  
  pitcher_hot_board = pitcher_hot_board,
  pitcher_hot_detail = pitcher_hot_detail,
  
  pitch_of_day = pitch_of_day,
  split_spotlight = split_spotlight,
  watch_list = watch_list,
  trend_legend = trend_legend,
  
  chart_of_day = chart_of_day,
  rolling_ops_comparison = rolling_ops_comparison
)

saveRDS(daily, daily_path)

archive_path <- file.path(
  archive_dir,
  paste0(format(report_date, "%Y-%m-%d"), "_newsletter.rds")
)

saveRDS(daily, archive_path)

message("Saved newsletter object: ", daily_path)
message("Saved archive object: ", archive_path)
message("Done.")