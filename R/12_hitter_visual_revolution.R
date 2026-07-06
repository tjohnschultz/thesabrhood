# -----------------------------------------------------------------------------
# R/13_hitter_visual_revolution.R
# Hitter Visual Revolution module
#
# Adds hitter equivalents of the pitcher pitch-type visualization workflow:
#   - hitter stat-block league profile
#   - hitter by-pitch-type league profile
#   - hitter by-pitch-type distribution profile
#   - hitter stat-block trends over year/month/date
#   - hitter spray-direction profile
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Hitter summary helpers
# -----------------------------------------------------------------------------

vr_apply_hitter_filters <- function(data,
                                    group = "none",
                                    bat_hand = "none",
                                    pit_hand = "none",
                                    start_date = NULL,
                                    end_date = NULL,
                                    pitch_type = NA) {
  library(dplyr)

  if (exists("apply_split_filters", mode = "function")) {
    out <- tryCatch(
      apply_split_filters(
        data,
        group = group,
        bat_hand = bat_hand,
        pit_hand = pit_hand,
        start_date = start_date,
        end_date = end_date,
        pitch_type = pitch_type
      ),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }

  out <- data

  if (!is.null(start_date) && "game_date" %in% names(out)) {
    out <- out %>% filter(as.Date(game_date) >= as.Date(start_date))
  }
  if (!is.null(end_date) && "game_date" %in% names(out)) {
    out <- out %>% filter(as.Date(game_date) <= as.Date(end_date))
  }
  if (!is.na(pitch_type) && "pitch_type" %in% names(out)) {
    out <- out %>% filter(.data$pitch_type %in% pitch_type)
  }
  if (!identical(bat_hand, "none") && "matchup.batSide.code" %in% names(out)) {
    out <- out %>% filter(.data[["matchup.batSide.code"]] %in% bat_hand)
  }
  if (!identical(pit_hand, "none") && "matchup.pitchHand.code" %in% names(out)) {
    out <- out %>% filter(.data[["matchup.pitchHand.code"]] %in% pit_hand)
  }

  out
}

vr_add_hitter_derived_stats <- function(df) {
  library(dplyr)

  out <- df

  if (all(c("SLG", "AVG") %in% names(out)) && !"ISO" %in% names(out)) {
    out <- out %>% mutate(ISO = SLG - AVG)
  }

  if (all(c("HR", "PA") %in% names(out)) && !"HR_pct" %in% names(out)) {
    out <- out %>% mutate(HR_pct = vr_safe_rate(HR, PA))
  }

  if (all(c("XBH", "PA") %in% names(out)) && !"XBH_pct" %in% names(out)) {
    out <- out %>% mutate(XBH_pct = vr_safe_rate(XBH, PA))
  }

  if ("Barrel_pct" %in% names(out) && !"barrel_pct" %in% names(out)) {
    out <- out %>% mutate(barrel_pct = Barrel_pct)
  }

  out
}

summarize_pa_by_pitchtype_batter <- function(pa_tbl,
                                             level = "player",
                                             group.by = NULL,
                                             team_col = "batting_team",
                                             pitch_type_col = "pitch_type") {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)

  if (!pitch_type_col %in% names(pa_tbl)) {
    stop("PA table does not contain pitch_type_col: ", pitch_type_col, call. = FALSE)
  }

  id_col <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"

  if (level != "team") {
    pa_tbl <- pa_tbl %>% filter(!is.na(.data[[id_col]]))
  }

  if (!"total_bases" %in% names(pa_tbl)) pa_tbl$total_bases <- NA_real_
  if (!"is_hit" %in% names(pa_tbl)) pa_tbl$is_hit <- 0L
  if (!"is_hr" %in% names(pa_tbl)) pa_tbl$is_hr <- 0L

  grouping_cols <- c(id_col, group.by, pitch_type_col)

  pa_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
      hand = dplyr::first(.data[["matchup.batSide.code"]]),
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      XBH = sum(is_hit == 1 & total_bases >= 2, na.rm = TRUE),
      AVG = vr_safe_rate(H, AB),
      K_pct = vr_safe_rate(SO, PA),
      BB_pct = vr_safe_rate(BB, PA),
      HR_pct = vr_safe_rate(HR, PA),
      XBH_pct = vr_safe_rate(XBH, PA),
      SLG = vr_safe_rate(TB, AB),
      OBP = vr_safe_rate(H + BB, PA),
      OPS = SLG + OBP,
      ISO = SLG - AVG,
      .groups = "drop"
    )
}

summarize_disc_by_pitchtype_batter <- function(pitch_tbl,
                                               level = "player",
                                               group.by = NULL,
                                               team_col = "batting_team",
                                               pitch_type_col = "pitch_type") {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)

  if (!pitch_type_col %in% names(pitch_tbl)) {
    stop("Pitch table does not contain pitch_type_col: ", pitch_type_col, call. = FALSE)
  }

  id_col <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"

  if (level != "team") {
    pitch_tbl <- pitch_tbl %>% filter(!is.na(.data[[id_col]]))
  }

  grouping_cols <- c(id_col, group.by, pitch_type_col)

  pitch_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
      hand = dplyr::first(.data[["matchup.batSide.code"]]),
      Pitches = n(),
      Swings = sum(is_swing, na.rm = TRUE),
      Whiffs = sum(is_whiff, na.rm = TRUE),
      Contact = sum(is_contact, na.rm = TRUE),
      ZoneP = sum(is_zone, na.rm = TRUE),
      OZoneP = sum(is_ozone, na.rm = TRUE),
      Chase = sum(is_chase, na.rm = TRUE),
      ZSwings = sum(is_z_swing, na.rm = TRUE),
      OContact = sum(is_o_contact, na.rm = TRUE),
      ZContact = sum(is_z_contact, na.rm = TRUE),
      Heart = sum(is_heart, na.rm = TRUE),
      Swing_pct = vr_safe_rate(Swings, Pitches),
      Whiff_pct = vr_safe_rate(Whiffs, Swings),
      Contact_pct = vr_safe_rate(Contact, Swings),
      Zone_pct = vr_safe_rate(ZoneP, Pitches),
      Chase_pct = vr_safe_rate(Chase, OZoneP),
      ZSwing_pct = vr_safe_rate(ZSwings, ZoneP),
      OContact_pct = vr_safe_rate(OContact, Chase),
      ZContact_pct = vr_safe_rate(ZContact, ZSwings),
      Heart_pct = vr_safe_rate(Heart, Pitches),
      Shadow_pct = Zone_pct - Heart_pct,
      .groups = "drop"
    )
}

summarize_bbe_by_pitchtype_batter <- function(bbe_tbl,
                                              level = "player",
                                              group.by = NULL,
                                              team_col = "batting_team",
                                              pitch_type_col = "pitch_type") {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)

  if (!pitch_type_col %in% names(bbe_tbl)) {
    stop("BBE table does not contain pitch_type_col: ", pitch_type_col, call. = FALSE)
  }

  if (!"hitData.trajectory" %in% names(bbe_tbl)) bbe_tbl$hitData.trajectory <- NA_character_
  if (!"spray_direction" %in% names(bbe_tbl)) bbe_tbl$spray_direction <- NA_character_

  id_col <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"

  if (level != "team") {
    bbe_tbl <- bbe_tbl %>% filter(!is.na(.data[[id_col]]))
  }

  grouping_cols <- c(id_col, group.by, pitch_type_col)

  bbe_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    mutate(
      .is_ld = as.integer(hitData.trajectory %in% c("line_drive", "bunt_line_drive")),
      .is_pu = as.integer(hitData.trajectory %in% c("popup", "bunt_popup")),
      .is_fb_any = as.integer(hitData.trajectory %in% c("fly_ball", "popup", "bunt_popup")),
      .is_pull = as.integer(as.character(spray_direction) == "Pull"),
      .is_middle = as.integer(as.character(spray_direction) == "Middle"),
      .is_oppo = as.integer(as.character(spray_direction) == "Oppo")
    ) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
      hand = dplyr::first(.data[["matchup.batSide.code"]]),
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      LD = sum(.is_ld, na.rm = TRUE),
      PU = sum(.is_pu, na.rm = TRUE),
      Pull = sum(.is_pull, na.rm = TRUE),
      Middle = sum(.is_middle, na.rm = TRUE),
      Oppo = sum(.is_oppo, na.rm = TRUE),
      pull_fb = sum(is_pull_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HardHit_pct = vr_safe_rate(HardHit, BBE),
      barrel_pct = vr_safe_rate(barrels, BBE),
      GB_pct = vr_safe_rate(GB, BBE),
      FB_pct = vr_safe_rate(FB, BBE),
      LD_pct = vr_safe_rate(LD, BBE),
      PU_pct = vr_safe_rate(PU, BBE),
      Pull_pct = vr_safe_rate(Pull, BBE),
      Middle_pct = vr_safe_rate(Middle, BBE),
      Oppo_pct = vr_safe_rate(Oppo, BBE),
      pull_fbpct = vr_safe_rate(pull_fb, BBE),
      HRFB = vr_safe_rate(FBHR, FB),
      EV = mean(ev, na.rm = TRUE),
      maxEV = suppressWarnings(max(ev, na.rm = TRUE)),
      LA = mean(la, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(maxEV = ifelse(is.infinite(maxEV), NA_real_, maxEV))
}

summarize_overall_batter_by_pitchtype <- function(views,
                                                  group = "none",
                                                  bat_hand = "none",
                                                  pit_hand = "none",
                                                  level = "player",
                                                  start_date = NULL,
                                                  end_date = NULL,
                                                  group.by = NULL,
                                                  team_col = "batting_team",
                                                  pitch_type = NA,
                                                  pitch_type_col = "pitch_type",
                                                  min_pitch_pct = NULL,
                                                  min_bbe_pct = NULL,
                                                  min_pa_pct = NULL) {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)

  if (!level %in% c("player", "team")) {
    stop("summarize_overall_batter_by_pitchtype() supports level = 'player' or 'team'.", call. = FALSE)
  }

  pitch_tbl <- vr_apply_hitter_filters(views$pitch, group, bat_hand, pit_hand, start_date, end_date, pitch_type)
  bbe_tbl <- vr_apply_hitter_filters(views$bbe, group, bat_hand, pit_hand, start_date, end_date, pitch_type)
  pa_tbl <- vr_apply_hitter_filters(views$pa, group, bat_hand, pit_hand, start_date, end_date, pitch_type)

  pa_sum <- summarize_pa_by_pitchtype_batter(pa_tbl, level, group.by, team_col, pitch_type_col)
  disc_sum <- summarize_disc_by_pitchtype_batter(pitch_tbl, level, group.by, team_col, pitch_type_col)
  bbe_sum <- summarize_bbe_by_pitchtype_batter(bbe_tbl, level, group.by, team_col, pitch_type_col)

  id_col <- if (level == "team") team_col else "matchup.batter.id"
  join_cols <- c(id_col, group.by, pitch_type_col)

  if (!is.null(min_pa_pct) && "PA" %in% names(pa_sum) && exists("mask_below_threshold", mode = "function")) {
    pa_sum <- mask_below_threshold(pa_sum, "PA", min_pa_pct, keep_cols = c(id_col, "name", group.by, pitch_type_col))
  }
  if (!is.null(min_pitch_pct) && "Pitches" %in% names(disc_sum) && exists("mask_below_threshold", mode = "function")) {
    disc_sum <- mask_below_threshold(disc_sum, "Pitches", min_pitch_pct, keep_cols = c(id_col, "name", group.by, pitch_type_col))
  }
  if (!is.null(min_bbe_pct) && "BBE" %in% names(bbe_sum) && exists("mask_below_threshold", mode = "function")) {
    bbe_sum <- mask_below_threshold(bbe_sum, "BBE", min_bbe_pct, keep_cols = c(id_col, "name", group.by, pitch_type_col))
  }

  pa_sum %>%
    full_join(disc_sum %>% select(-any_of(c("name", "team", "hand"))), by = join_cols) %>%
    full_join(bbe_sum %>% select(-any_of(c("name", "team", "hand"))), by = join_cols) %>%
    vr_add_hitter_derived_stats() %>%
    arrange(desc(PA), desc(Pitches), desc(BBE))
}

# -----------------------------------------------------------------------------
# Generic hitter comparison builders
# -----------------------------------------------------------------------------

vr_build_hitter_league_profile <- function(summary_df,
                                           hitter,
                                           stats,
                                           group_cols = NULL,
                                           hitter_col = "name",
                                           league_method = c("weighted_mean", "median", "mean"),
                                           exclude_player_from_league = TRUE) {
  library(dplyr)
  library(tidyr)

  league_method <- match.arg(league_method)
  group_cols <- vr_clean_group_by(group_cols)

  summary_df <- vr_add_hitter_derived_stats(summary_df)
  stats <- stats[stats %in% names(summary_df)]

  if (length(stats) == 0) stop("No requested stats were available for hitter profile.", call. = FALSE)

  player_df <- summary_df %>% filter(.data[[hitter_col]] == hitter)
  if (nrow(player_df) == 0) stop("No rows found for hitter: ", hitter, call. = FALSE)

  league_pool <- summary_df
  if (isTRUE(exclude_player_from_league)) {
    league_pool <- league_pool %>% filter(.data[[hitter_col]] != hitter)
  }

  league_values <- vr_league_values_by_group(
    summary_df = league_pool,
    stats = stats,
    group_cols = group_cols,
    method = league_method
  )

  player_long <- player_df %>%
    pivot_longer(cols = all_of(stats), names_to = "stat", values_to = "player_value") %>%
    filter(!is.na(player_value))

  comparison <- player_long %>%
    left_join(league_values, by = c(group_cols, "stat")) %>%
    rowwise() %>%
    mutate(
      percentile = {
        cur_stat <- stat
        cur_row <- dplyr::cur_data()
        tmp <- summary_df %>% filter(!is.na(.data[[cur_stat]]))
        if (length(group_cols) > 0) {
          for (gc in group_cols) {
            cur_val <- cur_row[[gc]][1]
            tmp <- tmp %>% filter(.data[[gc]] == cur_val)
          }
        }
        vr_pct_rank_value(player_value, tmp[[cur_stat]])
      }
    ) %>%
    ungroup()

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_pool,
    league_values = league_values,
    comparison = comparison,
    stats = stats
  )
}

vr_hitter_profile_plot_data <- function(comparison,
                                        hitter,
                                        stats,
                                        group_cols = NULL,
                                        percent_stats = vr_percent_stats(),
                                        scale_percents = TRUE,
                                        stat_labels = NULL,
                                        league_label = "League Avg",
                                        show_percentiles = TRUE) {
  library(dplyr)
  library(tidyr)

  group_cols <- vr_clean_group_by(group_cols)

  comparison %>%
    filter(stat %in% stats) %>%
    pivot_longer(cols = c(player_value, league_value), names_to = "comparison_type", values_to = "raw_value") %>%
    mutate(
      comparison = if_else(comparison_type == "player_value", hitter, league_label),
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      pct_label = if_else(comparison == hitter & show_percentiles & !is.na(percentile), paste0(round(percentile), "th pct"), NA_character_),
      label = if_else(comparison == hitter & !is.na(pct_label), paste0(value_label, "\n", pct_label), value_label),
      comparison = factor(comparison, levels = c(hitter, league_label)),
      stat_label = factor(stat_label, levels = vr_label_stats(stats, stat_labels = stat_labels, percent_stats = NULL))
    ) %>%
    filter(!is.na(value))
}

vr_plot_hitter_comparison_facets <- function(plot_data,
                                             hitter,
                                             x_col = "comparison",
                                             title = NULL,
                                             subtitle = NULL,
                                             facet_scales = "free_y",
                                             show_values = TRUE,
                                             ncol = NULL,
                                             base_size = 12,
                                             use_team_theme = FALSE,
                                             team_abbr = NULL) {
  library(ggplot2)

  if (is.null(title)) title <- paste0(hitter, " Hitter Profile")
  if (is.null(subtitle)) subtitle <- "Bars compare selected hitter to league average. Player labels include value percentile."

  p <- ggplot(plot_data, aes(x = .data[[x_col]], y = value, fill = comparison)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.45) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Value", fill = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    )

  if (isTRUE(use_team_theme) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(p, team = team_abbr, base_size = base_size, apply_fill = TRUE)
  }

  if (isTRUE(show_values)) {
    p <- p + geom_text(aes(label = label), vjust = -0.2, size = 2.8, lineheight = 0.9)
  }

  p
}

# -----------------------------------------------------------------------------
# 1. Overall hitter stat-block profile
# -----------------------------------------------------------------------------

plot_hitter_stat_blocks <- function(views,
                                    hitter,
                                    stat_block = "full",
                                    stats = NULL,
                                    group = "none",
                                    bat_hand = "none",
                                    pit_hand = "none",
                                    start_date = NULL,
                                    end_date = NULL,
                                    group.by = NULL,
                                    team_col = "batting_team",
                                    min_pa = 50,
                                    min_pitch_pct = NULL,
                                    min_bbe_pct = NULL,
                                    min_pa_pct = NULL,
                                    league_method = c("weighted_mean", "median", "mean"),
                                    exclude_player_from_league = TRUE,
                                    percent_stats = vr_percent_stats(),
                                    scale_percents = TRUE,
                                    stat_labels = NULL,
                                    league_label = "League Avg",
                                    show_percentiles = TRUE,
                                    facet_scales = "free_y",
                                    show_values = TRUE,
                                    ncol = NULL,
                                    base_size = 12,
                                    title = NULL,
                                    subtitle = NULL,
                                    use_team_theme = FALSE,
                                    team_abbr = NULL,
                                    print_plot = TRUE) {
  library(dplyr)

  league_method <- match.arg(league_method)
  group.by <- vr_clean_group_by(group.by)

  summary_df <- summarize_overall_batter(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    group.by = group.by,
    team_col = team_col,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct,
    min_pa_pct = min_pa_pct
  ) %>%
    vr_add_hitter_derived_stats()

  if (!is.null(min_pa) && "PA" %in% names(summary_df)) {
    summary_df <- summary_df %>% filter(PA >= min_pa | name == hitter)
  }

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = hitter_stat_blocks(),
    available_stats = names(summary_df)
  )

  profile <- vr_build_hitter_league_profile(
    summary_df = summary_df,
    hitter = hitter,
    stats = requested_stats,
    group_cols = group.by,
    league_method = league_method,
    exclude_player_from_league = exclude_player_from_league
  )

  plot_data <- vr_hitter_profile_plot_data(
    comparison = profile$comparison,
    hitter = hitter,
    stats = profile$stats,
    group_cols = group.by,
    percent_stats = percent_stats,
    scale_percents = scale_percents,
    stat_labels = stat_labels,
    league_label = league_label,
    show_percentiles = show_percentiles
  )

  if (is.null(title)) title <- paste0(hitter, " ", tools::toTitleCase(gsub("_", " ", stat_block)), " Profile")
  if (is.null(subtitle)) subtitle <- paste0("Stat block: ", stat_block, " | min PA in league pool: ", min_pa)

  p <- vr_plot_hitter_comparison_facets(
    plot_data = plot_data,
    hitter = hitter,
    title = title,
    subtitle = subtitle,
    facet_scales = facet_scales,
    show_values = show_values,
    ncol = ncol,
    base_size = base_size,
    use_team_theme = use_team_theme,
    team_abbr = team_abbr
  )

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = profile$summary_df,
    player_df = profile$player_df,
    league_df = profile$league_df,
    league_values = profile$league_values,
    comparison_table = profile$comparison,
    plot_data = plot_data,
    stat_block = stat_block,
    kept_stats = profile$stats,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# 2. Hitter pitch-type league profile: hitter vs league bars by pitch type
# -----------------------------------------------------------------------------

plot_hitter_pitchtype_profile <- function(views,
                                          hitter,
                                          stat_block = "damage",
                                          stats = NULL,
                                          always_include = "Usage_pct",
                                          group = "none",
                                          bat_hand = "none",
                                          pit_hand = "none",
                                          start_date = NULL,
                                          end_date = NULL,
                                          group.by = NULL,
                                          team_col = "batting_team",
                                          pitch_type = NA,
                                          pitch_type_col = "pitch_type",
                                          hitter_col = "name",
                                          player_id_col = "matchup.batter.id",
                                          min_player_pitches = 10,
                                          min_league_pitches = 25,
                                          league_method = c("weighted_mean", "median", "mean"),
                                          exclude_player_from_league = TRUE,
                                          percent_stats = vr_percent_stats(),
                                          scale_percents = TRUE,
                                          stat_labels = NULL,
                                          league_label = "League Avg",
                                          show_percentiles = TRUE,
                                          facet_scales = "free_y",
                                          show_values = TRUE,
                                          ncol = NULL,
                                          base_size = 12,
                                          title = NULL,
                                          subtitle = NULL,
                                          use_team_theme = FALSE,
                                          team_abbr = NULL,
                                          print_plot = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  league_method <- match.arg(league_method)
  group.by <- vr_clean_group_by(group.by)

  summary_df <- summarize_overall_batter_by_pitchtype(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    group.by = group.by,
    team_col = team_col,
    pitch_type = pitch_type,
    pitch_type_col = pitch_type_col
  )

  summary_df <- summary_df %>%
    add_pitchtype_usage_pct(
      id_col = player_id_col,
      group.by = group.by,
      pitch_type_col = pitch_type_col,
      pitch_count_col = "Pitches",
      usage_col = "Usage_pct"
    ) %>%
    vr_add_hitter_derived_stats()

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = hitter_stat_blocks(),
    always_include = always_include,
    available_stats = names(summary_df)
  )

  if (!is.null(min_league_pitches) && "Pitches" %in% names(summary_df)) {
    summary_df <- summary_df %>% filter(Pitches >= min_league_pitches | .data[[hitter_col]] == hitter)
  }

  player_df <- summary_df %>%
    filter(.data[[hitter_col]] == hitter) %>%
    filter(is.na(min_player_pitches) | !"Pitches" %in% names(.) | Pitches >= min_player_pitches)

  if (nrow(player_df) == 0) stop("No pitch-type rows found for hitter: ", hitter, call. = FALSE)

  league_pool <- summary_df
  if (isTRUE(exclude_player_from_league)) league_pool <- league_pool %>% filter(.data[[hitter_col]] != hitter)

  league_values <- vr_league_values_by_group(
    summary_df = league_pool,
    stats = requested_stats,
    group_cols = c(group.by, pitch_type_col),
    method = league_method
  )

  player_long <- player_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "player_value") %>%
    filter(!is.na(player_value))

  comparison <- player_long %>%
    left_join(league_values, by = c(group.by, pitch_type_col, "stat")) %>%
    rowwise() %>%
    mutate(
      percentile = {
        cur_stat <- stat
        cur_pitch <- .data[[pitch_type_col]]
        vals <- summary_df %>%
          filter(.data[[pitch_type_col]] == cur_pitch, !is.na(.data[[cur_stat]])) %>%
          pull(all_of(cur_stat))
        vr_pct_rank_value(player_value, vals)
      }
    ) %>%
    ungroup()

  plot_data <- comparison %>%
    pivot_longer(cols = c(player_value, league_value), names_to = "comparison_type", values_to = "raw_value") %>%
    mutate(
      comparison = if_else(comparison_type == "player_value", hitter, league_label),
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      pct_label = if_else(comparison == hitter & show_percentiles & !is.na(percentile), paste0(round(percentile), "th pct"), NA_character_),
      label = if_else(comparison == hitter & !is.na(pct_label), paste0(value_label, "\n", pct_label), value_label),
      comparison = factor(comparison, levels = c(hitter, league_label)),
      stat_label = factor(stat_label, levels = vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL))
    ) %>%
    filter(!is.na(value), !is.na(.data[[pitch_type_col]]))

  usage_order <- player_df %>% arrange(desc(Usage_pct), desc(Pitches)) %>% pull(all_of(pitch_type_col)) %>% unique()
  plot_data[[pitch_type_col]] <- factor(plot_data[[pitch_type_col]], levels = usage_order)

  if (is.null(title)) title <- paste0(hitter, " Pitch-Type Hitter Profile")
  if (is.null(subtitle)) subtitle <- paste0("Stat block: ", stat_block, " | hitter vs league average by pitch type")

  p <- ggplot(plot_data, aes(x = .data[[pitch_type_col]], y = value, fill = comparison)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.45) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Value", fill = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.position = "top"
    )

  if (isTRUE(use_team_theme) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(p, team = team_abbr, base_size = base_size, apply_fill = TRUE)
  }

  if (isTRUE(show_values)) {
    p <- p + geom_text(aes(label = label), position = position_dodge(width = 0.8), vjust = -0.2, size = 2.7, lineheight = 0.9)
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_pool,
    league_values = league_values,
    comparison_table = comparison,
    plot_data = plot_data,
    stat_block = stat_block,
    kept_stats = requested_stats,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# 3. Hitter pitch-type distribution profile: league boxplot + hitter point
# -----------------------------------------------------------------------------

plot_hitter_pitchtype_distribution_profile <- function(views,
                                                       hitter,
                                                       stat_block = "damage",
                                                       stats = NULL,
                                                       always_include = "Usage_pct",
                                                       group = "none",
                                                       bat_hand = "none",
                                                       pit_hand = "none",
                                                       start_date = NULL,
                                                       end_date = NULL,
                                                       group.by = NULL,
                                                       team_col = "batting_team",
                                                       pitch_type = NA,
                                                       pitch_type_col = "pitch_type",
                                                       hitter_col = "name",
                                                       player_id_col = "matchup.batter.id",
                                                       min_player_pitches = 10,
                                                       min_league_pitches = 25,
                                                       percent_stats = vr_percent_stats(),
                                                       scale_percents = TRUE,
                                                       stat_labels = NULL,
                                                       facet_scales = "free_y",
                                                       show_values = TRUE,
                                                       ncol = NULL,
                                                       base_size = 12,
                                                       title = NULL,
                                                       subtitle = NULL,
                                                       use_team_theme = FALSE,
                                                       team_abbr = NULL,
                                                       print_plot = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  group.by <- vr_clean_group_by(group.by)

  summary_df <- summarize_overall_batter_by_pitchtype(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    group.by = group.by,
    team_col = team_col,
    pitch_type = pitch_type,
    pitch_type_col = pitch_type_col
  ) %>%
    add_pitchtype_usage_pct(
      id_col = player_id_col,
      group.by = group.by,
      pitch_type_col = pitch_type_col,
      pitch_count_col = "Pitches",
      usage_col = "Usage_pct"
    ) %>%
    vr_add_hitter_derived_stats()

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = hitter_stat_blocks(),
    always_include = always_include,
    available_stats = names(summary_df)
  )

  league_df <- summary_df %>% filter(is.na(min_league_pitches) | Pitches >= min_league_pitches)
  player_df <- summary_df %>%
    filter(.data[[hitter_col]] == hitter) %>%
    filter(is.na(min_player_pitches) | Pitches >= min_player_pitches)

  if (nrow(player_df) == 0) stop("No pitch-type rows found for hitter: ", hitter, call. = FALSE)

  league_long <- league_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "raw_value") %>%
    filter(!is.na(raw_value), !is.na(.data[[pitch_type_col]])) %>%
    mutate(
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL)
    )

  player_long <- player_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "raw_value") %>%
    filter(!is.na(raw_value), !is.na(.data[[pitch_type_col]])) %>%
    mutate(
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL)
    ) %>%
    rowwise() %>%
    mutate(
      percentile = {
        cur_stat <- stat
        cur_pitch <- .data[[pitch_type_col]]
        vals <- league_long %>% filter(.data$stat == cur_stat, .data[[pitch_type_col]] == cur_pitch) %>% pull(raw_value)
        vr_pct_rank_value(raw_value, vals)
      },
      value_label = vr_format_value(raw_value, stat, percent_stats, scale_percents),
      label = ifelse(!is.na(percentile), paste0(value_label, "\n", round(percentile), "th pct"), value_label)
    ) %>%
    ungroup()

  usage_order <- player_df %>% arrange(desc(Usage_pct), desc(Pitches)) %>% pull(all_of(pitch_type_col)) %>% unique()
  league_long[[pitch_type_col]] <- factor(league_long[[pitch_type_col]], levels = usage_order)
  player_long[[pitch_type_col]] <- factor(player_long[[pitch_type_col]], levels = usage_order)

  stat_levels <- vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL)
  league_long$stat_label <- factor(league_long$stat_label, levels = stat_levels)
  player_long$stat_label <- factor(player_long$stat_label, levels = stat_levels)

  if (is.null(title)) title <- paste0(hitter, " Pitch-Type Distribution Profile")
  if (is.null(subtitle)) subtitle <- paste0("Box plots show league hitter distribution by pitch type; point shows ", hitter)

  p <- ggplot(league_long, aes(x = .data[[pitch_type_col]], y = value)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.62, alpha = 0.45) +
    geom_point(data = player_long, aes(x = .data[[pitch_type_col]], y = value), size = 2.7, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.35) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Value") +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.position = "none"
    )

  if (isTRUE(use_team_theme) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(p, team = team_abbr, base_size = base_size, apply_fill = FALSE)
  }

  if (isTRUE(show_values)) {
    p <- p + geom_text(data = player_long, aes(x = .data[[pitch_type_col]], y = value, label = label), vjust = -0.4, size = 2.7, lineheight = 0.9, inherit.aes = FALSE)
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_df,
    league_long = league_long,
    player_long = player_long,
    percentile_table = player_long %>% select(any_of(c(player_id_col, hitter_col, "team", pitch_type_col, "stat", "raw_value", "percentile"))),
    stat_block = stat_block,
    kept_stats = requested_stats,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# 4. Hitter stat-block trend over year/month/date
# -----------------------------------------------------------------------------

plot_hitter_stat_block_trend <- function(views,
                                         hitter,
                                         stat_block = "full",
                                         stats = NULL,
                                         time_unit = c("year", "month", "date"),
                                         group = "none",
                                         bat_hand = "none",
                                         pit_hand = "none",
                                         start_date = NULL,
                                         end_date = NULL,
                                         team_col = "batting_team",
                                         hitter_col = "name",
                                         min_pa = 10,
                                         min_pitch_pct = NULL,
                                         min_bbe_pct = NULL,
                                         min_pa_pct = NULL,
                                         percent_stats = vr_percent_stats(),
                                         scale_percents = TRUE,
                                         stat_labels = NULL,
                                         facet_scales = "free_y",
                                         ncol = NULL,
                                         base_size = 12,
                                         title = NULL,
                                         subtitle = NULL,
                                         use_team_theme = FALSE,
                                         team_abbr = NULL,
                                         print_plot = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  time_unit <- match.arg(time_unit)
  views_time <- vr_add_time_unit_to_views(views, time_unit = time_unit, out_col = ".time_group")

  summary_df <- summarize_overall_batter(
    views = views_time,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    group.by = ".time_group",
    team_col = team_col,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct,
    min_pa_pct = min_pa_pct
  ) %>%
    vr_add_hitter_derived_stats()

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = hitter_stat_blocks(),
    available_stats = names(summary_df)
  )

  player_df <- summary_df %>%
    filter(.data[[hitter_col]] == hitter) %>%
    filter(is.na(min_pa) | !"PA" %in% names(.) | PA >= min_pa)

  if (nrow(player_df) == 0) stop("No trend rows found for hitter: ", hitter, call. = FALSE)

  plot_data <- player_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "raw_value") %>%
    filter(!is.na(raw_value), !is.na(.time_group)) %>%
    mutate(
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      stat_label = factor(stat_label, levels = vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL))
    )

  if (is.null(title)) title <- paste0(hitter, " Hitter Trends")
  if (is.null(subtitle)) subtitle <- paste0("Time unit: ", time_unit, " | stat block: ", stat_block, " | min PA per point: ", min_pa)

  p <- ggplot(plot_data, aes(x = .time_group, y = value, group = stat_label)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    geom_point(size = 2, alpha = 0.85) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Value") +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )

  if (isTRUE(use_team_theme) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(p, team = team_abbr, base_size = base_size, apply_fill = FALSE)
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = summary_df,
    player_df = player_df,
    plot_data = plot_data,
    stat_block = stat_block,
    kept_stats = requested_stats,
    time_unit = time_unit,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# 5. Spray-direction profile: hitter vs league by Pull/Middle/Oppo
# -----------------------------------------------------------------------------

plot_hitter_spray_profile <- function(views,
                                      hitter,
                                      stat_block = "contact_quality",
                                      stats = NULL,
                                      group = "none",
                                      bat_hand = "none",
                                      pit_hand = "none",
                                      start_date = NULL,
                                      end_date = NULL,
                                      team_col = "batting_team",
                                      hitter_col = "name",
                                      player_id_col = "matchup.batter.id",
                                      spray_col = "spray_direction",
                                      min_player_bbe = 5,
                                      min_league_bbe = 20,
                                      league_method = c("weighted_mean", "median", "mean"),
                                      percent_stats = vr_percent_stats(),
                                      scale_percents = TRUE,
                                      stat_labels = NULL,
                                      league_label = "League Avg",
                                      show_values = TRUE,
                                      facet_scales = "free_y",
                                      ncol = NULL,
                                      base_size = 12,
                                      title = NULL,
                                      subtitle = NULL,
                                      use_team_theme = FALSE,
                                      team_abbr = NULL,
                                      print_plot = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  league_method <- match.arg(league_method)

  bbe_tbl <- vr_apply_hitter_filters(views$bbe, group, bat_hand, pit_hand, start_date, end_date)

  if (!spray_col %in% names(bbe_tbl)) stop("Could not find spray column: ", spray_col, call. = FALSE)
  if (!"hitData.trajectory" %in% names(bbe_tbl)) bbe_tbl$hitData.trajectory <- NA_character_

  spray_summary <- bbe_tbl %>%
    filter(!is.na(.data[[spray_col]]), !is.na(.data[[player_id_col]])) %>%
    mutate(
      .is_ld = as.integer(hitData.trajectory %in% c("line_drive", "bunt_line_drive")),
      .is_pu = as.integer(hitData.trajectory %in% c("popup", "bunt_popup")),
      .is_hit = if ("is_hit" %in% names(.)) is_hit else 0L,
      .tb = if ("total_bases" %in% names(.)) total_bases else NA_real_,
      .is_xbh = as.integer(.is_hit == 1 & .tb >= 2)
    ) %>%
    group_by(.data[[player_id_col]], .data[[spray_col]]) %>%
    summarise(
      name = first(.data[["matchup.batter.fullName"]]),
      team = first(.data[[team_col]]),
      BBE = n(),
      H = sum(.is_hit, na.rm = TRUE),
      XBH = sum(.is_xbh, na.rm = TRUE),
      TB = sum(.tb, na.rm = TRUE),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      LD = sum(.is_ld, na.rm = TRUE),
      PU = sum(.is_pu, na.rm = TRUE),
      HardHit_pct = vr_safe_rate(HardHit, BBE),
      barrel_pct = vr_safe_rate(barrels, BBE),
      GB_pct = vr_safe_rate(GB, BBE),
      FB_pct = vr_safe_rate(FB, BBE),
      LD_pct = vr_safe_rate(LD, BBE),
      PU_pct = vr_safe_rate(PU, BBE),
      AVG = vr_safe_rate(H, BBE),
      SLG = vr_safe_rate(TB, BBE),
      XBH_pct = vr_safe_rate(XBH, BBE),
      EV = mean(ev, na.rm = TRUE),
      maxEV = suppressWarnings(max(ev, na.rm = TRUE)),
      LA = mean(la, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(maxEV = ifelse(is.infinite(maxEV), NA_real_, maxEV))

  names(spray_summary)[names(spray_summary) == spray_col] <- "spray_direction"

  blocks <- list(
    contact_quality = c("EV", "maxEV", "LA", "HardHit_pct", "barrel_pct"),
    batted_ball = c("GB_pct", "FB_pct", "LD_pct", "PU_pct"),
    damage = c("AVG", "SLG", "XBH_pct"),
    full = c("EV", "HardHit_pct", "barrel_pct", "GB_pct", "FB_pct", "AVG", "SLG", "XBH_pct")
  )

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = blocks,
    available_stats = names(spray_summary)
  )

  league_pool <- spray_summary %>% filter(BBE >= min_league_bbe | name == hitter)
  player_df <- league_pool %>% filter(name == hitter, BBE >= min_player_bbe)

  if (nrow(player_df) == 0) stop("No spray rows found for hitter: ", hitter, call. = FALSE)

  league_values <- vr_league_values_by_group(
    summary_df = league_pool %>% filter(name != hitter),
    stats = requested_stats,
    group_cols = "spray_direction",
    method = league_method
  )

  player_long <- player_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "player_value") %>%
    filter(!is.na(player_value))

  comparison <- player_long %>%
    left_join(league_values, by = c("spray_direction", "stat"))

  plot_data <- comparison %>%
    pivot_longer(cols = c(player_value, league_value), names_to = "comparison_type", values_to = "raw_value") %>%
    mutate(
      comparison = if_else(comparison_type == "player_value", hitter, league_label),
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      label = value_label,
      spray_direction = factor(as.character(spray_direction), levels = c("Pull", "Middle", "Oppo")),
      comparison = factor(comparison, levels = c(hitter, league_label)),
      stat_label = factor(stat_label, levels = vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL))
    ) %>%
    filter(!is.na(value))

  if (is.null(title)) title <- paste0(hitter, " Spray-Direction Profile")
  if (is.null(subtitle)) subtitle <- paste0("Stat block: ", stat_block, " | hitter vs league average by batted-ball direction")

  p <- ggplot(plot_data, aes(x = spray_direction, y = value, fill = comparison)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.45) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Value", fill = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )

  if (isTRUE(use_team_theme) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(p, team = team_abbr, base_size = base_size, apply_fill = TRUE)
  }

  if (isTRUE(show_values)) {
    p <- p + geom_text(aes(label = label), position = position_dodge(width = 0.8), vjust = -0.2, size = 2.8)
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = spray_summary,
    player_df = player_df,
    league_df = league_pool,
    league_values = league_values,
    comparison_table = comparison,
    plot_data = plot_data,
    stat_block = stat_block,
    kept_stats = requested_stats,
    plot = p
  )
}
