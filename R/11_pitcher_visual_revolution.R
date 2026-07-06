# -----------------------------------------------------------------------------
# R/12_pitcher_visual_revolution.R
# Pitcher Visual Revolution add-ons
#
# Adds:
#   - stat_block wrappers for your existing pitch-type league/split pack
#   - boxplot/percentile pitch-type distribution profiles
#   - multi-season pitch-type trend plots using year/month/date
# -----------------------------------------------------------------------------

# Friendly aliases around the existing pack. These keep old function names alive
# while making the new naming language more plot-first.
plot_pitcher_pitchtype_split_profile <- function(...) {
  make_pitcher_pitchtype_viz_pack(...)
}

plot_pitcher_pitchtype_league_profile <- function(...) {
  make_pitcher_pitchtype_league_profile(...)
}

# -----------------------------------------------------------------------------
# 1. Stat-block wrapper for the existing league-profile bar chart
# -----------------------------------------------------------------------------

plot_pitcher_pitchtype_stat_blocks <- function(views,
                                               pitcher,
                                               stat_block = "miss_bat",
                                               stats = NULL,
                                               always_include = "Usage_pct",
                                               include_usage_facet = TRUE,
                                               group = "none",
                                               bat_hand = "none",
                                               pit_hand = "none",
                                               start_date = NULL,
                                               end_date = NULL,
                                               risp = "all",
                                               group.by = NULL,
                                               team_col = "fielding_team",
                                               pitch_type = NA,
                                               min_league_pitches = 25,
                                               min_pitch_pct = NULL,
                                               min_bbe_pct = NULL,
                                               use_team_theme = FALSE,
                                               team_abbr = NULL,
                                               title = NULL,
                                               subtitle = NULL,
                                               print_plot = TRUE,
                                               ...) {
  blocks <- pitcher_stat_blocks()
  facet_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = blocks,
    always_include = if (isTRUE(include_usage_facet)) always_include else NULL
  )

  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type ", tools::toTitleCase(gsub("_", " ", stat_block)), " Profile")
  }

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Stat block: ", stat_block,
      " | bars compare pitcher to league average by pitch type | labels show value percentile"
    )
  }

  make_pitcher_pitchtype_league_profile(
    views = views,
    pitcher = pitcher,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date,
    risp = risp,
    group.by = group.by,
    team_col = team_col,
    pitch_type = pitch_type,
    facet_stats = facet_stats,
    always_include = always_include,
    include_usage_facet = FALSE,
    min_league_pitches = min_league_pitches,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct,
    use_team_theme = use_team_theme,
    team_abbr = team_abbr,
    title = title,
    subtitle = subtitle,
    print_plot = print_plot,
    ...
  )
}

# -----------------------------------------------------------------------------
# 2. Pitch-type distribution profile: league boxplots + selected pitcher points
# -----------------------------------------------------------------------------

plot_pitcher_pitchtype_distribution_profile <- function(views,
                                                        pitcher,
                                                        stat_block = "miss_bat",
                                                        stats = NULL,
                                                        always_include = "Usage_pct",
                                                        group = "none",
                                                        bat_hand = "none",
                                                        pit_hand = "none",
                                                        start_date = NULL,
                                                        end_date = NULL,
                                                        risp = "all",
                                                        group.by = NULL,
                                                        team_col = "fielding_team",
                                                        pitch_type = NA,
                                                        pitcher_col = "name",
                                                        player_id_col = "matchup.pitcher.id",
                                                        pitch_type_col = "pitch_type",
                                                        min_player_pitches = 10,
                                                        min_league_pitches = 25,
                                                        min_pitch_pct = NULL,
                                                        min_bbe_pct = NULL,
                                                        percent_stats = vr_percent_stats(),
                                                        scale_percents = TRUE,
                                                        stat_labels = NULL,
                                                        exclude_player_from_league = FALSE,
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

  summary_df <- summarize_overall_pitcher_by_pitchtype(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    risp = risp,
    group.by = group.by,
    team_col = team_col,
    pitch_type = pitch_type,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct
  )

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    stop("No pitcher pitch-type summary rows were returned.", call. = FALSE)
  }

  summary_df <- summary_df %>%
    add_pitchtype_usage_pct(
      id_col = player_id_col,
      group.by = group.by,
      pitch_type_col = pitch_type_col,
      pitch_count_col = "Pitches",
      usage_col = "Usage_pct"
    )

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = pitcher_stat_blocks(),
    always_include = always_include,
    available_stats = names(summary_df)
  )

  if (length(requested_stats) == 0) {
    stop("None of the requested pitcher stats are available in the summary table.", call. = FALSE)
  }

  player_df <- summary_df %>%
    filter(.data[[pitcher_col]] == pitcher) %>%
    filter(is.na(min_player_pitches) | !"Pitches" %in% names(.) | Pitches >= min_player_pitches)

  if (nrow(player_df) == 0) {
    stop("No rows found for pitcher: ", pitcher, call. = FALSE)
  }

  league_df <- summary_df %>%
    filter(is.na(min_league_pitches) | !"Pitches" %in% names(.) | Pitches >= min_league_pitches)

  if (isTRUE(exclude_player_from_league)) {
    league_df <- league_df %>% filter(.data[[pitcher_col]] != pitcher)
  }

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
    )

  player_long <- player_long %>%
    rowwise() %>%
    mutate(
      percentile = {
        cur_stat <- stat
        cur_pitch <- .data[[pitch_type_col]]
        vals <- league_long %>%
          filter(.data$stat == cur_stat, .data[[pitch_type_col]] == cur_pitch) %>%
          pull(raw_value)
        vr_pct_rank_value(raw_value, vals)
      },
      value_label = vr_format_value(raw_value, stat, percent_stats, scale_percents),
      label = ifelse(!is.na(percentile), paste0(value_label, "\n", round(percentile), "th pct"), value_label)
    ) %>%
    ungroup()

  usage_order <- player_df %>%
    arrange(desc(Usage_pct), desc(Pitches)) %>%
    pull(all_of(pitch_type_col)) %>%
    unique()

  if (length(usage_order) > 0) {
    league_long[[pitch_type_col]] <- factor(league_long[[pitch_type_col]], levels = usage_order)
    player_long[[pitch_type_col]] <- factor(player_long[[pitch_type_col]], levels = usage_order)
  }

  stat_levels <- vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL)
  league_long$stat_label <- factor(league_long$stat_label, levels = stat_levels)
  player_long$stat_label <- factor(player_long$stat_label, levels = stat_levels)

  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type Distribution Profile")
  }

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Box plots show league distribution by pitch type; point shows ", pitcher,
      " | stat block: ", stat_block,
      " | min league pitches: ", min_league_pitches
    )
  }

  p <- ggplot(league_long, aes(x = .data[[pitch_type_col]], y = value)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.62, alpha = 0.45) +
    geom_point(
      data = player_long,
      aes(x = .data[[pitch_type_col]], y = value),
      size = 2.7,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.35) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Value"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.position = "none"
    )

  theme_team <- team_abbr
  if (isTRUE(use_team_theme)) {
    theme_team <- resolve_pitcher_theme_team(
      df = player_df,
      pitcher = pitcher,
      use_team_theme = TRUE,
      team_abbr = team_abbr,
      pitcher_col = pitcher_col,
      team_cols = c("team", team_col, "fielding_team")
    )
    p <- apply_mlb_team_plot_style(p, team = theme_team, base_size = base_size, apply_fill = FALSE)
  }

  if (isTRUE(show_values)) {
    p <- p +
      geom_text(
        data = player_long,
        aes(x = .data[[pitch_type_col]], y = value, label = label),
        vjust = -0.4,
        size = 2.7,
        lineheight = 0.9,
        inherit.aes = FALSE
      )
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_df,
    league_long = league_long,
    player_long = player_long,
    percentile_table = player_long %>% select(any_of(c(player_id_col, pitcher_col, "team", pitch_type_col, "stat", "raw_value", "percentile"))),
    stat_block = stat_block,
    kept_stats = requested_stats,
    team_theme = theme_team,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# 3. Pitch-type trend over year/month/date
# -----------------------------------------------------------------------------

plot_pitcher_pitchtype_trend <- function(views,
                                         pitcher,
                                         stat_block = "miss_bat",
                                         stats = NULL,
                                         always_include = "Usage_pct",
                                         time_unit = c("year", "month", "date"),
                                         group = "none",
                                         bat_hand = "none",
                                         pit_hand = "none",
                                         start_date = NULL,
                                         end_date = NULL,
                                         risp = "all",
                                         team_col = "fielding_team",
                                         pitch_type = NA,
                                         pitcher_col = "name",
                                         player_id_col = "matchup.pitcher.id",
                                         pitch_type_col = "pitch_type",
                                         min_pitches = 10,
                                         min_pitch_pct = NULL,
                                         min_bbe_pct = NULL,
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

  summary_df <- summarize_overall_pitcher_by_pitchtype(
    views = views_time,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = "player",
    start_date = start_date,
    end_date = end_date,
    risp = risp,
    group.by = ".time_group",
    team_col = team_col,
    pitch_type = pitch_type,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct
  )

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    stop("No pitcher trend summary rows were returned.", call. = FALSE)
  }

  summary_df <- summary_df %>%
    add_pitchtype_usage_pct(
      id_col = player_id_col,
      group.by = ".time_group",
      pitch_type_col = pitch_type_col,
      pitch_count_col = "Pitches",
      usage_col = "Usage_pct"
    )

  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = pitcher_stat_blocks(),
    always_include = always_include,
    available_stats = names(summary_df)
  )

  player_df <- summary_df %>%
    filter(.data[[pitcher_col]] == pitcher) %>%
    filter(is.na(min_pitches) | !"Pitches" %in% names(.) | Pitches >= min_pitches)

  if (nrow(player_df) == 0) {
    stop("No trend rows found for pitcher: ", pitcher, call. = FALSE)
  }

  plot_data <- player_df %>%
    pivot_longer(cols = all_of(requested_stats), names_to = "stat", values_to = "raw_value") %>%
    filter(!is.na(raw_value), !is.na(.data[[pitch_type_col]]), !is.na(.time_group)) %>%
    mutate(
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1))
    )

  usage_order <- player_df %>%
    group_by(.data[[pitch_type_col]]) %>%
    summarise(total_pitches = sum(Pitches, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_pitches)) %>%
    pull(all_of(pitch_type_col))

  plot_data[[pitch_type_col]] <- factor(plot_data[[pitch_type_col]], levels = usage_order)
  plot_data$stat_label <- factor(plot_data$stat_label, levels = vr_label_stats(requested_stats, stat_labels = stat_labels, percent_stats = NULL))

  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type Trends")
  }

  if (is.null(subtitle)) {
    subtitle <- paste0("Time unit: ", time_unit, " | stat block: ", stat_block, " | min pitches per point: ", min_pitches)
  }

  p <- ggplot(plot_data, aes(x = .time_group, y = value, color = .data[[pitch_type_col]], group = .data[[pitch_type_col]])) +
    geom_line(linewidth = 1, alpha = 0.9) +
    geom_point(size = 2, alpha = 0.85) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Value",
      color = "Pitch type"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )

  theme_team <- team_abbr
  if (isTRUE(use_team_theme)) {
    theme_team <- resolve_pitcher_theme_team(
      df = player_df,
      pitcher = pitcher,
      use_team_theme = TRUE,
      team_abbr = team_abbr,
      pitcher_col = pitcher_col,
      team_cols = c("team", team_col, "fielding_team")
    )
    p <- apply_mlb_team_plot_style(p, team = theme_team, base_size = base_size, apply_fill = FALSE)
  }

  if (isTRUE(print_plot)) print(p)

  list(
    summary_df = summary_df,
    player_df = player_df,
    plot_data = plot_data,
    stat_block = stat_block,
    kept_stats = requested_stats,
    time_unit = time_unit,
    team_theme = theme_team,
    plot = p
  )
}
