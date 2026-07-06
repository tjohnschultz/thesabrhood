# R/08_general_plotting.R
# General plotting utilities from your pasted 08_general_plotting script.
# Cleaner public names are used first; older names are kept as aliases.

plot_outlier_dots <- function(input,
                              x_axis,
                              y_axis,
                              label_col = "name",
                              threshold = 0.9,
                              point_color = "steelblue") {
  library(dplyr)
  library(ggplot2)
  library(ggrepel)

  if (!x_axis %in% names(input)) stop("x_axis not found: ", x_axis, call. = FALSE)
  if (!y_axis %in% names(input)) stop("y_axis not found: ", y_axis, call. = FALSE)
  if (!label_col %in% names(input)) stop("label_col not found: ", label_col, call. = FALSE)

  x_max <- max(input[[x_axis]], na.rm = TRUE)
  y_max <- max(input[[y_axis]], na.rm = TRUE)

  label_data <- input %>%
    filter(.data[[x_axis]] > x_max * threshold | .data[[y_axis]] > y_max * threshold) %>%
    mutate(
      custom_label = paste0(
        .data[[label_col]], "\n(", .data[[x_axis]], ", ", .data[[y_axis]], ")"
      )
    )

  ggplot(data = input, aes(x = .data[[x_axis]], y = .data[[y_axis]])) +
    geom_point(color = point_color, size = 3) +
    geom_text_repel(
      data = label_data,
      aes(label = custom_label),
      size = 3,
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "grey50"
    )
}

# Backward-compatible original name
dot_plot_outliers <- plot_outlier_dots

plot_rolling_rate <- function(data,
                              num_col,
                              den_col,
                              date_col = "game_date",
                              group_cols = NULL,
                              window = 3,
                              rate_name = "rolling_rate",
                              title = NULL) {
  rolling_data <- rolling_daily_rate(
    data = data,
    num_col = num_col,
    den_col = den_col,
    date_col = date_col,
    group_cols = group_cols,
    window = window,
    rate_name = rate_name
  )

  plot_rolling_trend(
    data = rolling_data,
    x_col = date_col,
    y_col = rate_name,
    title = title,
    y_is_pct = TRUE
  )
}

# Backward-compatible original name
rolling_rate_plot <- plot_rolling_rate

plot_stat_box_percentiles <- function(df,
                                      player,
                                      player_col,
                                      stats,
                                      stat_labels = NULL,
                                      title = NULL,
                                      subtitle = NULL,
                                      rate_scale = "auto",
                                      ncol = 2,
                                      label_size = 3.0,
                                      point_size = 3.2,
                                      base_size = 11) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)

  if (is.null(stat_labels)) {
    stat_labels <- stats
    names(stat_labels) <- stats
  }

  if (is.null(names(stat_labels)) || any(names(stat_labels) == "")) {
    names(stat_labels) <- stats
  }

  missing_stats <- setdiff(stats, names(df))
  if (length(missing_stats) > 0) {
    stop("Missing stat columns: ", paste(missing_stats, collapse = ", "), call. = FALSE)
  }

  if (!player_col %in% names(df)) {
    stop("player_col not found: ", player_col, call. = FALSE)
  }

  long_df <- df %>%
    select(all_of(c(player_col, stats))) %>%
    pivot_longer(cols = all_of(stats), names_to = "stat", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      stat = factor(stat, levels = stats),
      stat_label = unname(stat_labels[as.character(stat)]),
      stat_label = ifelse(is.na(stat_label), as.character(stat), stat_label)
    )

  max_val <- max(long_df$value, na.rm = TRUE)

  if (rate_scale == "auto") {
    rate_scale <- ifelse(max_val <= 1.5, "decimal", "percent")
  }

  format_rate <- function(x) {
    if (rate_scale == "decimal") scales::percent(x, accuracy = 0.1) else paste0(round(x, 1), "%")
  }

  x_label_fun <- function(x) {
    if (rate_scale == "decimal") scales::percent(x, accuracy = 1) else paste0(round(x, 0), "%")
  }

  player_df <- long_df %>%
    filter(.data[[player_col]] == player) %>%
    group_by(stat, stat_label) %>%
    slice(1) %>%
    ungroup()

  if (nrow(player_df) == 0) {
    stop("Player not found: ", player, call. = FALSE)
  }

  player_df <- player_df %>%
    rowwise() %>%
    mutate(
      percentile = mean(long_df$value[long_df$stat == stat] <= value) * 100,
      label = paste0(format_rate(value), "\n", round(percentile), "th pct")
    ) %>%
    ungroup()

  league_summary <- long_df %>%
    group_by(stat, stat_label) %>%
    summarise(league_median = median(value, na.rm = TRUE), .groups = "drop")

  if (is.null(title)) title <- paste0(player, " Stat Percentile Profile")
  if (is.null(subtitle)) subtitle <- "Boxplots show league distribution. Gold point marks selected player."

  ggplot(long_df, aes(x = value, y = "")) +
    geom_boxplot(width = 0.28, fill = "#ECECEC", color = "gray35", outlier.alpha = 0.35, outlier.size = 1.6) +
    geom_vline(data = league_summary, aes(xintercept = league_median), linetype = "dashed", color = "gray45", linewidth = 0.7, inherit.aes = FALSE) +
    geom_point(data = player_df, aes(x = value, y = ""), color = "#DAA520", size = point_size, inherit.aes = FALSE) +
    geom_label(data = player_df, aes(x = value, y = "", label = label), nudge_y = 0.18, fill = "#DAA520", color = "black", size = label_size, label.size = 0.25, inherit.aes = FALSE) +
    facet_wrap(~stat_label, scales = "free_x", ncol = ncol) +
    scale_x_continuous(labels = x_label_fun) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 6),
      plot.subtitle = element_text(size = base_size),
      strip.text = element_text(face = "bold", size = base_size),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.6, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

# Backward-compatible original name
plot_player_stat_box_facets <- plot_stat_box_percentiles
