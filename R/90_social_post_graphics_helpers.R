# =============================================================================
# 18_post_graphics_helpers.R
# Visual Revolution: EDA facet -> post-worthy graphic helpers
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# -----------------------------------------------------------------------------
# Safe helpers that work even if some VR helpers are not loaded
# -----------------------------------------------------------------------------

vr_percent_stats_safe <- function() {
  if (exists("vr_percent_stats", mode = "function")) {
    vr_percent_stats()
  } else {
    c(
      "K_pct", "BB_pct", "Whiff_pct", "Chase_pct", "Swing_pct",
      "Zone_pct", "Heart_pct", "Contact_pct", "ZContact_pct",
      "OContact_pct", "HardHit_pct", "barrel_pct", "Barrel_pct",
      "GB_pct", "FB_pct", "LD_pct", "PU_pct",
      "Pull_pct", "Middle_pct", "Oppo_pct",
      "Pull_FB_pct", "Middle_FB_pct", "Oppo_FB_pct",
      "HR_pct", "XBH_pct", "HRFB", "Usage_pct"
    )
  }
}

vr_label_stats_safe <- function(x) {
  if (exists("vr_label_stats", mode = "function")) {
    vr_label_stats(x)
  } else {
    x
  }
}

vr_format_value_safe <- function(value, stat, scale_percents = TRUE) {
  if (is.na(value)) return(NA_character_)
  
  pct_stats <- vr_percent_stats_safe()
  
  if (stat %in% pct_stats) {
    percent(value, accuracy = 0.1)
  } else if (stat %in% c("AVG", "OBP", "SLG", "OPS", "ISO", "BA")) {
    number(value, accuracy = 0.001)
  } else if (stat %in% c("EV", "maxEV", "velo", "spin", "IVB", "HB", "LA")) {
    number(value, accuracy = 0.1)
  } else {
    number(value, accuracy = 0.01)
  }
}

# -----------------------------------------------------------------------------
# Pull plot_data from a Visual Revolution output
# -----------------------------------------------------------------------------

vr_get_plot_data <- function(x) {
  if (inherits(x, "data.frame")) {
    return(x)
  }
  
  if (is.list(x) && "plot_data" %in% names(x)) {
    return(x$plot_data)
  }
  
  stop(
    "Input must be either a data frame or a Visual Revolution output with $plot_data.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Check available facets after making an EDA plot
# -----------------------------------------------------------------------------

vr_available_facets <- function(x) {
  df <- vr_get_plot_data(x)
  
  if (!"stat" %in% names(df)) {
    stop("plot_data does not contain a 'stat' column.", call. = FALSE)
  }
  
  df %>%
    distinct(
      stat,
      stat_label = if ("stat_label" %in% names(.)) stat_label else vr_label_stats_safe(stat)
    ) %>%
    arrange(stat_label)
}

# -----------------------------------------------------------------------------
# Filter EDA output to one or a few selected facets
# -----------------------------------------------------------------------------

vr_filter_facets <- function(x,
                             keep_stats = NULL,
                             keep_stat_labels = NULL) {
  df <- vr_get_plot_data(x)
  
  if (!"stat" %in% names(df)) {
    stop("plot_data does not contain a 'stat' column.", call. = FALSE)
  }
  
  if (!"stat_label" %in% names(df)) {
    df <- df %>%
      mutate(stat_label = vr_label_stats_safe(stat))
  }
  
  if (!is.null(keep_stats)) {
    df <- df %>% filter(stat %in% keep_stats)
  }
  
  if (!is.null(keep_stat_labels)) {
    df <- df %>% filter(stat_label %in% keep_stat_labels)
  }
  
  if (nrow(df) == 0) {
    stop("No rows left after filtering facets.", call. = FALSE)
  }
  
  df
}

# -----------------------------------------------------------------------------
# Post theme
# -----------------------------------------------------------------------------

vr_theme_post <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 6),
      plot.subtitle = element_text(size = base_size + 1, margin = margin(b = 10)),
      plot.caption = element_text(size = base_size - 3, color = "grey40"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey15"),
      strip.text = element_text(face = "bold", size = base_size),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

vr_apply_post_style <- function(p,
                                team_abbr = NULL,
                                base_size = 15,
                                apply_fill = TRUE) {
  p <- p + vr_theme_post(base_size = base_size)
  
  if (!is.null(team_abbr) && exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(
      p,
      team = team_abbr,
      base_size = base_size,
      apply_fill = apply_fill
    )
  }
  
  p
}

# -----------------------------------------------------------------------------
# Auto-detect chart structure
# -----------------------------------------------------------------------------

vr_guess_x_col <- function(df) {
  candidates <- c(
    "pitch_type", "pitch_family", "spray_direction",
    "comparison", "split", "name", "team", "year"
  )
  
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    stop(
      "Could not guess x_col. Pass x_col manually.",
      call. = FALSE
    )
  }
  
  hit[[1]]
}

vr_guess_group_col <- function(df, x_col) {
  candidates <- c("comparison", "split", "pitch_type", "pitch_family")
  
  candidates <- setdiff(candidates, x_col)
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    return(NULL)
  }
  
  hit[[1]]
}

vr_guess_time_col <- function(df) {
  candidates <- c(
    "game_date", "date", "month_date",
    "year_month", "time_value", "period", "year"
  )
  
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    stop(
      "Could not guess time_col. Pass time_col manually.",
      call. = FALSE
    )
  }
  
  hit[[1]]
}

# -----------------------------------------------------------------------------
# Main helper 1:
# Faceted EDA bar chart -> one/few-facet post bar chart
# -----------------------------------------------------------------------------

vr_post_comparison_bar <- function(x,
                                   keep_stats = NULL,
                                   keep_stat_labels = NULL,
                                   x_col = NULL,
                                   group_col = NULL,
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   team_abbr = NULL,
                                   base_size = 15,
                                   facet = NULL,
                                   ncol = NULL,
                                   label_bars = TRUE,
                                   flip = FALSE,
                                   scale_percents = TRUE) {
  df <- vr_filter_facets(
    x,
    keep_stats = keep_stats,
    keep_stat_labels = keep_stat_labels
  )
  
  x_col <- x_col %||% vr_guess_x_col(df)
  group_col <- group_col %||% vr_guess_group_col(df, x_col)
  
  if (!"raw_value" %in% names(df) && !"value" %in% names(df)) {
    stop("plot_data must contain either raw_value or value.", call. = FALSE)
  }
  
  if (!"raw_value" %in% names(df)) {
    df <- df %>% mutate(raw_value = value)
  }
  
  df <- df %>%
    mutate(
      stat_label = if ("stat_label" %in% names(.)) stat_label else vr_label_stats_safe(stat),
      value = if_else(
        scale_percents & stat %in% vr_percent_stats_safe(),
        raw_value * 100,
        raw_value
      ),
      value_label = if ("value_label" %in% names(.)) {
        value_label
      } else {
        mapply(vr_format_value_safe, raw_value, stat)
      }
    )
  
  n_stats <- n_distinct(df$stat)
  facet <- facet %||% n_stats > 1
  
  dodge <- position_dodge(width = 0.75)
  
  p <- ggplot(
    df,
    aes(x = .data[[x_col]], y = value)
  )
  
  if (!is.null(group_col)) {
    p <- p +
      geom_col(
        aes(fill = .data[[group_col]]),
        position = dodge,
        width = 0.68
      )
    
    if (label_bars) {
      p <- p +
        geom_text(
          aes(label = value_label, group = .data[[group_col]]),
          position = dodge,
          vjust = -0.25,
          size = 4,
          fontface = "bold"
        )
    }
  } else {
    p <- p +
      geom_col(width = 0.68)
    
    if (label_bars) {
      p <- p +
        geom_text(
          aes(label = value_label),
          vjust = -0.25,
          size = 4,
          fontface = "bold"
        )
    }
  }
  
  if (facet) {
    p <- p +
      facet_wrap(~ stat_label, scales = "free_y", ncol = ncol)
  }
  
  p <- p +
    labs(
      title = title %||% paste(unique(df$stat_label), collapse = " / "),
      subtitle = subtitle,
      caption = caption,
      x = NULL,
      y = NULL
    )
  
  if (flip) {
    p <- p + coord_flip()
  }
  
  p <- vr_apply_post_style(
    p,
    team_abbr = team_abbr,
    base_size = base_size,
    apply_fill = !is.null(group_col)
  )
  
  list(
    plot_data = df,
    plot = p,
    kept_stats = unique(df$stat),
    x_col = x_col,
    group_col = group_col
  )
}

# -----------------------------------------------------------------------------
# Main helper 2:
# Faceted EDA trend chart -> one/few-facet post trend chart
# -----------------------------------------------------------------------------

vr_post_trend_line <- function(x,
                               keep_stats = NULL,
                               keep_stat_labels = NULL,
                               time_col = NULL,
                               group_col = NULL,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               team_abbr = NULL,
                               base_size = 15,
                               facet = NULL,
                               ncol = NULL,
                               points = TRUE,
                               scale_percents = TRUE) {
  df <- vr_filter_facets(
    x,
    keep_stats = keep_stats,
    keep_stat_labels = keep_stat_labels
  )
  
  time_col <- time_col %||% vr_guess_time_col(df)
  group_col <- group_col %||% vr_guess_group_col(df, time_col)
  
  if (!"raw_value" %in% names(df) && !"value" %in% names(df)) {
    stop("plot_data must contain either raw_value or value.", call. = FALSE)
  }
  
  if (!"raw_value" %in% names(df)) {
    df <- df %>% mutate(raw_value = value)
  }
  
  df <- df %>%
    mutate(
      stat_label = if ("stat_label" %in% names(.)) stat_label else vr_label_stats_safe(stat),
      value = if_else(
        scale_percents & stat %in% vr_percent_stats_safe(),
        raw_value * 100,
        raw_value
      )
    )
  
  n_stats <- n_distinct(df$stat)
  facet <- facet %||% n_stats > 1
  
  p <- ggplot(
    df,
    aes(x = .data[[time_col]], y = value)
  )
  
  if (!is.null(group_col)) {
    p <- p +
      geom_line(aes(group = .data[[group_col]], color = .data[[group_col]]), linewidth = 1.3)
    
    if (points) {
      p <- p +
        geom_point(aes(color = .data[[group_col]]), size = 2.7)
    }
  } else {
    p <- p +
      geom_line(linewidth = 1.3)
    
    if (points) {
      p <- p + geom_point(size = 2.7)
    }
  }
  
  if (facet) {
    p <- p +
      facet_wrap(~ stat_label, scales = "free_y", ncol = ncol)
  }
  
  p <- p +
    labs(
      title = title %||% paste(unique(df$stat_label), collapse = " / "),
      subtitle = subtitle,
      caption = caption,
      x = NULL,
      y = NULL
    )
  
  p <- vr_apply_post_style(
    p,
    team_abbr = team_abbr,
    base_size = base_size,
    apply_fill = FALSE
  )
  
  list(
    plot_data = df,
    plot = p,
    kept_stats = unique(df$stat),
    time_col = time_col,
    group_col = group_col
  )
}

# -----------------------------------------------------------------------------
# Save social sizes
# -----------------------------------------------------------------------------

vr_save_social_plot <- function(plot,
                                filename,
                                size = c("square", "portrait", "wide"),
                                dpi = 300,
                                bg = "white") {
  size <- match.arg(size)
  
  dims <- switch(
    size,
    square = c(width = 8, height = 8),
    portrait = c(width = 8, height = 10),
    wide = c(width = 12, height = 6.75)
  )
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = dims[["width"]],
    height = dims[["height"]],
    dpi = dpi,
    bg = bg
  )
  
  invisible(filename)
}