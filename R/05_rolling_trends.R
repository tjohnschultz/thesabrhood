#Function 3.1.1
#Function to get the rolling, pivoted long dataframe from another dataframe. Conversion for rolling.
rolling_daily_rate <- function(data,
                               num_col,
                               den_col,
                               date_col = "game_date",
                               group_cols = NULL,
                               window = 7,
                               rate_name = "rolling_rate") {
  library(dplyr)
  library(slider)
  
  grouping <- c(group_cols, date_col)
  
  daily_tbl <- data %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      numerator   = sum(.data[[num_col]], na.rm = TRUE),
      denominator = sum(.data[[den_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(c(group_cols, date_col))))
  
  if (!is.null(group_cols) && length(group_cols) > 0) {
    daily_tbl <- daily_tbl %>%
      group_by(across(all_of(group_cols))) %>%
      mutate(
        rolling_num = slide_dbl(numerator, sum, .before = window - 1, .complete = TRUE),
        rolling_den = slide_dbl(denominator, sum, .before = window - 1, .complete = TRUE),
        !!rate_name := ifelse(rolling_den > 0, rolling_num / rolling_den, NA_real_)
      ) %>%
      ungroup()
  } else {
    daily_tbl <- daily_tbl %>%
      mutate(
        rolling_num = slide_dbl(numerator, sum, .before = window - 1, .complete = TRUE),
        rolling_den = slide_dbl(denominator, sum, .before = window - 1, .complete = TRUE),
        !!rate_name := ifelse(rolling_den > 0, rolling_num / rolling_den, NA_real_)
      )
  }
  
  daily_tbl
}

#Function 3.1.2
#Same as function 3.1.1, but with counting stats rather than a rate.
rolling_daily_stat <- function(data,
                               value_col,
                               date_col = "game_date",
                               group_cols = NULL,
                               window = 7,
                               stat_type = c("rate", "sum", "mean"),
                               den_col = NULL,
                               stat_name = "rolling_stat",
                               min_obs = window) {
  library(dplyr)
  library(slider)
  
  stat_type <- match.arg(stat_type)
  
  grouping <- c(group_cols, date_col)
  
  if (stat_type == "rate" && is.null(den_col)) {
    stop("For stat_type = 'rate', you must supply den_col.")
  }
  
  daily_tbl <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      den   = if (!is.null(den_col)) sum(.data[[den_col]], na.rm = TRUE) else NA_real_,
      n_obs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(grouping)))
  
  if (!is.null(group_cols) && length(group_cols) > 0) {
    
    daily_tbl <- daily_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::mutate(
        rolling_value = slider::slide_dbl(
          value,
          ~ sum(.x, na.rm = TRUE),
          .before = window - 1,
          .complete = FALSE
        ),
        rolling_den = if (stat_type == "rate") {
          slider::slide_dbl(
            den,
            ~ sum(.x, na.rm = TRUE),
            .before = window - 1,
            .complete = FALSE
          )
        } else {
          NA_real_
        },
        rolling_obs = slider::slide_dbl(
          n_obs,
          ~ sum(.x, na.rm = TRUE),
          .before = window - 1,
          .complete = FALSE
        )
      ) %>%
      dplyr::ungroup()
    
  } else {
    
    daily_tbl <- daily_tbl %>%
      dplyr::mutate(
        rolling_value = slider::slide_dbl(
          value,
          ~ sum(.x, na.rm = TRUE),
          .before = window - 1,
          .complete = FALSE
        ),
        rolling_den = if (stat_type == "rate") {
          slider::slide_dbl(
            den,
            ~ sum(.x, na.rm = TRUE),
            .before = window - 1,
            .complete = FALSE
          )
        } else {
          NA_real_
        },
        rolling_obs = slider::slide_dbl(
          n_obs,
          ~ sum(.x, na.rm = TRUE),
          .before = window - 1,
          .complete = FALSE
        )
      )
  }
  
  if (stat_type == "rate") {
    daily_tbl <- daily_tbl %>%
      dplyr::mutate(
        !!stat_name := dplyr::if_else(
          rolling_den > 0 & rolling_obs >= min_obs,
          rolling_value / rolling_den,
          NA_real_
        )
      )
  }
  
  if (stat_type == "sum") {
    daily_tbl <- daily_tbl %>%
      dplyr::mutate(
        !!stat_name := dplyr::if_else(
          rolling_obs >= min_obs,
          rolling_value,
          NA_real_
        )
      )
  }
  
  if (stat_type == "mean") {
    daily_tbl <- daily_tbl %>%
      dplyr::mutate(
        !!stat_name := dplyr::if_else(
          rolling_obs >= min_obs,
          rolling_value / rolling_obs,
          NA_real_
        )
      )
  }
  
  return(daily_tbl)
}

#Function 3.2.1
#Uses data from rolling_daily_rate or rolling_daily_stat to plot a rolling trend of a selected value.
plot_rolling_trend <- function(data,
                               x_col = "game_date",
                               y_col,
                               color_col = NULL,
                               title = NULL,
                               subtitle = NULL,
                               y_label = NULL,
                               color_label = NULL,
                               caption = NULL,
                               y_is_pct = FALSE,
                               show_points = TRUE,
                               line_width = 1,
                               point_size = 2) {
  library(ggplot2)
  
  # Defensive checks ---------------------------------------------------------
  if (!x_col %in% names(data)) {
    stop(paste0("x_col '", x_col, "' was not found in data."))
  }
  
  if (!y_col %in% names(data)) {
    stop(paste0("y_col '", y_col, "' was not found in data."))
  }
  
  if (!is.null(color_col) && !color_col %in% names(data)) {
    stop(paste0("color_col '", color_col, "' was not found in data."))
  }
  
  # Safe labels --------------------------------------------------------------
  final_y_label <- if (is.null(y_label)) y_col else y_label
  final_color_label <- if (is.null(color_label)) color_col else color_label
  
  # Base plot ---------------------------------------------------------------
  p <- ggplot(
    data,
    aes(
      x = .data[[x_col]],
      y = .data[[y_col]]
    )
  )
  
  # Single-line rolling trend -----------------------------------------------
  if (is.null(color_col)) {
    p <- p +
      geom_line(linewidth = line_width, na.rm = TRUE)
    
    if (show_points) {
      p <- p +
        geom_point(size = point_size, stroke = 1.75, na.rm = TRUE)
    }
  }
  
  # Multi-line rolling trend ------------------------------------------------
  if (!is.null(color_col)) {
    p <- p +
      geom_line(
        aes(
          color = .data[[color_col]],
          group = .data[[color_col]]
        ),
        linewidth = line_width,
        na.rm = TRUE
      )
    
    if (show_points) {
      p <- p +
        geom_point(
          aes(color = .data[[color_col]]),
          size = point_size,
          na.rm = TRUE
        )
    }
  }
  
  # Labels/theme ------------------------------------------------------------
  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Date",
      y = final_y_label,
      color = final_color_label
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0),
      plot.background = element_rect(
        color = "#003399",
        fill = "white",
        linewidth = 1.2
      )
    )
  
  # Percent axis ------------------------------------------------------------
  if (y_is_pct) {
    p <- p +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1)
      )
  }
  
  p
}

rolling_rate_plot <- function(data, 
                              num_col, 
                              den_col, 
                              date_col = "game_date", 
                              group_cols = NULL, 
                              window = 3, 
                              rate_name = "rolling_rate", 
                              title = NULL) {
  
  # Added group_cols = group_cols so the grouping variables actually get used
  rolling_data <- rolling_daily_rate(data, 
                                     num_col, 
                                     den_col, 
                                     date_col = date_col, 
                                     group_cols = group_cols, 
                                     window = window, 
                                     rate_name = rate_name)
  
  plot_rolling_trend(data = rolling_data, 
                     x_col = date_col, 
                     y_col = names(rolling_data)[ncol(rolling_data)], 
                     title = title)
}