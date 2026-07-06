#Function 5.1.1
summarize_batter_split_pair <- function(views,
                                        split_a = list(),
                                        split_b = list(),
                                        split_a_name = "A",
                                        split_b_name = "B",
                                        level = "player",
                                        team_col = "batting_team",
                                        group.by = NULL,
                                        stats = c("OPS", "K_pct", "BB_pct", "Whiff_pct", "Chase_pct", "Contact_pct", "HardHit_pct", "barrel_pct", "EV", "LA", "HR", "Swing_pct"),
                                        min_pa_each = NULL,
                                        min_pitches_each = NULL,
                                        min_bbe_each = NULL,
                                        diff_direction = "A_minus_B") {
  library(dplyr)
  level <- normalize_level(level)
  
  common <- list(views = views, level = level, team_col = team_col, group.by = group.by)
  a <- do.call(summarize_overall_batter, modifyList(common, split_a)) %>%
    filter_min_counts(min_pa = min_pa_each, min_pitches = min_pitches_each, min_bbe = min_bbe_each)
  b <- do.call(summarize_overall_batter, modifyList(common, split_b)) %>%
    filter_min_counts(min_pa = min_pa_each, min_pitches = min_pitches_each, min_bbe = min_bbe_each)
  
  id_col <- if (level == "team") team_col else "matchup.batter.id"
  compare_split_summaries(a, b, split_a_name, split_b_name, id_col = id_col, name_col = "name", team_col = "team", stats = stats, diff_direction = diff_direction)
}

#Function 5.1.2
summarize_pitcher_split_pair <- function(views,
                                         split_a = list(),
                                         split_b = list(),
                                         split_a_name = "A",
                                         split_b_name = "B",
                                         level = "player",
                                         team_col = "fielding_team",
                                         group.by = NULL,
                                         stats = c("OPS", "K_pct", "BB_pct", "Whiff_pct",
                                                   "Chase_pct", "Contact_pct", "HardHit_pct",
                                                   "barrel_pct", "EV", "LA", "HR", "Swing_pct"),
                                         min_pa_each = NULL,
                                         min_pitches_each = NULL,
                                         min_bbe_each = NULL,
                                         diff_direction = "A_minus_B",
                                         join_type = "full") {
  library(dplyr)
  
  level <- normalize_level(level)
  
  if (is.null(views)) {
    stop("`views` is NULL.", call. = FALSE)
  }
  
  if (!is.list(views)) {
    stop("`views` must be a list of data frames.", call. = FALSE)
  }
  
  common <- list(
    views = views,
    level = level,
    team_col = team_col,
    group.by = group.by
  )
  
  make_split <- function(split, split_label) {
    raw <- do.call(
      summarize_overall_pitcher,
      modifyList(common, split)
    )
    
    if (is.null(raw)) {
      stop(
        "summarize_overall_pitcher() returned NULL for split: ",
        split_label,
        ". This usually means a required table inside `views` is missing, empty, ",
        "or the date filters removed all usable data.",
        call. = FALSE
      )
    }
    
    if (!is.data.frame(raw)) {
      stop(
        "summarize_overall_pitcher() did not return a data frame for split: ",
        split_label,
        ". Returned class: ",
        paste(class(raw), collapse = ", "),
        call. = FALSE
      )
    }
    
    if (nrow(raw) == 0) {
      stop(
        "summarize_overall_pitcher() returned 0 rows for split: ",
        split_label,
        ". Check your date filter or sample-size filters.",
        call. = FALSE
      )
    }
    
    filtered <- filter_min_counts(
      raw,
      min_pa = min_pa_each,
      min_pitches = min_pitches_each,
      min_bbe = min_bbe_each
    )
    
    if (is.null(filtered)) {
      stop(
        "filter_min_counts() returned NULL for split: ",
        split_label,
        ". It should return a data frame, even if there are 0 rows.",
        call. = FALSE
      )
    }
    
    if (!is.data.frame(filtered)) {
      stop(
        "filter_min_counts() did not return a data frame for split: ",
        split_label,
        ". Returned class: ",
        paste(class(filtered), collapse = ", "),
        call. = FALSE
      )
    }
    
    if (nrow(filtered) == 0) {
      stop(
        "After applying min_pa_each/min_pitches_each/min_bbe_each, split ",
        split_label,
        " has 0 rows. Try lowering sample filters.",
        call. = FALSE
      )
    }
    
    filtered
  }
  
  a <- make_split(split_a, split_a_name)
  b <- make_split(split_b, split_b_name)
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  
  if (!id_col %in% names(a)) {
    stop("ID column `", id_col, "` was not found in split ", split_a_name, ".", call. = FALSE)
  }
  
  if (!id_col %in% names(b)) {
    stop("ID column `", id_col, "` was not found in split ", split_b_name, ".", call. = FALSE)
  }
  
  compare_split_summaries(
    group_a = a,
    group_b = b,
    group_a_name = split_a_name,
    group_b_name = split_b_name,
    id_col = id_col,
    name_col = "name",
    team_col = "team",
    group_cols = group.by,
    stats = stats,
    diff_direction = diff_direction,
    join_type = join_type
  )
}

#Function 5.2.1
compare_expected_contact_splits_general <- function(raw_pbp,
                                                    split_a = list(),
                                                    split_b = list(),
                                                    split_a_name = "A",
                                                    split_b_name = "B",
                                                    contact_matrix_path = "contact_matrix.csv",
                                                    group_type = "player",
                                                    perspective = "batter",
                                                    min_bbe_each = 20,
                                                    already_enhanced = FALSE,
                                                    diff_direction = "A_minus_B") {
  library(dplyr)
  
  pbp_use <- if (already_enhanced) raw_pbp else enhance_pbp(raw_pbp)
  contact_matrix <- load_contact_matrix(contact_matrix_path)
  
  a_pbp <- filter_pbp_for_split(pbp_use, split_a)
  b_pbp <- filter_pbp_for_split(pbp_use, split_b)
  
  a <- calc_expected_contact_stats(a_pbp, contact_matrix, group_type = group_type, perspective = perspective) %>%
    filter_min_counts(min_bbe = min_bbe_each)
  b <- calc_expected_contact_stats(b_pbp, contact_matrix, group_type = group_type, perspective = perspective) %>%
    filter_min_counts(min_bbe = min_bbe_each)
  
  id_col <- dplyr::case_when(
    group_type == "player" ~ "player_id",
    group_type == "team" ~ "team",
    TRUE ~ NA_character_
  )
  if (is.na(id_col)) stop("Use group_type = 'player' or 'team' for split comparisons.")
  
  compare_split_summaries(
    group_a = a,
    group_b = b,
    group_a_name = split_a_name,
    group_b_name = split_b_name,
    id_col = id_col,
    name_col = if (group_type == "player") "name" else "team",
    team_col = "team",
    stats = c("BBE", "BA_on_contact", "SLG_on_contact", "xBA", "xSLG", "BA_v_exp", "SLG_v_exp"),
    keep_counts = "BBE",
    diff_direction = diff_direction
  )
}

comparison_plot <- function(split_df,
                            pitcher,
                            stats = c("OPS", "K_pct", "Whiff_pct", "Chase_pct", "Contact_pct"),
                            split_names = c("v_LHH", "v_RHH"),
                            pitcher_col = "name",
                            percent_stats = c("K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                              "Contact_pct", "HardHit_pct", "barrel_pct",
                                              "Swing_pct"),
                            scale_percents = TRUE,
                            include_diff = TRUE,
                            diff_label = "diff",
                            show_values = TRUE) {
  library(dplyr)
  library(ggplot2)
  
  if (!pitcher_col %in% names(split_df)) {
    stop("pitcher_col '", pitcher_col, "' was not found in split_df.")
  }
  
  pitcher_row <- split_df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(pitcher_row) == 0) {
    stop("No pitcher found matching: ", pitcher)
  }
  
  if (nrow(pitcher_row) > 1) {
    warning("More than one row matched pitcher = '", pitcher, "'. Using the first match.")
    pitcher_row <- pitcher_row %>% slice(1)
  }
  
  plot_data <- lapply(stats, function(stat) {
    value_cols <- paste0(stat, "_", split_names)
    
    if (include_diff) {
      value_cols <- c(value_cols, paste0(stat, "_diff"))
    }
    
    value_cols <- value_cols[value_cols %in% names(pitcher_row)]
    
    if (length(value_cols) == 0) {
      return(NULL)
    }
    
    data.frame(
      stat = stat,
      comparison = value_cols,
      value = as.numeric(pitcher_row[1, value_cols]),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
    
  if (nrow(plot_data) == 0) {
    stop("None of the requested stat columns were found. Check your `stats` and `split_names` arguments.")
  }
  
  plot_data <- plot_data %>%
    rowwise() %>%
    mutate(
      comparison = case_when(
        comparison == paste0(stat, "_diff") ~ diff_label,
        TRUE ~ substr(comparison, nchar(stat) + 2, nchar(comparison))
      )
    ) %>%
    ungroup() %>%
    mutate(
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, value * 100, value),
      stat_label = if_else(is_percent, paste0(stat, " (%)"), stat),
      comparison = factor(comparison, levels = c(split_names, diff_label))
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      value_label = case_when(
        is_percent ~ paste0(round(value, 1), "%"),
        stat %in% c("EV", "LA") ~ as.character(round(value, 1)),
        TRUE ~ as.character(round(value, 3))
      )
    )
  
  ggplot(plot_data, aes(x = comparison, y = value)) +
    geom_col(width = 0.7, fill = "black", color = "black") +
    geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.5) +
    facet_wrap(~ stat_label, scales = "free_y") +
    labs(
      title = paste0(pitcher, " Split Comparison"),
      subtitle = paste0(paste(split_names, collapse = " vs "), " with ", diff_label),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    {
      if (show_values) {
        geom_text(
          aes(label = value_label),
          vjust = ifelse(plot_data$value >= 0, 1.25, 1.25),
          size = 3.5, 
          color = "white"
        )
      }
    }
}

filter_views_to_pitcher <- function(views, pitcher) {
  library(dplyr)
  library(purrr)
  
  pitcher_cols <- c(
    "matchup.pitcher.fullName",
    "pitcher_name",
    "pitcher",
    "name"
  )
  
  purrr::map(views, function(df) {
    if (is.null(df)) return(NULL)
    if (!is.data.frame(df)) return(df)
    
    found_col <- pitcher_cols[pitcher_cols %in% names(df)][1]
    
    if (is.na(found_col)) {
      return(df)
    }
    
    df %>%
      filter(.data[[found_col]] == pitcher)
  })
}

plot_pitcher_split_bars <- function(views,
                                    pitcher,
                                    split_a = list(),
                                    split_b = list(),
                                    split_a_name = "A",
                                    split_b_name = "B",
                                    level = "player",
                                    team_col = "fielding_team",
                                    group.by = NULL,
                                    stats = c("OPS", "K_pct", "Whiff_pct", "Chase_pct", "Contact_pct"),
                                    min_pa_each = NULL,
                                    min_pitches_each = NULL,
                                    min_bbe_each = NULL,
                                    diff_direction = "A_minus_B",
                                    pitcher_col = "name",
                                    percent_stats = c("K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                                      "Contact_pct", "HardHit_pct", "barrel_pct",
                                                      "Swing_pct"),
                                    scale_percents = TRUE,
                                    include_diff = TRUE,
                                    diff_label = "Diff",
                                    show_values = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # 1. Create split-comparison table using your existing function
  split_df <- summarize_pitcher_split_pair(
    views = views,
    split_a = split_a,
    split_b = split_b,
    split_a_name = split_a_name,
    split_b_name = split_b_name,
    level = level,
    team_col = team_col,
    group.by = group.by,
    stats = stats,
    min_pa_each = min_pa_each,
    min_pitches_each = min_pitches_each,
    min_bbe_each = min_bbe_each,
    diff_direction = diff_direction
  )
  
  # 2. Make sure pitcher column exists
  if (!pitcher_col %in% names(split_df)) {
    stop("pitcher_col '", pitcher_col, "' was not found in the split summary table.")
  }
  
  # 3. Filter to selected pitcher
  pitcher_row <- split_df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(pitcher_row) == 0) {
    stop("No pitcher found matching: ", pitcher)
  }
  
  if (nrow(pitcher_row) > 1) {
    warning("More than one row matched pitcher = '", pitcher, "'. Using the first match.")
    pitcher_row <- pitcher_row %>% slice(1)
  }
  
  # 4. Build expected columns
  split_names <- c(split_a_name, split_b_name)
  
  plot_data <- lapply(stats, function(stat) {
    value_cols <- paste0(stat, "_", split_names)
    
    if (include_diff) {
      value_cols <- c(value_cols, paste0(stat, "_diff"))
    }
    
    value_cols <- value_cols[value_cols %in% names(pitcher_row)]
    
    if (length(value_cols) == 0) {
      return(NULL)
    }
    
    data.frame(
      stat = stat,
      comparison = value_cols,
      value = as.numeric(pitcher_row[1, value_cols]),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
  
  if (nrow(plot_data) == 0) {
    stop("None of the requested stat columns were found. Check your `stats` names.")
  }
  
  # 5. Clean comparison labels
  plot_data <- plot_data %>%
    rowwise() %>%
    mutate(
      comparison = case_when(
        comparison == paste0(stat, "_diff") ~ diff_label,
        TRUE ~ substr(comparison, nchar(stat) + 2, nchar(comparison))
      )
    ) %>%
    ungroup()
  
  # 6. Scale percentage stats up by 100
  plot_data <- plot_data %>%
    mutate(
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, value * 100, value),
      stat_label = if_else(is_percent, paste0(stat, " (%)"), stat),
      comparison = factor(comparison, levels = c(split_names, diff_label))
    ) %>%
    filter(!is.na(value))
  
  # 7. Value labels
  plot_data <- plot_data %>%
    mutate(
      value_label = case_when(
        is_percent ~ paste0(round(value, 1), "%"),
        stat %in% c("EV", "LA") ~ as.character(round(value, 1)),
        TRUE ~ as.character(round(value, 3))
      )
    )
  
  # 8. Plot
  p <- ggplot(plot_data, aes(x = comparison, y = value)) +
    geom_col(width = 0.7, fill = "black", color = "black") +
    geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.5) +
    facet_wrap(~ stat_label, scales = "free_y") +
    labs(
      title = paste0(pitcher, " Split Comparison"),
      subtitle = paste0(split_a_name, " vs ", split_b_name, " with ", diff_label),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  if (show_values) {
    p <- p +
      geom_text(
        aes(label = value_label),
        vjust = ifelse(plot_data$value >= 0, 1.25, 1.25),
        size = 3.5, 
        color = "white"
      )
  }
  
  p
}


compare_split_summaries <- function(group_a,
                                    group_b,
                                    group_a_name = "A",
                                    group_b_name = "B",
                                    id_col,
                                    name_col = "name",
                                    team_col = "team",
                                    group_cols = NULL,
                                    stats = c("OPS", "K_pct", "BB_pct", "Whiff_pct",
                                              "Chase_pct", "Contact_pct", "HardHit_pct",
                                              "barrel_pct", "EV", "LA"),
                                    keep_counts = c("PA", "Pitches", "BBE"),
                                    diff_direction = c("A_minus_B", "B_minus_A"),
                                    join_type = c("full", "inner")) {
  library(dplyr)
  
  diff_direction <- match.arg(diff_direction)
  join_type <- match.arg(join_type)
  
  if (!id_col %in% names(group_a) || !id_col %in% names(group_b)) {
    stop("id_col must exist in both group_a and group_b.")
  }
  
  if (!is.null(group_cols)) {
    missing_a <- setdiff(group_cols, names(group_a))
    missing_b <- setdiff(group_cols, names(group_b))
    
    if (length(missing_a) > 0) {
      stop("These group_cols are missing from group_a: ", paste(missing_a, collapse = ", "))
    }
    
    if (length(missing_b) > 0) {
      stop("These group_cols are missing from group_b: ", paste(missing_b, collapse = ", "))
    }
  }
  
  key_cols <- unique(c(id_col, group_cols))
  
  stats <- intersect(stats, intersect(names(group_a), names(group_b)))
  keep_counts <- intersect(keep_counts, union(names(group_a), names(group_b)))
  
  base_cols <- unique(c(
    key_cols,
    name_col,
    team_col,
    keep_counts,
    stats
  ))
  
  a <- group_a %>%
    select(any_of(base_cols)) %>%
    rename_with(
      ~ paste0(.x, "_", group_a_name),
      .cols = -all_of(key_cols)
    )
  
  b <- group_b %>%
    select(any_of(base_cols)) %>%
    rename_with(
      ~ paste0(.x, "_", group_b_name),
      .cols = -all_of(key_cols)
    )
  
  out <- if (join_type == "inner") {
    inner_join(a, b, by = key_cols)
  } else {
    full_join(a, b, by = key_cols)
  }
  
  a_name <- paste0(name_col, "_", group_a_name)
  b_name <- paste0(name_col, "_", group_b_name)
  a_team <- paste0(team_col, "_", group_a_name)
  b_team <- paste0(team_col, "_", group_b_name)
  
  if (a_name %in% names(out) && b_name %in% names(out)) {
    out <- out %>%
      mutate(name = coalesce(.data[[b_name]], .data[[a_name]]))
  }
  
  if (a_team %in% names(out) && b_team %in% names(out)) {
    out <- out %>%
      mutate(team = coalesce(.data[[b_team]], .data[[a_team]]))
  }
  
  for (stat in stats) {
    a_col <- paste0(stat, "_", group_a_name)
    b_col <- paste0(stat, "_", group_b_name)
    diff_col <- paste0(stat, "_diff")
    
    if (a_col %in% names(out) && b_col %in% names(out)) {
      if (diff_direction == "A_minus_B") {
        out <- out %>%
          mutate("{diff_col}" := .data[[a_col]] - .data[[b_col]])
      } else {
        out <- out %>%
          mutate("{diff_col}" := .data[[b_col]] - .data[[a_col]])
      }
    }
  }
  
  front <- unique(c(id_col, group_cols, "name", "team"))
  
  out %>%
    relocate(any_of(front))
}

plot_pitcher_pitchtype_split_bars <- function(split_df,
                                              pitcher,
                                              stats = c("OPS", "Whiff_pct", "Chase_pct", "HardHit_pct"),
                                              split_names = c("pre_may", "post_may"),
                                              pitch_type_col = "details.type.description",
                                              pitcher_col = "name",
                                              percent_stats = c("K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                                                "Contact_pct", "HardHit_pct", "barrel_pct",
                                                                "Swing_pct", "Zone_pct"),
                                              scale_percents = TRUE,
                                              include_diff = TRUE,
                                              diff_label = "Diff",
                                              plot_mode = c("values_plus_diff", "values_only", "diff_only"),
                                              facet_scales = "free_y",
                                              show_values = TRUE) {
  library(dplyr)
  library(ggplot2)
  
  plot_mode <- match.arg(plot_mode)
  
  if (!pitcher_col %in% names(split_df)) {
    stop("pitcher_col '", pitcher_col, "' was not found in split_df.")
  }
  
  if (!pitch_type_col %in% names(split_df)) {
    stop("pitch_type_col '", pitch_type_col, "' was not found in split_df.")
  }
  
  pitcher_df <- split_df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(pitcher_df) == 0) {
    stop("No pitcher found matching: ", pitcher)
  }
  
  plot_data <- lapply(stats, function(stat) {
    value_cols <- paste0(stat, "_", split_names)
    
    if (include_diff) {
      value_cols <- c(value_cols, paste0(stat, "_diff"))
    }
    
    value_cols <- value_cols[value_cols %in% names(pitcher_df)]
    
    if (length(value_cols) == 0) {
      return(NULL)
    }
    
    lapply(value_cols, function(col) {
      data.frame(
        pitch_type = pitcher_df[[pitch_type_col]],
        stat = stat,
        comparison = col,
        value = as.numeric(pitcher_df[[col]]),
        stringsAsFactors = FALSE
      )
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
  
  if (nrow(plot_data) == 0) {
    stop("None of the requested stat columns were found. Check `stats` and `split_names`.")
  }
  
  plot_data <- plot_data %>%
    rowwise() %>%
    mutate(
      comparison = case_when(
        comparison == paste0(stat, "_diff") ~ diff_label,
        TRUE ~ substr(comparison, nchar(stat) + 2, nchar(comparison))
      )
    ) %>%
    ungroup()
  
  if (plot_mode == "values_only") {
    plot_data <- plot_data %>%
      filter(comparison %in% split_names)
  }
  
  if (plot_mode == "diff_only") {
    plot_data <- plot_data %>%
      filter(comparison == diff_label)
  }
  
  plot_data <- plot_data %>%
    mutate(
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, value * 100, value),
      stat_label = if_else(is_percent, paste0(stat, " (%)"), stat),
      comparison = factor(comparison, levels = c(split_names, diff_label)),
      value_label = case_when(
        is_percent ~ paste0(round(value, 1), "%"),
        stat %in% c("EV", "LA") ~ as.character(round(value, 1)),
        TRUE ~ as.character(round(value, 3))
      )
    ) %>%
    filter(!is.na(value))
  
  p <- ggplot(
    plot_data,
    aes(x = pitch_type, y = value, fill = comparison)
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.4,
      alpha = 0.5
    ) +
    facet_wrap(~ stat_label, scales = facet_scales) +
    labs(
      title = paste0(pitcher, " Pitch-Type Split Changes"),
      subtitle = paste0(paste(split_names, collapse = " vs "), ifelse(include_diff, " with diff", "")),
      x = NULL,
      y = "Value",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1)
    )
  
  if (show_values) {
    p <- p +
      geom_text(
        aes(label = value_label),
        position = position_dodge(width = 0.8),
        vjust = -0.35,
        size = 3
      )
  }
  
  p
}

# =============================================================================
# PATCHED HITTER SPLIT BARS
# Fixes: "Insufficient values in manual scale. 3 needed but only 2 provided."
# Reason: split plots usually have 3 fill levels: A, B, Change
# =============================================================================

vr_mlb_split_team_colors <- function(team_abbr = NULL) {
  # Primary / secondary colors for common MLB team abbreviations.
  # Fallback keeps the function working even if a team is missing.
  colors <- list(
    BOS = c("#BD3039", "#0C2340"),
    NYY = c("#0C2340", "#C4CED4"),
    BAL = c("#DF4601", "#000000"),
    TB  = c("#092C5C", "#8FBCE6"),
    TOR = c("#134A8E", "#E8291C"),
    CLE = c("#00385D", "#E50022"),
    DET = c("#0C2340", "#FA4616"),
    KC  = c("#004687", "#BD9B60"),
    MIN = c("#002B5C", "#D31145"),
    CWS = c("#27251F", "#C4CED4"),
    HOU = c("#002D62", "#EB6E1F"),
    LAA = c("#BA0021", "#003263"),
    OAK = c("#003831", "#EFB21E"),
    SEA = c("#0C2C56", "#005C5C"),
    TEX = c("#003278", "#C0111F"),
    ATL = c("#13274F", "#CE1141"),
    MIA = c("#00A3E0", "#EF3340"),
    NYM = c("#002D72", "#FF5910"),
    PHI = c("#E81828", "#002D72"),
    WSH = c("#AB0003", "#14225A"),
    CHC = c("#0E3386", "#CC3433"),
    CIN = c("#C6011F", "#000000"),
    MIL = c("#12284B", "#FFC52F"),
    PIT = c("#27251F", "#FDB827"),
    STL = c("#C41E3A", "#0C2340"),
    ARI = c("#A71930", "#E3D4AD"),
    COL = c("#33006F", "#C4CED4"),
    LAD = c("#005A9C", "#EF3E42"),
    SD  = c("#2F241D", "#FFC425"),
    SF  = c("#FD5A1E", "#27251F")
  )
  
  team_abbr <- toupper(team_abbr %||% "")
  
  if (team_abbr %in% names(colors)) {
    colors[[team_abbr]]
  } else {
    c("#111111", "#777777")
  }
}

vr_split_fill_values <- function(comparison_levels,
                                 team_abbr = NULL,
                                 diff_label = "Change",
                                 diff_color = "#444444") {
  team_cols <- vr_mlb_split_team_colors(team_abbr)
  
  vals <- rep(team_cols, length.out = length(comparison_levels))
  names(vals) <- comparison_levels
  
  # Make the diff/change bar neutral so A and B keep the team identity.
  if (diff_label %in% names(vals)) {
    vals[[diff_label]] <- diff_color
  }
  
  vals
}

plot_hitter_split_bars <- function(views,
                                   hitter,
                                   split_a = list(),
                                   split_b = list(),
                                   split_a_name = "A",
                                   split_b_name = "B",
                                   level = "player",
                                   team_col = "batting_team",
                                   group.by = NULL,
                                   stats = c("OPS", "SLG", "K_pct", "BB_pct", "Whiff_pct",
                                             "Chase_pct", "Contact_pct", "HardHit_pct"),
                                   stat_block = NULL,
                                   min_pa_each = NULL,
                                   min_pitches_each = NULL,
                                   min_bbe_each = NULL,
                                   diff_direction = "A_minus_B",
                                   hitter_col = "name",
                                   percent_stats = c("K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                                     "Contact_pct", "HardHit_pct", "barrel_pct",
                                                     "Swing_pct", "Zone_pct", "ZSwing_pct",
                                                     "OContact_pct", "ZContact_pct",
                                                     "GB_pct", "FB_pct", "LD_pct", "PU_pct",
                                                     "Pull_pct", "Middle_pct", "Oppo_pct",
                                                     "Pull_FB_pct", "Middle_FB_pct", "Oppo_FB_pct",
                                                     "HR_pct", "XBH_pct", "HRFB"),
                                   scale_percents = TRUE,
                                   include_diff = TRUE,
                                   diff_label = "Change",
                                   show_values = TRUE,
                                   use_team_theme = TRUE,
                                   team_abbr = NULL,
                                   base_size = 13,
                                   facet_ncol = NULL,
                                   print_plot = TRUE,
                                   diff_color = "#444444") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  if (!is.null(stat_block)) {
    if (!exists("hitter_stat_blocks", mode = "function")) {
      stop("stat_block was supplied, but hitter_stat_blocks() was not found.", call. = FALSE)
    }
    
    blocks <- hitter_stat_blocks()
    
    if (!stat_block %in% names(blocks)) {
      stop(
        "Unknown stat_block: ", stat_block,
        "\nAvailable blocks: ", paste(names(blocks), collapse = ", "),
        call. = FALSE
      )
    }
    
    stats <- blocks[[stat_block]]
  }
  
  split_df <- summarize_batter_split_pair(
    views = views,
    split_a = split_a,
    split_b = split_b,
    split_a_name = split_a_name,
    split_b_name = split_b_name,
    level = level,
    team_col = team_col,
    group.by = group.by,
    stats = stats,
    min_pa_each = min_pa_each,
    min_pitches_each = min_pitches_each,
    min_bbe_each = min_bbe_each,
    diff_direction = diff_direction
  )
  
  if (!hitter_col %in% names(split_df)) {
    stop("hitter_col '", hitter_col, "' was not found in the split summary table.", call. = FALSE)
  }
  
  hitter_row <- split_df %>%
    filter(.data[[hitter_col]] == hitter)
  
  if (nrow(hitter_row) == 0) {
    stop("No hitter found matching: ", hitter, call. = FALSE)
  }
  
  if (nrow(hitter_row) > 1) {
    warning("More than one row matched hitter = '", hitter, "'. Using the first match.", call. = FALSE)
    hitter_row <- hitter_row %>% slice(1)
  }
  
  split_names <- c(split_a_name, split_b_name)
  
  plot_data <- lapply(stats, function(stat) {
    value_cols <- paste0(stat, "_", split_names)
    
    if (include_diff) {
      value_cols <- c(value_cols, paste0(stat, "_diff"))
    }
    
    value_cols <- value_cols[value_cols %in% names(hitter_row)]
    
    if (length(value_cols) == 0) {
      return(NULL)
    }
    
    data.frame(
      stat = stat,
      comparison = value_cols,
      raw_value = as.numeric(hitter_row[1, value_cols]),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
  
  if (nrow(plot_data) == 0) {
    stop("None of the requested stat columns were found. Check your `stats` names.", call. = FALSE)
  }
  
  plot_data <- plot_data %>%
    rowwise() %>%
    mutate(
      comparison = case_when(
        comparison == paste0(stat, "_diff") ~ diff_label,
        TRUE ~ substr(comparison, nchar(stat) + 2, nchar(comparison))
      )
    ) %>%
    ungroup()
  
  comparison_levels <- if (include_diff) {
    c(split_names, diff_label)
  } else {
    split_names
  }
  
  plot_data <- plot_data %>%
    mutate(
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, raw_value * 100, raw_value),
      stat_label = if (exists("vr_label_stats", mode = "function")) {
        vr_label_stats(stat)
      } else {
        if_else(is_percent, paste0(stat, " (%)"), stat)
      },
      comparison = factor(comparison, levels = comparison_levels)
    ) %>%
    filter(!is.na(value))
  
  plot_data <- plot_data %>%
    mutate(
      value_label = case_when(
        is_percent ~ paste0(round(value, 1), "%"),
        stat %in% c("AVG", "OBP", "SLG", "OPS", "ISO", "BA") ~ sprintf("%.3f", value),
        stat %in% c("EV", "maxEV", "LA") ~ as.character(round(value, 1)),
        TRUE ~ as.character(round(value, 3))
      ),
      label_vjust = if_else(value >= 0, 1.25, -0.35)
    )
  
  # Resolve team for theme/colors
  resolved_team <- team_abbr
  
  if (is.null(resolved_team) && "team" %in% names(hitter_row)) {
    resolved_team <- unique(na.omit(as.character(hitter_row$team)))[1]
  }
  
  fill_values <- vr_split_fill_values(
    comparison_levels = comparison_levels,
    team_abbr = resolved_team,
    diff_label = diff_label,
    diff_color = diff_color
  )
  
  p <- ggplot(plot_data, aes(x = comparison, y = value, fill = comparison)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.25) +
    geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.5) +
    facet_wrap(~ stat_label, scales = "free_y", ncol = facet_ncol) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    labs(
      title = paste0(hitter, " Split Comparison"),
      subtitle = paste0(
        split_a_name, " vs ", split_b_name,
        if (include_diff) paste0(" with ", diff_label) else ""
      ),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none"
    )
  
  if (show_values) {
    p <- p +
      geom_text(
        aes(label = value_label, vjust = label_vjust),
        size = 3.5,
        color = "white",
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  # Important:
  # apply_fill = FALSE prevents team theme from adding a 2-color fill scale
  # on top of our 3-level split scale.
  if (isTRUE(use_team_theme) &&
      !is.null(resolved_team) &&
      !is.na(resolved_team) &&
      exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(
      p,
      team = resolved_team,
      base_size = base_size,
      apply_fill = FALSE
    )
    
    # Re-apply our 3-level fill scale after team styling.
    p <- p + scale_fill_manual(values = fill_values, drop = FALSE)
  }
  
  out <- list(
    split_df = split_df,
    hitter_row = hitter_row,
    plot_data = plot_data,
    plot = p,
    kept_stats = unique(plot_data$stat),
    fill_values = fill_values
  )
  
  if (isTRUE(print_plot)) print(p)
  
  out
}