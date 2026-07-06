# -----------------------------------------------------------------------------
# R/10_pitchtype_viz_pack.R
# Clean pitch-type visualization module
#
# Contains:
#   1. Common helpers
#   2. MLB team theme helpers
#   3. Pitch-type split viz pack
#   4. Pitch-type league profile
#
# Requires existing project functions:
#   - summarize_overall_pitcher_by_pitchtype()
#   - compare_split_summaries()
#   - normalize_level() [optional]
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Common helpers
# -----------------------------------------------------------------------------

clean_group_by_for_split <- function(group.by = NULL) {
  if (length(group.by) == 1 && is.na(group.by)) return(NULL)
  group.by
}

filter_pitchtype_min_counts <- function(df,
                                        min_pitches = NULL,
                                        min_bbe = NULL) {
  library(dplyr)
  
  if (is.null(df)) {
    stop("filter_pitchtype_min_counts() received NULL.", call. = FALSE)
  }
  
  if (!is.data.frame(df)) {
    stop(
      "filter_pitchtype_min_counts() expected a data frame. Received class: ",
      paste(class(df), collapse = ", "),
      call. = FALSE
    )
  }
  
  out <- df
  
  if (!is.null(min_pitches) && "Pitches" %in% names(out)) {
    out <- out %>% filter(!is.na(Pitches), Pitches >= min_pitches)
  }
  
  if (!is.null(min_bbe) && "BBE" %in% names(out)) {
    out <- out %>% filter(is.na(BBE) | BBE >= min_bbe)
  }
  
  out
}

add_pitchtype_usage_pct <- function(df,
                                    id_col,
                                    group.by = NULL,
                                    pitch_type_col = "pitch_type",
                                    pitch_count_col = "Pitches",
                                    usage_col = "Usage_pct") {
  library(dplyr)
  
  group.by <- clean_group_by_for_split(group.by)
  
  required <- c(id_col, pitch_type_col, pitch_count_col)
  missing_required <- setdiff(required, names(df))
  
  if (length(missing_required) > 0) {
    stop(
      "add_pitchtype_usage_pct() is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }
  
  parent_cols <- unique(c(id_col, group.by))
  
  df %>%
    group_by(across(all_of(parent_cols))) %>%
    mutate(
      .usage_total = sum(.data[[pitch_count_col]], na.rm = TRUE),
      "{usage_col}" := case_when(
        .usage_total > 0 ~ as.numeric(.data[[pitch_count_col]]) / .usage_total,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-.usage_total)
}

pitchtype_default_stat_labels <- function() {
  c(
    Usage_pct = "Usage %",
    Swing_pct = "Swing %",
    Whiff_pct = "Whiff %",
    Chase_pct = "Chase %",
    Zone_pct = "Zone %",
    Contact_pct = "Contact %",
    OContact_pct = "O-contact %",
    ZContact_pct = "Z-contact %",
    ZSwing_pct = "Z-swing %",
    Heart_pct = "Heart %",
    Shadow_pct = "Shadow %",
    HardHit_pct = "Hard-hit %",
    barrel_pct = "Barrel %",
    GB_pct = "Ground-ball %",
    HRFB = "HR/FB %",
    EV = "Exit velocity",
    LA = "Launch angle",
    velo = "Velocity",
    spin = "Spin rate",
    IVB = "IVB",
    HB = "HB",
    HAA = "HAA",
    VAA = "VAA"
  )
}

label_pitchtype_stats <- function(stats, stat_labels = NULL, percent_stats = NULL) {
  labels <- pitchtype_default_stat_labels()
  
  if (!is.null(stat_labels)) {
    if (is.null(names(stat_labels))) {
      stop("`stat_labels` must be a named character vector.", call. = FALSE)
    }
    labels[names(stat_labels)] <- stat_labels
  }
  
  out <- ifelse(stats %in% names(labels), unname(labels[stats]), stats)
  
  if (!is.null(percent_stats)) {
    out <- ifelse(
      stats %in% percent_stats & !grepl("%", out, fixed = TRUE),
      paste0(out, " (%)"),
      out
    )
  }
  
  out
}

available_split_stats <- function(split_df,
                                  stats,
                                  split_names = c("A", "B"),
                                  require_diff = TRUE) {
  stats <- unique(stats)
  
  stats[vapply(stats, function(stat) {
    needed <- paste0(stat, "_", split_names)
    if (require_diff) needed <- c(needed, paste0(stat, "_diff"))
    all(needed %in% names(split_df))
  }, logical(1))]
}

pitchtype_league_weight_col <- function(stat, available_cols) {
  preferred <- switch(
    stat,
    Usage_pct = "Pitches",
    Swing_pct = "Pitches",
    Zone_pct = "Pitches",
    Heart_pct = "Pitches",
    Shadow_pct = "Pitches",
    Whiff_pct = "Swings",
    Contact_pct = "Swings",
    Chase_pct = "OZoneP",
    ZSwing_pct = "ZoneP",
    OContact_pct = "Chase",
    ZContact_pct = "ZSwings",
    HardHit_pct = "BBE",
    barrel_pct = "BBE",
    GB_pct = "BBE",
    EV = "BBE",
    LA = "BBE",
    HRFB = "FB",
    velo = "Shape_Pitches",
    spin = "Shape_Pitches",
    IVB = "Shape_Pitches",
    HB = "Shape_Pitches",
    HAA = "Shape_Pitches",
    VAA = "Shape_Pitches",
    "Pitches"
  )
  
  fallbacks <- unique(c(preferred, "Pitches", "Shape_Pitches", "BBE"))
  hit <- fallbacks[fallbacks %in% available_cols]
  
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) return(NA_real_)
  
  sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

format_pitchtype_profile_value <- function(value, stat, percent_stats, scale_percents = TRUE) {
  if (is.na(value)) return(NA_character_)
  
  if (stat %in% percent_stats) {
    val <- if (isTRUE(scale_percents)) value * 100 else value
    return(paste0(round(val, 1), "%"))
  }
  
  if (stat %in% c("EV", "LA", "velo", "IVB", "HB", "HAA", "VAA")) {
    return(as.character(round(value, 1)))
  }
  
  if (stat == "spin") {
    return(as.character(round(value, 0)))
  }
  
  as.character(round(value, 3))
}


# -----------------------------------------------------------------------------
# MLB team theme helpers
# -----------------------------------------------------------------------------

mlb_team_colors <- tibble::tribble(
  ~team_abbr, ~team_name, ~primary_color, ~secondary_color,
  "ARI", "Arizona Diamondbacks", "#A71930", "#000000",
  "ATL", "Atlanta Braves", "#CE1141", "#13274F",
  "BAL", "Baltimore Orioles", "#DF4601", "#000000",
  "BOS", "Boston Red Sox", "#BD3039", "#0C2340",
  "CHC", "Chicago Cubs", "#0E3386", "#CC3433",
  "CWS", "Chicago White Sox", "#27251F", "#C4CED4",
  "CIN", "Cincinnati Reds", "#C6011F", "#000000",
  "CLE", "Cleveland Guardians", "#E31937", "#0C2340",
  "COL", "Colorado Rockies", "#33006F", "#C4CED4",
  "DET", "Detroit Tigers", "#0C2340", "#FA4616",
  "HOU", "Houston Astros", "#002D62", "#EB6E1F",
  "KC",  "Kansas City Royals", "#004687", "#BD9B60",
  "LAA", "Los Angeles Angels", "#BA0021", "#003263",
  "LAD", "Los Angeles Dodgers", "#005A9C", "#EF3E42",
  "MIA", "Miami Marlins", "#00A3E0", "#EF3340",
  "MIL", "Milwaukee Brewers", "#12284B", "#FFC52F",
  "MIN", "Minnesota Twins", "#002B5C", "#D31145",
  "NYM", "New York Mets", "#002D72", "#FF5910",
  "NYY", "New York Yankees", "#0C2340", "#C4CED4",
  "ATH", "Athletics", "#003831", "#EFB21E",
  "PHI", "Philadelphia Phillies", "#E81828", "#002D72",
  "PIT", "Pittsburgh Pirates", "#FDB827", "#27251F",
  "SD",  "San Diego Padres", "#2F241D", "#FFC425",
  "SF",  "San Francisco Giants", "#FD5A1E", "#27251F",
  "SEA", "Seattle Mariners", "#0C2C56", "#005C5C",
  "STL", "St. Louis Cardinals", "#C41E3A", "#0C2340",
  "TB",  "Tampa Bay Rays", "#092C5C", "#8FBCE6",
  "TEX", "Texas Rangers", "#003278", "#C0111F",
  "TOR", "Toronto Blue Jays", "#134A8E", "#E8291C",
  "WSH", "Washington Nationals", "#AB0003", "#14225A"
)

normalize_mlb_team_key <- function(team) {
  if (is.null(team) || length(team) == 0 || all(is.na(team))) return(NA_character_)
  
  team <- toupper(trimws(as.character(team[1])))
  
  aliases <- c(
    AZ = "ARI",
    ARZ = "ARI",
    CHW = "CWS",
    WSN = "WSH",
    WAS = "WSH",
    TBR = "TB",
    KCR = "KC",
    SFG = "SF",
    SDP = "SD",
    OAK = "ATH",
    A = "ATH"
  )
  
  if (team %in% names(aliases)) return(unname(aliases[[team]]))
  team
}

get_team_colors <- function(team) {
  library(dplyr)
  
  team_key <- normalize_mlb_team_key(team)
  
  if (is.na(team_key)) {
    return(mlb_team_colors[0, ])
  }
  
  colors <- mlb_team_colors %>%
    filter(
      toupper(team_abbr) == team_key |
        toupper(team_name) == team_key
    ) %>%
    slice(1)
  
  colors
}

theme_mlb_team <- function(team, base_size = 13) {
  library(ggplot2)
  
  colors <- get_team_colors(team)
  
  if (nrow(colors) == 0) {
    warning("Could not find MLB team colors for `", team, "`. Returning theme_minimal().", call. = FALSE)
    return(theme_minimal(base_size = base_size))
  }
  
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(
        color = colors$primary_color,
        face = "bold",
        size = base_size + 5
      ),
      plot.subtitle = element_text(
        color = colors$secondary_color,
        size = base_size
      ),
      axis.title = element_text(
        color = colors$primary_color,
        face = "bold"
      ),
      axis.text = element_text(color = "#222222"),
      panel.grid.major = element_line(
        color = scales::alpha(colors$secondary_color, 0.25)
      ),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(
        fill = colors$primary_color,
        color = NA
      ),
      strip.text = element_text(
        color = "white",
        face = "bold"
      ),
      legend.title = element_text(
        color = colors$primary_color,
        face = "bold"
      )
    )
}

scale_fill_mlb_team <- function(team) {
  library(ggplot2)
  
  colors <- get_team_colors(team)
  
  if (nrow(colors) == 0) {
    return(NULL)
  }
  
  scale_fill_manual(
    values = c(colors$primary_color, colors$secondary_color),
    drop = FALSE
  )
}

apply_mlb_team_plot_style <- function(p,
                                      team,
                                      base_size = 12,
                                      apply_fill = TRUE) {
  if (is.null(team) || length(team) == 0 || is.na(team)) {
    return(p)
  }
  
  p <- p + theme_mlb_team(team, base_size = base_size)
  
  if (isTRUE(apply_fill)) {
    fill_scale <- scale_fill_mlb_team(team)
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }
  }
  
  p
}

infer_pitcher_team_from_df <- function(df,
                                       pitcher,
                                       pitcher_col = "name",
                                       team_cols = c(
                                         "team", "team_post", "team_pre", "team_B", "team_A",
                                         "team_post_may", "team_pre_may", "fielding_team"
                                       )) {
  library(dplyr)
  
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_character_)
  if (!pitcher_col %in% names(df)) return(NA_character_)
  
  matched <- df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(matched) == 0) return(NA_character_)
  
  existing_team_cols <- team_cols[team_cols %in% names(matched)]
  
  if (length(existing_team_cols) == 0) return(NA_character_)
  
  for (col in existing_team_cols) {
    vals <- matched[[col]]
    vals <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
    
    if (length(vals) > 0) {
      return(as.character(vals[1]))
    }
  }
  
  NA_character_
}

resolve_pitcher_theme_team <- function(df,
                                       pitcher,
                                       use_team_theme = FALSE,
                                       team_abbr = NULL,
                                       pitcher_col = "name",
                                       team_cols = c(
                                         "team", "team_post", "team_pre", "team_B", "team_A",
                                         "team_post_may", "team_pre_may", "fielding_team"
                                       )) {
  if (!isTRUE(use_team_theme)) {
    return(NA_character_)
  }
  
  if (!is.null(team_abbr) && length(team_abbr) > 0 && !is.na(team_abbr[1])) {
    return(normalize_mlb_team_key(team_abbr[1]))
  }
  
  inferred <- infer_pitcher_team_from_df(
    df = df,
    pitcher = pitcher,
    pitcher_col = pitcher_col,
    team_cols = team_cols
  )
  
  inferred <- normalize_mlb_team_key(inferred)
  
  if (is.na(inferred)) {
    warning(
      "use_team_theme = TRUE, but the selected pitcher's team could not be inferred. ",
      "Use `team_abbr = \"LAA\"` or another team abbreviation to override.",
      call. = FALSE
    )
  }
  
  inferred
}


# -----------------------------------------------------------------------------
# Pitch-type split viz pack
# -----------------------------------------------------------------------------

summarize_pitcher_pitchtype_split_pair <- function(views,
                                                   split_a = list(),
                                                   split_b = list(),
                                                   split_a_name = "A",
                                                   split_b_name = "B",
                                                   level = "player",
                                                   team_col = "fielding_team",
                                                   group.by = NULL,
                                                   stats = c(
                                                     "Usage_pct", "Whiff_pct", "Chase_pct", "Zone_pct",
                                                     "Contact_pct", "OContact_pct", "ZContact_pct",
                                                     "Heart_pct", "Shadow_pct", "Swing_pct", "ZSwing_pct",
                                                     "velo", "spin", "IVB", "HB", "HAA", "VAA",
                                                     "HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB"
                                                   ),
                                                   min_pitches_each = NULL,
                                                   min_bbe_each = NULL,
                                                   min_pitch_pct = NULL,
                                                   min_bbe_pct = NULL,
                                                   diff_direction = "A_minus_B",
                                                   join_type = "full") {
  library(dplyr)
  
  group.by <- clean_group_by_for_split(group.by)
  
  if (exists("normalize_level", mode = "function")) {
    level <- normalize_level(level)
  }
  
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
    group.by = group.by,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct
  )
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  
  make_split <- function(split, split_label) {
    raw <- do.call(
      summarize_overall_pitcher_by_pitchtype,
      modifyList(common, split)
    )
    
    if (is.null(raw)) {
      stop(
        "summarize_overall_pitcher_by_pitchtype() returned NULL for split: ",
        split_label,
        call. = FALSE
      )
    }
    
    if (!is.data.frame(raw)) {
      stop(
        "summarize_overall_pitcher_by_pitchtype() did not return a data frame for split: ",
        split_label,
        ". Returned class: ",
        paste(class(raw), collapse = ", "),
        call. = FALSE
      )
    }
    
    if (nrow(raw) == 0) {
      stop(
        "summarize_overall_pitcher_by_pitchtype() returned 0 rows for split: ",
        split_label,
        ". Check your date filters.",
        call. = FALSE
      )
    }
    
    if (!id_col %in% names(raw)) {
      stop("ID column `", id_col, "` was not found in split ", split_label, ".", call. = FALSE)
    }
    
    if (!"pitch_type" %in% names(raw)) {
      stop("Column `pitch_type` was not found in split ", split_label, ".", call. = FALSE)
    }
    
    raw %>%
      filter_pitchtype_min_counts(
        min_pitches = min_pitches_each,
        min_bbe = min_bbe_each
      ) %>%
      add_pitchtype_usage_pct(
        id_col = id_col,
        group.by = group.by,
        pitch_type_col = "pitch_type",
        pitch_count_col = "Pitches",
        usage_col = "Usage_pct"
      )
  }
  
  a <- make_split(split_a, split_a_name)
  b <- make_split(split_b, split_b_name)
  
  stats <- unique(stats)
  stats <- intersect(stats, intersect(names(a), names(b)))
  
  if (length(stats) == 0) {
    stop(
      "None of the requested `stats` were found in both pitch-type split summaries.",
      call. = FALSE
    )
  }
  
  compare_split_summaries(
    group_a = a,
    group_b = b,
    group_a_name = split_a_name,
    group_b_name = split_b_name,
    id_col = id_col,
    name_col = "name",
    team_col = "team",
    group_cols = unique(c(group.by, "pitch_type")),
    stats = stats,
    keep_counts = c("Pitches", "Shape_Pitches", "BBE", "Swings", "Whiffs", "Contact"),
    diff_direction = diff_direction,
    join_type = join_type
  )
}

compute_pitchtype_split_zscores <- function(split_df,
                                            pitcher,
                                            stats,
                                            split_names = c("A", "B"),
                                            pitcher_col = "name",
                                            pitch_type_col = "pitch_type",
                                            z_threshold = 1.25,
                                            z_baseline = c("all_pitch_types", "same_pitch_type"),
                                            min_baseline_n = 5) {
  library(dplyr)
  
  z_baseline <- match.arg(z_baseline)
  
  if (!pitcher_col %in% names(split_df)) {
    stop("pitcher_col `", pitcher_col, "` was not found in split_df.", call. = FALSE)
  }
  
  if (!pitch_type_col %in% names(split_df)) {
    stop("pitch_type_col `", pitch_type_col, "` was not found in split_df.", call. = FALSE)
  }
  
  stats <- available_split_stats(
    split_df = split_df,
    stats = stats,
    split_names = split_names,
    require_diff = TRUE
  )
  
  if (length(stats) == 0) {
    stop(
      "No requested stats had complete split columns: _",
      split_names[1], ", _", split_names[2], ", and _diff.",
      call. = FALSE
    )
  }
  
  long_all <- lapply(stats, function(stat) {
    a_col <- paste0(stat, "_", split_names[1])
    b_col <- paste0(stat, "_", split_names[2])
    d_col <- paste0(stat, "_diff")
    
    data.frame(
      row_id = seq_len(nrow(split_df)),
      name = as.character(split_df[[pitcher_col]]),
      team = if ("team" %in% names(split_df)) as.character(split_df[["team"]]) else NA_character_,
      pitch_type = as.character(split_df[[pitch_type_col]]),
      stat = stat,
      value_a = as.numeric(split_df[[a_col]]),
      value_b = as.numeric(split_df[[b_col]]),
      diff = as.numeric(split_df[[d_col]]),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
  
  if (z_baseline == "same_pitch_type") {
    long_z <- long_all %>%
      group_by(stat, pitch_type) %>%
      mutate(
        baseline_n = sum(!is.na(diff)),
        baseline_mean_diff = mean(diff, na.rm = TRUE),
        baseline_sd_diff = sd(diff, na.rm = TRUE)
      ) %>%
      ungroup()
  } else {
    long_z <- long_all %>%
      group_by(stat) %>%
      mutate(
        baseline_n = sum(!is.na(diff)),
        baseline_mean_diff = mean(diff, na.rm = TRUE),
        baseline_sd_diff = sd(diff, na.rm = TRUE)
      ) %>%
      ungroup()
  }
  
  long_z <- long_z %>%
    mutate(
      z_score = if_else(
        baseline_n >= min_baseline_n & !is.na(baseline_sd_diff) & baseline_sd_diff > 0,
        (diff - baseline_mean_diff) / baseline_sd_diff,
        NA_real_
      ),
      abs_z_score = abs(z_score),
      notable = !is.na(abs_z_score) & abs_z_score >= z_threshold
    )
  
  findings <- long_z %>%
    filter(name == pitcher, notable) %>%
    arrange(desc(abs_z_score), stat, pitch_type)
  
  usage_cols <- c(
    paste0("Pitches_", split_names),
    paste0("BBE_", split_names),
    paste0("Usage_pct_", split_names)
  )
  usage_cols <- usage_cols[usage_cols %in% names(split_df)]
  
  if (length(usage_cols) > 0 && nrow(findings) > 0) {
    context <- split_df %>%
      mutate(row_id = row_number()) %>%
      select(row_id, any_of(usage_cols))
    
    findings <- findings %>%
      left_join(context, by = "row_id")
  }
  
  if ("value_a" %in% names(findings)) {
    names(findings)[names(findings) == "value_a"] <- paste0("value_", split_names[1])
  }
  
  if ("value_b" %in% names(findings)) {
    names(findings)[names(findings) == "value_b"] <- paste0("value_", split_names[2])
  }
  
  findings <- findings %>%
    select(-any_of("row_id"))
  
  list(
    z_table = long_z,
    findings = findings,
    kept_stats = unique(findings$stat)
  )
}

build_pitchtype_split_plot_data <- function(split_df,
                                            pitcher,
                                            stats,
                                            split_names = c("A", "B"),
                                            pitcher_col = "name",
                                            pitch_type_col = "pitch_type",
                                            percent_stats = c(
                                              "Usage_pct", "K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                              "Contact_pct", "OContact_pct", "ZContact_pct", "ZSwing_pct",
                                              "HardHit_pct", "barrel_pct", "Swing_pct", "Zone_pct",
                                              "Heart_pct", "Shadow_pct", "GB_pct", "HRFB", "pull_fbpct"
                                            ),
                                            scale_percents = TRUE,
                                            stat_labels = NULL) {
  library(dplyr)
  
  if (!pitcher_col %in% names(split_df)) {
    stop("pitcher_col `", pitcher_col, "` was not found in split_df.", call. = FALSE)
  }
  
  if (!pitch_type_col %in% names(split_df)) {
    stop("pitch_type_col `", pitch_type_col, "` was not found in split_df.", call. = FALSE)
  }
  
  pitcher_df <- split_df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(pitcher_df) == 0) {
    stop("No pitcher found matching: ", pitcher, call. = FALSE)
  }
  
  stats <- available_split_stats(
    split_df = split_df,
    stats = stats,
    split_names = split_names,
    require_diff = FALSE
  )
  
  if (length(stats) == 0) {
    stop("None of the selected plot stats had split value columns.", call. = FALSE)
  }
  
  plot_data <- lapply(stats, function(stat) {
    value_cols <- paste0(stat, "_", split_names)
    value_cols <- value_cols[value_cols %in% names(pitcher_df)]
    
    lapply(value_cols, function(col) {
      data.frame(
        pitch_type = as.character(pitcher_df[[pitch_type_col]]),
        stat = stat,
        comparison = substr(col, nchar(stat) + 2, nchar(col)),
        value = as.numeric(pitcher_df[[col]]),
        stringsAsFactors = FALSE
      )
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
  
  plot_data %>%
    mutate(
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, value * 100, value),
      stat_label = label_pitchtype_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      comparison = factor(comparison, levels = split_names),
      stat = factor(stat, levels = stats),
      stat_label = factor(
        stat_label,
        levels = unique(label_pitchtype_stats(stats, stat_labels = stat_labels, percent_stats = NULL))
      ),
      value_label = case_when(
        is_percent ~ paste0(round(value, 1), "%"),
        stat %in% c("EV", "LA", "velo", "IVB", "HB", "HAA", "VAA") ~ as.character(round(value, 1)),
        stat == "spin" ~ as.character(round(value, 0)),
        TRUE ~ as.character(round(value, 3))
      )
    ) %>%
    filter(!is.na(value), !is.na(pitch_type))
}


resolve_pitchtype_facet_stats <- function(split_df,
                                          auto_stats = character(0),
                                          facet_stats = NULL,
                                          facet_mode = c("auto", "manual", "auto_plus_manual"),
                                          always_include = "Usage_pct",
                                          include_usage_facet = TRUE,
                                          split_names = c("A", "B")) {
  facet_mode <- match.arg(facet_mode)
  manual_stats <- if (is.null(facet_stats)) character(0) else unique(facet_stats)
  auto_stats <- unique(auto_stats)
  always_include <- unique(always_include)
  always_include_use <- if (isTRUE(include_usage_facet)) always_include else character(0)
  
  if (facet_mode == "auto") {
    requested_plot_stats <- auto_stats
  }
  
  if (facet_mode == "manual") {
    if (length(manual_stats) == 0) {
      stop(
        "facet_mode = 'manual' requires `facet_stats`.",
        call. = FALSE
      )
    }
    requested_plot_stats <- manual_stats
  }
  
  if (facet_mode == "auto_plus_manual") {
    if (length(manual_stats) == 0) {
      warning(
        "facet_mode = 'auto_plus_manual' was used without `facet_stats`. Falling back to auto facets.",
        call. = FALSE
      )
      requested_plot_stats <- auto_stats
    } else {
      requested_plot_stats <- unique(c(auto_stats, manual_stats))
    }
  }
  
  requested_plot_stats <- unique(c(always_include_use, requested_plot_stats))
  
  kept_stats <- available_split_stats(
    split_df = split_df,
    stats = requested_plot_stats,
    split_names = split_names,
    require_diff = FALSE
  )
  
  missing_plot_stats <- setdiff(requested_plot_stats, kept_stats)
  
  if (length(missing_plot_stats) > 0) {
    warning(
      "These requested facet stats were not available in the split summary and were dropped: ",
      paste(missing_plot_stats, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(kept_stats) == 0) {
    stop(
      "No valid stats were available to plot after applying facet selection.",
      call. = FALSE
    )
  }
  
  list(
    requested_plot_stats = requested_plot_stats,
    kept_stats = kept_stats,
    missing_plot_stats = missing_plot_stats
  )
}

plot_pitchtype_split_facets <- function(plot_data,
                                        pitcher,
                                        split_names = c("A", "B"),
                                        title = NULL,
                                        subtitle = NULL,
                                        facet_scales = "free_y",
                                        show_values = TRUE,
                                        ncol = NULL,
                                        base_size = 12,
                                        use_team_theme = FALSE,
                                        team_abbr = NULL) {
  library(ggplot2)
  
  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type Split Changes")
  }
  
  if (is.null(subtitle)) {
    subtitle <- paste0(paste(split_names, collapse = " vs "), " | usage always shown; other facets pass z-score threshold")
  }
  
  p <- ggplot(
    plot_data,
    aes(x = pitch_type, y = value, fill = comparison)
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.45) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Value",
      fill = NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.position = "top"
    )
  
  if (isTRUE(use_team_theme)) {
    p <- apply_mlb_team_plot_style(
      p = p,
      team = team_abbr,
      base_size = base_size,
      apply_fill = TRUE
    )
  }
  
  if (show_values) {
    p <- p +
      geom_text(
        aes(label = value_label),
        position = position_dodge(width = 0.8),
        vjust = -0.25,
        size = 2.8
      )
  }
  
  p
}

make_pitcher_pitchtype_viz_pack <- function(views,
                                            pitcher,
                                            split_a = list(),
                                            split_b = list(),
                                            split_a_name = "A",
                                            split_b_name = "B",
                                            level = "player",
                                            team_col = "fielding_team",
                                            group.by = NULL,
                                            candidate_stats = c(
                                              "Whiff_pct", "Chase_pct", "Zone_pct", "Contact_pct",
                                              "OContact_pct", "ZContact_pct", "Heart_pct", "Shadow_pct",
                                              "Swing_pct", "ZSwing_pct", "velo", "spin", "IVB", "HB",
                                              "HAA", "VAA", "HardHit_pct", "barrel_pct", "GB_pct",
                                              "EV", "LA", "HRFB"
                                            ),
                                            always_include = "Usage_pct",
                                            facet_stats = NULL,
                                            facet_mode = c("auto", "manual", "auto_plus_manual"),
                                            include_usage_facet = TRUE,
                                            z_threshold = 1.25,
                                            z_baseline = c("all_pitch_types", "same_pitch_type"),
                                            min_baseline_n = 5,
                                            min_pitches_each = NULL,
                                            min_bbe_each = NULL,
                                            min_pitch_pct = NULL,
                                            min_bbe_pct = NULL,
                                            diff_direction = "A_minus_B",
                                            join_type = "full",
                                            pitcher_col = "name",
                                            pitch_type_col = "pitch_type",
                                            percent_stats = c(
                                              "Usage_pct", "K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                              "Contact_pct", "OContact_pct", "ZContact_pct", "ZSwing_pct",
                                              "HardHit_pct", "barrel_pct", "Swing_pct", "Zone_pct",
                                              "Heart_pct", "Shadow_pct", "GB_pct", "HRFB", "pull_fbpct"
                                            ),
                                            scale_percents = TRUE,
                                            stat_labels = NULL,
                                            facet_scales = "free_y",
                                            show_values = TRUE,
                                            ncol = NULL,
                                            base_size = 12,
                                            print_plot = TRUE,
                                            use_team_theme = FALSE,
                                            team_abbr = NULL) {
  library(dplyr)
  
  z_baseline <- match.arg(z_baseline)
  facet_mode <- match.arg(facet_mode)
  group.by <- clean_group_by_for_split(group.by)
  
  split_names <- c(split_a_name, split_b_name)
  manual_stats <- if (is.null(facet_stats)) character(0) else unique(facet_stats)
  all_stats <- unique(c(always_include, candidate_stats, manual_stats))
  
  split_df <- summarize_pitcher_pitchtype_split_pair(
    views = views,
    split_a = split_a,
    split_b = split_b,
    split_a_name = split_a_name,
    split_b_name = split_b_name,
    level = level,
    team_col = team_col,
    group.by = group.by,
    stats = all_stats,
    min_pitches_each = min_pitches_each,
    min_bbe_each = min_bbe_each,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct,
    diff_direction = diff_direction,
    join_type = join_type
  )
  
  z_out <- compute_pitchtype_split_zscores(
    split_df = split_df,
    pitcher = pitcher,
    stats = candidate_stats,
    split_names = split_names,
    pitcher_col = pitcher_col,
    pitch_type_col = pitch_type_col,
    z_threshold = z_threshold,
    z_baseline = z_baseline,
    min_baseline_n = min_baseline_n
  )
  
  facet_out <- resolve_pitchtype_facet_stats(
    split_df = split_df,
    auto_stats = z_out$kept_stats,
    facet_stats = facet_stats,
    facet_mode = facet_mode,
    always_include = always_include,
    include_usage_facet = include_usage_facet,
    split_names = split_names
  )
  
  requested_plot_stats <- facet_out$requested_plot_stats
  kept_stats <- facet_out$kept_stats
  
  if (facet_mode == "auto" && length(setdiff(kept_stats, always_include)) == 0) {
    message(
      "No candidate stats crossed |z| >= ", z_threshold,
      ". Plotting `", paste(kept_stats, collapse = ", "), "`."
    )
  }
  
  plot_data <- build_pitchtype_split_plot_data(
    split_df = split_df,
    pitcher = pitcher,
    stats = kept_stats,
    split_names = split_names,
    pitcher_col = pitcher_col,
    pitch_type_col = pitch_type_col,
    percent_stats = percent_stats,
    scale_percents = scale_percents,
    stat_labels = stat_labels
  )
  
  theme_team <- resolve_pitcher_theme_team(
    df = split_df,
    pitcher = pitcher,
    use_team_theme = use_team_theme,
    team_abbr = team_abbr,
    pitcher_col = pitcher_col,
    team_cols = c("team", paste0("team_", split_b_name), paste0("team_", split_a_name), team_col)
  )
  
  subtitle <- paste0(
    split_a_name, " vs ", split_b_name,
    " | facet mode: ", facet_mode,
    ifelse(
      facet_mode == "auto",
      paste0(" | auto facets require |z| >= ", z_threshold, " | baseline: ", z_baseline),
      ""
    )
  )
  
  p <- plot_pitchtype_split_facets(
    plot_data = plot_data,
    pitcher = pitcher,
    split_names = split_names,
    title = paste0(pitcher, " Pitch-Type Split Viz Pack"),
    subtitle = subtitle,
    facet_scales = facet_scales,
    show_values = show_values,
    ncol = ncol,
    base_size = base_size,
    use_team_theme = use_team_theme,
    team_abbr = theme_team
  )
  
  if (isTRUE(print_plot)) {
    print(p)
  }
  
  list(
    split_df = split_df,
    findings = z_out$findings,
    z_table = z_out$z_table,
    facet_mode = facet_mode,
    requested_plot_stats = requested_plot_stats,
    kept_stats = kept_stats,
    missing_plot_stats = facet_out$missing_plot_stats,
    plot_data = plot_data,
    team_theme = theme_team,
    plot = p
  )
}


# -----------------------------------------------------------------------------
# Pitch-type league profile
# -----------------------------------------------------------------------------

summarize_pitchtype_league_values <- function(summary_df,
                                              stats,
                                              group_cols,
                                              league_method = c("weighted_mean", "median", "mean"),
                                              min_league_pitches = 25) {
  library(dplyr)
  
  league_method <- match.arg(league_method)
  
  if (!is.data.frame(summary_df)) {
    stop("`summary_df` must be a data frame.", call. = FALSE)
  }
  
  if (length(group_cols) == 0) {
    stop("`group_cols` must include at least `pitch_type`.", call. = FALSE)
  }
  
  missing_group_cols <- setdiff(group_cols, names(summary_df))
  if (length(missing_group_cols) > 0) {
    stop(
      "summarize_pitchtype_league_values() is missing grouping columns: ",
      paste(missing_group_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.null(min_league_pitches) && "Pitches" %in% names(summary_df)) {
    summary_df <- summary_df %>%
      filter(!is.na(Pitches), Pitches >= min_league_pitches)
  }
  
  stats <- stats[stats %in% names(summary_df)]
  
  if (length(stats) == 0) {
    stop("None of the requested `stats` were found in `summary_df`.", call. = FALSE)
  }
  
  out <- lapply(stats, function(stat) {
    weight_col <- pitchtype_league_weight_col(stat, names(summary_df))
    
    tmp <- summary_df %>%
      mutate(
        .stat = stat,
        .value = as.numeric(.data[[stat]]),
        .weight = if (!is.na(weight_col) && weight_col %in% names(summary_df)) {
          as.numeric(.data[[weight_col]])
        } else {
          rep(1, n())
        }
      )
    
    tmp %>%
      group_by(across(all_of(group_cols)), .stat) %>%
      summarise(
        league_value = case_when(
          league_method == "weighted_mean" ~ weighted_mean_safe(.value, .weight),
          league_method == "median" ~ median(.value, na.rm = TRUE),
          league_method == "mean" ~ mean(.value, na.rm = TRUE),
          TRUE ~ NA_real_
        ),
        league_n = sum(!is.na(.value)),
        league_weight_col = first(weight_col),
        league_weight_total = sum(.weight[!is.na(.value)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(stat = .stat)
  }) %>%
    bind_rows()
  
  out
}

compute_pitchtype_profile_percentiles <- function(player_df,
                                                  league_df,
                                                  stats,
                                                  group_cols,
                                                  player_id_col = "matchup.pitcher.id") {
  library(dplyr)
  
  stats <- stats[stats %in% names(player_df) & stats %in% names(league_df)]
  
  if (length(stats) == 0) {
    return(tibble())
  }
  
  out <- lapply(stats, function(stat) {
    player_stat <- player_df %>%
      mutate(
        stat = stat,
        pitcher_value = as.numeric(.data[[stat]])
      ) %>%
      select(any_of(c(player_id_col, "name", "team")), all_of(group_cols), stat, pitcher_value)
    
    league_stat <- league_df %>%
      mutate(.league_value_dist = as.numeric(.data[[stat]])) %>%
      select(all_of(group_cols), .league_value_dist)
    
    player_stat %>%
      left_join(league_stat, by = group_cols, relationship = "many-to-many") %>%
      group_by(across(any_of(c(player_id_col, "name", "team"))), across(all_of(group_cols)), stat, pitcher_value) %>%
      summarise(
        percentile = if_else(
          sum(!is.na(.league_value_dist)) > 0 & !is.na(first(pitcher_value)),
          mean(.league_value_dist <= first(pitcher_value), na.rm = TRUE) * 100,
          NA_real_
        ),
        percentile_n = sum(!is.na(.league_value_dist)),
        .groups = "drop"
      )
  }) %>%
    bind_rows()
  
  out
}

build_pitchtype_league_profile_table <- function(summary_df,
                                                 pitcher,
                                                 stats,
                                                 always_include = "Usage_pct",
                                                 pitcher_col = "name",
                                                 player_id_col = "matchup.pitcher.id",
                                                 pitch_type_col = "pitch_type",
                                                 group.by = NULL,
                                                 league_method = c("weighted_mean", "median", "mean"),
                                                 min_league_pitches = 25,
                                                 exclude_player_from_league = TRUE) {
  library(dplyr)
  
  league_method <- match.arg(league_method)
  group.by <- clean_group_by_for_split(group.by)
  
  if (!pitcher_col %in% names(summary_df)) {
    stop("pitcher_col `", pitcher_col, "` was not found in `summary_df`.", call. = FALSE)
  }
  
  if (!pitch_type_col %in% names(summary_df)) {
    stop("pitch_type_col `", pitch_type_col, "` was not found in `summary_df`.", call. = FALSE)
  }
  
  if (!player_id_col %in% names(summary_df)) {
    stop("player_id_col `", player_id_col, "` was not found in `summary_df`.", call. = FALSE)
  }
  
  stats <- unique(c(always_include, stats))
  stats <- stats[stats %in% names(summary_df)]
  
  if (length(stats) == 0) {
    stop("None of the requested stats were found in the pitch-type summary.", call. = FALSE)
  }
  
  group_cols <- unique(c(group.by, pitch_type_col))
  
  player_df <- summary_df %>%
    filter(.data[[pitcher_col]] == pitcher)
  
  if (nrow(player_df) == 0) {
    stop("No pitcher found matching: ", pitcher, call. = FALSE)
  }
  
  player_ids <- unique(player_df[[player_id_col]])
  
  league_df <- summary_df
  
  if (isTRUE(exclude_player_from_league)) {
    league_df <- league_df %>%
      filter(!.data[[player_id_col]] %in% player_ids)
  }
  
  league_values <- summarize_pitchtype_league_values(
    summary_df = league_df,
    stats = stats,
    group_cols = group_cols,
    league_method = league_method,
    min_league_pitches = min_league_pitches
  )
  
  percentile_df <- compute_pitchtype_profile_percentiles(
    player_df = player_df,
    league_df = league_df,
    stats = stats,
    group_cols = group_cols,
    player_id_col = player_id_col
  )
  
  comparison <- percentile_df %>%
    left_join(league_values, by = c(group_cols, "stat")) %>%
    mutate(
      diff = pitcher_value - league_value,
      pct_diff = if_else(!is.na(league_value) & league_value != 0, diff / abs(league_value), NA_real_),
      league_method = league_method,
      min_league_pitches = min_league_pitches,
      exclude_player_from_league = exclude_player_from_league
    ) %>%
    arrange(stat, across(all_of(group_cols)))
  
  list(
    player_df = player_df,
    league_df = league_df,
    league_values = league_values,
    comparison = comparison,
    stats = stats,
    group_cols = group_cols
  )
}

build_pitchtype_league_plot_data <- function(comparison_table,
                                             pitcher,
                                             stats,
                                             pitch_type_col = "pitch_type",
                                             percent_stats = c(
                                               "Usage_pct", "K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                               "Contact_pct", "OContact_pct", "ZContact_pct", "ZSwing_pct",
                                               "HardHit_pct", "barrel_pct", "Swing_pct", "Zone_pct",
                                               "Heart_pct", "Shadow_pct", "GB_pct", "HRFB", "pull_fbpct"
                                             ),
                                             scale_percents = TRUE,
                                             stat_labels = NULL,
                                             league_label = "League Avg",
                                             show_percentiles = TRUE) {
  library(dplyr)
  library(tidyr)
  
  if (!pitch_type_col %in% names(comparison_table)) {
    stop("pitch_type_col `", pitch_type_col, "` was not found in `comparison_table`.", call. = FALSE)
  }
  
  stats <- stats[stats %in% unique(comparison_table$stat)]
  
  if (length(stats) == 0) {
    stop("None of the selected stats were found in `comparison_table`.", call. = FALSE)
  }
  
  long <- comparison_table %>%
    filter(stat %in% stats) %>%
    select(any_of(c("name", "team", "matchup.pitcher.id")), all_of(pitch_type_col), stat, pitcher_value, league_value, percentile) %>%
    pivot_longer(
      cols = c(pitcher_value, league_value),
      names_to = "comparison_type",
      values_to = "raw_value"
    ) %>%
    mutate(
      comparison = case_when(
        comparison_type == "pitcher_value" ~ pitcher,
        comparison_type == "league_value" ~ league_label,
        TRUE ~ comparison_type
      ),
      is_percent = stat %in% percent_stats,
      value = if_else(scale_percents & is_percent, raw_value * 100, raw_value),
      stat_label = label_pitchtype_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(
        seq_along(raw_value),
        function(i) format_pitchtype_profile_value(raw_value[i], stat[i], percent_stats, scale_percents = scale_percents),
        character(1)
      ),
      pct_label = if_else(
        comparison == pitcher & show_percentiles & !is.na(percentile),
        paste0(round(percentile), "th pct"),
        NA_character_
      ),
      label = case_when(
        comparison == pitcher & show_percentiles & !is.na(pct_label) ~ paste0(value_label, "\n", pct_label),
        TRUE ~ value_label
      )
    ) %>%
    filter(!is.na(value), !is.na(.data[[pitch_type_col]]))
  
  long %>%
    mutate(
      stat = factor(stat, levels = stats),
      stat_label = factor(
        stat_label,
        levels = unique(label_pitchtype_stats(stats, stat_labels = stat_labels, percent_stats = NULL))
      ),
      comparison = factor(comparison, levels = c(pitcher, league_label))
    )
}

plot_pitchtype_league_facets <- function(plot_data,
                                         pitcher,
                                         pitch_type_col = "pitch_type",
                                         title = NULL,
                                         subtitle = NULL,
                                         facet_scales = "free_y",
                                         show_values = TRUE,
                                         ncol = NULL,
                                         base_size = 12,
                                         use_team_theme = FALSE,
                                         team_abbr = NULL) {
  library(ggplot2)
  
  if (!pitch_type_col %in% names(plot_data)) {
    stop("pitch_type_col `", pitch_type_col, "` was not found in `plot_data`.", call. = FALSE)
  }
  
  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type League Profile")
  }
  
  if (is.null(subtitle)) {
    subtitle <- "Bars compare selected pitcher to league average within each pitch type. Player labels include value percentile."
  }
  
  p <- ggplot(
    plot_data,
    aes(x = .data[[pitch_type_col]], y = value, fill = comparison)
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_hline(yintercept = 0, linewidth = 0.35, alpha = 0.45) +
    facet_wrap(~ stat_label, scales = facet_scales, ncol = ncol) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Value",
      fill = NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.position = "top"
    )
  
  if (isTRUE(use_team_theme)) {
    p <- apply_mlb_team_plot_style(
      p = p,
      team = team_abbr,
      base_size = base_size,
      apply_fill = TRUE
    )
  }
  
  if (show_values) {
    p <- p +
      geom_text(
        aes(label = label),
        position = position_dodge(width = 0.8),
        vjust = -0.2,
        size = 2.8,
        lineheight = 0.9
      )
  }
  
  p
}

make_pitcher_pitchtype_league_profile <- function(views,
                                                  pitcher,
                                                  group = "none",
                                                  bat_hand = "none",
                                                  pit_hand = "none",
                                                  level = "player",
                                                  start_date = NULL,
                                                  end_date = NULL,
                                                  risp = "all",
                                                  group.by = NULL,
                                                  team_col = "fielding_team",
                                                  pitch_type = NA,
                                                  stats = c(
                                                    "Whiff_pct", "Chase_pct", "Zone_pct", "Contact_pct",
                                                    "OContact_pct", "ZContact_pct", "Heart_pct", "Shadow_pct",
                                                    "Swing_pct", "ZSwing_pct", "velo", "spin",
                                                    "HAA", "VAA", "HardHit_pct", "barrel_pct", "GB_pct",
                                                    "EV", "LA", "HRFB", "SLG", "K_pct", "BB_pct"
                                                  ),
                                                  facet_stats = NULL,
                                                  always_include = "Usage_pct",
                                                  include_usage_facet = TRUE,
                                                  league_method = c("weighted_mean", "median", "mean"),
                                                  min_league_pitches = 25,
                                                  exclude_player_from_league = TRUE,
                                                  min_pitch_pct = NULL,
                                                  min_bbe_pct = NULL,
                                                  pitcher_col = "name",
                                                  player_id_col = "matchup.pitcher.id",
                                                  pitch_type_col = "pitch_type",
                                                  percent_stats = c(
                                                    "Usage_pct", "K_pct", "BB_pct", "Whiff_pct", "Chase_pct",
                                                    "Contact_pct", "OContact_pct", "ZContact_pct", "ZSwing_pct",
                                                    "HardHit_pct", "barrel_pct", "Swing_pct", "Zone_pct",
                                                    "Heart_pct", "Shadow_pct", "GB_pct", "HRFB", "pull_fbpct"
                                                  ),
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
                                                  print_plot = TRUE,
                                                  use_team_theme = FALSE,
                                                  team_abbr = NULL) {
  library(dplyr)
  
  league_method <- match.arg(league_method)
  group.by <- clean_group_by_for_split(group.by)
  
  if (is.null(views)) {
    stop("`views` is NULL.", call. = FALSE)
  }
  
  if (!is.list(views)) {
    stop("`views` must be a list of data frames.", call. = FALSE)
  }
  
  if (exists("normalize_level", mode = "function")) {
    level <- normalize_level(level)
  }
  
  if (level != "player") {
    stop("make_pitcher_pitchtype_league_profile() currently supports level = 'player' only.", call. = FALSE)
  }
  
  plot_stats <- if (is.null(facet_stats)) stats else unique(facet_stats)
  always_include_use <- if (isTRUE(include_usage_facet)) always_include else character(0)
  summary_stats <- unique(c(always_include_use, plot_stats))
  
  summary_df <- summarize_overall_pitcher_by_pitchtype(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    level = level,
    start_date = start_date,
    end_date = end_date,
    risp = risp,
    group.by = group.by,
    team_col = team_col,
    pitch_type = pitch_type,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct
  )
  
  if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) {
    stop(
      "summarize_overall_pitcher_by_pitchtype() returned no usable rows. Check date filters and input views.",
      call. = FALSE
    )
  }
  
  summary_df <- summary_df %>%
    add_pitchtype_usage_pct(
      id_col = player_id_col,
      group.by = group.by,
      pitch_type_col = pitch_type_col,
      pitch_count_col = "Pitches",
      usage_col = "Usage_pct"
    )
  
  available_profile_stats <- summary_stats[summary_stats %in% names(summary_df)]
  missing_profile_stats <- setdiff(summary_stats, available_profile_stats)
  
  if (length(missing_profile_stats) > 0) {
    warning(
      "These requested league-profile facet stats were not available and were dropped: ",
      paste(missing_profile_stats, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(available_profile_stats) == 0) {
    stop("No valid stats were available for the league-profile plot.", call. = FALSE)
  }
  
  profile <- build_pitchtype_league_profile_table(
    summary_df = summary_df,
    pitcher = pitcher,
    stats = available_profile_stats,
    always_include = character(0),
    pitcher_col = pitcher_col,
    player_id_col = player_id_col,
    pitch_type_col = pitch_type_col,
    group.by = group.by,
    league_method = league_method,
    min_league_pitches = min_league_pitches,
    exclude_player_from_league = exclude_player_from_league
  )
  
  kept_stats <- profile$stats
  kept_stats <- unique(c(intersect(always_include_use, kept_stats), setdiff(kept_stats, always_include_use)))
  
  plot_data <- build_pitchtype_league_plot_data(
    comparison_table = profile$comparison,
    pitcher = pitcher,
    stats = kept_stats,
    pitch_type_col = pitch_type_col,
    percent_stats = percent_stats,
    scale_percents = scale_percents,
    stat_labels = stat_labels,
    league_label = league_label,
    show_percentiles = show_percentiles
  )
  
  usage_order <- profile$comparison %>%
    filter(stat == "Usage_pct") %>%
    arrange(desc(pitcher_value)) %>%
    pull(all_of(pitch_type_col)) %>%
    unique()
  
  if (length(usage_order) > 0) {
    plot_data[[pitch_type_col]] <- factor(plot_data[[pitch_type_col]], levels = usage_order)
  }
  
  theme_team <- resolve_pitcher_theme_team(
    df = profile$player_df,
    pitcher = pitcher,
    use_team_theme = use_team_theme,
    team_abbr = team_abbr,
    pitcher_col = pitcher_col,
    team_cols = c("team", team_col, "fielding_team")
  )
  
  if (is.null(title)) {
    title <- paste0(pitcher, " Pitch-Type League Profile")
  }
  
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Pitcher vs ", league_label,
      " by pitch type | league method: ", league_method,
      " | min league pitches: ", min_league_pitches,
      ifelse(exclude_player_from_league, " | pitcher excluded from league", " | pitcher included in league")
    )
  }
  
  p <- plot_pitchtype_league_facets(
    plot_data = plot_data,
    pitcher = pitcher,
    pitch_type_col = pitch_type_col,
    title = title,
    subtitle = subtitle,
    facet_scales = facet_scales,
    show_values = show_values,
    ncol = ncol,
    base_size = base_size,
    use_team_theme = use_team_theme,
    team_abbr = theme_team
  )
  
  if (isTRUE(print_plot)) {
    print(p)
  }
  
  list(
    summary_df = summary_df,
    player_df = profile$player_df,
    league_df = profile$league_df,
    league_values = profile$league_values,
    comparison_table = profile$comparison,
    facet_stats = facet_stats,
    requested_plot_stats = summary_stats,
    kept_stats = kept_stats,
    missing_plot_stats = missing_profile_stats,
    plot_data = plot_data,
    team_theme = theme_team,
    plot = p
  )
}

# -----------------------------------------------------------------------------
# Example usage
# -----------------------------------------------------------------------------
# Source project dependencies first, then this file:
# source("R/00_packages.R")
# source("R/01_helpers.R")
# source("R/02_summary_functions.R")
# source("R/06_comparisons.R")
# source("R/10_pitchtype_viz_pack.R")
#
# 1. Auto-discovery split pack:
# soriano_auto <- make_pitcher_pitchtype_viz_pack(
#   views = views_day,
#   pitcher = "José Soriano",
#   split_a = list(end_date = as.Date("2026-04-30")),
#   split_b = list(start_date = as.Date("2026-05-01")),
#   split_a_name = "pre",
#   split_b_name = "post",
#   facet_mode = "auto",
#   z_threshold = 1.25,
#   use_team_theme = TRUE,
#   print_plot = TRUE
# )
#
# 2. Manual split facets:
# soriano_manual <- make_pitcher_pitchtype_viz_pack(
#   views = views_day,
#   pitcher = "José Soriano",
#   split_a = list(end_date = as.Date("2026-04-30")),
#   split_b = list(start_date = as.Date("2026-05-01")),
#   split_a_name = "pre",
#   split_b_name = "post",
#   facet_mode = "manual",
#   facet_stats = c("Whiff_pct", "Zone_pct", "Chase_pct", "velo"),
#   include_usage_facet = TRUE,
#   use_team_theme = TRUE,
#   print_plot = TRUE
# )
#
# 3. Auto findings plus manual shape metrics:
# soriano_combo <- make_pitcher_pitchtype_viz_pack(
#   views = views_day,
#   pitcher = "José Soriano",
#   split_a = list(end_date = as.Date("2026-04-30")),
#   split_b = list(start_date = as.Date("2026-05-01")),
#   split_a_name = "pre",
#   split_b_name = "post",
#   facet_mode = "auto_plus_manual",
#   facet_stats = c("velo", "spin", "IVB", "HB"),
#   use_team_theme = TRUE,
#   print_plot = TRUE
# )
#
# 4. League profile using the auto split-pack facets:
# soriano_league_from_auto <- make_pitcher_pitchtype_league_profile(
#   views = views_day,
#   pitcher = "José Soriano",
#   facet_stats = soriano_auto$kept_stats,
#   league_method = "weighted_mean",
#   min_league_pitches = 25,
#   use_team_theme = TRUE,
#   print_plot = TRUE
# )
#
# 5. Manual league profile facets:
# soriano_league_manual <- make_pitcher_pitchtype_league_profile(
#   views = views_day,
#   pitcher = "José Soriano",
#   facet_stats = c("Whiff_pct", "Zone_pct", "Chase_pct", "HardHit_pct"),
#   include_usage_facet = TRUE,
#   league_method = "median",
#   use_team_theme = TRUE,
#   print_plot = TRUE
# )