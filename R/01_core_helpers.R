rate <- function(num, den) ifelse(den > 0, num / den, NA_real_)

#Function 2.3.1
#Helper function to split pbp data by certain parameters.
apply_split_filters <- function(tbl,
                                group = "none",
                                bat_hand = "none",
                                pit_hand = "none", 
                                start_date = NULL,
                                risp = 'all',
                                pitch_type = "all", 
                                end_date = NULL) {
  library(dplyr)
  
  fastballs <- c("Four-Seam Fastball", "Cutter", "Sinker", "Fastball")
  breaking  <- c("Knuckle Curve", "Slider", "Sweeper", "Curveball", "Slurve")
  offspeed  <- c("Changeup", "Splitter", "Forkball")
  
  if (group == "fastballs") {
    tbl <- tbl %>% filter(details.type.description %in% fastballs)
  } else if (group == "breaking") {
    tbl <- tbl %>% filter(details.type.description %in% breaking)
  } else if (group == "offspeed") {
    tbl <- tbl %>% filter(details.type.description %in% offspeed)
  }
  
  if (bat_hand %in% c("LHH", "LHB")) {
    tbl <- tbl %>% filter(matchup.batSide.description == "Left")
  } else if (bat_hand %in% c("RHH", "RHB")) {
    tbl <- tbl %>% filter(matchup.batSide.description == "Right")
  }
  
  if (pit_hand == "LHP") {
    tbl <- tbl %>% filter(matchup.pitchHand.description == "Left")
  } else if (pit_hand == "RHP") {
    tbl <- tbl %>% filter(matchup.pitchHand.description == "Right")
  }
  
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    tbl <- tbl %>% filter(game_date >= start_date)
  }
  
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    tbl <- tbl %>% filter(game_date <= end_date)
  }
  
  if (risp == "yes") {
    tbl <- tbl %>% filter(is_RISP == TRUE)
  } else if (risp == "no") {
    tbl <- tbl %>% filter(is_RISP == FALSE)
  }
  
  if (!is.null(pitch_type) && pitch_type != "all" && !is.na(pitch_type)) {
    tbl <- tbl %>% filter(details.type.description == !!pitch_type)
  }
  tbl
}

#Function 2.3.2
#Helper function to the major summary functions, establishes sample size minimums.
mask_below_threshold <- function(df, count_col, pct_threshold = NULL, keep_cols = "name") {
  library(dplyr)
  
  if (is.null(pct_threshold) || !(count_col %in% names(df)) || nrow(df) == 0) {
    return(df)
  }
  
  max_count <- suppressWarnings(max(df[[count_col]], na.rm = TRUE))
  
  if (!is.finite(max_count)) {
    return(df)
  }
  
  cutoff <- max_count * pct_threshold
  metric_cols <- setdiff(names(df), c(keep_cols, count_col))
  
  df %>%
    mutate(qualifies_threshold = .data[[count_col]] >= cutoff) %>%
    mutate(
      across(
        all_of(metric_cols),
        ~ ifelse(qualifies_threshold, ., NA)
      )
    ) %>%
    select(-qualifies_threshold)
}

#Function 2.3.3
add_metric_ranks <- function(df, high_good = NULL, low_good = NULL, by_group = NULL) {
  library(dplyr)
  
  high_good <- intersect(high_good %||% character(), names(df))
  low_good  <- intersect(low_good  %||% character(), names(df))
  
  if (length(c(high_good, low_good)) == 0) {
    return(df)
  }
  
  rank_one <- function(x, descending = TRUE) {
    out <- rep(NA_integer_, length(x))
    good <- !is.na(x)
    
    if (any(good)) {
      if (descending) {
        out[good] <- min_rank(desc(x[good]))
      } else {
        out[good] <- min_rank(x[good])
      }
    }
    out
  }
  
  if (!is.null(by_group) && length(by_group) > 0) {
    by_group <- intersect(by_group, names(df))
    
    df <- df %>%
      group_by(across(all_of(by_group))) %>%
      mutate(
        across(
          all_of(high_good),
          ~ rank_one(., descending = TRUE),
          .names = "{.col}_rank"
        ),
        across(
          all_of(low_good),
          ~ rank_one(., descending = FALSE),
          .names = "{.col}_rank"
        )
      ) %>%
      ungroup()
  } else {
    df <- df %>%
      mutate(
        across(
          all_of(high_good),
          ~ rank_one(., descending = TRUE),
          .names = "{.col}_rank"
        ),
        across(
          all_of(low_good),
          ~ rank_one(., descending = FALSE),
          .names = "{.col}_rank"
        )
      )
  }
  
  df
}

#Function 2.3.4
`%||%` <- function(x, y) if (is.null(x)) y else x

# NEW HELPER - normalize player/team level inputs
normalize_level <- function(level) {
  if (is.null(level)) return("player")
  level <- as.character(level)[1]
  if (level %in% c("players", "Player", "Players")) return("player")
  if (level %in% c("teams", "Team", "Teams")) return("team")
  level
}

# NEW HELPER - standard ID/name/team columns for batter/pitcher workflows
get_entity_cols <- function(entity = c("batter", "pitcher"), level = "player", team_col = NULL) {
  entity <- match.arg(entity)
  level <- normalize_level(level)
  
  if (entity == "batter") {
    list(
      id_col = if (level == "team") team_col %||% "batting_team" else "matchup.batter.id",
      name_col = if (level == "team") team_col %||% "batting_team" else "matchup.batter.fullName",
      team_col = team_col %||% "batting_team"
    )
  } else {
    list(
      id_col = if (level == "team") team_col %||% "fielding_team" else "matchup.pitcher.id",
      name_col = if (level == "team") team_col %||% "fielding_team" else "matchup.pitcher.fullName",
      team_col = team_col %||% "fielding_team"
    )
  }
}

filter_min_counts <- function(df,
                              min_pa = NULL,
                              min_pitches = NULL,
                              min_bbe = NULL,
                              pa_col = "PA",
                              pitches_col = "Pitches",
                              bbe_col = "BBE") {
  library(dplyr)
  out <- df
  if (!is.null(min_pa) && pa_col %in% names(out)) out <- out %>% filter(.data[[pa_col]] >= min_pa)
  if (!is.null(min_pitches) && pitches_col %in% names(out)) out <- out %>% filter(.data[[pitches_col]] >= min_pitches)
  if (!is.null(min_bbe) && bbe_col %in% names(out)) out <- out %>% filter(.data[[bbe_col]] >= min_bbe)
  out
}

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
  
  common <- list(
    views = views,
    level = level,
    team_col = team_col,
    group.by = group.by
  )
  
  a <- do.call(summarize_overall_pitcher, modifyList(common, split_a)) %>%
    filter_min_counts(
      min_pa = min_pa_each,
      min_pitches = min_pitches_each,
      min_bbe = min_bbe_each
    )
  
  b <- do.call(summarize_overall_pitcher, modifyList(common, split_b)) %>%
    filter_min_counts(
      min_pa = min_pa_each,
      min_pitches = min_pitches_each,
      min_bbe = min_bbe_each
    )
  
  id_col <- if (level == "team") team_col else "matchup.pitcher.id"
  
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


