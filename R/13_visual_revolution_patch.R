# -----------------------------------------------------------------------------
# R/15_visual_revolution_patch.R
# Visual Revolution patch layer
#
# Source this AFTER:
#   05_data_loading.R
#   02_summary_functions.R
#   10_pitchtype_viz_pack.R
#   11_visual_revolution_helpers.R
#   12_pitcher_visual_revolution.R
#   13_hitter_visual_revolution.R
#
# What this patch fixes/adds:
#   1. Repairs batted-ball direction flags in enhance_pbp()
#   2. Adds Pull/Middle/Oppo and pull/middle/oppo fly-ball coding
#   3. Recovers terminal pitch type for PA result summaries
#   4. Adds PA-ending results to pitcher pitch-type summaries
#   5. Fixes the hitter cur_stat percentile bug
#   6. Adds pitcher/hitter snapshot functions not grouped by pitch type
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Small helpers
# -----------------------------------------------------------------------------

vr_null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

vr_first_existing_col <- function(df, cols) {
  hit <- cols[cols %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

vr_add_missing_col <- function(df, col, value = NA) {
  if (!col %in% names(df)) df[[col]] <- value
  df
}

# -----------------------------------------------------------------------------
# Enhanced batted-ball coding repair
# -----------------------------------------------------------------------------

vr_add_batted_ball_direction_flags <- function(data,
                                               pitch_type_col = "pitch_type") {
  library(dplyr)
  library(lubridate)

  out <- data

  # Create a stable pitch_type column if the project data only has MLBAM names.
  if (!pitch_type_col %in% names(out)) {
    pitch_col <- vr_first_existing_col(
      out,
      c(
        "details.type.description",
        "pitchData.type.description",
        "pitchData.type.code",
        "details.type.code"
      )
    )
    if (!is.na(pitch_col)) out[[pitch_type_col]] <- out[[pitch_col]]
  }

  out <- vr_add_missing_col(out, "game_date", as.Date(NA))
  out <- vr_add_missing_col(out, "hitData.trajectory", NA_character_)
  out <- vr_add_missing_col(out, "hitData.location", NA_real_)
  out <- vr_add_missing_col(out, "matchup.batSide.description", NA_character_)
  out <- vr_add_missing_col(out, "matchup.batSide.code", NA_character_)
  out <- vr_add_missing_col(out, "hitData.launchSpeed", NA_real_)
  out <- vr_add_missing_col(out, "hitData.launchAngle", NA_real_)
  out <- vr_add_missing_col(out, "is_bbe", NA_integer_)
  out <- vr_add_missing_col(out, "is_hr", NA_integer_)

  out %>%
    mutate(
      game_date = suppressWarnings(as.Date(game_date)),
      year = ifelse(!is.na(game_date), lubridate::year(game_date), NA_integer_),
      year_month = ifelse(!is.na(game_date), format(game_date, "%Y-%m"), NA_character_),

      ev = suppressWarnings(as.numeric(.data[["hitData.launchSpeed"]])),
      la = suppressWarnings(as.numeric(.data[["hitData.launchAngle"]])),
      hit_location = suppressWarnings(as.numeric(.data[["hitData.location"]])),

      .traj = as.character(.data[["hitData.trajectory"]]),
      .bat_side = dplyr::case_when(
        !is.na(.data[["matchup.batSide.description"]]) ~ as.character(.data[["matchup.batSide.description"]]),
        .data[["matchup.batSide.code"]] == "R" ~ "Right",
        .data[["matchup.batSide.code"]] == "L" ~ "Left",
        TRUE ~ NA_character_
      ),

      # Trajectory-first batted-ball flags. Launch-angle fallbacks only fire when
      # trajectory is missing, which avoids mixing two definitions for FB/HRFB.
      is_gb = as.integer(
        is_bbe == 1 &
          (
            .traj %in% c("ground_ball", "bunt_grounder") |
              (is.na(.traj) & !is.na(la) & la < 10)
          )
      ),
      is_ld = as.integer(
        is_bbe == 1 &
          (
            .traj %in% c("line_drive", "bunt_line_drive") |
              (is.na(.traj) & !is.na(la) & la >= 10 & la < 25)
          )
      ),
      is_fb = as.integer(
        is_bbe == 1 &
          (
            .traj %in% c("fly_ball") |
              (is.na(.traj) & !is.na(la) & la >= 25 & la < 50)
          )
      ),
      is_pu = as.integer(
        is_bbe == 1 &
          (
            .traj %in% c("popup", "bunt_popup") |
              (is.na(.traj) & !is.na(la) & la >= 50)
          )
      ),
      is_air = as.integer(is_ld == 1 | is_fb == 1 | is_pu == 1),
      is_hard_hit = as.integer(is_bbe == 1 & !is.na(ev) & ev >= 95),
      is_barrel = as.integer(is_bbe == 1 & !is.na(ev) & !is.na(la) & ev >= 98 & la >= 24 & la <= 33),

      bbe_type = case_when(
        is_bbe != 1 ~ NA_character_,
        is_gb == 1 ~ "Grounder",
        is_ld == 1 ~ "LineDrive",
        is_fb == 1 ~ "FlyBall",
        is_pu == 1 ~ "Popup",
        TRUE ~ NA_character_
      ),

      spray_direction = case_when(
        is_bbe != 1 ~ NA_character_,

        .bat_side == "Right" & hit_location %in% c(5, 6, 7) ~ "Pull",
        .bat_side == "Right" & hit_location %in% c(8, 78, 89) ~ "Middle",
        .bat_side == "Right" & hit_location %in% c(9, 4, 3) ~ "Oppo",

        .bat_side == "Left" & hit_location %in% c(9, 4, 3) ~ "Pull",
        .bat_side == "Left" & hit_location %in% c(8, 78, 89) ~ "Middle",
        .bat_side == "Left" & hit_location %in% c(5, 6, 7) ~ "Oppo",

        TRUE ~ NA_character_
      ),

      is_pull = as.integer(spray_direction == "Pull"),
      is_middle = as.integer(spray_direction == "Middle"),
      is_center = is_middle,
      is_oppo = as.integer(spray_direction == "Oppo"),

      is_fbhr = as.integer(is_fb == 1 & is_hr == 1),
      is_pull_fb = as.integer(is_fb == 1 & is_pull == 1),
      is_middle_fb = as.integer(is_fb == 1 & is_middle == 1),
      is_center_fb = is_middle_fb,
      is_oppo_fb = as.integer(is_fb == 1 & is_oppo == 1),

      contact_quality = case_when(
        is_bbe != 1 | is.na(ev) ~ NA_character_,
        ev < 84.4 ~ "Weak",
        ev < 97.2 ~ "Average",
        TRUE ~ "Hard"
      ),

      bbe_type = factor(bbe_type, levels = c("Grounder", "LineDrive", "FlyBall", "Popup")),
      spray_direction = factor(spray_direction, levels = c("Pull", "Middle", "Oppo")),
      contact_quality = factor(contact_quality, levels = c("Weak", "Average", "Hard"))
    ) %>%
    select(-any_of(c(".traj", ".bat_side")))
}

# Store the old enhancer once, then override enhance_pbp() so new views inherit
# repaired batted-ball flags automatically.
if (exists("enhance_pbp", mode = "function") && !exists("enhance_pbp_base_vr", mode = "function")) {
  enhance_pbp_base_vr <- enhance_pbp
}

enhance_pbp <- function(data) {
  if (exists("enhance_pbp_base_vr", mode = "function")) {
    out <- enhance_pbp_base_vr(data)
  } else {
    out <- data
  }
  vr_add_batted_ball_direction_flags(out)
}

repair_visual_revolution_views <- function(views) {
  if (is.null(views) || !is.list(views)) stop("`views` must be a views list.", call. = FALSE)

  if ("pbp" %in% names(views) && is.data.frame(views$pbp)) {
    views$pbp <- vr_add_batted_ball_direction_flags(views$pbp)
  }
  if ("pitch" %in% names(views) && is.data.frame(views$pitch)) {
    views$pitch <- vr_add_batted_ball_direction_flags(views$pitch)
  }
  if ("pa" %in% names(views) && is.data.frame(views$pa)) {
    views$pa <- vr_add_batted_ball_direction_flags(views$pa)
  }
  if ("bbe" %in% names(views) && is.data.frame(views$bbe)) {
    views$bbe <- vr_add_batted_ball_direction_flags(views$bbe)
  }

  views
}

# -----------------------------------------------------------------------------
# Terminal PA table helper
# -----------------------------------------------------------------------------

vr_terminal_pa_table <- function(views,
                                 pitch_type_col = "pitch_type") {
  library(dplyr)

  if (is.null(views) || !is.list(views)) stop("`views` must be a views list.", call. = FALSE)
  if (!"pitch" %in% names(views) || is.null(views$pitch)) stop("views$pitch is required.", call. = FALSE)

  pitch_tbl <- vr_add_batted_ball_direction_flags(views$pitch, pitch_type_col = pitch_type_col)
  pa_tbl <- if ("pa" %in% names(views) && is.data.frame(views$pa)) {
    vr_add_batted_ball_direction_flags(views$pa, pitch_type_col = pitch_type_col)
  } else {
    NULL
  }

  # If the PA table already has pitch_type, use it. Otherwise recover the terminal
  # pitch row and join PA outcome flags back onto it.
  if (!is.null(pa_tbl) && pitch_type_col %in% names(pa_tbl) && any(!is.na(pa_tbl[[pitch_type_col]]))) {
    return(pa_tbl)
  }

  if (!pitch_type_col %in% names(pitch_tbl)) {
    stop("Could not find or create `", pitch_type_col, "` in views$pitch.", call. = FALSE)
  }

  key_cols <- c("game_pk", "atBatIndex")
  if (!all(key_cols %in% names(pitch_tbl))) {
    stop("views$pitch must contain game_pk and atBatIndex to recover terminal pitch type.", call. = FALSE)
  }

  if ("pitchNumber" %in% names(pitch_tbl)) {
    pitch_tbl$.vr_pitch_number <- suppressWarnings(as.numeric(pitch_tbl$pitchNumber))
  } else {
    pitch_tbl$.vr_pitch_number <- seq_len(nrow(pitch_tbl))
  }

  terminal <- pitch_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    group_by(across(all_of(key_cols))) %>%
    arrange(.data$.vr_pitch_number, .by_group = TRUE) %>%
    filter(
      if ("is_last_pitch" %in% names(.)) {
        is_last_pitch == 1 | dplyr::row_number() == dplyr::n()
      } else {
        dplyr::row_number() == dplyr::n()
      }
    ) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-any_of(".vr_pitch_number"))

  if (!is.null(pa_tbl) && all(key_cols %in% names(pa_tbl))) {
    outcome_cols <- intersect(
      c(
        key_cols,
        "is_pa", "is_at_bat", "is_hit", "is_k", "is_bb", "is_hr",
        "total_bases", "result.eventType",
        "matchup.batter.id", "matchup.batter.fullName",
        "matchup.pitcher.id", "matchup.pitcher.fullName",
        "batting_team", "fielding_team",
        "matchup.batSide.code", "matchup.batSide.description",
        "matchup.pitchHand.code", "matchup.pitchHand.description"
      ),
      names(pa_tbl)
    )

    outcome <- pa_tbl %>%
      select(all_of(outcome_cols)) %>%
      distinct(across(all_of(key_cols)), .keep_all = TRUE)

    terminal <- terminal %>%
      left_join(outcome, by = key_cols, suffix = c("", ".pa"))

    overwrite_cols <- setdiff(outcome_cols, key_cols)
    for (col in overwrite_cols) {
      pa_col <- paste0(col, ".pa")
      if (pa_col %in% names(terminal)) {
        terminal[[col]] <- terminal[[pa_col]]
        terminal[[pa_col]] <- NULL
      }
    }
  }

  terminal
}

# -----------------------------------------------------------------------------
# Safer percentile builder, fixing the cur_stat bug
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
    left_join(league_values, by = c(group_cols, "stat"))

  if (nrow(comparison) > 0) {
    comparison$percentile <- vapply(seq_len(nrow(comparison)), function(i) {
      cur_stat <- comparison$stat[i]
      tmp <- summary_df
      if (length(group_cols) > 0) {
        for (gc in group_cols) {
          tmp <- tmp[tmp[[gc]] == comparison[[gc]][i], , drop = FALSE]
        }
      }
      vr_pct_rank_value(comparison$player_value[i], tmp[[cur_stat]])
    }, numeric(1))
  } else {
    comparison$percentile <- numeric(0)
  }

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_pool,
    league_values = league_values,
    comparison = comparison,
    stats = stats
  )
}

# -----------------------------------------------------------------------------
# Hitter by-pitch-type summary patch: uses terminal pitch rows for PA results
# -----------------------------------------------------------------------------

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
  pa_tbl <- vr_terminal_pa_table(views, pitch_type_col = pitch_type_col) %>%
    vr_apply_hitter_filters(group, bat_hand, pit_hand, start_date, end_date, pitch_type)

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

# More accurate BBE-by-pitchtype summary with direction and FB-denominator pull FB.
summarize_bbe_by_pitchtype_batter <- function(bbe_tbl,
                                              level = "player",
                                              group.by = NULL,
                                              team_col = "batting_team",
                                              pitch_type_col = "pitch_type") {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)
  bbe_tbl <- vr_add_batted_ball_direction_flags(bbe_tbl, pitch_type_col = pitch_type_col)

  if (!pitch_type_col %in% names(bbe_tbl)) {
    stop("BBE table does not contain pitch_type_col: ", pitch_type_col, call. = FALSE)
  }

  id_col <- if (level == "team") team_col else "matchup.batter.id"
  name_col <- if (level == "team") team_col else "matchup.batter.fullName"

  if (level != "team") {
    bbe_tbl <- bbe_tbl %>% filter(!is.na(.data[[id_col]]))
  }

  grouping_cols <- c(id_col, group.by, pitch_type_col)

  bbe_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
      hand = dplyr::first(.data[["matchup.batSide.code"]]),
      BBE = n(),
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      LD = sum(is_ld, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      PU = sum(is_pu, na.rm = TRUE),
      Pull = sum(is_pull, na.rm = TRUE),
      Middle = sum(is_middle, na.rm = TRUE),
      Oppo = sum(is_oppo, na.rm = TRUE),
      pull_fb = sum(is_pull_fb, na.rm = TRUE),
      middle_fb = sum(is_middle_fb, na.rm = TRUE),
      oppo_fb = sum(is_oppo_fb, na.rm = TRUE),
      FBHR = sum(is_fbhr, na.rm = TRUE),
      HardHit_pct = vr_safe_rate(HardHit, BBE),
      barrel_pct = vr_safe_rate(barrels, BBE),
      GB_pct = vr_safe_rate(GB, BBE),
      LD_pct = vr_safe_rate(LD, BBE),
      FB_pct = vr_safe_rate(FB, BBE),
      PU_pct = vr_safe_rate(PU, BBE),
      Pull_pct = vr_safe_rate(Pull, BBE),
      Middle_pct = vr_safe_rate(Middle, BBE),
      Oppo_pct = vr_safe_rate(Oppo, BBE),
      pull_fbpct = vr_safe_rate(pull_fb, FB),
      Pull_FB_pct = vr_safe_rate(pull_fb, FB),
      Middle_FB_pct = vr_safe_rate(middle_fb, FB),
      Oppo_FB_pct = vr_safe_rate(oppo_fb, FB),
      HRFB = vr_safe_rate(FBHR, FB),
      EV = mean(ev, na.rm = TRUE),
      maxEV = suppressWarnings(max(ev, na.rm = TRUE)),
      LA = mean(la, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(maxEV = ifelse(is.infinite(maxEV), NA_real_, maxEV))
}

# -----------------------------------------------------------------------------
# Pitcher PA-ending pitch-type results patch
# -----------------------------------------------------------------------------

if (exists("summarize_overall_pitcher_by_pitchtype", mode = "function") &&
    !exists("summarize_overall_pitcher_by_pitchtype_base_vr", mode = "function")) {
  summarize_overall_pitcher_by_pitchtype_base_vr <- summarize_overall_pitcher_by_pitchtype
}

summarize_pa_by_pitchtype_pitcher_vr <- function(pa_tbl,
                                                 level = "player",
                                                 group.by = NULL,
                                                 team_col = "fielding_team",
                                                 pitch_type_col = "pitch_type") {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)
  pa_tbl <- vr_add_batted_ball_direction_flags(pa_tbl, pitch_type_col = pitch_type_col)

  if (!pitch_type_col %in% names(pa_tbl)) {
    stop("PA terminal table does not contain pitch_type_col: ", pitch_type_col, call. = FALSE)
  }

  id_col <- if (level == "team") team_col else if (level == "league") ".league" else "matchup.pitcher.id"
  name_col <- if (level == "team") team_col else if (level == "league") ".league" else "matchup.pitcher.fullName"

  if (level == "league") pa_tbl <- pa_tbl %>% mutate(.league = "League")
  if (level == "player") pa_tbl <- pa_tbl %>% filter(!is.na(.data[[id_col]]))

  grouping_cols <- c(id_col, group.by, pitch_type_col)

  pa_tbl %>%
    filter(!is.na(.data[[pitch_type_col]])) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      name = dplyr::first(.data[[name_col]]),
      team = dplyr::first(.data[[team_col]]),
      PA = n(),
      AB = sum(is_at_bat, na.rm = TRUE),
      H = sum(is_hit, na.rm = TRUE),
      SO = sum(is_k, na.rm = TRUE),
      BB = sum(is_bb, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      AVG = vr_safe_rate(H, AB),
      K_pct = vr_safe_rate(SO, PA),
      BB_pct = vr_safe_rate(BB, PA),
      HR_pct = vr_safe_rate(HR, PA),
      SLG = vr_safe_rate(TB, AB),
      OBP = vr_safe_rate(H + BB, PA),
      OPS = SLG + OBP,
      ISO = SLG - AVG,
      .groups = "drop"
    )
}

summarize_overall_pitcher_by_pitchtype <- function(views,
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
                                                   min_pitch_pct = NULL,
                                                   min_bbe_pct = NULL) {
  library(dplyr)

  group.by <- vr_clean_group_by(group.by)

  if (!exists("summarize_overall_pitcher_by_pitchtype_base_vr", mode = "function")) {
    stop("Base summarize_overall_pitcher_by_pitchtype() was not found. Source 02_summary_functions.R first.", call. = FALSE)
  }

  base_df <- summarize_overall_pitcher_by_pitchtype_base_vr(
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

  terminal_pa <- vr_terminal_pa_table(views, pitch_type_col = "pitch_type")

  if (exists("apply_split_filters", mode = "function")) {
    terminal_pa <- apply_split_filters(
      terminal_pa,
      group = group,
      bat_hand = bat_hand,
      pit_hand = pit_hand,
      start_date = start_date,
      end_date = end_date,
      risp = risp,
      pitch_type = pitch_type
    )
  }

  pa_sum <- summarize_pa_by_pitchtype_pitcher_vr(
    terminal_pa,
    level = level,
    group.by = group.by,
    team_col = team_col,
    pitch_type_col = "pitch_type"
  )

  id_keep <- if (level == "team") team_col else if (level == "league") ".league" else "matchup.pitcher.id"
  join_cols <- c(id_keep, group.by, "pitch_type")

  base_df %>%
    full_join(
      pa_sum %>% select(-any_of(c("name", "team", "hand"))),
      by = join_cols
    ) %>%
    mutate(
      pitch = pitch_type,
      name_ref_1 = name,
      pitch_ref_1 = pitch_type
    )
}

# -----------------------------------------------------------------------------
# Pitcher stat blocks adjusted now that pitch-type result stats are supported
# -----------------------------------------------------------------------------

pitcher_stat_blocks <- function() {
  list(
    usage = c("Usage_pct"),
    command = c("Zone_pct", "Heart_pct", "Shadow_pct"),
    approach = c("Swing_pct", "Chase_pct", "ZSwing_pct"),
    miss_bat = c("Whiff_pct", "Contact_pct", "OContact_pct", "ZContact_pct"),
    contact_allowed = c("HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB"),
    shape = c("velo", "spin", "IVB", "HB", "HAA", "VAA"),
    results = c("K_pct", "BB_pct", "AVG", "SLG", "OPS", "HR_pct"),
    full = c(
      "Usage_pct", "Zone_pct", "Heart_pct", "Chase_pct", "Whiff_pct", "Contact_pct",
      "velo", "spin", "IVB", "HB", "HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB",
      "K_pct", "BB_pct", "SLG"
    )
  )
}

hitter_stat_blocks <- function() {
  list(
    plate_discipline = c("BB_pct", "K_pct", "Swing_pct", "Chase_pct", "Zone_pct", "ZSwing_pct"),
    contact = c("Contact_pct", "Whiff_pct", "ZContact_pct", "OContact_pct"),
    contact_quality = c("EV", "maxEV", "LA", "HardHit_pct", "barrel_pct"),
    batted_ball = c("GB_pct", "LD_pct", "FB_pct", "PU_pct"),
    spray = c("Pull_pct", "Middle_pct", "Oppo_pct", "Pull_FB_pct", "Middle_FB_pct", "Oppo_FB_pct"),
    damage = c("AVG", "OBP", "SLG", "OPS", "ISO", "HR_pct", "XBH_pct"),
    full = c(
      "BB_pct", "K_pct", "Chase_pct", "Whiff_pct", "Contact_pct",
      "EV", "HardHit_pct", "barrel_pct", "GB_pct", "Pull_pct", "SLG", "OPS", "ISO"
    )
  )
}

# -----------------------------------------------------------------------------
# Snapshot profiles: not grouped by pitch type
# -----------------------------------------------------------------------------

vr_build_snapshot_profile <- function(summary_df,
                                      player,
                                      stats,
                                      player_col = "name",
                                      group_cols = NULL,
                                      league_method = c("weighted_mean", "median", "mean"),
                                      exclude_player_from_league = TRUE) {
  library(dplyr)
  library(tidyr)

  league_method <- match.arg(league_method)
  group_cols <- vr_clean_group_by(group_cols)
  stats <- stats[stats %in% names(summary_df)]

  if (length(stats) == 0) stop("No requested stats were available for snapshot profile.", call. = FALSE)

  player_df <- summary_df %>% filter(.data[[player_col]] == player)
  if (nrow(player_df) == 0) stop("No rows found for player: ", player, call. = FALSE)

  league_pool <- summary_df
  if (isTRUE(exclude_player_from_league)) league_pool <- league_pool %>% filter(.data[[player_col]] != player)

  league_values <- vr_league_values_by_group(
    summary_df = league_pool,
    stats = stats,
    group_cols = group_cols,
    method = league_method
  )

  player_long <- player_df %>%
    tidyr::pivot_longer(cols = all_of(stats), names_to = "stat", values_to = "player_value") %>%
    filter(!is.na(player_value))

  comparison <- player_long %>%
    left_join(league_values, by = c(group_cols, "stat"))

  if (nrow(comparison) > 0) {
    comparison$percentile <- vapply(seq_len(nrow(comparison)), function(i) {
      cur_stat <- comparison$stat[i]
      tmp <- summary_df
      if (length(group_cols) > 0) {
        for (gc in group_cols) tmp <- tmp[tmp[[gc]] == comparison[[gc]][i], , drop = FALSE]
      }
      vr_pct_rank_value(comparison$player_value[i], tmp[[cur_stat]])
    }, numeric(1))
  } else {
    comparison$percentile <- numeric(0)
  }

  list(
    summary_df = summary_df,
    player_df = player_df,
    league_df = league_pool,
    league_values = league_values,
    comparison = comparison,
    stats = stats
  )
}

vr_snapshot_plot_data <- function(comparison,
                                  player,
                                  stats,
                                  percent_stats = vr_percent_stats(),
                                  scale_percents = TRUE,
                                  stat_labels = NULL,
                                  league_label = "League Avg",
                                  show_percentiles = TRUE) {
  library(dplyr)
  library(tidyr)

  comparison %>%
    filter(stat %in% stats) %>%
    pivot_longer(cols = c(player_value, league_value), names_to = "comparison_type", values_to = "raw_value") %>%
    mutate(
      comparison = if_else(comparison_type == "player_value", player, league_label),
      value = if_else(scale_percents & stat %in% percent_stats, raw_value * 100, raw_value),
      stat_label = vr_label_stats(stat, stat_labels = stat_labels, percent_stats = NULL),
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      pct_label = if_else(comparison == player & show_percentiles & !is.na(percentile), paste0(round(percentile), "th pct"), NA_character_),
      label = if_else(comparison == player & !is.na(pct_label), paste0(value_label, "\n", pct_label), value_label),
      comparison = factor(comparison, levels = c(player, league_label)),
      stat_label = factor(stat_label, levels = vr_label_stats(stats, stat_labels = stat_labels, percent_stats = NULL))
    ) %>%
    filter(!is.na(value))
}

vr_plot_snapshot_facets <- function(plot_data,
                                    player,
                                    title = NULL,
                                    subtitle = NULL,
                                    facet_scales = "free_y",
                                    show_values = TRUE,
                                    ncol = NULL,
                                    base_size = 12,
                                    use_team_theme = FALSE,
                                    team_abbr = NULL) {
  library(ggplot2)

  if (is.null(title)) title <- paste0(player, " Snapshot Profile")
  if (is.null(subtitle)) subtitle <- "Bars compare selected player to league average. Player labels show raw value percentile."

  p <- ggplot(plot_data, aes(x = comparison, y = value, fill = comparison)) +
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

plot_pitcher_snapshot_stat_blocks <- function(views,
                                              pitcher,
                                              stat_block = "full",
                                              stats = NULL,
                                              group = "none",
                                              bat_hand = "none",
                                              pit_hand = "none",
                                              start_date = NULL,
                                              end_date = NULL,
                                              group.by = NULL,
                                              team_col = "fielding_team",
                                              min_pa = 50,
                                              min_pitch_pct = NULL,
                                              min_bbe_pct = NULL,
                                              min_pa_pct = NULL,
                                              risp = "all",
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

  blocks <- list(
    results = c("K_pct", "BB_pct", "AVG", "OBP", "SLG", "OPS", "HR"),
    command = c("Zone_pct", "Heart_pct", "Shadow_pct"),
    approach = c("Swing_pct", "Chase_pct", "ZSwing_pct"),
    miss_bat = c("Whiff_pct", "Contact_pct", "OContact_pct", "ZContact_pct"),
    contact_allowed = c("HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB"),
    full = c("K_pct", "BB_pct", "Whiff_pct", "Chase_pct", "Zone_pct", "HardHit_pct", "barrel_pct", "GB_pct", "SLG", "HRFB")
  )

  summary_df <- summarize_overall_pitcher(
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
    min_pa_pct = min_pa_pct,
    risp = risp
  )

  if (!is.null(min_pa) && "PA" %in% names(summary_df)) summary_df <- summary_df %>% filter(PA >= min_pa | name == pitcher)

  requested_stats <- vr_resolve_stat_block(stat_block, stats, blocks, available_stats = names(summary_df))

  profile <- vr_build_snapshot_profile(
    summary_df = summary_df,
    player = pitcher,
    stats = requested_stats,
    player_col = "name",
    group_cols = group.by,
    league_method = league_method,
    exclude_player_from_league = exclude_player_from_league
  )

  plot_data <- vr_snapshot_plot_data(
    comparison = profile$comparison,
    player = pitcher,
    stats = profile$stats,
    percent_stats = percent_stats,
    scale_percents = scale_percents,
    stat_labels = stat_labels,
    league_label = league_label,
    show_percentiles = show_percentiles
  )

  if (is.null(title)) title <- paste0(pitcher, " Pitcher Snapshot")
  if (is.null(subtitle)) subtitle <- paste0("Overall profile | stat block: ", stat_block, " | min PA: ", min_pa)

  p <- vr_plot_snapshot_facets(plot_data, pitcher, title, subtitle, facet_scales, show_values, ncol, base_size, use_team_theme, team_abbr)
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

plot_hitter_snapshot_stat_blocks <- function(views,
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
  plot_hitter_stat_blocks(
    views = views,
    hitter = hitter,
    stat_block = stat_block,
    stats = stats,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date,
    group.by = group.by,
    team_col = team_col,
    min_pa = min_pa,
    min_pitch_pct = min_pitch_pct,
    min_bbe_pct = min_bbe_pct,
    min_pa_pct = min_pa_pct,
    league_method = match.arg(league_method),
    exclude_player_from_league = exclude_player_from_league,
    percent_stats = percent_stats,
    scale_percents = scale_percents,
    stat_labels = stat_labels,
    league_label = league_label,
    show_percentiles = show_percentiles,
    facet_scales = facet_scales,
    show_values = show_values,
    ncol = ncol,
    base_size = base_size,
    title = vr_null_coalesce(title, paste0(hitter, " Hitter Snapshot")),
    subtitle = vr_null_coalesce(subtitle, paste0("Overall profile | stat block: ", stat_block, " | min PA: ", min_pa)),
    use_team_theme = use_team_theme,
    team_abbr = team_abbr,
    print_plot = print_plot
  )
}

# -----------------------------------------------------------------------------
# Patched hitter pitch-type profile and distribution functions
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
    left_join(league_values, by = c(group.by, pitch_type_col, "stat"))

  if (nrow(comparison) > 0) {
    comparison$percentile <- vapply(seq_len(nrow(comparison)), function(i) {
      cur_stat <- comparison$stat[i]
      cur_pitch <- comparison[[pitch_type_col]][i]
      vals <- summary_df %>%
        filter(.data[[pitch_type_col]] == cur_pitch, !is.na(.data[[cur_stat]])) %>%
        pull(all_of(cur_stat))
      vr_pct_rank_value(comparison$player_value[i], vals)
    }, numeric(1))
  } else {
    comparison$percentile <- numeric(0)
  }

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

  if (!is.null(min_league_pitches) && "Pitches" %in% names(summary_df)) {
    league_df <- summary_df %>% filter(Pitches >= min_league_pitches | .data[[hitter_col]] == hitter)
  } else {
    league_df <- summary_df
  }

  player_df <- league_df %>%
    filter(.data[[hitter_col]] == hitter) %>%
    filter(is.na(min_player_pitches) | !"Pitches" %in% names(.) | Pitches >= min_player_pitches)

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
    )

  if (nrow(player_long) > 0) {
    player_long$percentile <- vapply(seq_len(nrow(player_long)), function(i) {
      cur_stat <- player_long$stat[i]
      cur_pitch <- player_long[[pitch_type_col]][i]
      vals <- league_long %>%
        filter(.data$stat == cur_stat, .data[[pitch_type_col]] == cur_pitch) %>%
        pull(raw_value)
      vr_pct_rank_value(player_long$raw_value[i], vals)
    }, numeric(1))
  } else {
    player_long$percentile <- numeric(0)
  }

  player_long <- player_long %>%
    mutate(
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      label = ifelse(!is.na(percentile), paste0(value_label, "\n", round(percentile), "th pct"), value_label)
    )

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


# Patched pitcher distribution profile: same visual as 12_ file, but percentile
# calculation avoids rowwise cur_stat scoping issues.
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

  if (nrow(player_df) == 0) stop("No rows found for pitcher: ", pitcher, call. = FALSE)

  league_df <- summary_df %>%
    filter(is.na(min_league_pitches) | !"Pitches" %in% names(.) | Pitches >= min_league_pitches)

  if (isTRUE(exclude_player_from_league)) league_df <- league_df %>% filter(.data[[pitcher_col]] != pitcher)

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

  if (nrow(player_long) > 0) {
    player_long$percentile <- vapply(seq_len(nrow(player_long)), function(i) {
      cur_stat <- player_long$stat[i]
      cur_pitch <- player_long[[pitch_type_col]][i]
      vals <- league_long %>%
        filter(.data$stat == cur_stat, .data[[pitch_type_col]] == cur_pitch) %>%
        pull(raw_value)
      vr_pct_rank_value(player_long$raw_value[i], vals)
    }, numeric(1))
  } else {
    player_long$percentile <- numeric(0)
  }

  player_long <- player_long %>%
    mutate(
      value_label = vapply(seq_along(raw_value), function(i) vr_format_value(raw_value[i], stat[i], percent_stats, scale_percents), character(1)),
      label = ifelse(!is.na(percentile), paste0(value_label, "\n", round(percentile), "th pct"), value_label)
    )

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

  if (is.null(title)) title <- paste0(pitcher, " Pitch-Type Distribution Profile")
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Box plots show league distribution by pitch type; point shows ", pitcher,
      " | stat block: ", stat_block,
      " | min league pitches: ", min_league_pitches
    )
  }

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
    p <- p + geom_text(data = player_long, aes(x = .data[[pitch_type_col]], y = value, label = label), vjust = -0.4, size = 2.7, lineheight = 0.9, inherit.aes = FALSE)
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
# R/16_visual_revolution_spray_ba_patch.R
# Visual Revolution patch: spray-profile AVG/SLG denominator repair
#
# Source this AFTER 15_visual_revolution_patch.R.
#
# Why this exists:
#   The original spray-direction profile calculated AVG and SLG as H / BBE and
#   TB / BBE. That is not the same as batting average or slugging. For a spray
#   split, use directed official at-bats as the denominator:
#       AVG = H / AB
#       SLG = TB / AB
#   BBE remains the denominator for contact-quality and batted-ball rates.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Small internal helpers
# -----------------------------------------------------------------------------

vr_spray_safe_rate <- function(num, den) {
  ifelse(!is.na(den) & den > 0, num / den, NA_real_)
}

vr_spray_format_value <- function(value,
                                  stat,
                                  percent_stats = vr_percent_stats(),
                                  scale_percents = TRUE) {
  if (length(value) == 0 || is.na(value)) return(NA_character_)
  
  if (stat %in% percent_stats) {
    val <- if (isTRUE(scale_percents)) value * 100 else value
    return(paste0(round(val, 1), "%"))
  }
  
  if (stat %in% c("AVG", "SLG", "ISO", "BABIP")) {
    return(sprintf("%.3f", round(value, 3)))
  }
  
  if (stat %in% c("EV", "maxEV", "LA", "velo", "IVB", "HB", "HAA", "VAA")) {
    return(as.character(round(value, 1)))
  }
  
  if (stat == "spin") return(as.character(round(value, 0)))
  
  as.character(round(value, 2))
}

vr_spray_weight_col_for_stat <- function(stat, available_cols) {
  preferred <- switch(
    stat,
    AVG = "AB",
    SLG = "AB",
    ISO = "AB",
    BABIP = "BABIP_denom",
    XBH_pct = "AB",
    HR_pct = "AB",
    HardHit_pct = "BBE",
    barrel_pct = "BBE",
    Barrel_pct = "BBE",
    GB_pct = "BBE",
    FB_pct = "BBE",
    LD_pct = "BBE",
    PU_pct = "BBE",
    Pull_pct = "BBE",
    Middle_pct = "BBE",
    Oppo_pct = "BBE",
    EV = "BBE",
    maxEV = "BBE",
    LA = "BBE",
    HRFB = "FB",
    "BBE"
  )
  
  fallbacks <- unique(c(preferred, "AB", "BBE", "PA", "Pitches"))
  hit <- fallbacks[fallbacks %in% available_cols]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

vr_spray_league_values_by_group <- function(summary_df,
                                            stats,
                                            group_cols = "spray_direction",
                                            method = c("weighted_mean", "median", "mean"),
                                            min_weight = 0) {
  library(dplyr)
  library(tidyr)
  
  method <- match.arg(method)
  group_cols <- vr_clean_group_by(group_cols)
  stats <- stats[stats %in% names(summary_df)]
  
  if (length(stats) == 0) {
    stop("No selected stats are present in `summary_df`.", call. = FALSE)
  }
  
  base_df <- summary_df %>% dplyr::mutate(.vr_row_id = dplyr::row_number())
  
  long <- base_df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stats), names_to = "stat", values_to = "value") %>%
    dplyr::mutate(
      .weight_col = vapply(stat, vr_spray_weight_col_for_stat, character(1), available_cols = names(base_df)),
      .weight = vapply(
        seq_len(dplyr::n()),
        function(i) {
          col <- .weight_col[i]
          row_id <- .vr_row_id[i]
          if (is.na(col) || !col %in% names(base_df)) return(NA_real_)
          suppressWarnings(as.numeric(base_df[[col]][row_id]))
        },
        numeric(1)
      )
    ) %>%
    dplyr::filter(!is.na(value))
  
  if (!is.null(group_cols) && length(group_cols) > 0) {
    group_expr <- rlang::syms(c(group_cols, "stat"))
  } else {
    group_expr <- rlang::syms("stat")
  }
  
  long %>%
    dplyr::group_by(!!!group_expr) %>%
    dplyr::summarise(
      league_value = dplyr::case_when(
        method == "weighted_mean" ~ vr_weighted_mean_safe(value, .weight),
        method == "median" ~ median(value, na.rm = TRUE),
        method == "mean" ~ mean(value, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      league_n = dplyr::n(),
      league_weight = sum(.weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.na(min_weight) | league_weight >= min_weight | league_n > 0)
}

vr_prepare_spray_bbe_table <- function(bbe_tbl, spray_col = "spray_direction") {
  library(dplyr)
  
  if (exists("vr_add_batted_ball_direction_flags", mode = "function")) {
    bbe_tbl <- vr_add_batted_ball_direction_flags(bbe_tbl)
  }
  
  if (!spray_col %in% names(bbe_tbl)) {
    stop("Could not find spray column: ", spray_col, call. = FALSE)
  }
  
  if (!"result.eventType" %in% names(bbe_tbl)) bbe_tbl$result.eventType <- NA_character_
  if (!"is_hit" %in% names(bbe_tbl)) {
    bbe_tbl$is_hit <- as.integer(bbe_tbl$result.eventType %in% c("single", "double", "triple", "home_run"))
  }
  if (!"is_hr" %in% names(bbe_tbl)) {
    bbe_tbl$is_hr <- as.integer(bbe_tbl$result.eventType == "home_run")
  }
  if (!"is_at_bat" %in% names(bbe_tbl)) {
    pa_only_events <- c(
      "walk", "intent_walk", "hit_by_pitch",
      "sac_fly", "sac_bunt", "sac_fly_double_play",
      "sac_bunt_double_play", "catcher_interf"
    )
    bbe_tbl$is_at_bat <- as.integer(!(bbe_tbl$result.eventType %in% pa_only_events))
  }
  if (!"total_bases" %in% names(bbe_tbl)) {
    bbe_tbl$total_bases <- dplyr::case_when(
      bbe_tbl$result.eventType == "single" ~ 1,
      bbe_tbl$result.eventType == "double" ~ 2,
      bbe_tbl$result.eventType == "triple" ~ 3,
      bbe_tbl$result.eventType == "home_run" ~ 4,
      bbe_tbl$is_at_bat == 1 ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  for (col in c("is_hard_hit", "is_barrel", "is_gb", "is_fb", "is_ld", "is_pu")) {
    if (!col %in% names(bbe_tbl)) bbe_tbl[[col]] <- NA_integer_
  }
  if (!"ev" %in% names(bbe_tbl)) bbe_tbl$ev <- NA_real_
  if (!"la" %in% names(bbe_tbl)) bbe_tbl$la <- NA_real_
  
  bbe_tbl %>%
    mutate(
      .event = as.character(result.eventType),
      .is_hit = as.integer(is_hit),
      .is_hr = as.integer(is_hr),
      .is_at_bat = as.integer(is_at_bat),
      .tb = suppressWarnings(as.numeric(total_bases)),
      .is_xbh = as.integer(.is_hit == 1 & .tb >= 2),
      .is_sf = as.integer(.event %in% c("sac_fly", "sac_fly_double_play"))
    )
}

# -----------------------------------------------------------------------------
# Reusable spray summary table
# -----------------------------------------------------------------------------

summarize_hitter_spray_direction <- function(views,
                                             group = "none",
                                             bat_hand = "none",
                                             pit_hand = "none",
                                             start_date = NULL,
                                             end_date = NULL,
                                             team_col = "batting_team",
                                             player_id_col = "matchup.batter.id",
                                             hitter_col = "matchup.batter.fullName",
                                             spray_col = "spray_direction") {
  library(dplyr)
  
  bbe_tbl <- vr_apply_hitter_filters(views$bbe, group, bat_hand, pit_hand, start_date, end_date)
  bbe_tbl <- vr_prepare_spray_bbe_table(bbe_tbl, spray_col = spray_col)
  
  if (!player_id_col %in% names(bbe_tbl)) {
    stop("Could not find player_id_col: ", player_id_col, call. = FALSE)
  }
  if (!hitter_col %in% names(bbe_tbl)) {
    stop("Could not find hitter_col: ", hitter_col, call. = FALSE)
  }
  if (!team_col %in% names(bbe_tbl)) bbe_tbl[[team_col]] <- NA_character_
  
  out <- bbe_tbl %>%
    filter(!is.na(.data[[spray_col]]), !is.na(.data[[player_id_col]])) %>%
    group_by(.data[[player_id_col]], .data[[spray_col]]) %>%
    summarise(
      name = dplyr::first(.data[[hitter_col]]),
      team = dplyr::first(.data[[team_col]]),
      BBE = n(),
      AB = sum(.is_at_bat, na.rm = TRUE),
      H = sum(.is_hit, na.rm = TRUE),
      HR = sum(.is_hr, na.rm = TRUE),
      XBH = sum(.is_xbh, na.rm = TRUE),
      TB = sum(.tb, na.rm = TRUE),
      SF = sum(.is_sf, na.rm = TRUE),
      BABIP_denom = AB - HR + SF,
      HardHit = sum(is_hard_hit, na.rm = TRUE),
      barrels = sum(is_barrel, na.rm = TRUE),
      GB = sum(is_gb, na.rm = TRUE),
      FB = sum(is_fb, na.rm = TRUE),
      LD = sum(is_ld, na.rm = TRUE),
      PU = sum(is_pu, na.rm = TRUE),
      HardHit_pct = vr_spray_safe_rate(HardHit, BBE),
      barrel_pct = vr_spray_safe_rate(barrels, BBE),
      GB_pct = vr_spray_safe_rate(GB, BBE),
      FB_pct = vr_spray_safe_rate(FB, BBE),
      LD_pct = vr_spray_safe_rate(LD, BBE),
      PU_pct = vr_spray_safe_rate(PU, BBE),
      AVG = vr_spray_safe_rate(H, AB),
      SLG = vr_spray_safe_rate(TB, AB),
      ISO = SLG - AVG,
      BABIP = vr_spray_safe_rate(H - HR, BABIP_denom),
      HR_pct = vr_spray_safe_rate(HR, AB),
      XBH_pct = vr_spray_safe_rate(XBH, AB),
      EV = mean(ev, na.rm = TRUE),
      maxEV = suppressWarnings(max(ev, na.rm = TRUE)),
      LA = mean(la, na.rm = TRUE),
      AVG_old_bbe_denominator = vr_spray_safe_rate(H, BBE),
      SLG_old_bbe_denominator = vr_spray_safe_rate(TB, BBE),
      .groups = "drop"
    ) %>%
    mutate(
      maxEV = ifelse(is.infinite(maxEV), NA_real_, maxEV)
    )
  
  names(out)[names(out) == spray_col] <- "spray_direction"
  out
}

# -----------------------------------------------------------------------------
# Replacement spray profile plot
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
  
  spray_summary <- summarize_hitter_spray_direction(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date,
    team_col = team_col,
    player_id_col = player_id_col,
    hitter_col = "matchup.batter.fullName",
    spray_col = spray_col
  )
  
  blocks <- list(
    contact_quality = c("EV", "maxEV", "LA", "HardHit_pct", "barrel_pct"),
    batted_ball = c("GB_pct", "FB_pct", "LD_pct", "PU_pct"),
    damage = c("AVG", "SLG", "ISO", "BABIP", "HR_pct", "XBH_pct"),
    full = c("EV", "HardHit_pct", "barrel_pct", "GB_pct", "FB_pct", "AVG", "SLG", "ISO", "XBH_pct")
  )
  
  requested_stats <- vr_resolve_stat_block(
    stat_block = stat_block,
    stats = stats,
    blocks = blocks,
    available_stats = names(spray_summary)
  )
  
  local_labels <- c(
    AVG = "AVG by direction",
    SLG = "SLG by direction",
    ISO = "ISO by direction",
    BABIP = "BABIP by direction",
    HR_pct = "HR / directed AB",
    XBH_pct = "XBH / directed AB"
  )
  if (!is.null(stat_labels)) local_labels[names(stat_labels)] <- stat_labels
  
  league_pool <- spray_summary %>% filter(BBE >= min_league_bbe | name == hitter)
  player_df <- league_pool %>% filter(name == hitter, BBE >= min_player_bbe)
  
  if (nrow(player_df) == 0) stop("No spray rows found for hitter: ", hitter, call. = FALSE)
  
  league_values <- vr_spray_league_values_by_group(
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
      stat_label = vr_label_stats(stat, stat_labels = local_labels, percent_stats = NULL),
      value_label = vapply(
        seq_along(raw_value),
        function(i) vr_spray_format_value(raw_value[i], stat[i], percent_stats, scale_percents),
        character(1)
      ),
      label = value_label,
      spray_direction = factor(as.character(spray_direction), levels = c("Pull", "Middle", "Oppo")),
      comparison = factor(comparison, levels = c(hitter, league_label)),
      stat_label = factor(stat_label, levels = vr_label_stats(requested_stats, stat_labels = local_labels, percent_stats = NULL))
    ) %>%
    filter(!is.na(value))
  
  if (is.null(title)) title <- paste0(hitter, " Spray-Direction Profile")
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Damage stats use directed AB denominators: AVG = H/AB, SLG = TB/AB. ",
      "Contact-quality rates still use BBE denominators."
    )
  }
  
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
    denominator_note = "Spray damage stats use directed AB denominators: AVG = H/AB, SLG = TB/AB, ISO = SLG - AVG, HR_pct = HR/AB, XBH_pct = XBH/AB. BBE rates use BBE denominators.",
    plot = p
  )
}

# -----------------------------------------------------------------------------
# Diagnostic helper: compare old BBE-denominator AVG with fixed AB-denominator AVG
# -----------------------------------------------------------------------------

check_hitter_spray_avg <- function(views,
                                   hitter,
                                   group = "none",
                                   bat_hand = "none",
                                   pit_hand = "none",
                                   start_date = NULL,
                                   end_date = NULL,
                                   team_col = "batting_team",
                                   player_id_col = "matchup.batter.id",
                                   spray_col = "spray_direction") {
  library(dplyr)
  
  summarize_hitter_spray_direction(
    views = views,
    group = group,
    bat_hand = bat_hand,
    pit_hand = pit_hand,
    start_date = start_date,
    end_date = end_date,
    team_col = team_col,
    player_id_col = player_id_col,
    hitter_col = "matchup.batter.fullName",
    spray_col = spray_col
  ) %>%
    filter(name == hitter) %>%
    select(
      name, team, spray_direction, BBE, AB, H, TB, SF,
      AVG_fixed = AVG,
      AVG_old_bbe_denominator,
      SLG_fixed = SLG,
      SLG_old_bbe_denominator,
      BABIP, HR, XBH
    ) %>%
    arrange(factor(as.character(spray_direction), levels = c("Pull", "Middle", "Oppo")))
}

# =============================================================================
# TEAM VISUAL REVOLUTION EXAMPLES
# =============================================================================

plot_team_vs_league_facets <- function(team_df,
                                       league_df,
                                       team_abbr,
                                       stats,
                                       title,
                                       subtitle = NULL,
                                       team_label = team_abbr,
                                       league_label = "League Avg",
                                       percent_stats = vr_percent_stats(),
                                       scale_percents = TRUE,
                                       ncol = NULL,
                                       base_size = 12) {
  available_stats <- intersect(stats, names(team_df))
  available_stats <- intersect(available_stats, names(league_df))
  
  dropped <- setdiff(stats, available_stats)
  if (length(dropped) > 0) {
    warning(
      "Dropped missing stats: ",
      paste(dropped, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(available_stats) == 0) {
    stop("None of the requested stats are available.", call. = FALSE)
  }
  
  team_row <- team_df %>%
    filter(
      name == team_abbr |
        team == team_abbr |
        batting_team == team_abbr |
        fielding_team == team_abbr
    ) %>%
    slice(1)
  
  if (nrow(team_row) == 0) {
    stop("No team row found for: ", team_abbr, call. = FALSE)
  }
  
  league_row <- league_df %>% slice(1)
  
  plot_data <- bind_rows(
    team_row %>% mutate(comparison = team_label),
    league_row %>% mutate(comparison = league_label)
  ) %>%
    select(comparison, all_of(available_stats)) %>%
    pivot_longer(
      cols = all_of(available_stats),
      names_to = "stat",
      values_to = "raw_value"
    ) %>%
    mutate(
      value = if_else(
        scale_percents & stat %in% percent_stats,
        raw_value * 100,
        raw_value
      ),
      stat_label = vr_label_stats(stat),
      value_label = mapply(
        vr_format_value,
        raw_value,
        stat,
        MoreArgs = list(
          percent_stats = percent_stats,
          scale_percents = scale_percents
        )
      )
    )
  
  p <- ggplot(
    plot_data,
    aes(x = comparison, y = value, fill = comparison)
  ) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_text(
      aes(label = value_label),
      vjust = -0.25,
      size = 3.2,
      fontface = "bold"
    ) +
    facet_wrap(~ stat_label, scales = "free_y", ncol = ncol) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold")
    )
  
  if (exists("apply_mlb_team_plot_style", mode = "function")) {
    p <- apply_mlb_team_plot_style(
      p,
      team = team_abbr,
      base_size = base_size,
      apply_fill = TRUE
    )
  }
  
  list(
    plot_data = plot_data,
    kept_stats = available_stats,
    plot = p
  )
}