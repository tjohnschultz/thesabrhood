load_contact_matrix <- function(path = "contact_matrix.csv") {
  library(dplyr)
  
  read.csv(path) %>%
    select(contact_quality, bbe_type, spray_direction, batting_avg, SLG_pct) %>%
    rename(
      xBA_bucket = batting_avg,
      xSLG_bucket = SLG_pct
    )
}

# 4.1.1 build_contact_pa_table()
#   Input: enhanced pitch-by-pitch data
#   Output: one row per batted-ball PA
#   Use: creates the base table for the contact matrix
build_contact_pa_table <- function(enhanced_pbp,
                                   perspective = "batter") {
  library(dplyr)
  perspective <- match.arg(perspective, c("batter", "pitcher"))
  
  id_col <- if (perspective == "batter") "matchup.batter.id" else "matchup.pitcher.id"
  player_col <- if (perspective == "batter") "matchup.batter.fullName" else "matchup.pitcher.fullName"
  team_col <- if (perspective == "batter") "batting_team" else "fielding_team"
  
  enhanced_pbp %>%
    group_by(game_pk, atBatIndex) %>%
    summarise(
      player_id = first(na.omit(.data[[id_col]])),
      name = first(na.omit(.data[[player_col]])),
      team = first(na.omit(.data[[team_col]])),
      has_bbe = any(is_bbe == 1, na.rm = TRUE),
      contact_quality = first(contact_quality[is_bbe == 1 & !is.na(contact_quality)]),
      bbe_type = first(bbe_type[is_bbe == 1 & !is.na(bbe_type)]),
      spray_direction = first(spray_direction[is_bbe == 1 & !is.na(spray_direction)]),
      bbe = as.integer(any(is_bbe == 1, na.rm = TRUE)),
      hits = sum(is_hit, na.rm = TRUE),
      HR = sum(is_hr, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(has_bbe, !is.na(contact_quality), !is.na(bbe_type), !is.na(spray_direction)) %>%
    select(-has_bbe)
}

# 4.1.2 build_contact_matrix()
#   Input: enhanced pitch-by-pitch data
#   Output: grouped contact bucket counts by player, team, or league
#   Use: summarizes actual production inside each contact bucket
build_contact_matrix <- function(enhanced_pbp,
                                 group_type = "player",
                                 perspective = "batter") {
  library(dplyr)
  
  group_type <- match.arg(group_type, c("player", "team", "league"))
  
  contact_pa_tbl <- build_contact_pa_table(
    enhanced_pbp = enhanced_pbp,
    perspective = perspective
  )
  
  if (group_type == "player") {
    
    out <- contact_pa_tbl %>%
      group_by(
        player_id,
        name,
        team,
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarise(
        bbe = sum(bbe, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if (group_type == "team") {
    
    out <- contact_pa_tbl %>%
      group_by(
        team,
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarise(
        bbe = sum(bbe, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    
    out <- contact_pa_tbl %>%
      group_by(
        contact_quality,
        bbe_type,
        spray_direction
      ) %>%
      summarise(
        bbe = sum(bbe, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  out
}

# 4.1.3 load_contact_matrix()
#   Input: saved contact_matrix.csv
#   Output: cleaned matrix with xBA/xSLG bucket values
#   Use: loads the expected weights for each contact bucket
make_contact_matrix_csv <- function(raw_pbp,
                                    output_path = "contact_matrix.csv",
                                    perspective = "batter",
                                    season_type = "Regular Season",
                                    already_enhanced = FALSE) {
  library(dplyr)
  library(readr)
  
  pbp_use <- if (already_enhanced) raw_pbp else enhance_pbp(raw_pbp)
  
  if ("seriesDescription" %in% names(pbp_use) && !is.null(season_type)) {
    pbp_use <- pbp_use %>% filter(seriesDescription == season_type)
  }
  
  matrix <- build_contact_matrix(
    enhanced_pbp = pbp_use,
    group_type = "league",
    perspective = perspective,
    add_rates = TRUE
  ) %>%
    arrange(contact_quality, bbe_type, spray_direction)
  
  readr::write_csv(matrix, output_path)
  message("Saved league-wide contact matrix to: ", output_path)
  matrix
}

#Function 4.1.4
build_expected_contact_pa_rates <- function(enhanced_pbp,
                                            perspective = "batter",
                                            group_type = "player",
                                            so_col = "is_so",
                                            bb_col = "is_bb") {
  library(dplyr)
  
  perspective <- match.arg(perspective, c("batter", "pitcher"))
  group_type <- match.arg(group_type, c("player", "team", "league"))
  
  id_col <- if (perspective == "batter") {
    "matchup.batter.id"
  } else {
    "matchup.pitcher.id"
  }
  
  player_col <- if (perspective == "batter") {
    "matchup.batter.fullName"
  } else {
    "matchup.pitcher.fullName"
  }
  
  team_col <- if (perspective == "batter") {
    "batting_team"
  } else {
    "fielding_team"
  }
  
  if (!so_col %in% names(enhanced_pbp)) {
    stop(glue::glue("Column '{so_col}' was not found in enhanced_pbp."))
  }
  
  if (!bb_col %in% names(enhanced_pbp)) {
    stop(glue::glue("Column '{bb_col}' was not found in enhanced_pbp."))
  }
  
  pa_tbl <- enhanced_pbp %>%
    group_by(game_pk, atBatIndex) %>%
    summarise(
      player_id = first(na.omit(.data[[id_col]])),
      name = first(na.omit(.data[[player_col]])),
      team = first(na.omit(.data[[team_col]])),
      
      PA = 1L,
      SO = as.integer(any(.data[[so_col]] == 1, na.rm = TRUE)),
      BB = as.integer(any(.data[[bb_col]] == 1, na.rm = TRUE)),
      BBE_PA = as.integer(any(is_bbe == 1, na.rm = TRUE)),
      
      .groups = "drop"
    )
  
  if (group_type == "player") {
    
    out <- pa_tbl %>%
      group_by(player_id, name, team) %>%
      summarise(
        PA = sum(PA, na.rm = TRUE),
        SO = sum(SO, na.rm = TRUE),
        BB = sum(BB, na.rm = TRUE),
        BBE_PA = sum(BBE_PA, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if (group_type == "team") {
    
    out <- pa_tbl %>%
      group_by(team) %>%
      summarise(
        PA = sum(PA, na.rm = TRUE),
        SO = sum(SO, na.rm = TRUE),
        BB = sum(BB, na.rm = TRUE),
        BBE_PA = sum(BBE_PA, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    
    out <- pa_tbl %>%
      summarise(
        PA = sum(PA, na.rm = TRUE),
        SO = sum(SO, na.rm = TRUE),
        BB = sum(BB, na.rm = TRUE),
        BBE_PA = sum(BBE_PA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(name = "League")
  }
  
  out %>%
    mutate(
      SO_pct = round(SO / PA, 3),
      K_pct = SO_pct,
      BB_pct = round(BB / PA, 3),
      BBE_pct = round(BBE_PA / PA, 3)
    )
}

# 4.2.1 expected_contact_detail()
#   Input: enhanced PBP + contact matrix
#   Output: bucket-level table with expected hit and TB contributions
#   Use: joins observed batted-ball buckets to expected values
expected_contact_detail <- function(pbp,
                                    contact_matrix,
                                    group_type = "player",
                                    perspective = "batter") {
  library(dplyr)
  
  build_contact_matrix(
    enhanced_pbp = pbp,
    group_type = group_type,
    perspective = perspective
  ) %>%
    left_join(
      contact_matrix,
      by = c("contact_quality", "bbe_type", "spray_direction")
    ) %>%
    mutate(
      xH_contrib = bbe * xBA_bucket,
      xTB_contrib = bbe * xSLG_bucket
    )
}

# 4.2.2 calc_expected_contact_stats()
#   Input: enhanced PBP + contact matrix
#   Output: player/team/league xBA and xSLG summary
#   Use: final expected contact leaderboard/table
calc_expected_contact_stats <- function(raw_pbp,
                                        contact_matrix_path = "contact_matrix.csv",
                                        group_type = "player",
                                        perspective = "batter",
                                        already_enhanced = FALSE,
                                        so_col = "is_so",
                                        bb_col = "is_bb",
                                        bb_weight = 0.69,
                                        xh_weight = 0.88,
                                        xtb_bonus_weight = 0.32,
                                        so_penalty = 0, 
                                        min_bbe = NA) {
  library(dplyr)
  
  group_type <- match.arg(group_type, c("player", "team", "league"))
  perspective <- match.arg(perspective, c("batter", "pitcher"))
  
  safe_rate <- function(num, den) {
    ifelse(is.na(den) | den == 0, NA_real_, num / den)
  }
  
  pbp_use <- if (already_enhanced) {
    raw_pbp
  } else {
    enhance_pbp(raw_pbp)
  }
  
  contact_matrix <- load_contact_matrix(contact_matrix_path)
  
  detail_tbl <- build_contact_matrix(
    enhanced_pbp = pbp_use,
    group_type = group_type,
    perspective = perspective
  ) %>%
    left_join(
      contact_matrix,
      by = c("contact_quality", "bbe_type", "spray_direction")
    ) %>%
    mutate(
      xH_contrib = bbe * xBA_bucket,
      xTB_contrib = bbe * xSLG_bucket
    )
  
  pa_rates <- build_expected_contact_pa_rates(
    enhanced_pbp = pbp_use,
    perspective = perspective,
    group_type = group_type,
    so_col = so_col,
    bb_col = bb_col
  )
  
  if (group_type == "player") {
    
    out <- detail_tbl %>%
      group_by(player_id, name, team) %>%
      summarise(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(safe_rate(H, BBE), 3),
        SLG_on_contact = round(safe_rate(TB, BBE), 3),
        xBA = round(safe_rate(xH, BBE), 3),
        xSLG = round(safe_rate(xTB, BBE), 3),
        BA_v_exp = round(BA_on_contact - xBA, 3),
        SLG_v_exp = round(SLG_on_contact - xSLG, 3),
        .groups = "drop"
      ) %>%
      left_join(pa_rates, by = c("player_id", "name", "team"))
    
  } else if (group_type == "team") {
    
    out <- detail_tbl %>%
      group_by(team) %>%
      summarise(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(safe_rate(H, BBE), 3),
        SLG_on_contact = round(safe_rate(TB, BBE), 3),
        xBA = round(safe_rate(xH, BBE), 3),
        xSLG = round(safe_rate(xTB, BBE), 3),
        BA_v_exp = round(BA_on_contact - xBA, 3),
        SLG_v_exp = round(SLG_on_contact - xSLG, 3),
        .groups = "drop"
      ) %>%
      left_join(pa_rates, by = "team")
    
  } else if (group_type == "league") {
    
    out <- detail_tbl %>%
      summarise(
        BBE = sum(bbe, na.rm = TRUE),
        H = sum(hits, na.rm = TRUE),
        HR = sum(HR, na.rm = TRUE),
        TB = sum(TB, na.rm = TRUE),
        xH = sum(xH_contrib, na.rm = TRUE),
        xTB = sum(xTB_contrib, na.rm = TRUE),
        BA_on_contact = round(safe_rate(H, BBE), 3),
        SLG_on_contact = round(safe_rate(TB, BBE), 3),
        xBA = round(safe_rate(xH, BBE), 3),
        xSLG = round(safe_rate(xTB, BBE), 3),
        BA_v_exp = round(BA_on_contact - xBA, 3),
        SLG_v_exp = round(SLG_on_contact - xSLG, 3)
      ) %>%
      bind_cols(
        pa_rates %>%
          select(PA, SO, BB, BBE_PA, SO_pct, K_pct, BB_pct, BBE_pct)
      ) %>%
      mutate(name = "League")
  }
  
  out <- out %>%
    mutate(
      xBA_per_PA = round(safe_rate(xH, PA), 3),
      xSLG_per_PA = round(safe_rate(xTB, PA), 3),
      xExtraBases = pmax(xTB - xH, 0),
      
      my_xwOBA = round(
        safe_rate(
          bb_weight * BB +
            xh_weight * xH +
            xtb_bonus_weight * xExtraBases -
            so_penalty * SO,
          PA
        ),
        3
      ), 
      zxSLG = round(as.vector(scale(xSLG)), 3) * 100
    ) %>%
    arrange(desc(my_xwOBA))
  
  if (min_bbe != is.na(min_bbe)) {
    out <- out %>% filter(BBE >= min_bbe)
  } else{
    
  }
  return(out)
}

# 4.3.1 run_expected_contact_workflow()
#   Input: raw or enhanced PBP
#   Output: pa_table, detail, or summary
#   Use: main wrapper for the whole expected contact workflow
run_expected_contact_workflow <- function(raw_pbp,
                                          contact_matrix_path = "contact_matrix.csv",
                                          group_type = "player",
                                          perspective = "batter",
                                          return_type = "summary",
                                          already_enhanced = FALSE) {
  library(dplyr)
  
  pbp_use <- if (already_enhanced) raw_pbp else enhance_pbp(raw_pbp)
  
  contact_matrix <- load_contact_matrix(contact_matrix_path)
  
  if (return_type == "pa_table") {
    out <- build_contact_pa_table(
      enhanced_pbp = pbp_use,
      perspective = perspective
    )
    
  } else if (return_type == "detail") {
    out <- expected_contact_detail(
      pbp = pbp_use,
      contact_matrix = contact_matrix,
      group_type = group_type,
      perspective = perspective
    )
    
  } else if (return_type == "summary") {
    out <- calc_expected_contact_stats(
      pbp = pbp_use,
      contact_matrix = contact_matrix,
      group_type = group_type,
      perspective = perspective
    )
    
  } else {
    stop("return_type must be 'summary', 'detail', or 'pa_table'")
  }
  
  out
}