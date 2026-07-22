.primary_position <- function(position) {
  position <- toupper(trimws(as.character(position)))
  output <- sub("/.*$", "", position)
  output[output %in% c("LF", "CF", "RF")] <- "OF"
  output
}

#' Build team positional WAR and need boards
#'
#' Aggregates compact FanGraphs player leaderboards into a common set of team
#' position groups. Multi-position hitters are assigned to the first listed
#' FanGraphs position. Pitchers are assigned to starter or relief groups using
#' their season start share.
#'
#' @param hitters Compact hitter table from [standardize_fangraphs_season()].
#' @param pitchers Compact pitcher table from [standardize_fangraphs_season()].
#' @param minimum_pa Minimum hitter plate appearances retained.
#' @param minimum_outs Minimum pitcher outs retained.
#' @return One row per team and position group with MLB rank and need label.
#' @export
build_team_positional_war <- function(hitters, pitchers, minimum_pa = 1L, minimum_outs = 3L) {
  stopifnot(is.data.frame(hitters), is.data.frame(pitchers))
  hitter_required <- c("team", "league", "player_id", "player_name", "position", "pa", "war")
  pitcher_required <- c("team", "league", "player_id", "player_name", "games", "starts", "innings_outs", "war")
  if (length(setdiff(hitter_required, names(hitters)))) stop("`hitters` is missing positional WAR fields.", call. = FALSE)
  if (length(setdiff(pitcher_required, names(pitchers)))) stop("`pitchers` is missing positional WAR fields.", call. = FALSE)

  bat <- hitters |>
    dplyr::mutate(position_group = .primary_position(.data$position)) |>
    dplyr::filter(
      .data$pa >= minimum_pa, is.finite(.data$war),
      .data$position_group %in% c("C", "1B", "2B", "3B", "SS", "OF", "DH")
    ) |>
    dplyr::transmute(
      .data$team, .data$league, .data$player_id, .data$player_name,
      position = .data$position_group, war = as.numeric(.data$war)
    )
  pit <- pitchers |>
    dplyr::filter(.data$innings_outs >= minimum_outs, is.finite(.data$war)) |>
    dplyr::mutate(position = ifelse(.data$starts >= pmax(.data$games, 1) * 0.5, "SP", "RP")) |>
    dplyr::transmute(
      .data$team, .data$league, .data$player_id, .data$player_name,
      .data$position, war = as.numeric(.data$war)
    )
  players <- dplyr::bind_rows(bat, pit)
  if (!nrow(players)) return(tibble::tibble())

  labels <- c(C = "Catcher", `1B` = "First base", `2B` = "Second base", `3B` = "Third base",
    SS = "Shortstop", OF = "Outfield", DH = "Designated hitter", SP = "Starting pitching", RP = "Relief pitching")
  board <- players |>
    dplyr::group_by(.data$team, .data$league, .data$position) |>
    dplyr::arrange(dplyr::desc(.data$war), .by_group = TRUE) |>
    dplyr::summarise(
      player_count = dplyr::n_distinct(.data$player_id),
      top_player = dplyr::first(.data$player_name),
      top_player_war = dplyr::first(.data$war),
      war = sum(.data$war, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$position) |>
    dplyr::arrange(dplyr::desc(.data$war), .by_group = TRUE) |>
    dplyr::mutate(
      mlb_rank = dplyr::row_number(),
      teams_ranked = dplyr::n(),
      percentile = round(100 * (1 - (.data$mlb_rank - 1) / pmax(.data$teams_ranked - 1, 1)), 1),
      status = dplyr::case_when(
        .data$mlb_rank <= 10 ~ "strength",
        .data$mlb_rank > .data$teams_ranked - 10 ~ "need",
        TRUE ~ "middle"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      position_label = unname(labels[.data$position]),
      evidence = paste0(round(.data$war, 1), " WAR | MLB rank ", .data$mlb_rank, " of ", .data$teams_ranked,
        " | led by ", .data$top_player),
      positional_war_method = "primary_position_fangraphs_war_v1"
    ) |>
    dplyr::arrange(.data$team, .data$mlb_rank)
  board
}
