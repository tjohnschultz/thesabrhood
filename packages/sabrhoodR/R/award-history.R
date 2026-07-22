#' Build award race history from dated FanGraphs checkpoints
#'
#' Recalculates an award board at every checkpoint using only the cumulative
#' statistics available through that date. MVP ratings add a playing-time
#' reliability adjustment so early-season percentile spikes are moderated.
#'
#' @param checkpoints A list of lists containing `date`, `hitters`, and
#'   `pitchers`. Hitter and pitcher tables must already be standardized with
#'   [standardize_fangraphs_season()].
#' @param prior_hitters Optional prior-season compact hitter table used for
#'   rookie screening.
#' @param prior_pitchers Optional prior-season compact pitcher table used for
#'   rookie screening.
#' @param award Award name returned by [build_award_race_boards()].
#' @param top_n Number of current leaders traced across the season.
#' @param opening_date Opening date used to scale qualification thresholds.
#' @return A list containing full `history`, current-top-player `display`,
#'   detected `events`, and current `leaders`.
#' @export
build_award_race_history <- function(checkpoints, prior_hitters = NULL, prior_pitchers = NULL,
                                     award = "MVP", top_n = 8L, opening_date = NULL) {
  if (!is.list(checkpoints) || !length(checkpoints)) stop("`checkpoints` must be a non-empty list.", call. = FALSE)
  dates <- as.Date(vapply(checkpoints, function(item) as.character(item$date), character(1)))
  if (anyNA(dates)) stop("Every checkpoint must contain a valid `date`.", call. = FALSE)
  order_index <- order(dates)
  checkpoints <- checkpoints[order_index]
  dates <- dates[order_index]
  if (is.null(opening_date)) opening_date <- min(dates)
  opening_date <- as.Date(opening_date)

  history_rows <- lapply(seq_along(checkpoints), function(index) {
    item <- checkpoints[[index]]
    if (!is.data.frame(item$hitters) || !is.data.frame(item$pitchers)) {
      stop("Every checkpoint must contain standardized hitter and pitcher tables.", call. = FALSE)
    }
    day_index <- max(1L, as.integer(dates[[index]] - opening_date) + 1L)
    minimum_pa <- max(25L, min(200L, round(2.25 * day_index)))
    minimum_outs <- max(24L, min(150L, round(1.85 * day_index)))
    board <- build_award_race_boards(
      item$hitters, item$pitchers, prior_hitters, prior_pitchers,
      minimum_pa = minimum_pa, minimum_outs = minimum_outs
    )
    board <- board[board$award == award, , drop = FALSE]
    if (!nrow(board)) return(NULL)
    board$checkpoint_date <- dates[[index]]
    board$season_day <- day_index
    board$minimum_pa <- minimum_pa
    board$minimum_outs <- minimum_outs
    reliability_target <- ifelse(board$volume_label == "PA", pmax(40, 3.1 * day_index), pmax(24, 1.85 * day_index / 3))
    board$reliability <- pmin(1, suppressWarnings(as.numeric(board$volume_value)) / reliability_target)
    board$reliability[!is.finite(board$reliability)] <- 0
    board$race_rating <- 50 + (board$award_score - 50) * (0.35 + 0.65 * board$reliability)
    board$race_rating <- round(pmin(pmax(board$race_rating, 0), 100), 1)
    board <- do.call(rbind, lapply(split(board, board$league), function(rows) {
      rows <- rows[order(-rows$race_rating, -rows$war), , drop = FALSE]
      rows$race_rank <- seq_len(nrow(rows))
      rows
    }))
    rownames(board) <- NULL
    board
  })
  history_rows <- history_rows[!vapply(history_rows, is.null, logical(1))]
  if (!length(history_rows)) stop("No award rows were produced from the checkpoints.", call. = FALSE)
  history <- do.call(rbind, history_rows)
  rownames(history) <- NULL
  history <- history[order(history$league, history$checkpoint_date, history$race_rank), , drop = FALSE]

  history$rating_change <- NA_real_
  history$rank_change <- NA_real_
  player_groups <- split(seq_len(nrow(history)), paste(history$league, history$player_id, sep = "|"))
  for (indices in player_groups) {
    indices <- indices[order(history$checkpoint_date[indices])]
    history$rating_change[indices] <- c(NA_real_, diff(history$race_rating[indices]))
    history$rank_change[indices] <- c(NA_real_, -diff(history$race_rank[indices]))
  }

  latest_date <- max(history$checkpoint_date)
  leaders <- history[history$checkpoint_date == latest_date & history$race_rank <= top_n, , drop = FALSE]
  display_keys <- paste(leaders$league, leaders$player_id, sep = "|")
  display <- history[paste(history$league, history$player_id, sep = "|") %in% display_keys, , drop = FALSE]
  latest_leaders <- leaders[leaders$race_rank == 1L, c("league", "player_id"), drop = FALSE]
  display$is_current_leader <- paste(display$league, display$player_id, sep = "|") %in%
    paste(latest_leaders$league, latest_leaders$player_id, sep = "|")

  event_rows <- list()
  event_index <- 0L
  for (league_name in unique(history$league)) {
    league_rows <- history[history$league == league_name, , drop = FALSE]
    checkpoints_in_league <- sort(unique(league_rows$checkpoint_date))
    prior_leader <- NA_character_
    for (checkpoint in checkpoints_in_league) {
      checkpoint_date <- as.Date(checkpoint, origin = "1970-01-01")
      rows <- league_rows[league_rows$checkpoint_date == checkpoint_date, , drop = FALSE]
      leader <- rows[rows$race_rank == 1L, , drop = FALSE][1L, ]
      if (!is.na(prior_leader) && leader$player_id[[1L]] != prior_leader) {
        event_index <- event_index + 1L
        event_rows[[event_index]] <- data.frame(
          award = award, league = league_name, checkpoint_date = checkpoint_date,
          event_type = "new_leader", player_id = leader$player_id[[1L]], player_name = leader$player_name[[1L]],
          team = leader$team[[1L]], race_rating = leader$race_rating[[1L]],
          detail = paste0(leader$player_name[[1L]], " moved into first place."), stringsAsFactors = FALSE
        )
      }
      prior_leader <- leader$player_id[[1L]]
      risers <- rows[is.finite(rows$rating_change), , drop = FALSE]
      if (nrow(risers)) {
        riser <- risers[order(-risers$rating_change), , drop = FALSE][1L, ]
        event_index <- event_index + 1L
        event_rows[[event_index]] <- data.frame(
          award = award, league = league_name, checkpoint_date = checkpoint_date,
          event_type = "fastest_riser", player_id = riser$player_id[[1L]], player_name = riser$player_name[[1L]],
          team = riser$team[[1L]], race_rating = riser$race_rating[[1L]],
          detail = paste0(riser$player_name[[1L]], " gained ", format(round(riser$rating_change[[1L]], 1), nsmall = 1), " rating points."),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  events <- if (length(event_rows)) do.call(rbind, event_rows) else data.frame(
    award = character(), league = character(), checkpoint_date = as.Date(character()), event_type = character(),
    player_id = character(), player_name = character(), team = character(), race_rating = numeric(), detail = character()
  )
  list(history = history, display = display, events = events, leaders = leaders)
}
