.simulate_one_starter_matchup <- function(
    event_probs,
    starter_bf,
    simulation_id,
    run_weights = c(BB = 0.33, X1B = 0.47, X2B = 0.78, X3B = 1.09, HR = 1.40)) {
  .validate_event_probabilities(event_probs)
  event_levels <- c("BB", "K", "1B", "2B", "3B", "HR", "OUT")
  probability_columns <- c("p_BB", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  hitter_index <- ((seq_len(starter_bf) - 1L) %% nrow(event_probs)) + 1L
  events <- vapply(
    hitter_index,
    function(index) {
      sample(event_levels, size = 1L, prob = as.numeric(event_probs[index, probability_columns]))
    },
    character(1)
  )

  event_log <- tibble::tibble(
    sim = simulation_id,
    pa_number = seq_len(starter_bf),
    lineup_spot = event_probs$lineup_spot[hitter_index],
    player = event_probs$player[hitter_index],
    team = event_probs$team[hitter_index],
    event = events,
    BB = as.integer(events == "BB"),
    K = as.integer(events == "K"),
    X1B = as.integer(events == "1B"),
    X2B = as.integer(events == "2B"),
    X3B = as.integer(events == "3B"),
    HR = as.integer(events == "HR"),
    OUT = as.integer(events == "OUT"),
    H = as.integer(events %in% c("1B", "2B", "3B", "HR")),
    XBH = as.integer(events %in% c("2B", "3B", "HR")),
    TB = dplyr::case_when(
      events == "1B" ~ 1,
      events == "2B" ~ 2,
      events == "3B" ~ 3,
      events == "HR" ~ 4,
      TRUE ~ 0
    )
  )

  starter_line <- dplyr::summarise(
    event_log,
    sim = dplyr::first(.data$sim),
    BF = dplyr::n(),
    H = sum(.data$H), BB = sum(.data$BB), K = sum(.data$K), HR = sum(.data$HR),
    X1B = sum(.data$X1B), X2B = sum(.data$X2B), X3B = sum(.data$X3B),
    XBH = sum(.data$XBH), TB = sum(.data$TB), OUT = sum(.data$OUT),
    est_runs_simple = run_weights[["BB"]] * sum(.data$BB) +
      run_weights[["X1B"]] * sum(.data$X1B) +
      run_weights[["X2B"]] * sum(.data$X2B) +
      run_weights[["X3B"]] * sum(.data$X3B) +
      run_weights[["HR"]] * sum(.data$HR)
  )
  hitter_line <- event_log |>
    dplyr::group_by(.data$sim, .data$lineup_spot, .data$player, .data$team) |>
    dplyr::summarise(
      PA = dplyr::n(), H = sum(.data$H), BB = sum(.data$BB), K = sum(.data$K),
      HR = sum(.data$HR), X1B = sum(.data$X1B), X2B = sum(.data$X2B),
      X3B = sum(.data$X3B), XBH = sum(.data$XBH), TB = sum(.data$TB),
      .groups = "drop"
    )
  list(starter_line = starter_line, hitter_line = hitter_line, event_log = event_log)
}

#' Summarize simulated starter results
#' @param simulation_results One row per simulation.
#' @return A one-row tibble of expected outcomes and intervals.
#' @export
make_pitcher_summary <- function(simulation_results) {
  stats <- c("H", "BB", "K", "HR", "XBH", "TB", "est_runs_simple")
  missing <- setdiff(stats, names(simulation_results))
  if (length(missing) > 0L) stop("Missing simulation columns: ", paste(missing, collapse = ", "), call. = FALSE)
  values <- list(sims = nrow(simulation_results))
  for (stat in stats) {
    x <- simulation_results[[stat]]
    values[[paste0("avg_", stat)]] <- mean(x)
    values[[paste0("median_", stat)]] <- stats::median(x)
    values[[paste0(stat, "_p10")]] <- unname(stats::quantile(x, 0.10))
    values[[paste0(stat, "_p90")]] <- unname(stats::quantile(x, 0.90))
  }
  tibble::as_tibble(values)
}

#' Create starter threshold probabilities
#' @param simulation_results One row per simulation.
#' @return A tibble of thresholds and probabilities.
#' @export
make_probability_table <- function(simulation_results) {
  thresholds <- tibble::tribble(
    ~outcome, ~threshold,
    "H", 4, "H", 5, "H", 6, "BB", 1, "BB", 2, "BB", 3,
    "K", 4, "K", 5, "K", 6, "K", 7, "HR", 1, "HR", 2,
    "XBH", 2, "XBH", 3, "XBH", 4, "TB", 6, "TB", 8, "TB", 10,
    "est_runs_simple", 2, "est_runs_simple", 3, "est_runs_simple", 4
  )
  thresholds$probability <- vapply(
    seq_len(nrow(thresholds)),
    function(index) mean(simulation_results[[thresholds$outcome[index]]] >= thresholds$threshold[index]),
    numeric(1)
  )
  thresholds$probability_pct <- thresholds$probability * 100
  thresholds$label <- paste0("P(", thresholds$outcome, " >= ", thresholds$threshold, ")")
  thresholds
}

#' Summarize simulated hitter results
#' @param hitter_sim_results One row per hitter per simulation appearance.
#' @return A tibble with expected counts and event probabilities by hitter.
#' @export
make_hitter_summary <- function(hitter_sim_results) {
  hitter_sim_results |>
    dplyr::group_by(.data$lineup_spot, .data$player, .data$team) |>
    dplyr::summarise(
      sims = dplyr::n(), avg_PA = mean(.data$PA), avg_H = mean(.data$H),
      avg_BB = mean(.data$BB), avg_K = mean(.data$K), avg_HR = mean(.data$HR),
      avg_XBH = mean(.data$XBH), avg_TB = mean(.data$TB),
      p_1_plus_H = mean(.data$H >= 1), p_2_plus_H = mean(.data$H >= 2),
      p_1_plus_BB = mean(.data$BB >= 1), p_2_plus_K = mean(.data$K >= 2),
      p_1_plus_HR = mean(.data$HR >= 1), p_1_plus_XBH = mean(.data$XBH >= 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$lineup_spot)
}

.attach_metadata <- function(data, metadata) {
  if (ncol(metadata) == 0L || nrow(data) == 0L) return(data)
  dplyr::bind_cols(metadata[rep(1L, nrow(data)), , drop = FALSE], data)
}

#' Run a starter-matchup Monte Carlo simulation
#'
#' @param lineup_df One row per hitter, in batting order.
#' @param n_sims Number of simulations.
#' @param starter_bf Batters faced in each simulated starter appearance.
#' @param player_col Column containing player names.
#' @param team_col Column containing team identifiers.
#' @param triple_share Share of non-home-run extra-base hits assigned as triples.
#' @param seed Reproducible local seed, or `NULL` to use the caller's RNG state.
#' @param keep_event_log Whether to retain the plate-appearance event log.
#'
#' @return A list of event probabilities, simulation rows, and derived summaries.
#' @export
simulate_starter_matchup <- function(
    lineup_df,
    n_sims = 10000L,
    starter_bf = 22L,
    player_col = "player",
    team_col = "team",
    triple_share = 0.08,
    seed = 123L,
    keep_event_log = FALSE) {
  if (length(n_sims) != 1L || is.na(n_sims) || n_sims < 1 || n_sims != as.integer(n_sims)) {
    stop("`n_sims` must be a positive whole number.", call. = FALSE)
  }
  if (length(starter_bf) != 1L || is.na(starter_bf) || starter_bf < 1 || starter_bf != as.integer(starter_bf)) {
    stop("`starter_bf` must be a positive whole number.", call. = FALSE)
  }
  event_probs <- prepare_lineup_event_probs(
    lineup_df, player_col = player_col, team_col = team_col,
    triple_share = triple_share, use_features = TRUE
  )
  run <- function() {
    simulations <- lapply(
      seq_len(n_sims),
      function(index) .simulate_one_starter_matchup(event_probs, starter_bf, index)
    )
    starter_rows <- dplyr::bind_rows(lapply(simulations, `[[`, "starter_line"))
    hitter_rows <- dplyr::bind_rows(lapply(simulations, `[[`, "hitter_line"))
    event_rows <- if (isTRUE(keep_event_log)) dplyr::bind_rows(lapply(simulations, `[[`, "event_log")) else NULL
    list(starter_rows = starter_rows, hitter_rows = hitter_rows, event_rows = event_rows)
  }
  results <- if (is.null(seed)) run() else withr::with_seed(seed, run())

  metadata_columns <- intersect(
    c("game_pk", "batting_side", "opposing_pitcher", "opposing_pitcher_hand", "opposing_pitcher_side"),
    names(lineup_df)
  )
  metadata <- tibble::as_tibble(lineup_df[1L, metadata_columns, drop = FALSE])
  pitcher_metadata <- metadata
  rename_map <- c(
    opposing_pitcher = "pitcher", opposing_pitcher_hand = "pitcher_hand",
    opposing_pitcher_side = "pitcher_side", batting_side = "opposing_lineup"
  )
  for (old_name in intersect(names(rename_map), names(pitcher_metadata))) {
    names(pitcher_metadata)[names(pitcher_metadata) == old_name] <- rename_map[[old_name]]
  }

  output <- list(
    event_probabilities = event_probs,
    simulation_results = .attach_metadata(results$starter_rows, metadata),
    pitcher_summary = .attach_metadata(make_pitcher_summary(results$starter_rows), pitcher_metadata),
    probability_table = .attach_metadata(make_probability_table(results$starter_rows), metadata),
    hitter_sim_results = results$hitter_rows,
    hitter_summary = .attach_metadata(make_hitter_summary(results$hitter_rows), metadata)
  )
  if (isTRUE(keep_event_log)) output$event_log <- .attach_metadata(results$event_rows, metadata)
  output
}
