#' Simulate a baseball game plate appearance by plate appearance
#'
#' Uses mutually exclusive plate-appearance event probabilities for each
#' lineup spot against the opposing starter and bullpen. The simulation keeps
#' inning, outs, occupied bases, score, batting-order continuity, starter
#' workload, bullpen transitions, walk-offs, and the extra-inning automatic
#' runner.
#'
#' @param away_starter_probs Away hitters versus the home starter.
#' @param home_starter_probs Home hitters versus the away starter.
#' @param away_bullpen_probs Away hitters versus the home bullpen.
#' @param home_bullpen_probs Home hitters versus the away bullpen.
#' @param n_sims Number of simulated games.
#' @param away_starter_bf Expected batters faced by the away starter.
#' @param home_starter_bf Expected batters faced by the home starter.
#' @param seed Random seed.
#' @param max_innings Maximum inning before an unresolved game is recorded as
#'   a tie.
#' @param automatic_runner Whether to begin each half-inning after the ninth
#'   with a runner on second base.
#'
#' @return A list containing game-level results and summaries plus batter
#'   counting-stat and threshold summaries.
#' @export
simulate_game_state <- function(
    away_starter_probs,
    home_starter_probs,
    away_bullpen_probs,
    home_bullpen_probs,
    n_sims = 1000L,
    away_starter_bf = 21,
    home_starter_bf = 21,
    seed = 1L,
    max_innings = 15L,
    automatic_runner = TRUE) {
  event_cols <- c("p_BB", "p_HBP", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  event_names <- sub("^p_", "", event_cols)

  validate_lineup <- function(x, label) {
    x <- as.data.frame(x)
    missing_cols <- setdiff(event_cols, names(x))
    if (length(missing_cols) > 0L) {
      stop(label, " is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    if (nrow(x) != 9L) {
      stop(label, " must contain exactly nine ordered hitters.", call. = FALSE)
    }
    if (!"batting_order" %in% names(x)) x$batting_order <- seq_len(nrow(x))
    x <- x[order(x$batting_order), , drop = FALSE]
    p <- as.matrix(x[, event_cols, drop = FALSE])
    storage.mode(p) <- "double"
    if (any(!is.finite(p)) || any(p < 0)) {
      stop(label, " contains invalid probabilities.", call. = FALSE)
    }
    row_sums <- rowSums(p)
    if (any(row_sums <= 0)) {
      stop(label, " contains a row with no probability mass.", call. = FALSE)
    }
    x[, event_cols] <- p / row_sums
    x
  }

  away_starter_probs <- validate_lineup(away_starter_probs, "away_starter_probs")
  home_starter_probs <- validate_lineup(home_starter_probs, "home_starter_probs")
  away_bullpen_probs <- validate_lineup(away_bullpen_probs, "away_bullpen_probs")
  home_bullpen_probs <- validate_lineup(home_bullpen_probs, "home_bullpen_probs")

  n_sims <- as.integer(n_sims)
  max_innings <- as.integer(max_innings)
  if (!is.finite(n_sims) || n_sims < 1L) {
    stop("n_sims must be a positive integer.", call. = FALSE)
  }
  if (!is.finite(max_innings) || max_innings < 9L) {
    stop("max_innings must be at least nine.", call. = FALSE)
  }

  clamp <- function(x, lo, hi) min(max(as.numeric(x), lo), hi)
  away_starter_bf <- clamp(away_starter_bf, 12, 30)
  home_starter_bf <- clamp(home_starter_bf, 12, 30)

  player_label <- function(x) {
    candidates <- c("batter_name", "player_name", "name")
    col <- candidates[candidates %in% names(x)][1]
    if (is.na(col)) paste("Lineup spot", seq_len(nrow(x))) else as.character(x[[col]])
  }
  player_id <- function(x) {
    candidates <- c("batter_id", "player_id", "mlbam_id")
    col <- candidates[candidates %in% names(x)][1]
    if (is.na(col)) seq_len(nrow(x)) else x[[col]]
  }

  away_names <- player_label(away_starter_probs)
  home_names <- player_label(home_starter_probs)
  away_ids <- player_id(away_starter_probs)
  home_ids <- player_id(home_starter_probs)

  stat_names <- c("PA", "H", "BB", "HBP", "K", "HR", "XBH", "TB")
  away_player <- array(0, dim = c(n_sims, 9L, length(stat_names)),
                       dimnames = list(NULL, NULL, stat_names))
  home_player <- array(0, dim = c(n_sims, 9L, length(stat_names)),
                       dimnames = list(NULL, NULL, stat_names))

  game_results <- data.frame(
    simulation_id = seq_len(n_sims),
    away_runs = integer(n_sims),
    home_runs = integer(n_sims),
    innings = integer(n_sims),
    winner = character(n_sims),
    went_extras = logical(n_sims),
    away_starter_bf = integer(n_sims),
    home_starter_bf = integer(n_sims),
    away_starter_pitches = integer(n_sims),
    home_starter_pitches = integer(n_sims),
    away_bullpen_pa = integer(n_sims),
    home_bullpen_pa = integer(n_sims),
    stringsAsFactors = FALSE
  )

  advance_runners <- function(event, bases) {
    runs <- 0L
    if (event %in% c("BB", "HBP")) {
      if (bases[1] && bases[2] && bases[3]) runs <- runs + 1L
      new_third <- bases[3] || (bases[1] && bases[2])
      new_second <- bases[2] || bases[1]
      bases <- c(TRUE, new_second, new_third)
    } else if (event == "1B") {
      if (bases[3]) runs <- runs + 1L
      runner_two_scores <- bases[2] && stats::runif(1) < 0.60
      if (runner_two_scores) runs <- runs + 1L
      runner_one_to_third <- bases[1] && stats::runif(1) < 0.30
      bases <- c(TRUE, bases[1] && !runner_one_to_third,
                 (bases[2] && !runner_two_scores) || runner_one_to_third)
    } else if (event == "2B") {
      runs <- runs + as.integer(bases[2]) + as.integer(bases[3])
      runner_one_scores <- bases[1] && stats::runif(1) < 0.55
      if (runner_one_scores) runs <- runs + 1L
      bases <- c(FALSE, TRUE, bases[1] && !runner_one_scores)
    } else if (event == "3B") {
      runs <- runs + sum(bases)
      bases <- c(FALSE, FALSE, TRUE)
    } else if (event == "HR") {
      runs <- runs + sum(bases) + 1L
      bases <- c(FALSE, FALSE, FALSE)
    }
    list(bases = bases, runs = runs)
  }

  pitch_count_for_event <- function(event) {
    means <- c(BB = 5.3, HBP = 3.4, K = 4.8, `1B` = 3.5, `2B` = 3.5,
               `3B` = 3.5, HR = 3.6, OUT = 3.4)
    max(1L, as.integer(stats::rpois(1, lambda = means[[event]] - 1) + 1L))
  }

  event_increment <- function(event) {
    result <- stats::setNames(rep(0, length(stat_names)), stat_names)
    result[["PA"]] <- 1
    if (event %in% c("1B", "2B", "3B", "HR")) result[["H"]] <- 1
    if (event == "BB") result[["BB"]] <- 1
    if (event == "HBP") result[["HBP"]] <- 1
    if (event == "K") result[["K"]] <- 1
    if (event == "HR") result[["HR"]] <- 1
    if (event %in% c("2B", "3B", "HR")) result[["XBH"]] <- 1
    total_bases <- c(`1B` = 1, `2B` = 2, `3B` = 3, HR = 4)
    if (event %in% names(total_bases)) result[["TB"]] <- total_bases[[event]]
    result
  }

  set.seed(as.integer(seed))
  away_starter_matrix <- as.matrix(away_starter_probs[, event_cols, drop = FALSE])
  home_starter_matrix <- as.matrix(home_starter_probs[, event_cols, drop = FALSE])
  away_bullpen_matrix <- as.matrix(away_bullpen_probs[, event_cols, drop = FALSE])
  home_bullpen_matrix <- as.matrix(home_bullpen_probs[, event_cols, drop = FALSE])

  for (sim in seq_len(n_sims)) {
    score <- c(away = 0L, home = 0L)
    lineup_index <- c(away = 1L, home = 1L)
    starter_target <- c(
      away = as.integer(round(stats::rnorm(1, away_starter_bf, 2.6))),
      home = as.integer(round(stats::rnorm(1, home_starter_bf, 2.6)))
    )
    starter_target <- pmin(pmax(starter_target, 12L), 30L)
    pitch_limit <- c(
      away = as.integer(round(stats::rnorm(1, 92, 8))),
      home = as.integer(round(stats::rnorm(1, 92, 8)))
    )
    pitch_limit <- pmin(pmax(pitch_limit, 70L), 112L)
    starter_seen <- c(away = 0L, home = 0L)
    starter_pitches <- c(away = 0L, home = 0L)
    bullpen_pa <- c(away = 0L, home = 0L)
    game_over <- FALSE
    final_inning <- 9L

    for (inning in seq_len(max_innings)) {
      final_inning <- inning
      for (half in c("top", "bottom")) {
        if (inning >= 9L && half == "bottom" && score[["home"]] > score[["away"]]) {
          game_over <- TRUE
          break
        }

        offense <- if (half == "top") "away" else "home"
        defense <- if (half == "top") "home" else "away"
        outs <- 0L
        bases <- c(FALSE, FALSE, FALSE)
        if (automatic_runner && inning >= 10L) bases[2] <- TRUE
        half_pa <- 0L

        while (outs < 3L && half_pa < 40L) {
          half_pa <- half_pa + 1L
          spot <- lineup_index[[offense]]
          starter_active <- starter_seen[[defense]] < starter_target[[defense]] &&
            starter_pitches[[defense]] < pitch_limit[[defense]]

          if (offense == "away") {
            p <- if (starter_active) away_starter_matrix[spot, ] else away_bullpen_matrix[spot, ]
          } else {
            p <- if (starter_active) home_starter_matrix[spot, ] else home_bullpen_matrix[spot, ]
          }
          event <- sample(event_names, size = 1L, prob = p)

          if (starter_active) {
            starter_seen[[defense]] <- starter_seen[[defense]] + 1L
            starter_pitches[[defense]] <- starter_pitches[[defense]] + pitch_count_for_event(event)
          } else {
            bullpen_pa[[defense]] <- bullpen_pa[[defense]] + 1L
          }

          increment <- event_increment(event)
          if (offense == "away") {
            away_player[sim, spot, ] <- away_player[sim, spot, ] + increment
          } else {
            home_player[sim, spot, ] <- home_player[sim, spot, ] + increment
          }

          if (event %in% c("K", "OUT")) {
            outs <- outs + 1L
          } else {
            advancement <- advance_runners(event, bases)
            bases <- advancement$bases
            score[[offense]] <- score[[offense]] + advancement$runs
          }
          lineup_index[[offense]] <- if (spot == 9L) 1L else spot + 1L

          if (inning >= 9L && half == "bottom" && score[["home"]] > score[["away"]]) {
            game_over <- TRUE
            break
          }
        }
        if (game_over) break
      }

      if (game_over) break
      if (inning >= 9L && score[["away"]] != score[["home"]]) break
    }

    winner <- if (score[["away"]] > score[["home"]]) {
      "away"
    } else if (score[["home"]] > score[["away"]]) {
      "home"
    } else {
      "tie"
    }
    game_results[sim, c("away_runs", "home_runs", "innings", "winner", "went_extras",
                        "away_starter_bf", "home_starter_bf",
                        "away_starter_pitches", "home_starter_pitches",
                        "away_bullpen_pa", "home_bullpen_pa")] <- list(
      score[["away"]], score[["home"]], final_inning, winner, final_inning > 9L,
      starter_seen[["away"]], starter_seen[["home"]],
      starter_pitches[["away"]], starter_pitches[["home"]],
      bullpen_pa[["away"]], bullpen_pa[["home"]]
    )
  }

  summarize_players <- function(arr, team, ids, names) {
    rows <- lapply(seq_len(9L), function(i) {
      stats <- matrix(arr[, i, , drop = FALSE], nrow = n_sims,
                      dimnames = list(NULL, stat_names))
      data.frame(
        team_side = team,
        batting_order = i,
        batter_id = ids[i],
        batter_name = names[i],
        expected_pa = mean(stats[, "PA"]),
        expected_hits = mean(stats[, "H"]),
        expected_bb = mean(stats[, "BB"]),
        expected_hbp = mean(stats[, "HBP"]),
        expected_k = mean(stats[, "K"]),
        expected_hr = mean(stats[, "HR"]),
        expected_xbh = mean(stats[, "XBH"]),
        expected_tb = mean(stats[, "TB"]),
        prob_1plus_hit = mean(stats[, "H"] >= 1),
        prob_2plus_hit = mean(stats[, "H"] >= 2),
        prob_1plus_hr = mean(stats[, "HR"] >= 1),
        prob_1plus_xbh = mean(stats[, "XBH"] >= 1),
        prob_2plus_tb = mean(stats[, "TB"] >= 2),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }

  total_pa <- rowSums(away_player[, , "PA"]) + rowSums(home_player[, , "PA"])
  total_bullpen_pa <- game_results$away_bullpen_pa + game_results$home_bullpen_pa
  game_summary <- data.frame(
    n_sims = n_sims,
    away_win_probability = mean(game_results$winner == "away"),
    home_win_probability = mean(game_results$winner == "home"),
    tie_probability = mean(game_results$winner == "tie"),
    away_mean_runs = mean(game_results$away_runs),
    home_mean_runs = mean(game_results$home_runs),
    mean_total_runs = mean(game_results$away_runs + game_results$home_runs),
    median_total_runs = stats::median(game_results$away_runs + game_results$home_runs),
    one_run_probability = mean(abs(game_results$away_runs - game_results$home_runs) == 1),
    extra_innings_probability = mean(game_results$went_extras),
    away_starter_mean_bf = mean(game_results$away_starter_bf),
    home_starter_mean_bf = mean(game_results$home_starter_bf),
    away_starter_mean_pitches = mean(game_results$away_starter_pitches),
    home_starter_mean_pitches = mean(game_results$home_starter_pitches),
    bullpen_pa_share = mean(total_bullpen_pa / pmax(total_pa, 1)),
    model_version = "plate_appearance_state_machine_v1",
    stringsAsFactors = FALSE
  )

  list(
    game_summary = game_summary,
    game_results = game_results,
    hitter_summary = rbind(
      summarize_players(away_player, "away", away_ids, away_names),
      summarize_players(home_player, "home", home_ids, home_names)
    )
  )
}
