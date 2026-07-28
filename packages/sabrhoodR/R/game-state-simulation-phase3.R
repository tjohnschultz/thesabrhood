#' Simulate a detailed plate-appearance game with named relievers
#'
#' Extends the state-machine simulation with runner identity, stolen-base
#' attempts, double plays, productive outs, sacrifice flies, park-sensitive
#' advancement, and individual reliever selection and workload.
#'
#' @inheritParams simulate_game_state
#' @param away_reliever_probs Away hitters versus individual home relievers.
#' @param home_reliever_probs Home hitters versus individual away relievers.
#' @param park_baserunning Named multipliers for park-sensitive advancement.
#' @param baserunning_rates Optional overrides for league baserunning rates.
#' @param away_runner_profiles Long-form empirical profiles for away runners.
#' @param home_runner_profiles Long-form empirical profiles for home runners.
#' @param defense_steal_multipliers Named away/home defensive multipliers for
#'   steal-attempt probability. Values below one suppress attempts.
#' @param manager_hook_coefficients Optional coefficient table with `term` and
#'   `estimate` columns from the pooled manager hook model. When supplied, the
#'   starter-to-bullpen transition is sampled after every plate appearance
#'   using current workload, times through the order, inning, score, and result.
#' @param away_starter_pitch_limit,home_starter_pitch_limit Pregame expected
#'   pitch-count limits. These are sampled in each draw and remain hard safety
#'   limits even when the manager hook model is active.
#' @param starter_pitch_limit_sd Standard deviation around the pregame pitch
#'   limits.
#' @param run_environment_multiplier Park-and-weather multiplier applied to
#'   extra-base-hit and on-base event odds before the game is simulated.
#'
#' @return A list with game, hitter, reliever, and event summaries.
#' @export
simulate_game_state_phase3 <- function(
    away_starter_probs,
    home_starter_probs,
    away_bullpen_probs,
    home_bullpen_probs,
    away_reliever_probs = NULL,
    home_reliever_probs = NULL,
    n_sims = 500L,
    away_starter_bf = 21,
    home_starter_bf = 21,
    seed = 1L,
    max_innings = 15L,
    automatic_runner = TRUE,
    park_baserunning = list(),
    baserunning_rates = list(),
    away_runner_profiles = NULL,
    home_runner_profiles = NULL,
    defense_steal_multipliers = c(away = 1, home = 1),
    manager_hook_coefficients = NULL,
    away_starter_pitch_limit = 92,
    home_starter_pitch_limit = 92,
    starter_pitch_limit_sd = 7,
    run_environment_multiplier = 1) {
  event_cols <- c("p_BB", "p_HBP", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  event_names <- sub("^p_", "", event_cols)
  stat_names <- c(
    "PA", "H", "BB", "HBP", "K", "HR", "XBH", "TB",
    "R", "RBI", "SB", "CS", "SF", "GDP"
  )

  validate_lineup <- function(x, label) {
    x <- as.data.frame(x)
    missing_cols <- setdiff(event_cols, names(x))
    if (length(missing_cols)) {
      stop(label, " is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    if (nrow(x) != 9L) stop(label, " must contain exactly nine ordered hitters.", call. = FALSE)
    if (!"batting_order" %in% names(x)) x$batting_order <- seq_len(nrow(x))
    x <- x[order(x$batting_order), , drop = FALSE]
    probabilities <- as.matrix(x[, event_cols, drop = FALSE])
    storage.mode(probabilities) <- "double"
    if (any(!is.finite(probabilities)) || any(probabilities < 0)) {
      stop(label, " contains invalid probabilities.", call. = FALSE)
    }
    totals <- rowSums(probabilities)
    if (any(totals <= 0)) stop(label, " contains a row with no probability mass.", call. = FALSE)
    x[, event_cols] <- probabilities / totals
    x
  }

  prepare_reliever_pool <- function(x, label) {
    if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(NULL)
    required <- c("pitcher_id", "pitcher_name", "batting_order", event_cols)
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    pitcher_ids <- unique(as.character(x$pitcher_id))
    complete_ids <- pitcher_ids[vapply(pitcher_ids, function(id) {
      length(unique(x$batting_order[as.character(x$pitcher_id) == id])) == 9L
    }, logical(1))]
    x <- x[as.character(x$pitcher_id) %in% complete_ids, , drop = FALSE]
    if (!nrow(x)) return(NULL)

    meta_rows <- !duplicated(as.character(x$pitcher_id))
    meta <- x[meta_rows, intersect(
      c("pitcher_id", "pitcher_name", "throws", "pitcher_role", "availability_score"),
      names(x)
    ), drop = FALSE]
    if (!"throws" %in% names(meta)) meta$throws <- "U"
    if (!"pitcher_role" %in% names(meta)) meta$pitcher_role <- "reliever"
    if (!"availability_score" %in% names(meta)) meta$availability_score <- 0.65
    meta$availability_score <- suppressWarnings(as.numeric(meta$availability_score))
    meta$availability_score[!is.finite(meta$availability_score)] <- 0.65
    meta$availability_score <- pmin(pmax(meta$availability_score, 0.05), 1)
    meta$pitcher_id <- as.character(meta$pitcher_id)

    matrices <- lapply(meta$pitcher_id, function(id) {
      rows <- x[as.character(x$pitcher_id) == id, , drop = FALSE]
      rows <- rows[order(rows$batting_order), , drop = FALSE]
      p <- as.matrix(rows[, event_cols, drop = FALSE])
      p / rowSums(p)
    })
    names(matrices) <- meta$pitcher_id
    estimated_woba <- vapply(matrices, function(p) {
      0.69 * p[, "p_BB"] + 0.72 * p[, "p_HBP"] + 0.88 * p[, "p_1B"] +
        1.24 * p[, "p_2B"] + 1.56 * p[, "p_3B"] + 2.01 * p[, "p_HR"]
    }, numeric(9L))
    if (is.null(dim(estimated_woba))) {
      estimated_woba <- matrix(estimated_woba, ncol = 1L)
    }
    colnames(estimated_woba) <- meta$pitcher_id
    list(meta = meta, matrices = matrices, estimated_woba = estimated_woba)
  }

  away_starter_probs <- validate_lineup(away_starter_probs, "away_starter_probs")
  home_starter_probs <- validate_lineup(home_starter_probs, "home_starter_probs")
  away_bullpen_probs <- validate_lineup(away_bullpen_probs, "away_bullpen_probs")
  home_bullpen_probs <- validate_lineup(home_bullpen_probs, "home_bullpen_probs")

  run_environment_multiplier <- as.numeric(run_environment_multiplier)[[1L]]
  if (!is.finite(run_environment_multiplier) || run_environment_multiplier <= 0) {
    stop("run_environment_multiplier must be one positive number.", call. = FALSE)
  }
  run_environment_multiplier <- pmin(pmax(run_environment_multiplier, 0.82), 1.22)
  adjust_environment <- function(x) {
    probability_matrix <- as.matrix(x[, event_cols, drop = FALSE])
    weights <- c(
      p_BB = 0.20, p_HBP = 0.10, p_K = 0, p_1B = 0.30,
      p_2B = 0.85, p_3B = 0.75, p_HR = 1.65, p_OUT = 0
    )
    probability_matrix <- sweep(
      probability_matrix,
      2L,
      run_environment_multiplier ^ weights[colnames(probability_matrix)],
      `*`
    )
    probability_matrix <- probability_matrix / rowSums(probability_matrix)
    x[, event_cols] <- probability_matrix
    x
  }
  away_starter_probs <- adjust_environment(away_starter_probs)
  home_starter_probs <- adjust_environment(home_starter_probs)
  away_bullpen_probs <- adjust_environment(away_bullpen_probs)
  home_bullpen_probs <- adjust_environment(home_bullpen_probs)
  if (!is.null(away_reliever_probs) && is.data.frame(away_reliever_probs) && nrow(away_reliever_probs)) {
    away_reliever_probs <- adjust_environment(away_reliever_probs)
  }
  if (!is.null(home_reliever_probs) && is.data.frame(home_reliever_probs) && nrow(home_reliever_probs)) {
    home_reliever_probs <- adjust_environment(home_reliever_probs)
  }
  away_pool <- prepare_reliever_pool(away_reliever_probs, "away_reliever_probs")
  home_pool <- prepare_reliever_pool(home_reliever_probs, "home_reliever_probs")

  n_sims <- as.integer(n_sims)
  max_innings <- as.integer(max_innings)
  if (!is.finite(n_sims) || n_sims < 1L) stop("n_sims must be positive.", call. = FALSE)
  if (!is.finite(max_innings) || max_innings < 9L) stop("max_innings must be at least nine.", call. = FALSE)
  clamp <- function(x, lo, hi) min(max(as.numeric(x), lo), hi)
  away_starter_bf <- clamp(away_starter_bf, 12, 30)
  home_starter_bf <- clamp(home_starter_bf, 12, 30)
  away_starter_pitch_limit <- clamp(away_starter_pitch_limit, 55, 115)
  home_starter_pitch_limit <- clamp(home_starter_pitch_limit, 55, 115)
  starter_pitch_limit_sd <- clamp(starter_pitch_limit_sd, 0, 20)

  hook_terms <- c(
    "(Intercept)", "pitches_over_60", "bf_over_18", "third_time",
    "late_inning", "close_game", "trailing_badly", "adverse_result",
    "starter_flag", "reliever_flag"
  )
  hook_coefficients <- NULL
  if (is.data.frame(manager_hook_coefficients) &&
      all(c("term", "estimate") %in% names(manager_hook_coefficients))) {
    estimates <- suppressWarnings(as.numeric(manager_hook_coefficients$estimate))
    names(estimates) <- as.character(manager_hook_coefficients$term)
    if (all(hook_terms %in% names(estimates)) &&
        all(is.finite(estimates[hook_terms]))) {
      hook_coefficients <- estimates[hook_terms]
    }
  }
  manager_hook_active <- !is.null(hook_coefficients)
  manager_hook_probability <- function(
      pitches, batters_faced, inning, fielding_score_diff, event) {
    adverse_events <- c("1B", "2B", "3B", "HR", "BB", "HBP")
    features <- c(
      "(Intercept)" = 1,
      pitches_over_60 = pmax(pitches - 60, 0) / 20,
      bf_over_18 = pmax(batters_faced - 18, 0) / 9,
      third_time = as.numeric(batters_faced >= 19),
      late_inning = pmax(inning - 6, 0) / 3,
      close_game = as.numeric(abs(fielding_score_diff) <= 2),
      trailing_badly = as.numeric(fielding_score_diff <= -4),
      adverse_result = as.numeric(event %in% adverse_events),
      starter_flag = 1,
      reliever_flag = 0
    )
    stats::plogis(sum(hook_coefficients[names(features)] * features))
  }

  defaults <- list(
    single_second_scores = 0.60,
    single_first_to_third = 0.30,
    double_first_scores = 0.55,
    ground_ball_double_play = 0.115,
    sac_fly_scores = 0.30,
    second_to_third_on_out = 0.20,
    first_to_second_on_out = 0.08,
    steal_second_attempt = 0.040,
    steal_second_success = 0.78,
    steal_third_attempt = 0.012,
    steal_third_success = 0.72
  )
  rates <- utils::modifyList(defaults, baserunning_rates)
  park_defaults <- list(
    single_second_score = 1,
    single_first_to_third = 1,
    double_first_score = 1,
    steal_attempt = 1
  )
  park <- utils::modifyList(park_defaults, park_baserunning)
  probability <- function(value) pmin(pmax(as.numeric(value), 0), 1)
  rates$single_second_scores <- probability(rates$single_second_scores * park$single_second_score)
  rates$single_first_to_third <- probability(rates$single_first_to_third * park$single_first_to_third)
  rates$double_first_scores <- probability(rates$double_first_scores * park$double_first_score)
  rates$steal_second_attempt <- probability(rates$steal_second_attempt * park$steal_attempt)
  rates$steal_third_attempt <- probability(rates$steal_third_attempt * park$steal_attempt)
  for (name in setdiff(names(rates), c(
    "single_second_scores", "single_first_to_third",
    "double_first_scores", "steal_second_attempt", "steal_third_attempt"
  ))) rates[[name]] <- probability(rates[[name]])

  player_label <- function(x) {
    candidate <- c("batter_name", "player_name", "name")
    column <- candidate[candidate %in% names(x)][1L]
    if (is.na(column)) paste("Lineup spot", seq_len(nrow(x))) else as.character(x[[column]])
  }
  player_id <- function(x) {
    candidate <- c("batter_id", "player_id", "mlbam_id")
    column <- candidate[candidate %in% names(x)][1L]
    if (is.na(column)) seq_len(nrow(x)) else x[[column]]
  }
  away_names <- player_label(away_starter_probs)
  home_names <- player_label(home_starter_probs)
  away_ids <- player_id(away_starter_probs)
  home_ids <- player_id(home_starter_probs)

  profile_metrics <- c(
    "single_second_scores", "single_first_to_third", "double_first_scores",
    "sac_fly_scores", "second_to_third_on_out", "first_to_second_on_out",
    "steal_second_attempt", "steal_second_success",
    "steal_third_attempt", "steal_third_success"
  )
  prepare_runner_rates <- function(profiles, player_ids) {
    output <- matrix(
      NA_real_,
      nrow = 9L,
      ncol = length(profile_metrics),
      dimnames = list(NULL, profile_metrics)
    )
    if (is.null(profiles) || !is.data.frame(profiles) || !nrow(profiles)) {
      return(output)
    }
    required <- c(
      "runner_id", "opportunity_type", "shrunk_rate",
      "shrunk_success_rate"
    )
    if (length(setdiff(required, names(profiles)))) return(output)
    profiles$runner_id <- as.character(profiles$runner_id)
    profiles$opportunity_type <- as.character(profiles$opportunity_type)
    for (spot in seq_len(9L)) {
      rows <- profiles$runner_id == as.character(player_ids[[spot]])
      player <- profiles[rows, , drop = FALSE]
      if (!nrow(player)) next
      advancement <- player[!grepl("^steal_", player$opportunity_type), , drop = FALSE]
      if (nrow(advancement)) {
        index <- match(advancement$opportunity_type, profile_metrics)
        valid <- !is.na(index)
        output[spot, index[valid]] <- as.numeric(advancement$shrunk_rate[valid])
      }
      for (base in c("second", "third")) {
        row <- player[player$opportunity_type == paste0("steal_", base), , drop = FALSE]
        if (!nrow(row)) next
        output[spot, paste0("steal_", base, "_attempt")] <- as.numeric(row$shrunk_rate[[1L]])
        output[spot, paste0("steal_", base, "_success")] <- as.numeric(
          row$shrunk_success_rate[[1L]]
        )
      }
    }
    output
  }
  runner_rates <- list(
    away = prepare_runner_rates(away_runner_profiles, away_ids),
    home = prepare_runner_rates(home_runner_profiles, home_ids)
  )
  runner_model_active <- any(is.finite(runner_rates$away)) ||
    any(is.finite(runner_rates$home))
  defense_steal_multipliers <- as.numeric(defense_steal_multipliers)
  if (length(defense_steal_multipliers) < 2L) {
    defense_steal_multipliers <- rep(defense_steal_multipliers[[1L]], 2L)
  }
  names(defense_steal_multipliers) <- c("away", "home")
  defense_steal_multipliers[!is.finite(defense_steal_multipliers)] <- 1
  defense_steal_multipliers <- pmin(pmax(defense_steal_multipliers, 0.70), 1.30)

  runner_probability <- function(offense, spot, metric) {
    value <- runner_rates[[offense]][spot, metric]
    if (is.finite(value)) {
      multiplier <- switch(
        metric,
        single_second_scores = park$single_second_score,
        single_first_to_third = park$single_first_to_third,
        double_first_scores = park$double_first_score,
        steal_second_attempt = park$steal_attempt,
        steal_third_attempt = park$steal_attempt,
        1
      )
      return(probability(value * multiplier))
    }
    rates[[metric]]
  }

  away_player <- array(0, c(n_sims, 9L, length(stat_names)), dimnames = list(NULL, NULL, stat_names))
  home_player <- array(0, c(n_sims, 9L, length(stat_names)), dimnames = list(NULL, NULL, stat_names))

  create_tracker <- function(pool, defense) {
    if (is.null(pool)) return(data.frame())
    data.frame(
      defense_side = defense,
      pitcher_id = pool$meta$pitcher_id,
      pitcher_name = pool$meta$pitcher_name,
      throws = pool$meta$throws,
      pitcher_role = pool$meta$pitcher_role,
      availability_score = pool$meta$availability_score,
      decision_opportunities = 0L,
      selection_probability_sum = 0,
      selection_count = 0L,
      bf_total = 0L,
      entry_inning_sum = 0,
      high_leverage_entries = 0L,
      stringsAsFactors = FALSE
    )
  }
  reliever_tracker <- rbind(
    create_tracker(away_pool, "home"),
    create_tracker(home_pool, "away")
  )

  game_results <- data.frame(
    simulation_id = seq_len(n_sims),
    away_runs = integer(n_sims), home_runs = integer(n_sims),
    innings = integer(n_sims), winner = character(n_sims),
    went_extras = logical(n_sims),
    away_starter_bf = integer(n_sims), home_starter_bf = integer(n_sims),
    away_starter_pitches = integer(n_sims), home_starter_pitches = integer(n_sims),
    away_bullpen_pa = integer(n_sims), home_bullpen_pa = integer(n_sims),
    double_plays = integer(n_sims), productive_out_runs = integer(n_sims),
    sacrifice_flies = integer(n_sims), steal_attempts = integer(n_sims),
    stolen_bases = integer(n_sims), caught_stealing = integer(n_sims),
    away_starter_hooked = logical(n_sims), home_starter_hooked = logical(n_sims),
    away_max_hook_probability = numeric(n_sims),
    home_max_hook_probability = numeric(n_sims),
    relievers_used = integer(n_sims),
    stringsAsFactors = FALSE
  )

  pitch_count_for_event <- function(event) {
    means <- c(BB = 5.3, HBP = 3.4, K = 4.8, `1B` = 3.5, `2B` = 3.5,
               `3B` = 3.5, HR = 3.6, OUT = 3.4)
    max(1L, as.integer(stats::rpois(1L, means[[event]] - 1) + 1L))
  }

  event_increment <- function(event) {
    output <- stats::setNames(rep(0, length(stat_names)), stat_names)
    output[["PA"]] <- 1
    if (event %in% c("1B", "2B", "3B", "HR")) output[["H"]] <- 1
    if (event == "BB") output[["BB"]] <- 1
    if (event == "HBP") output[["HBP"]] <- 1
    if (event == "K") output[["K"]] <- 1
    if (event == "HR") output[["HR"]] <- 1
    if (event %in% c("2B", "3B", "HR")) output[["XBH"]] <- 1
    total_bases <- c(`1B` = 1, `2B` = 2, `3B` = 3, HR = 4)
    if (event %in% names(total_bases)) output[["TB"]] <- total_bases[[event]]
    output
  }

  advance_runners <- function(event, bases, batter_spot, offense) {
    scored <- integer()
    if (event %in% c("BB", "HBP")) {
      if (bases[1L] != 0L) {
        if (bases[2L] != 0L) {
          if (bases[3L] != 0L) scored <- c(scored, bases[3L])
          bases[3L] <- bases[2L]
        }
        bases[2L] <- bases[1L]
      }
      bases[1L] <- batter_spot
    } else if (event == "1B") {
      new_bases <- c(batter_spot, 0L, 0L)
      if (bases[3L] != 0L) scored <- c(scored, bases[3L])
      if (bases[2L] != 0L) {
        if (stats::runif(1L) <
            runner_probability(offense, bases[2L], "single_second_scores")) {
          scored <- c(scored, bases[2L])
        }
        else new_bases[3L] <- bases[2L]
      }
      if (bases[1L] != 0L) {
        if (new_bases[3L] == 0L &&
            stats::runif(1L) <
              runner_probability(offense, bases[1L], "single_first_to_third")) {
          new_bases[3L] <- bases[1L]
        } else {
          new_bases[2L] <- bases[1L]
        }
      }
      bases <- new_bases
    } else if (event == "2B") {
      new_bases <- c(0L, batter_spot, 0L)
      if (bases[3L] != 0L) scored <- c(scored, bases[3L])
      if (bases[2L] != 0L) scored <- c(scored, bases[2L])
      if (bases[1L] != 0L) {
        if (stats::runif(1L) <
            runner_probability(offense, bases[1L], "double_first_scores")) {
          scored <- c(scored, bases[1L])
        }
        else new_bases[3L] <- bases[1L]
      }
      bases <- new_bases
    } else if (event == "3B") {
      scored <- c(scored, bases[bases != 0L])
      bases <- c(0L, 0L, batter_spot)
    } else if (event == "HR") {
      scored <- c(scored, bases[bases != 0L], batter_spot)
      bases <- c(0L, 0L, 0L)
    }
    list(bases = bases, scored = as.integer(scored))
  }

  softmax <- function(x, temperature = 5) {
    z <- exp(temperature * (x - max(x)))
    z / sum(z)
  }

  select_reliever <- function(pool, defense, used, upcoming_spot, inning, score_diff) {
    if (is.null(pool)) return(NULL)
    candidates <- pool$meta[!pool$meta$pitcher_id %in% used, , drop = FALSE]
    if (!nrow(candidates)) return(NULL)
    upcoming <- ((upcoming_spot - 1L + 0:2) %% 9L) + 1L
    woba <- vapply(candidates$pitcher_id, function(id) {
      mean(pool$estimated_woba[upcoming, id])
    }, numeric(1L))
    if (diff(range(woba)) > 1e-9) {
      matchup <- 1 - (woba - min(woba)) / diff(range(woba))
    } else {
      matchup <- rep(0.5, length(woba))
    }
    high_leverage <- inning >= 7L && abs(score_diff) <= 3
    role_text <- tolower(as.character(candidates$pitcher_role))
    role_bonus <- if (high_leverage) {
      ifelse(grepl("closer|high|late", role_text), 1, 0.55)
    } else {
      ifelse(grepl("long|bulk|middle", role_text), 1, 0.60)
    }
    score <- 0.50 * candidates$availability_score + 0.38 * matchup + 0.12 * role_bonus
    choice_probability <- softmax(score)

    tracker_index <- match(
      paste(defense, candidates$pitcher_id, sep = "|"),
      paste(reliever_tracker$defense_side, reliever_tracker$pitcher_id, sep = "|")
    )
    valid <- is.finite(tracker_index)
    reliever_tracker$decision_opportunities[tracker_index[valid]] <<-
      reliever_tracker$decision_opportunities[tracker_index[valid]] + 1L
    reliever_tracker$selection_probability_sum[tracker_index[valid]] <<-
      reliever_tracker$selection_probability_sum[tracker_index[valid]] + choice_probability[valid]

    selected_index <- sample(seq_len(nrow(candidates)), 1L, prob = choice_probability)
    selected <- candidates[selected_index, , drop = FALSE]
    selected$selection_probability <- choice_probability[selected_index]
    selected$selection_score <- score[selected_index]
    selected$high_leverage <- high_leverage
    selected
  }

  set.seed(as.integer(seed))
  away_starter_matrix <- as.matrix(away_starter_probs[, event_cols, drop = FALSE])
  home_starter_matrix <- as.matrix(home_starter_probs[, event_cols, drop = FALSE])
  away_bullpen_matrix <- as.matrix(away_bullpen_probs[, event_cols, drop = FALSE])
  home_bullpen_matrix <- as.matrix(home_bullpen_probs[, event_cols, drop = FALSE])

  for (sim in seq_len(n_sims)) {
    stats_by_team <- list(
      away = matrix(0, 9L, length(stat_names), dimnames = list(NULL, stat_names)),
      home = matrix(0, 9L, length(stat_names), dimnames = list(NULL, stat_names))
    )
    score <- c(away = 0L, home = 0L)
    lineup_index <- c(away = 1L, home = 1L)
    starter_target <- c(
      away = as.integer(round(stats::rnorm(1L, away_starter_bf, 2.6))),
      home = as.integer(round(stats::rnorm(1L, home_starter_bf, 2.6)))
    )
    starter_target <- pmin(pmax(starter_target, 12L), 30L)
    if (manager_hook_active) starter_target[] <- 30L
    pitch_limit <- c(
      away = as.integer(round(stats::rnorm(
        1L, away_starter_pitch_limit, starter_pitch_limit_sd
      ))),
      home = as.integer(round(stats::rnorm(
        1L, home_starter_pitch_limit, starter_pitch_limit_sd
      )))
    )
    pitch_limit <- pmin(pmax(pitch_limit, 55L), 115L)
    names(pitch_limit) <- c("away", "home")
    starter_seen <- starter_pitches <- bullpen_pa <- c(away = 0L, home = 0L)
    starter_removed <- c(away = FALSE, home = FALSE)
    starter_max_hook_probability <- c(away = 0, home = 0)
    current_reliever <- list(away = NULL, home = NULL)
    reliever_bf <- reliever_pitches <- c(away = 0L, home = 0L)
    reliever_target <- c(away = 0L, home = 0L)
    reliever_entry_inning <- c(away = 0L, home = 0L)
    used_relievers <- list(away = character(), home = character())
    event_totals <- c(DP = 0L, productive_runs = 0L, SF = 0L, SBA = 0L, SB = 0L, CS = 0L)
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
        pool <- if (offense == "away") away_pool else home_pool
        aggregate_matrix <- if (offense == "away") away_bullpen_matrix else home_bullpen_matrix
        starter_matrix <- if (offense == "away") away_starter_matrix else home_starter_matrix
        outs <- 0L
        bases <- c(0L, 0L, 0L)
        if (automatic_runner && inning >= 10L) {
          bases[2L] <- if (lineup_index[[offense]] == 1L) 9L else lineup_index[[offense]] - 1L
        }
        half_pa <- 0L

        while (outs < 3L && half_pa < 40L) {
          spot <- lineup_index[[offense]]

          if (bases[1L] != 0L && bases[2L] == 0L &&
              stats::runif(1L) <
                probability(
                  runner_probability(
                    offense,
                    bases[1L],
                    "steal_second_attempt"
                  ) * defense_steal_multipliers[[defense]]
                )) {
            event_totals[["SBA"]] <- event_totals[["SBA"]] + 1L
            runner <- bases[1L]
            if (stats::runif(1L) <
                runner_probability(offense, runner, "steal_second_success")) {
              bases[2L] <- runner
              bases[1L] <- 0L
              stats_by_team[[offense]][runner, "SB"] <- stats_by_team[[offense]][runner, "SB"] + 1
              event_totals[["SB"]] <- event_totals[["SB"]] + 1L
            } else {
              bases[1L] <- 0L
              outs <- outs + 1L
              stats_by_team[[offense]][runner, "CS"] <- stats_by_team[[offense]][runner, "CS"] + 1
              event_totals[["CS"]] <- event_totals[["CS"]] + 1L
              if (outs >= 3L) next
            }
          } else if (bases[2L] != 0L && bases[3L] == 0L &&
                     stats::runif(1L) <
                       probability(
                         runner_probability(
                           offense,
                           bases[2L],
                           "steal_third_attempt"
                         ) * defense_steal_multipliers[[defense]]
                       )) {
            event_totals[["SBA"]] <- event_totals[["SBA"]] + 1L
            runner <- bases[2L]
            if (stats::runif(1L) <
                runner_probability(offense, runner, "steal_third_success")) {
              bases[3L] <- runner
              bases[2L] <- 0L
              stats_by_team[[offense]][runner, "SB"] <- stats_by_team[[offense]][runner, "SB"] + 1
              event_totals[["SB"]] <- event_totals[["SB"]] + 1L
            } else {
              bases[2L] <- 0L
              outs <- outs + 1L
              stats_by_team[[offense]][runner, "CS"] <- stats_by_team[[offense]][runner, "CS"] + 1
              event_totals[["CS"]] <- event_totals[["CS"]] + 1L
              if (outs >= 3L) next
            }
          }

          half_pa <- half_pa + 1L
          starter_active <- !starter_removed[[defense]] &&
            starter_seen[[defense]] < starter_target[[defense]] &&
            starter_pitches[[defense]] < pitch_limit[[defense]]

          selected_pitcher_id <- NA_character_
          if (starter_active) {
            p <- starter_matrix[spot, ]
          } else {
            needs_reliever <- is.null(current_reliever[[defense]]) ||
              reliever_bf[[defense]] >= reliever_target[[defense]] ||
              reliever_pitches[[defense]] >= 30L ||
              (reliever_entry_inning[[defense]] < inning && stats::runif(1L) < 0.48)
            if (needs_reliever) {
              score_diff <- score[[defense]] - score[[offense]]
              selected <- select_reliever(
                pool, defense, used_relievers[[defense]], spot, inning, score_diff
              )
              current_reliever[[defense]] <- selected
              reliever_bf[[defense]] <- 0L
              reliever_pitches[[defense]] <- 0L
              reliever_entry_inning[[defense]] <- inning
              if (!is.null(selected)) {
                selected_pitcher_id <- as.character(selected$pitcher_id[[1L]])
                used_relievers[[defense]] <- c(used_relievers[[defense]], selected_pitcher_id)
                reliever_target[[defense]] <- pmin(
                  pmax(as.integer(round(stats::rnorm(
                  1L, if (isTRUE(selected$high_leverage[[1L]])) 4.6 else 6.0, 1.3
                  ))), 2L),
                  9L
                )
                tracker_row <- which(
                  reliever_tracker$defense_side == defense &
                    reliever_tracker$pitcher_id == selected_pitcher_id
                )
                reliever_tracker$selection_count[tracker_row] <- reliever_tracker$selection_count[tracker_row] + 1L
                reliever_tracker$entry_inning_sum[tracker_row] <- reliever_tracker$entry_inning_sum[tracker_row] + inning
                reliever_tracker$high_leverage_entries[tracker_row] <- reliever_tracker$high_leverage_entries[tracker_row] +
                  as.integer(selected$high_leverage[[1L]])
              } else {
                reliever_target[[defense]] <- 4L
              }
            }
            if (!is.null(current_reliever[[defense]])) {
              selected_pitcher_id <- as.character(current_reliever[[defense]]$pitcher_id[[1L]])
              p <- pool$matrices[[selected_pitcher_id]][spot, ]
            } else {
              p <- aggregate_matrix[spot, ]
            }
          }

          event <- sample(event_names, 1L, prob = p)
          pitches_in_pa <- pitch_count_for_event(event)
          if (starter_active) {
            starter_seen[[defense]] <- starter_seen[[defense]] + 1L
            starter_pitches[[defense]] <- starter_pitches[[defense]] + pitches_in_pa
          } else {
            bullpen_pa[[defense]] <- bullpen_pa[[defense]] + 1L
            if (!is.na(selected_pitcher_id)) {
              reliever_bf[[defense]] <- reliever_bf[[defense]] + 1L
              reliever_pitches[[defense]] <- reliever_pitches[[defense]] + pitches_in_pa
              tracker_row <- which(
                reliever_tracker$defense_side == defense &
                  reliever_tracker$pitcher_id == selected_pitcher_id
              )
              reliever_tracker$bf_total[tracker_row] <- reliever_tracker$bf_total[tracker_row] + 1L
            }
          }

          stats_by_team[[offense]][spot, ] <- stats_by_team[[offense]][spot, ] + event_increment(event)
          scored <- integer()
          rbi <- 0L
          if (event == "K") {
            outs <- outs + 1L
          } else if (event == "OUT") {
            outs_before <- outs
            if (bases[1L] != 0L && outs_before <= 1L &&
                stats::runif(1L) < rates$ground_ball_double_play) {
              bases[1L] <- 0L
              outs <- outs + 2L
              stats_by_team[[offense]][spot, "GDP"] <- stats_by_team[[offense]][spot, "GDP"] + 1
              event_totals[["DP"]] <- event_totals[["DP"]] + 1L
            } else {
              outs <- outs + 1L
              if (outs_before <= 1L && bases[3L] != 0L &&
                  stats::runif(1L) <
                    runner_probability(offense, bases[3L], "sac_fly_scores")) {
                scored <- bases[3L]
                bases[3L] <- 0L
                rbi <- 1L
                stats_by_team[[offense]][spot, "SF"] <- stats_by_team[[offense]][spot, "SF"] + 1
                event_totals[["SF"]] <- event_totals[["SF"]] + 1L
                event_totals[["productive_runs"]] <- event_totals[["productive_runs"]] + 1L
              }
              if (outs_before <= 1L && bases[2L] != 0L && bases[3L] == 0L &&
                  stats::runif(1L) <
                    runner_probability(
                      offense,
                      bases[2L],
                      "second_to_third_on_out"
                    )) {
                bases[3L] <- bases[2L]
                bases[2L] <- 0L
              }
              if (outs_before <= 1L && bases[1L] != 0L && bases[2L] == 0L &&
                  stats::runif(1L) <
                    runner_probability(
                      offense,
                      bases[1L],
                      "first_to_second_on_out"
                    )) {
                bases[2L] <- bases[1L]
                bases[1L] <- 0L
              }
            }
          } else {
            advancement <- advance_runners(event, bases, spot, offense)
            bases <- advancement$bases
            scored <- advancement$scored
            rbi <- length(scored)
          }

          if (length(scored)) {
            score[[offense]] <- score[[offense]] + length(scored)
            stats_by_team[[offense]][scored, "R"] <- stats_by_team[[offense]][scored, "R"] + 1
            stats_by_team[[offense]][spot, "RBI"] <- stats_by_team[[offense]][spot, "RBI"] + rbi
          }
          lineup_index[[offense]] <- if (spot == 9L) 1L else spot + 1L

          if (starter_active && manager_hook_active) {
            hook_probability <- manager_hook_probability(
              pitches = starter_pitches[[defense]],
              batters_faced = starter_seen[[defense]],
              inning = inning,
              fielding_score_diff = score[[defense]] - score[[offense]],
              event = event
            )
            starter_max_hook_probability[[defense]] <- max(
              starter_max_hook_probability[[defense]],
              hook_probability
            )
            starter_removed[[defense]] <- stats::runif(1L) < hook_probability
          }

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

    away_player[sim, , ] <- stats_by_team$away
    home_player[sim, , ] <- stats_by_team$home
    winner <- if (score[["away"]] > score[["home"]]) "away" else
      if (score[["home"]] > score[["away"]]) "home" else "tie"
    game_results[sim, setdiff(names(game_results), "simulation_id")] <- list(
      score[["away"]], score[["home"]], final_inning, winner, final_inning > 9L,
      starter_seen[["away"]], starter_seen[["home"]],
      starter_pitches[["away"]], starter_pitches[["home"]],
      bullpen_pa[["away"]], bullpen_pa[["home"]],
      event_totals[["DP"]], event_totals[["productive_runs"]], event_totals[["SF"]],
      event_totals[["SBA"]], event_totals[["SB"]], event_totals[["CS"]],
      starter_removed[["away"]], starter_removed[["home"]],
      starter_max_hook_probability[["away"]], starter_max_hook_probability[["home"]],
      length(unique(c(used_relievers$away, used_relievers$home)))
    )
  }

  summarize_players <- function(array, team, ids, labels) {
    do.call(rbind, lapply(seq_len(9L), function(i) {
      values <- matrix(array[, i, , drop = FALSE], nrow = n_sims, dimnames = list(NULL, stat_names))
      data.frame(
        team_side = team, batting_order = i, batter_id = ids[i], batter_name = labels[i],
        expected_pa = mean(values[, "PA"]), expected_hits = mean(values[, "H"]),
        expected_bb = mean(values[, "BB"]), expected_hbp = mean(values[, "HBP"]),
        expected_k = mean(values[, "K"]), expected_hr = mean(values[, "HR"]),
        expected_xbh = mean(values[, "XBH"]), expected_tb = mean(values[, "TB"]),
        expected_runs = mean(values[, "R"]), expected_rbi = mean(values[, "RBI"]),
        expected_sb = mean(values[, "SB"]), expected_cs = mean(values[, "CS"]),
        expected_sf = mean(values[, "SF"]), expected_gdp = mean(values[, "GDP"]),
        prob_1plus_hit = mean(values[, "H"] >= 1), prob_2plus_hit = mean(values[, "H"] >= 2),
        prob_1plus_hr = mean(values[, "HR"] >= 1), prob_1plus_xbh = mean(values[, "XBH"] >= 1),
        prob_2plus_tb = mean(values[, "TB"] >= 2), prob_1plus_run = mean(values[, "R"] >= 1),
        prob_1plus_rbi = mean(values[, "RBI"] >= 1), prob_1plus_sb = mean(values[, "SB"] >= 1),
        stringsAsFactors = FALSE
      )
    }))
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
    mean_double_plays = mean(game_results$double_plays),
    mean_productive_out_runs = mean(game_results$productive_out_runs),
    mean_sacrifice_flies = mean(game_results$sacrifice_flies),
    mean_steal_attempts = mean(game_results$steal_attempts),
    mean_stolen_bases = mean(game_results$stolen_bases),
    mean_caught_stealing = mean(game_results$caught_stealing),
    away_starter_hook_probability = mean(game_results$away_starter_hooked),
    home_starter_hook_probability = mean(game_results$home_starter_hooked),
    away_mean_max_hook_probability = mean(game_results$away_max_hook_probability),
    home_mean_max_hook_probability = mean(game_results$home_max_hook_probability),
    run_environment_multiplier = run_environment_multiplier,
    manager_hook_model_active = manager_hook_active,
    mean_relievers_used = mean(game_results$relievers_used),
    model_version = if (manager_hook_active) {
      "plate_appearance_state_machine_phase5_manager_hook_v1"
    } else if (runner_model_active) {
      "plate_appearance_state_machine_phase4_baserunning_v1"
    } else {
      "plate_appearance_state_machine_phase3_v1"
    },
    stringsAsFactors = FALSE
  )

  if (nrow(reliever_tracker)) {
    reliever_tracker$appearance_probability <- reliever_tracker$selection_count / n_sims
    reliever_tracker$mean_selection_likelihood <- ifelse(
      reliever_tracker$decision_opportunities > 0L,
      reliever_tracker$selection_probability_sum / reliever_tracker$decision_opportunities,
      NA_real_
    )
    reliever_tracker$mean_bf_per_appearance <- ifelse(
      reliever_tracker$selection_count > 0L,
      reliever_tracker$bf_total / reliever_tracker$selection_count,
      NA_real_
    )
    reliever_tracker$mean_entry_inning <- ifelse(
      reliever_tracker$selection_count > 0L,
      reliever_tracker$entry_inning_sum / reliever_tracker$selection_count,
      NA_real_
    )
    reliever_tracker$high_leverage_entry_share <- ifelse(
      reliever_tracker$selection_count > 0L,
      reliever_tracker$high_leverage_entries / reliever_tracker$selection_count,
      NA_real_
    )
    reliever_tracker <- reliever_tracker[order(
      reliever_tracker$defense_side, -reliever_tracker$appearance_probability
    ), , drop = FALSE]
  }

  event_summary <- data.frame(
    event = c(
      "double_plays", "productive_out_runs", "sacrifice_flies",
      "steal_attempts", "stolen_bases", "caught_stealing", "relievers_used"
    ),
    mean_per_game = c(
      mean(game_results$double_plays), mean(game_results$productive_out_runs),
      mean(game_results$sacrifice_flies), mean(game_results$steal_attempts),
      mean(game_results$stolen_bases), mean(game_results$caught_stealing),
      mean(game_results$relievers_used)
    ),
    stringsAsFactors = FALSE
  )

  list(
    game_summary = game_summary,
    game_results = game_results,
    hitter_summary = rbind(
      summarize_players(away_player, "away", away_ids, away_names),
      summarize_players(home_player, "home", home_ids, home_names)
    ),
    reliever_summary = reliever_tracker,
    event_summary = event_summary
  )
}
