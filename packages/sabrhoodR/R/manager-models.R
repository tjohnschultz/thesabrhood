.prepare_hook_features <- function(context) {
  required <- c(
    "pitches_in_appearance", "batters_faced_in_appearance",
    "times_through_order_proxy", "inning", "fielding_score_diff", "event_key"
  )
  missing <- setdiff(required, names(context))
  if (length(missing) > 0L) stop("Hook context is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  role <- if ("pitcher_role" %in% names(context)) {
    tolower(as.character(context$pitcher_role))
  } else {
    rep("unknown", nrow(context))
  }
  event <- .normalize_event_key(context$event_key)
  score_diff <- .numeric_value(context$fielding_score_diff)

  data.frame(
    pitches_over_60 = pmax(.numeric_value(context$pitches_in_appearance) - 60, 0) / 20,
    bf_over_18 = pmax(.numeric_value(context$batters_faced_in_appearance) - 18, 0) / 9,
    third_time = as.numeric(.numeric_value(context$times_through_order_proxy) >= 3),
    late_inning = pmax(.numeric_value(context$inning) - 6, 0) / 3,
    close_game = as.numeric(abs(score_diff) <= 2),
    trailing_badly = as.numeric(score_diff <= -4),
    adverse_result = as.numeric(event %in% c(
      "single", "double", "triple", "home_run", "walk", "intent_walk",
      "intentional_walk", "hit_by_pitch", "field_error"
    )),
    starter_flag = as.numeric(role %in% c("starter", "opener")),
    reliever_flag = as.numeric(role %in% c("reliever", "bulk", "hybrid")),
    stringsAsFactors = FALSE
  )
}

#' Fit a manager pitcher-hook probability model
#'
#' @param decisions Output from [build_manager_pitching_decisions()].
#'
#' @return A `sabrhood_hook_model` containing an auditable pooled logistic model.
#' @export
fit_manager_hook_model <- function(decisions) {
  if (!is.data.frame(decisions)) stop("`decisions` must be a data frame.", call. = FALSE)
  if (!"pitcher_pulled_after_pa" %in% names(decisions)) {
    stop("Decisions must contain `pitcher_pulled_after_pa`.", call. = FALSE)
  }

  features <- .prepare_hook_features(decisions)
  features$pitcher_pulled_after_pa <- as.numeric(decisions$pitcher_pulled_after_pa)
  features <- features[stats::complete.cases(features), , drop = FALSE]
  if (nrow(features) < 100L) stop("At least 100 complete decision rows are required.", call. = FALSE)
  if (length(unique(features$pitcher_pulled_after_pa)) < 2L) {
    stop("Hook training data must contain both pulled and retained outcomes.", call. = FALSE)
  }

  hook_formula <- stats::as.formula(
    "pitcher_pulled_after_pa ~ pitches_over_60 + bf_over_18 + third_time + late_inning + close_game + trailing_badly + adverse_result + starter_flag + reliever_flag",
    env = baseenv()
  )
  model <- stats::glm(
    hook_formula,
    data = features,
    family = stats::binomial(),
    model = FALSE,
    x = FALSE,
    y = FALSE
  )

  structure(
    list(
      model = model,
      training_rows = nrow(features),
      observed_hook_rate = mean(features$pitcher_pulled_after_pa),
      method = "sabrhood_pooled_hook_logit_v1",
      limitation = "Pooled league model; manager, team, and calibration layers are not yet included."
    ),
    class = "sabrhood_hook_model"
  )
}

#' Predict the probability a pitcher is pulled after a plate appearance
#'
#' @param model Output from [fit_manager_hook_model()].
#' @param context Completed plate-appearance context rows.
#'
#' @return The context with hook probability and method fields.
#' @export
predict_manager_hook_probability <- function(model, context) {
  if (!inherits(model, "sabrhood_hook_model")) {
    stop("`model` must come from `fit_manager_hook_model()`.", call. = FALSE)
  }
  features <- .prepare_hook_features(context)
  output <- tibble::as_tibble(context)
  output$hook_probability <- as.numeric(stats::predict(model$model, newdata = features, type = "response"))
  output$hook_probability <- pmax(pmin(output$hook_probability, 1), 0)
  output$hook_model_method <- model$method
  output
}

#' Validate a manager pitcher-hook model out of time
#'
#' @param decisions Output from [build_manager_pitching_decisions()]. Rows must
#'   be ordered in time or contain `game_date`.
#' @param validation_fraction Fraction of the latest observations reserved for
#'   validation when `train_through` is not supplied.
#' @param train_through Optional final training date. Later rows are validation.
#' @param calibration_bins Number of approximately equal-size probability bins.
#'
#' @return A `sabrhood_hook_validation` list containing the fitted training
#'   model, row-level validation predictions, overall metrics, and calibration.
#' @export
validate_manager_hook_model <- function(
    decisions,
    validation_fraction = 0.20,
    train_through = NULL,
    calibration_bins = 10L) {
  if (!is.data.frame(decisions)) stop("`decisions` must be a data frame.", call. = FALSE)
  if (!"pitcher_pulled_after_pa" %in% names(decisions)) {
    stop("Decisions must contain `pitcher_pulled_after_pa`.", call. = FALSE)
  }
  validation_fraction <- .numeric_value(validation_fraction)[[1L]]
  if (!is.finite(validation_fraction) || validation_fraction <= 0 || validation_fraction >= 0.5) {
    stop("`validation_fraction` must be greater than zero and less than 0.5.", call. = FALSE)
  }
  calibration_bins <- .integer_value(calibration_bins)[[1L]]
  if (is.na(calibration_bins) || calibration_bins < 2L || calibration_bins > 20L) {
    stop("`calibration_bins` must be an integer from 2 through 20.", call. = FALSE)
  }

  rows <- tibble::as_tibble(decisions)
  if ("game_date" %in% names(rows)) {
    dates <- as.Date(rows$game_date)
    if (anyNA(dates)) stop("`game_date` must contain valid, non-missing dates.", call. = FALSE)
    if (is.null(train_through)) {
      unique_dates <- sort(unique(dates))
      if (length(unique_dates) < 3L) stop("At least three distinct game dates are required.", call. = FALSE)
      cutoff_index <- max(1L, floor(length(unique_dates) * (1 - validation_fraction)))
      cutoff_index <- min(cutoff_index, length(unique_dates) - 1L)
      cutoff_date <- unique_dates[[cutoff_index]]
    } else {
      cutoff_date <- as.Date(train_through)
      if (length(cutoff_date) != 1L || is.na(cutoff_date)) {
        stop("`train_through` must be a valid date.", call. = FALSE)
      }
    }
    training <- rows[dates <= cutoff_date, , drop = FALSE]
    validation <- rows[dates > cutoff_date, , drop = FALSE]
    split_method <- "game_date_holdout"
  } else {
    if (!is.null(train_through)) stop("`train_through` requires a `game_date` column.", call. = FALSE)
    cutoff_index <- floor(nrow(rows) * (1 - validation_fraction))
    training <- rows[seq_len(cutoff_index), , drop = FALSE]
    validation <- rows[seq.int(cutoff_index + 1L, nrow(rows)), , drop = FALSE]
    cutoff_date <- as.Date(NA)
    split_method <- "ordered_row_holdout"
  }

  training <- training[!is.na(training$pitcher_pulled_after_pa), , drop = FALSE]
  validation <- validation[!is.na(validation$pitcher_pulled_after_pa), , drop = FALSE]
  if (nrow(training) < 100L) stop("The training split must contain at least 100 complete outcomes.", call. = FALSE)
  if (nrow(validation) < 25L) stop("The validation split must contain at least 25 complete outcomes.", call. = FALSE)
  if (length(unique(as.numeric(validation$pitcher_pulled_after_pa))) < 2L) {
    stop("The validation split must contain both pulled and retained outcomes.", call. = FALSE)
  }

  model <- fit_manager_hook_model(training)
  predictions <- predict_manager_hook_probability(model, validation)
  actual <- as.numeric(predictions$pitcher_pulled_after_pa)
  probability <- pmax(pmin(predictions$hook_probability, 1 - 1e-8), 1e-8)

  positive <- actual == 1
  negative <- actual == 0
  ranks <- rank(probability, ties.method = "average")
  auc <- (sum(ranks[positive]) - sum(positive) * (sum(positive) + 1) / 2) /
    (sum(positive) * sum(negative))

  metrics <- tibble::tibble(
    training_rows = nrow(training),
    validation_rows = nrow(validation),
    observed_hook_rate = mean(actual),
    mean_predicted_hook_rate = mean(probability),
    brier_score = mean((probability - actual)^2),
    log_loss = -mean(actual * log(probability) + (1 - actual) * log(1 - probability)),
    roc_auc = auc,
    split_method = split_method,
    train_through = as.character(cutoff_date),
    model_method = model$method,
    validation_method = "sabrhood_hook_time_holdout_v1"
  )

  bin <- pmin(
    calibration_bins,
    ceiling(rank(probability, ties.method = "first") / (length(probability) / calibration_bins))
  )
  predictions$calibration_bin <- as.integer(bin)
  calibration <- predictions |>
    dplyr::group_by(.data$calibration_bin) |>
    dplyr::summarise(
      rows = dplyr::n(),
      mean_predicted = mean(.data$hook_probability),
      observed_rate = mean(as.numeric(.data$pitcher_pulled_after_pa)),
      minimum_predicted = min(.data$hook_probability),
      maximum_predicted = max(.data$hook_probability),
      .groups = "drop"
    )

  structure(
    list(
      model = model,
      metrics = metrics,
      calibration = calibration,
      validation_predictions = predictions
    ),
    class = "sabrhood_hook_validation"
  )
}

#' Rank available relievers for an upcoming matchup
#'
#' @param candidates Bullpen availability rows, optionally with a zero-to-one
#'   `performance_score` column.
#' @param upcoming_batter_side Upcoming batter side: `L`, `R`, or `S`.
#' @param leverage_index Current leverage index; higher leverage increases the
#'   weight placed on the optional performance score.
#' @param max_days_since_appearance Fallback recency screen when a supplied
#'   `roster_active` column is unavailable.
#'
#' @return Candidates ranked by a transparent zero-to-100 selection score.
#' @export
rank_reliever_options <- function(
    candidates,
    upcoming_batter_side,
    leverage_index = 1,
    max_days_since_appearance = 30L) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0L) {
    stop("`candidates` must be a non-empty data frame.", call. = FALSE)
  }
  required <- c("pitcher_id", "availability_score", "throws")
  missing <- setdiff(required, names(candidates))
  if (length(missing) > 0L) stop("Reliever candidates are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  batter_side <- toupper(as.character(upcoming_batter_side)[[1L]])
  if (!batter_side %in% c("L", "R", "S")) stop("`upcoming_batter_side` must be L, R, or S.", call. = FALSE)
  leverage_index <- .numeric_value(leverage_index)[[1L]]
  if (!is.finite(leverage_index) || leverage_index < 0) stop("`leverage_index` must be non-negative.", call. = FALSE)
  max_days_since_appearance <- .integer_value(max_days_since_appearance)[[1L]]
  if (is.na(max_days_since_appearance) || max_days_since_appearance < 0L) {
    stop("`max_days_since_appearance` must be a non-negative integer.", call. = FALSE)
  }

  if ("roster_active" %in% names(candidates)) {
    candidates <- candidates[.logical_value(candidates$roster_active) %in% TRUE, , drop = FALSE]
    eligibility_method <- "supplied_roster_active"
  } else if ("days_rest" %in% names(candidates)) {
    candidates <- candidates[
      !is.na(candidates$days_rest) & .numeric_value(candidates$days_rest) <= max_days_since_appearance,
      ,
      drop = FALSE
    ]
    eligibility_method <- "recent_appearance_proxy"
  } else {
    eligibility_method <- "no_roster_screen"
  }
  if (nrow(candidates) == 0L) stop("No eligible reliever candidates remain after roster screening.", call. = FALSE)

  throws <- toupper(as.character(candidates$throws))
  matchup_score <- if (batter_side == "S") {
    rep(0.50, nrow(candidates))
  } else if (batter_side == "L") {
    ifelse(throws == "L", 1.00, ifelse(throws == "R", 0.35, 0.50))
  } else {
    ifelse(throws == "R", 0.70, ifelse(throws == "L", 0.40, 0.50))
  }

  performance_score <- if ("performance_score" %in% names(candidates)) {
    .numeric_value(candidates$performance_score)
  } else {
    rep(0.50, nrow(candidates))
  }
  performance_score[!is.finite(performance_score)] <- 0.50
  performance_score <- pmax(pmin(performance_score, 1), 0)
  availability_score <- pmax(pmin(.numeric_value(candidates$availability_score), 1), 0)
  role <- if ("pitcher_role" %in% names(candidates)) tolower(as.character(candidates$pitcher_role)) else rep("unknown", nrow(candidates))
  role_score <- dplyr::case_when(
    role == "reliever" ~ 1.00,
    role == "hybrid" ~ 0.85,
    role %in% c("opener", "bulk") ~ 0.70,
    role == "starter" ~ 0.15,
    TRUE ~ 0.40
  )

  performance_weight <- pmin(0.20 + 0.05 * pmax(leverage_index - 1, 0), 0.40)
  matchup_weight <- 0.20
  role_weight <- 0.15
  availability_weight <- 1 - performance_weight - matchup_weight - role_weight

  output <- tibble::as_tibble(candidates)
  output$matchup_score <- matchup_score
  output$performance_score_used <- performance_score
  output$role_fit_score <- role_score
  output$selection_score <- round(100 * (
    availability_weight * availability_score +
      matchup_weight * matchup_score +
      performance_weight * performance_score +
      role_weight * role_score
  ), 1)
  output$upcoming_batter_side <- batter_side
  output$leverage_index <- leverage_index
  output$selection_method <- "sabrhood_reliever_selector_v1"
  output$eligibility_method <- eligibility_method
  output |>
    dplyr::arrange(dplyr::desc(.data$selection_score), dplyr::desc(.data$availability_score)) |>
    dplyr::mutate(selection_rank = dplyr::row_number())
}
