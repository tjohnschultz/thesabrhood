#' Fit a score-scale calibration model
#'
#' Fits a Poisson calibration layer that maps an upstream run expectation to
#' observed team runs. The upstream expectation must have been produced from
#' information available before each game. A home indicator is retained so the
#' fitted layer can correct a misspecified home-field adjustment.
#'
#' @param training_rows Data frame with actual runs, raw expected runs, and a
#'   home indicator.
#' @param actual_col Name of the observed-runs column.
#' @param expected_col Name of the raw expected-runs column.
#' @param home_col Name of the home-indicator column.
#' @param min_rows Minimum complete rows required.
#' @return A `sabrhood_run_calibrator` object.
#' @export
fit_run_expectation_calibrator <- function(
    training_rows,
    actual_col = "actual_runs",
    expected_col = "raw_expected_runs",
    home_col = "is_home",
    min_rows = 100L) {
  stopifnot(is.data.frame(training_rows))
  required <- c(actual_col, expected_col, home_col)
  missing <- setdiff(required, names(training_rows))
  if (length(missing)) stop("Missing calibration columns: ", paste(missing, collapse = ", "), call. = FALSE)
  actual <- suppressWarnings(as.numeric(training_rows[[actual_col]]))
  expected <- suppressWarnings(as.numeric(training_rows[[expected_col]]))
  home <- suppressWarnings(as.integer(as.logical(training_rows[[home_col]])))
  keep <- is.finite(actual) & actual >= 0 & is.finite(expected) & expected > 0 & home %in% c(0L, 1L)
  if (sum(keep) < min_rows) stop("Run calibration requires at least ", min_rows, " complete pregame rows.", call. = FALSE)
  model_data <- data.frame(actual_runs = actual[keep], log_raw_expected = log(expected[keep]), is_home = home[keep])
  model <- stats::glm(actual_runs ~ log_raw_expected + is_home, data = model_data, family = stats::poisson(link = "log"))
  structure(
    list(
      model = model,
      rows = nrow(model_data),
      actual_mean = mean(model_data$actual_runs),
      raw_mean = mean(exp(model_data$log_raw_expected)),
      fitted_mean = mean(stats::fitted(model)),
      method = "poisson_log_scale_home_calibrator_v1"
    ),
    class = "sabrhood_run_calibrator"
  )
}

#' Apply a score-scale calibration model
#'
#' @param object A model returned by [fit_run_expectation_calibrator()].
#' @param raw_expected_runs Positive upstream run expectations.
#' @param is_home Logical or zero-one home indicators.
#' @param lower,upper Output bounds applied after prediction.
#' @return Calibrated expected runs.
#' @export
predict_run_expectation <- function(object, raw_expected_runs, is_home, lower = 1.5, upper = 8) {
  if (!inherits(object, "sabrhood_run_calibrator")) stop("`object` must be a sabrhood run calibrator.", call. = FALSE)
  raw <- suppressWarnings(as.numeric(raw_expected_runs))
  home <- suppressWarnings(as.integer(as.logical(is_home)))
  if (length(home) == 1L && length(raw) > 1L) home <- rep(home, length(raw))
  if (length(raw) != length(home) || any(!is.finite(raw) | raw <= 0) || any(!home %in% c(0L, 1L))) {
    stop("Prediction inputs must be positive run expectations and matching home indicators.", call. = FALSE)
  }
  prediction <- stats::predict(object$model, newdata = data.frame(log_raw_expected = log(raw), is_home = home), type = "response")
  pmin(pmax(as.numeric(prediction), lower), upper)
}

#' Score binary projection probabilities
#'
#' @param probability Numeric probabilities.
#' @param actual Binary outcomes.
#' @param epsilon Bound used for logarithmic loss.
#' @return One-row tibble with Brier score, log loss, accuracy, and calibration bias.
#' @export
score_projection_probabilities <- function(probability, actual, epsilon = 1e-6) {
  probability <- suppressWarnings(as.numeric(probability))
  actual <- suppressWarnings(as.integer(actual))
  keep <- is.finite(probability) & probability >= 0 & probability <= 1 & actual %in% c(0L, 1L)
  probability <- probability[keep]
  actual <- actual[keep]
  if (!length(actual)) stop("No complete probability outcomes were supplied.", call. = FALSE)
  bounded <- pmin(pmax(probability, epsilon), 1 - epsilon)
  tibble::tibble(
    observations = length(actual),
    brier_score = mean((probability - actual)^2),
    log_loss = -mean(actual * log(bounded) + (1 - actual) * log(1 - bounded)),
    classification_accuracy = mean((probability >= 0.5) == actual),
    mean_probability = mean(probability),
    observed_rate = mean(actual),
    calibration_bias = mean(actual - probability)
  )
}

#' Build fixed-width probability calibration bins
#'
#' @param probability Numeric probabilities.
#' @param actual Binary outcomes.
#' @param bins Number of equal-width bins.
#' @return Calibration rows for non-empty bins.
#' @export
build_probability_calibration <- function(probability, actual, bins = 10L) {
  probability <- suppressWarnings(as.numeric(probability))
  actual <- suppressWarnings(as.integer(actual))
  keep <- is.finite(probability) & probability >= 0 & probability <= 1 & actual %in% c(0L, 1L)
  probability <- probability[keep]
  actual <- actual[keep]
  if (!length(actual)) stop("No complete probability outcomes were supplied.", call. = FALSE)
  if (length(bins) != 1L || !is.finite(bins) || bins < 2L) stop("`bins` must be at least two.", call. = FALSE)
  breaks <- seq(0, 1, length.out = as.integer(bins) + 1L)
  bucket <- cut(probability, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
  groups <- split(seq_along(probability), bucket)
  output <- lapply(names(groups), function(name) {
    index <- groups[[name]]
    bin_id <- as.integer(name)
    tibble::tibble(
      probability_bin = bin_id,
      bin_lower = breaks[[bin_id]],
      bin_upper = breaks[[bin_id + 1L]],
      observations = length(index),
      mean_probability = mean(probability[index]),
      observed_rate = mean(actual[index]),
      calibration_gap = mean(actual[index]) - mean(probability[index])
    )
  })
  dplyr::bind_rows(output)
}

#' Fit a logistic probability calibration layer
#'
#' @param probability Raw model probabilities from archived pregame forecasts.
#' @param actual Binary settled outcomes.
#' @param min_rows Minimum complete settled predictions required.
#' @return A `sabrhood_probability_calibrator` object.
#' @export
fit_probability_calibrator <- function(probability, actual, min_rows = 300L) {
  probability <- suppressWarnings(as.numeric(probability))
  actual <- suppressWarnings(as.integer(actual))
  keep <- is.finite(probability) & probability > 0 & probability < 1 & actual %in% c(0L, 1L)
  if (sum(keep) < min_rows) stop("Probability calibration requires at least ", min_rows, " settled pregame predictions.", call. = FALSE)
  data <- data.frame(actual = actual[keep], raw_logit = stats::qlogis(probability[keep]))
  model <- stats::glm(actual ~ raw_logit, data = data, family = stats::binomial(link = "logit"))
  structure(list(model = model, rows = nrow(data), method = "platt_logistic_probability_calibrator_v1"), class = "sabrhood_probability_calibrator")
}

#' Apply a logistic probability calibration layer
#'
#' @param object A fitted probability calibrator.
#' @param probability Raw model probabilities.
#' @param epsilon Bound applied before and after prediction.
#' @return Calibrated probabilities.
#' @export
predict_calibrated_probability <- function(object, probability, epsilon = 1e-6) {
  if (!inherits(object, "sabrhood_probability_calibrator")) stop("`object` must be a SABRhood probability calibrator.", call. = FALSE)
  probability <- suppressWarnings(as.numeric(probability))
  if (any(!is.finite(probability) | probability < 0 | probability > 1)) stop("Probabilities must be finite values from zero to one.", call. = FALSE)
  bounded <- pmin(pmax(probability, epsilon), 1 - epsilon)
  output <- stats::predict(object$model, newdata = data.frame(raw_logit = stats::qlogis(bounded)), type = "response")
  pmin(pmax(as.numeric(output), epsilon), 1 - epsilon)
}
