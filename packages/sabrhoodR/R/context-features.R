#' Add within-plate-appearance pitch sequence features
#'
#' @param pitches A canonical pitch view.
#'
#' @return Pitch rows with previous-pitch, change, and location-separation fields.
#' @export
add_pitch_sequence_features <- function(pitches) {
  required <- c(
    "game_pk", "at_bat_index", "pitch_in_pa", "pitch_type", "start_speed",
    "plate_x", "plate_z", "call_description"
  )
  missing <- setdiff(required, names(pitches))
  if (length(missing) > 0L) stop("Pitches are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  order_index <- order(
    pitches$game_date, pitches$game_pk, pitches$at_bat_index,
    pitches$pitch_in_pa, na.last = TRUE
  )
  output <- pitches[order_index, , drop = FALSE]
  group_key <- paste(output$game_pk, output$at_bat_index, sep = "\034")
  first_in_group <- !duplicated(group_key)
  lag_within_group <- function(x, default = NA) {
    lagged <- c(default, x[-length(x)])
    lagged[first_in_group] <- default
    lagged
  }

  output$previous_pitch_type <- lag_within_group(output$pitch_type, NA_character_)
  output$previous_pitch_name <- lag_within_group(output$pitch_name, NA_character_)
  output$previous_call <- lag_within_group(output$call_description, NA_character_)
  output$previous_speed <- lag_within_group(output$start_speed, NA_real_)
  output$previous_plate_x <- lag_within_group(output$plate_x, NA_real_)
  output$previous_plate_z <- lag_within_group(output$plate_z, NA_real_)
  comparable <- output$pitch_in_pa > 1L & !is.na(output$pitch_type) & !is.na(output$previous_pitch_type)
  output$pitch_type_changed <- ifelse(comparable, output$pitch_type != output$previous_pitch_type, NA)
  output$velocity_change <- output$start_speed - output$previous_speed
  output$location_separation <- sqrt(
    (output$plate_x - output$previous_plate_x)^2 +
      (output$plate_z - output$previous_plate_z)^2
  )
  output$repeated_pitch <- ifelse(comparable, output$pitch_type == output$previous_pitch_type, NA)
  output$sequence_key <- paste0(
    ifelse(is.na(output$previous_pitch_type), "START", output$previous_pitch_type),
    "->",
    ifelse(is.na(output$pitch_type), "UNKNOWN", output$pitch_type)
  )
  output
}

.base_state_label <- function(state) {
  labels <- c(
    "empty", "1--", "-2-", "12-", "--3", "1-3", "-23", "123"
  )
  output <- rep(NA_character_, length(state))
  valid <- !is.na(state) & state >= 0L & state <= 7L
  output[valid] <- labels[as.integer(state[valid]) + 1L]
  output
}

#' Estimate an empirical 24-state run expectancy table
#'
#' @param plate_appearances Canonical plate appearances.
#' @param minimum_pa Minimum observations required to expose a state estimate.
#'
#' @return One row per outs/base state with expected remaining runs and reliability.
#' @export
build_run_expectancy_table <- function(plate_appearances, minimum_pa = 25L) {
  required <- c(
    "game_pk", "inning", "half_inning", "at_bat_index", "outs_before",
    "base_state_before", "batting_score_before", "batting_score_after"
  )
  missing <- setdiff(required, names(plate_appearances))
  if (length(missing) > 0L) stop("Plate appearances are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  minimum_pa <- .integer_value(minimum_pa)[[1L]]
  if (is.na(minimum_pa) || minimum_pa < 1L) stop("`minimum_pa` must be positive.", call. = FALSE)

  full_states <- expand.grid(
    outs_before = 0:2,
    base_state_before = 0:7,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  half_key <- paste(
    plate_appearances$game_pk,
    plate_appearances$inning,
    plate_appearances$half_inning,
    sep = "\034"
  )
  group_indices <- split(seq_len(nrow(plate_appearances)), half_key)
  inning_end_score <- rep(NA_real_, nrow(plate_appearances))
  for (indices in group_indices) {
    scores <- plate_appearances$batting_score_after[indices]
    available <- scores[is.finite(scores)]
    if (length(available) > 0L) inning_end_score[indices] <- max(available)
  }
  runs_to_end <- inning_end_score - plate_appearances$batting_score_before
  valid <- plate_appearances$outs_before %in% 0:2 &
    plate_appearances$base_state_before %in% 0:7 &
    is.finite(runs_to_end) & runs_to_end >= 0
  state_key <- paste(plate_appearances$outs_before, plate_appearances$base_state_before, sep = "-")
  output <- full_states
  output$plate_appearances <- NA_integer_
  output$expected_runs <- NA_real_
  output$run_sd <- NA_real_
  for (row in seq_len(nrow(output))) {
    key <- paste(output$outs_before[[row]], output$base_state_before[[row]], sep = "-")
    values <- runs_to_end[valid & state_key == key]
    if (length(values) > 0L) {
      output$plate_appearances[[row]] <- length(values)
      output$expected_runs[[row]] <- mean(values)
      output$run_sd[[row]] <- stats::sd(values)
    }
  }
  output$base_state_label <- .base_state_label(output$base_state_before)
  output$sample_reliability <- output$plate_appearances / (output$plate_appearances + 100)
  output$estimate_available <- !is.na(output$plate_appearances) & output$plate_appearances >= minimum_pa
  output$expected_runs[!output$estimate_available] <- NA_real_
  output$run_expectancy_method <- "sabrhood_empirical_re24_v1"
  dplyr::arrange(output, .data$outs_before, .data$base_state_before)
}

#' Add run expectancy and linear run value to plate appearances
#'
#' @param plate_appearances Canonical plate appearances.
#' @param run_expectancy Optional output from [build_run_expectancy_table()].
#'
#' @return Plate appearances with before/after expectancy and run value.
#' @export
add_run_expectancy <- function(plate_appearances, run_expectancy = NULL) {
  if (is.null(run_expectancy)) run_expectancy <- build_run_expectancy_table(plate_appearances)
  required <- c("outs_before", "base_state_before", "expected_runs")
  missing <- setdiff(required, names(run_expectancy))
  if (length(missing) > 0L) stop("Run expectancy is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  lookup_key <- paste(run_expectancy$outs_before, run_expectancy$base_state_before, sep = "-")
  before_key <- paste(plate_appearances$outs_before, plate_appearances$base_state_before, sep = "-")
  after_key <- paste(plate_appearances$outs_after, plate_appearances$base_state_after, sep = "-")
  output <- plate_appearances
  output$run_expectancy_before <- run_expectancy$expected_runs[match(before_key, lookup_key)]
  output$run_expectancy_after <- run_expectancy$expected_runs[match(after_key, lookup_key)]
  output$run_expectancy_after[output$outs_after >= 3L] <- 0
  output$run_value <- output$runs_scored_on_play +
    output$run_expectancy_after - output$run_expectancy_before
  output$run_value_method <- "sabrhood_empirical_re24_v1"
  output
}

#' Add score, pressure, and leverage-proxy context
#'
#' @param plate_appearances Canonical plate appearances.
#'
#' @return Plate appearances with deterministic game-state context fields.
#' @export
add_game_context <- function(plate_appearances) {
  required <- c(
    "inning", "outs_before", "base_state_before", "batting_score_diff_before"
  )
  missing <- setdiff(required, names(plate_appearances))
  if (length(missing) > 0L) stop("Plate appearances are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  output <- plate_appearances
  score_diff <- .numeric_value(output$batting_score_diff_before)
  inning_factor <- 0.60 + 0.40 * pmin(pmax(.numeric_value(output$inning), 1), 9) / 9
  score_factor <- 0.45 + 1.55 * exp(-abs(score_diff) / 2.25)
  runners <- (output$base_state_before %% 2L) +
    ((output$base_state_before %/% 2L) %% 2L) +
    ((output$base_state_before %/% 4L) %% 2L)
  base_out_factor <- 1 + 0.12 * runners + 0.08 * pmax(2 - .numeric_value(output$outs_before), 0)
  raw_proxy <- inning_factor * score_factor * base_out_factor
  baseline <- stats::median(raw_proxy[is.finite(raw_proxy)], na.rm = TRUE)
  output$leverage_proxy <- raw_proxy / baseline
  output$pressure_index <- round(100 * output$leverage_proxy / (output$leverage_proxy + 1), 1)
  output$is_close_game <- is.finite(score_diff) & abs(score_diff) <= 2
  output$is_high_leverage_proxy <- is.finite(output$leverage_proxy) & output$leverage_proxy >= 1.5
  output$game_state_label <- dplyr::case_when(
    !is.finite(score_diff) ~ NA_character_,
    score_diff >= 4 ~ "comfortable_lead",
    score_diff >= 1 ~ "ahead",
    score_diff == 0 ~ "tied",
    score_diff >= -3 ~ "behind",
    TRUE ~ "large_deficit"
  )
  output$leverage_method <- "sabrhood_context_proxy_v1"
  output
}
