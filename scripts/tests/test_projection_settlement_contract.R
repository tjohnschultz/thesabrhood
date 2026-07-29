source(file.path("scripts", "projection_settlement_contract.R"), local = FALSE)

fail <- function(message) stop(message, call. = FALSE)

builder <- readLines(
  file.path("scripts", "build_daily_player_simulations.R"),
  warn = FALSE
)
metric_lines <- grep(
  'add_metric\\("[^"]+"',
  builder,
  value = TRUE
)
generated_metrics <- unique(sub(
  '.*add_metric\\("([^"]+)".*',
  "\\1",
  metric_lines
))
settled_metrics <- player_projection_outcome_rules()$metric_id

if (!identical(sort(generated_metrics), sort(settled_metrics))) {
  missing_rules <- setdiff(generated_metrics, settled_metrics)
  unused_rules <- setdiff(settled_metrics, generated_metrics)
  fail(paste(
    "Generated and settled player projection metrics differ.",
    if (length(missing_rules)) {
      paste("Missing settlement rules:", paste(missing_rules, collapse = ", "))
    } else {
      ""
    },
    if (length(unused_rules)) {
      paste("Unused settlement rules:", paste(unused_rules, collapse = ", "))
    } else {
      ""
    }
  ))
}

examples <- data.frame(
  metric_id = c("batter_hr_2plus", "pitcher_k_10plus"),
  actual_H = c(0, 0),
  actual_HR = c(2, 0),
  actual_XBH = c(0, 0),
  actual_TB = c(0, 0),
  actual_K = c(0, 10)
)
actual <- settle_player_projection_outcomes(examples)
if (!identical(actual, c(1L, 1L))) {
  fail("New player projection thresholds were not settled correctly.")
}

unknown <- examples[1, , drop = FALSE]
unknown$metric_id <- "future_metric"
unknown_error <- tryCatch(
  {
    settle_player_projection_outcomes(unknown)
    ""
  },
  error = function(error) conditionMessage(error)
)
if (!grepl("No settlement rule exists", unknown_error, fixed = TRUE)) {
  fail("Unknown player projection metrics must fail with an explicit contract error.")
}

cat(
  "Projection settlement contract passed for",
  length(settled_metrics),
  "player metrics.\n"
)
