workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages({
  library(dplyr)
  library(sabrhoodR)
})

pbp_path <- file.path(workspace, ".private-data", "pbp", "2026", "current.rds")
venue_path <- file.path(
  workspace,
  ".private-data",
  "reference",
  "mlb-game-venues-2026.rds"
)
if (!file.exists(pbp_path)) stop("Missing current PBP cache: ", pbp_path, call. = FALSE)
if (!file.exists(venue_path)) {
  stop(
    "Missing historical venue cache. Run scripts/fetch_historical_game_venues.R first.",
    call. = FALSE
  )
}

pbp <- readRDS(pbp_path)
venues <- readRDS(venue_path)
opportunities <- build_baserunning_opportunity_view(pbp, venues)
if (!nrow(opportunities)) stop("No baserunning opportunities were reconstructed.", call. = FALSE)

private_model_dir <- file.path(workspace, ".private-data", "models")
dir.create(private_model_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  opportunities,
  file.path(private_model_dir, "phase4-baserunning-opportunities.rds")
)

is_steal <- grepl("^steal_", opportunities$opportunity_type)
opportunities$rate_numerator <- ifelse(
  is_steal,
  as.numeric(opportunities$attempted %in% TRUE),
  as.numeric(opportunities$success %in% TRUE)
)
opportunities$rate_denominator <- 1

league <- opportunities |>
  group_by(.data$opportunity_type) |>
  summarise(
    opportunities = dplyr::n(),
    attempts = sum(.data$attempted %in% TRUE),
    successes = sum(.data$success %in% TRUE, na.rm = TRUE),
    modeled_rate = mean(.data$rate_numerator, na.rm = TRUE),
    success_rate_when_attempted = ifelse(
      sum(.data$attempted %in% TRUE) > 0,
      sum(.data$success %in% TRUE, na.rm = TRUE) /
        sum(.data$attempted %in% TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  mutate(
    rate_definition = ifelse(
      grepl("^steal_", .data$opportunity_type),
      "attempts per eligible runner window",
      "successful advances per opportunity"
    )
  )

league_rate <- stats::setNames(league$modeled_rate, league$opportunity_type)
league_success <- stats::setNames(
  league$success_rate_when_attempted,
  league$opportunity_type
)
runner_prior_n <- 35
steal_success_prior_n <- 18
runner_profiles <- opportunities |>
  group_by(.data$runner_id, .data$runner_name, .data$opportunity_type) |>
  summarise(
    opportunities = dplyr::n(),
    attempts = sum(.data$attempted %in% TRUE),
    successes = sum(.data$success %in% TRUE, na.rm = TRUE),
    observed_rate = mean(.data$rate_numerator, na.rm = TRUE),
    observed_success_rate = ifelse(
      .data$attempts[[1L]] > 0,
      .data$successes[[1L]] / .data$attempts[[1L]],
      NA_real_
    ),
    .groups = "drop"
  ) |>
  mutate(
    league_rate = unname(league_rate[.data$opportunity_type]),
    league_success_rate = unname(league_success[.data$opportunity_type]),
    shrunk_rate = (
      .data$observed_rate * .data$opportunities +
        .data$league_rate * runner_prior_n
    ) / (.data$opportunities + runner_prior_n),
    shrunk_success_rate = ifelse(
      .data$attempts > 0 & is.finite(.data$league_success_rate),
      (
        .data$successes +
          .data$league_success_rate * steal_success_prior_n
      ) / (.data$attempts + steal_success_prior_n),
      .data$league_success_rate
    ),
    reliability = .data$opportunities / (.data$opportunities + runner_prior_n),
    profile_status = ifelse(
      .data$opportunities >= 25,
      "player signal",
      "league-shrunk"
    )
  )

steal_windows <- opportunities |>
  filter(grepl("^steal_", .data$opportunity_type))
pitcher_lookup <- unique(data.frame(
  pitcher_id = as.character(pbp$matchup.pitcher.id),
  pitcher_name = as.character(pbp$matchup.pitcher.fullName),
  stringsAsFactors = FALSE
))
pitcher_lookup <- pitcher_lookup[
  !is.na(pitcher_lookup$pitcher_id) &
    nzchar(pitcher_lookup$pitcher_id) &
    !duplicated(pitcher_lookup$pitcher_id),
  ,
  drop = FALSE
]
pitcher_prior_n <- 60
pitcher_hold <- steal_windows |>
  group_by(.data$pitcher_id, .data$fielding_team, .data$opportunity_type) |>
  summarise(
    runner_windows = dplyr::n(),
    attempts = sum(.data$attempted %in% TRUE),
    stolen_bases = sum(.data$success %in% TRUE, na.rm = TRUE),
    caught_stealing = sum(.data$attempted %in% TRUE & .data$success %in% FALSE),
    .groups = "drop"
  ) |>
  mutate(
    league_attempt_rate = unname(league_rate[.data$opportunity_type]),
    observed_attempt_rate = .data$attempts / .data$runner_windows,
    shrunk_attempt_rate = (
      .data$attempts + .data$league_attempt_rate * pitcher_prior_n
    ) / (.data$runner_windows + pitcher_prior_n),
    attempt_suppression_index = ifelse(
      .data$league_attempt_rate > 0,
      100 * .data$shrunk_attempt_rate / .data$league_attempt_rate,
      100
    ),
    league_success_rate = unname(league_success[.data$opportunity_type]),
    success_rate_allowed = ifelse(
      .data$attempts > 0,
      .data$stolen_bases / .data$attempts,
      NA_real_
    ),
    shrunk_success_rate_allowed = (
      .data$stolen_bases +
        .data$league_success_rate * steal_success_prior_n
    ) / (.data$attempts + steal_success_prior_n),
    hold_reliability = .data$runner_windows /
      (.data$runner_windows + pitcher_prior_n)
  ) |>
  left_join(pitcher_lookup, by = "pitcher_id") |>
  relocate("pitcher_name", .after = "pitcher_id")

advancement <- opportunities |>
  filter(
    !grepl("^steal_", .data$opportunity_type),
    !is.na(.data$venue_name),
    nzchar(.data$venue_name)
  )
park_prior_n <- 180
park_profiles <- advancement |>
  group_by(.data$venue_id, .data$venue_name, .data$opportunity_type) |>
  summarise(
    opportunities = dplyr::n(),
    successes = sum(.data$success %in% TRUE, na.rm = TRUE),
    observed_rate = mean(.data$success %in% TRUE),
    .groups = "drop"
  ) |>
  mutate(
    league_rate = unname(league_rate[.data$opportunity_type]),
    shrunk_rate = (
      .data$successes + .data$league_rate * park_prior_n
    ) / (.data$opportunities + park_prior_n),
    empirical_multiplier = pmin(
      pmax(.data$shrunk_rate / .data$league_rate, 0.85),
      1.15
    ),
    reliability = .data$opportunities / (.data$opportunities + park_prior_n),
    method = "descriptive_empirical_bayes_v1",
    publication_status = "Phase 4 shadow input; composition-adjusted model pending"
  )

model_card <- data.frame(
  source_start_date = as.character(min(opportunities$game_date, na.rm = TRUE)),
  source_end_date = as.character(max(opportunities$game_date, na.rm = TRUE)),
  games = dplyr::n_distinct(opportunities$game_pk),
  opportunities = nrow(opportunities),
  advancement_opportunities = sum(!is_steal),
  steal_windows = sum(is_steal),
  runners = dplyr::n_distinct(opportunities$runner_id),
  pitchers_with_hold_windows = dplyr::n_distinct(steal_windows$pitcher_id),
  venues = dplyr::n_distinct(advancement$venue_name),
  runner_prior_opportunities = runner_prior_n,
  pitcher_hold_prior_windows = pitcher_prior_n,
  park_prior_opportunities = park_prior_n,
  model_version = "phase4_empirical_baserunning_foundation_v1",
  publication_status = paste(
    "shadow only; runner movement is reconstructed from PBP transitions;",
    "catcher identity and composition-adjusted park effects remain open"
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

output_dir <- file.path(workspace, "data", "derived")
write_product <- function(data, name) {
  utils::write.csv(
    data,
    file.path(output_dir, name),
    row.names = FALSE,
    na = ""
  )
}
write_product(league, "baserunning-league-rates.csv")
write_product(runner_profiles, "baserunning-runner-profiles.csv")
write_product(pitcher_hold, "baserunning-pitcher-hold-profiles.csv")
write_product(park_profiles, "baserunning-park-factors.csv")
write_product(model_card, "baserunning-model-card.csv")

cat(
  "Phase 4 reconstructed", nrow(opportunities), "opportunities across",
  model_card$games, "games and", model_card$venues, "venues.\n"
)
