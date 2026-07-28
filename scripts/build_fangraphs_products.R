workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))
if (!requireNamespace("Lahman", quietly = TRUE)) stop("Lahman is required for the MVP era model.", call. = FALSE)

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
source_path <- file.path(workspace, ".private-data", "sources", paste0("fangraphs-season-", season_year, ".rds"))
if (!file.exists(source_path)) stop("Run scripts/fetch_fangraphs_season_source.R first.", call. = FALSE)

snapshot <- readRDS(source_path)
compact <- standardize_fangraphs_season(snapshot$hitters, snapshot$pitchers, season_year)
prior <- standardize_fangraphs_season(snapshot$prior_hitters, snapshot$prior_pitchers, season_year - 1L)
mvp_era <- build_mvp_era_profiles(Lahman::Batting, Lahman::AwardsPlayers)
awards <- build_award_race_boards(
  compact$hitters,
  compact$pitchers,
  prior$hitters,
  prior$pitchers,
  mvp_weights = mvp_era$weights
)
positional_war <- build_team_positional_war(compact$hitters, compact$pitchers)

build_player_market_pool <- function(hitters, pitchers, positional_war) {
  hitters <- hitters[
    suppressWarnings(as.numeric(hitters$pa)) >= 100 &
      as.character(hitters$position) != "P",
    ,
    drop = FALSE
  ]
  hitters$market_group <- sub("/.*$", "", as.character(hitters$position))
  hitters$market_group <- ifelse(
    hitters$market_group == "OF",
    "Outfielders",
    c(
      C = "Catchers", `1B` = "First basemen", `2B` = "Second basemen",
      `3B` = "Third basemen", SS = "Shortstops", DH = "Designated hitters"
    )[hitters$market_group]
  )
  hitters$market_group <- unname(hitters$market_group)
  hitter_pool <- data.frame(
    player_id = hitters$player_id,
    player_name = hitters$player_name,
    team = hitters$team,
    age = suppressWarnings(as.numeric(hitters$age)),
    market_group = hitters$market_group,
    role_family = "position player",
    hand = "",
    playing_time = suppressWarnings(as.numeric(hitters$pa)),
    playing_time_label = paste0(round(as.numeric(hitters$pa)), " PA"),
    war = suppressWarnings(as.numeric(hitters$war)),
    quality_rate = 600 * suppressWarnings(as.numeric(hitters$war)) /
      pmax(suppressWarnings(as.numeric(hitters$pa)), 1),
    quality_rate_label = "WAR / 600 PA",
    stringsAsFactors = FALSE
  )

  innings <- suppressWarnings(as.numeric(pitchers$innings_outs)) / 3
  starter <- suppressWarnings(as.numeric(pitchers$starts)) >= 5 &
    suppressWarnings(as.numeric(pitchers$starts)) /
      pmax(suppressWarnings(as.numeric(pitchers$games)), 1) >= 0.5
  pitcher_pool <- data.frame(
    player_id = pitchers$player_id,
    player_name = pitchers$player_name,
    team = pitchers$team,
    age = suppressWarnings(as.numeric(pitchers$age)),
    market_group = paste(
      ifelse(starter, "Starting", "Relief"),
      ifelse(as.character(pitchers$throws) == "L", "LHP", "RHP")
    ),
    role_family = ifelse(starter, "starter", "reliever"),
    hand = as.character(pitchers$throws),
    playing_time = innings,
    playing_time_label = paste0(round(innings, 1), " IP"),
    war = suppressWarnings(as.numeric(pitchers$war)),
    quality_rate = suppressWarnings(as.numeric(pitchers$war)) /
      pmax(innings, 1) * ifelse(starter, 180, 65),
    quality_rate_label = ifelse(starter, "WAR / 180 IP", "WAR / 65 IP"),
    stringsAsFactors = FALSE
  )
  pitcher_pool <- pitcher_pool[
    (pitcher_pool$role_family == "starter" & pitcher_pool$playing_time >= 30) |
      (pitcher_pool$role_family == "reliever" &
        pitcher_pool$playing_time >= 15),
    ,
    drop = FALSE
  ]
  pool <- rbind(hitter_pool, pitcher_pool)
  pool <- pool[!is.na(pool$market_group) & nzchar(pool$market_group), , drop = FALSE]

  need_count <- function(group) {
    position <- if (grepl("^Starting", group)) {
      "SP"
    } else if (grepl("^Relief", group)) {
      "RP"
    } else {
      c(
        Catchers = "C", `First basemen` = "1B",
        `Second basemen` = "2B", `Third basemen` = "3B",
        Shortstops = "SS", Outfielders = "OF",
        `Designated hitters` = "DH"
      )[[group]]
    }
    if (is.null(position)) return(NA_integer_)
    sum(
      positional_war$position == position &
        positional_war$status == "need",
      na.rm = TRUE
    )
  }

  group_rows <- lapply(split(pool, pool$market_group), function(rows) {
    rate_median <- stats::median(rows$quality_rate, na.rm = TRUE)
    above_average <- rows[
      is.finite(rows$quality_rate) &
        rows$quality_rate >= rate_median &
        rows$war > 0,
      ,
      drop = FALSE
    ]
    leaders <- rows[order(-rows$quality_rate, -rows$war), , drop = FALSE]
    data.frame(
      market_group = rows$market_group[[1L]],
      role_family = rows$role_family[[1L]],
      player_supply = nrow(rows),
      above_average_supply = nrow(above_average),
      median_age = stats::median(rows$age, na.rm = TRUE),
      median_quality_rate = rate_median,
      quality_rate_label = rows$quality_rate_label[[1L]],
      teams_with_need = need_count(rows$market_group[[1L]]),
      top_players = paste(utils::head(leaders$player_name, 3L), collapse = " · "),
      stringsAsFactors = FALSE
    )
  })
  groups <- do.call(rbind, group_rows)

  # A player count is not meaningful until it is compared with the number of
  # jobs clubs actually need to fill. The seven position-player groups divide
  # 13 roster spots per club. Pitchers divide the other 13: five rotation
  # places and eight bullpen places. Handedness shares split those pitching
  # allocations without double-counting the same roster jobs.
  position_slots <- c(
    Catchers = 2.00,
    `First basemen` = 1.25,
    `Second basemen` = 1.50,
    `Third basemen` = 1.50,
    Shortstops = 1.50,
    Outfielders = 4.25,
    `Designated hitters` = 1.00
  )
  starter_group <- grepl("^Starting", groups$market_group)
  relief_group <- grepl("^Relief", groups$market_group)
  starter_supply <- sum(groups$player_supply[starter_group], na.rm = TRUE)
  relief_supply <- sum(groups$player_supply[relief_group], na.rm = TRUE)
  groups$roster_slots_per_team <- vapply(
    seq_len(nrow(groups)),
    function(index) {
      group <- groups$market_group[[index]]
      if (grepl("^Starting", group)) {
        return(
          5 * groups$player_supply[[index]] / pmax(starter_supply, 1)
        )
      }
      if (grepl("^Relief", group)) {
        return(
          8 * groups$player_supply[[index]] / pmax(relief_supply, 1)
        )
      }
      unname(position_slots[[group]])
    },
    numeric(1)
  )
  groups$league_roster_slots <- 30 * groups$roster_slots_per_team
  groups$qualified_slot_coverage <- groups$player_supply /
    pmax(groups$league_roster_slots, 1)
  groups$upper_half_slot_coverage <- groups$above_average_supply /
    pmax(groups$league_roster_slots, 1)
  groups$need_weighted_roster_slots <- groups$teams_with_need *
    groups$roster_slots_per_team

  qualified_gap <- pmax(0, 1 - groups$qualified_slot_coverage)
  upper_half_gap <- pmax(0, 1 - groups$upper_half_slot_coverage)
  need_pressure <- groups$need_weighted_roster_slots /
    pmax(groups$above_average_supply, 1)
  scarcity_raw <-
    0.25 * qualified_gap +
    0.40 * upper_half_gap +
    0.35 * pmin(need_pressure, 2)
  groups$scarcity_index <- round(
    100 * rank(scarcity_raw, ties.method = "average", na.last = "keep") /
      sum(is.finite(scarcity_raw)),
    1
  )
  quality_raw <- groups$median_quality_rate
  groups$quality_index <- round(
    100 * rank(quality_raw, ties.method = "average", na.last = "keep") /
      sum(is.finite(quality_raw)),
    1
  )
  groups$market_read <- ifelse(
    groups$scarcity_index >= 75,
    "scarce",
    ifelse(groups$scarcity_index >= 45, "balanced", "deep")
  )
  groups$price_status <-
    "Contract, control, and transaction source required"
  groups$source_through <- as.character(Sys.Date())
  groups$market_method <-
    "roster_slot_adjusted_talent_supply_and_team_need_v2"
  pool$market_method <- "current_mlb_talent_supply_pool_v1"
  list(groups = groups, players = pool)
}

player_market <- build_player_market_pool(
  compact$hitters,
  compact$pitchers,
  positional_war
)
compact$hitters$source_acquired_at_utc <- snapshot$acquired_at_utc
compact$pitchers$source_acquired_at_utc <- snapshot$acquired_at_utc
positional_war$source_acquired_at_utc <- snapshot$acquired_at_utc
awards$source_acquired_at_utc <- snapshot$acquired_at_utc
awards$source_note <- "FanGraphs season leaderboard via BaseballR; award score is a transparent performance index, not a ballot forecast."

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path(workspace, "data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(compact$hitters, file.path(output_dir, "fangraphs-season-hitters.csv"), row.names = FALSE, na = "")
utils::write.csv(compact$pitchers, file.path(output_dir, "fangraphs-season-pitchers.csv"), row.names = FALSE, na = "")
utils::write.csv(awards, file.path(output_dir, "award-race-board.csv"), row.names = FALSE, na = "")
utils::write.csv(positional_war, file.path(output_dir, "team-positional-war.csv"), row.names = FALSE, na = "")
utils::write.csv(player_market$groups, file.path(output_dir, "player-market-groups.csv"), row.names = FALSE, na = "")
utils::write.csv(player_market$players, file.path(output_dir, "player-market-players.csv"), row.names = FALSE, na = "")
utils::write.csv(mvp_era$winners, file.path(output_dir, "mvp-era-winner-percentiles.csv"), row.names = FALSE, na = "")
utils::write.csv(mvp_era$profiles, file.path(output_dir, "mvp-era-stat-profiles.csv"), row.names = FALSE, na = "")
utils::write.csv(mvp_era$weights, file.path(output_dir, "mvp-modern-model-weights.csv"), row.names = FALSE, na = "")
cat("Built", nrow(compact$hitters), "FanGraphs hitter rows,", nrow(compact$pitchers), "pitcher rows,",
  nrow(awards), "award-watch rows,", nrow(positional_war), "team-position rows, and",
  nrow(mvp_era$profiles), "MVP decade-stat profiles, plus",
  nrow(player_market$groups), "player-market role groups.\n")
