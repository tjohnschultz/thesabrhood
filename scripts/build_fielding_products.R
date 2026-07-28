suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(sabrhoodR)
})

site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
season <- as.integer(Sys.getenv(
  "SABRHOOD_SEASON",
  unset = format(Sys.Date(), "%Y")
))
pbp_path <- Sys.getenv(
  "SABRHOOD_PBP_PATH",
  unset = file.path(
    site_root,
    ".private-data",
    "pbp",
    as.character(season),
    "current.rds"
  )
)
output_dir <- file.path(site_root, "data", "derived")
private_model_dir <- file.path(site_root, ".private-data", "models")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(private_model_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(pbp_path)) {
  stop("Private PBP cache is missing: ", pbp_path, call. = FALSE)
}

read_product <- function(name, required = TRUE) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) {
    if (required) stop("Missing derived product: ", path, call. = FALSE)
    return(data.frame())
  }
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
}

pbp <- readRDS(pbp_path)
mlb_team_names <- c(
  `108` = "Los Angeles Angels",
  `109` = "Arizona Diamondbacks",
  `110` = "Baltimore Orioles",
  `111` = "Boston Red Sox",
  `112` = "Chicago Cubs",
  `113` = "Cincinnati Reds",
  `114` = "Cleveland Guardians",
  `115` = "Colorado Rockies",
  `116` = "Detroit Tigers",
  `117` = "Houston Astros",
  `118` = "Kansas City Royals",
  `119` = "Los Angeles Dodgers",
  `120` = "Washington Nationals",
  `121` = "New York Mets",
  `133` = "Athletics",
  `134` = "Pittsburgh Pirates",
  `135` = "San Diego Padres",
  `136` = "Seattle Mariners",
  `137` = "San Francisco Giants",
  `138` = "St. Louis Cardinals",
  `139` = "Tampa Bay Rays",
  `140` = "Texas Rangers",
  `141` = "Toronto Blue Jays",
  `142` = "Minnesota Twins",
  `143` = "Philadelphia Phillies",
  `144` = "Atlanta Braves",
  `145` = "Chicago White Sox",
  `146` = "Miami Marlins",
  `147` = "New York Yankees",
  `158` = "Milwaukee Brewers"
)
regular_club_game <- as.character(pbp$home_team) %in% unname(mlb_team_names) &
  as.character(pbp$away_team) %in% unname(mlb_team_names)
pbp <- pbp[regular_club_game, , drop = FALSE]
source_through <- max(as.Date(pbp$game_date), na.rm = TRUE)
generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

rosters <- read_product("active-rosters.csv")
roster_reference <- data.frame(
  player_id = as.character(rosters$player_id),
  player_name = as.character(rosters$player_name),
  team = as.character(rosters$team_name),
  position = as.character(rosters$position_abbreviation),
  stringsAsFactors = FALSE
)
hitters <- read_product("hitter-performance-summary.csv", required = FALSE)
hitter_reference <- if (nrow(hitters)) {
  data.frame(
    player_id = as.character(hitters$player_id),
    player_name = as.character(hitters$player_name),
    team = as.character(hitters$team),
    position = NA_character_,
    stringsAsFactors = FALSE
  )
} else {
  roster_reference[0, , drop = FALSE]
}
player_reference <- bind_rows(roster_reference, hitter_reference)
player_reference <- player_reference[
  !is.na(player_reference$player_id) &
    nzchar(player_reference$player_id) &
    !duplicated(player_reference$player_id),
  ,
  drop = FALSE
]

cat("Building private batted-ball fielding opportunity view...\n")
fielding_opportunities <- build_fielding_opportunity_view(
  pbp,
  player_reference = player_reference
)
if (!nrow(fielding_opportunities)) {
  stop("No fielding opportunities were reconstructed.", call. = FALSE)
}
fielding_ratings <- build_fielding_ratings(fielding_opportunities)

cat("Building runner-advancement prevention model...\n")
advancement <- build_runner_advancement_fielding(
  pbp,
  fielding_opportunities = fielding_opportunities,
  player_reference = player_reference
)
if (!nrow(advancement$opportunities)) {
  stop("No runner-advancement opportunities were reconstructed.", call. = FALSE)
}

saveRDS(
  fielding_opportunities,
  file.path(private_model_dir, "fielding-opportunities.rds"),
  compress = "gzip"
)
saveRDS(
  advancement$opportunities,
  file.path(private_model_dir, "runner-advancement-fielding-opportunities.rds"),
  compress = "gzip"
)

read_savant_fielding <- function() {
  query <- paste0(
    "https://baseballsavant.mlb.com/leaderboard/fielding-run-value",
    "?gameType=Regular&groupBy=player&minInnings=0&minResults=1",
    "&position=0&seasonEnd=", season,
    "&seasonStart=", season,
    "&type=fielder"
  )
  html <- if (requireNamespace("curl", quietly = TRUE)) {
    response <- curl::curl_fetch_memory(
      query,
      handle = curl::new_handle(
        useragent = "The-SABRhood-data-pipeline/1.0"
      )
    )
    rawToChar(response$content)
  } else {
    connection <- url(
      query,
      open = "rb",
      headers = c("User-Agent" = "The-SABRhood-data-pipeline/1.0")
    )
    on.exit(close(connection), add = TRUE)
    paste(
      readLines(connection, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
  }
  marker <- "const data = "
  marker_start <- regexpr(marker, html, fixed = TRUE)[[1L]]
  if (marker_start < 0L) {
    stop("Savant fielding response did not contain the data array.", call. = FALSE)
  }
  json_start <- marker_start + nchar(marker)
  closing_arrays <- gregexpr("];", html, fixed = TRUE)[[1L]]
  closing_arrays <- closing_arrays[closing_arrays >= json_start]
  if (!length(closing_arrays)) {
    stop("Savant fielding response did not close its data array.", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(
    substring(html, json_start, closing_arrays[[1L]]),
    simplifyDataFrame = TRUE
  )
  standardize_fielding_run_value(parsed, season)
}

official_path <- file.path(output_dir, "official-fielding-run-value.csv")
official_fielding <- tryCatch(
  read_savant_fielding(),
  error = function(error) {
    if (!file.exists(official_path)) stop(error)
    warning(
      "Official fielding refresh failed; preserving the last good board: ",
      conditionMessage(error)
    )
    utils::read.csv(
      official_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
)
if (!nrow(official_fielding)) {
  stop("Official Fielding Run Value leaderboard was empty.", call. = FALSE)
}

official_fielding$team_short <- official_fielding$team
official_team_name <- unname(
  mlb_team_names[as.character(official_fielding$team_id)]
)
official_fielding <- official_fielding[
  !is.na(official_team_name) & nzchar(official_team_name),
  ,
  drop = FALSE
]
official_fielding$team <- official_team_name[
  !is.na(official_team_name) & nzchar(official_team_name)
]
standings <- read_product("mlb-standings-current.csv", required = FALSE)
league_lookup <- if (nrow(standings)) {
  unique(standings[, c("team_id", "league_id"), drop = FALSE])
} else {
  data.frame(
    team_id = names(mlb_team_names),
    league_id = ifelse(
      names(mlb_team_names) %in%
        c("108", "110", "111", "114", "116", "117", "118", "133",
          "136", "139", "140", "141", "142", "145", "147"),
      103,
      104
    ),
    stringsAsFactors = FALSE
  )
}
league_match <- match(
  as.character(official_fielding$team_id),
  as.character(league_lookup$team_id)
)
official_fielding$league <- ifelse(
  as.character(league_lookup$league_id[league_match]) == "103",
  "AL",
  ifelse(
    as.character(league_lookup$league_id[league_match]) == "104",
    "NL",
    NA_character_
  )
)
official_fielding <- official_fielding[
  official_fielding$league %in% c("AL", "NL"),
  ,
  drop = FALSE
]

gold_glove <- build_gold_glove_watch(
  official_fielding,
  advancement_ratings = advancement$player,
  minimum_innings = 100
)
play_of_day <- build_fielding_play_of_day(
  fielding_opportunities,
  advancement$opportunities
)

official_team <- official_fielding |>
  group_by(.data$team) |>
  summarise(
    fielding_runs = sum(.data$fielding_runs, na.rm = TRUE),
    range_runs = sum(.data$range_runs, na.rm = TRUE),
    arm_runs = sum(.data$arm_runs, na.rm = TRUE),
    double_play_runs = sum(.data$double_play_runs, na.rm = TRUE),
    catching_runs = sum(.data$catching_runs, na.rm = TRUE),
    framing_runs = sum(.data$framing_runs, na.rm = TRUE),
    throwing_runs = sum(.data$throwing_runs, na.rm = TRUE),
    blocking_runs = sum(.data$blocking_runs, na.rm = TRUE),
    innings = sum(.data$innings, na.rm = TRUE),
    players = dplyr::n_distinct(.data$player_id),
    .groups = "drop"
  ) |>
  arrange(desc(.data$fielding_runs))
official_team$official_fielding_rank <- seq_len(nrow(official_team))
official_team$source <- "Baseball Savant Fielding Run Value Leaderboard"
official_team$model_version <- "savant_fielding_run_value_team_v1"

model_card <- data.frame(
  model_version = "sabrhood_fielding_engine_v1",
  source_start_date = as.character(
    min(fielding_opportunities$game_date, na.rm = TRUE)
  ),
  source_through = as.character(source_through),
  games = dplyr::n_distinct(fielding_opportunities$game_pk),
  batted_ball_opportunities = nrow(fielding_opportunities),
  credited_fielders = dplyr::n_distinct(
    fielding_opportunities$fielder_id[
      !is.na(fielding_opportunities$fielder_id)
    ]
  ),
  fielder_id_coverage = mean(!is.na(fielding_opportunities$fielder_id)),
  advancement_opportunities = nrow(advancement$opportunities),
  advancement_fielder_coverage = mean(
    !is.na(advancement$opportunities$fielder_id)
  ),
  official_fielding_rows = nrow(official_fielding),
  gold_glove_watch_rows = nrow(gold_glove),
  play_of_day_rows = nrow(play_of_day),
  publication_status = paste(
    "development; expected-out model does not include starting position,",
    "jump, route, wall, or opportunity time; official FRV remains separate"
  ),
  generated_at_utc = generated_at_utc,
  stringsAsFactors = FALSE
)

write_product <- function(data, name) {
  if (!is.data.frame(data)) {
    stop("Product is not a data frame: ", name, call. = FALSE)
  }
  if (!"source_through" %in% names(data)) {
    data$source_through <- as.character(source_through)
  }
  if (!"generated_at_utc" %in% names(data)) {
    data$generated_at_utc <- generated_at_utc
  }
  utils::write.csv(
    data,
    file.path(output_dir, name),
    row.names = FALSE,
    na = ""
  )
  invisible(data)
}

write_product(official_fielding, "official-fielding-run-value.csv")
write_product(official_team, "official-team-fielding-run-value.csv")
write_product(fielding_ratings$player, "fielding-player-ratings.csv")
write_product(fielding_ratings$team, "fielding-team-ratings.csv")
write_product(advancement$player, "runner-advancement-fielding-ratings.csv")
write_product(advancement$team, "runner-advancement-team-ratings.csv")
write_product(play_of_day, "fielding-play-of-day.csv")
write_product(gold_glove, "gold-glove-watch.csv")
write_product(model_card, "fielding-model-card.csv")

cat(
  "Fielding Engine built",
  format(nrow(fielding_opportunities), big.mark = ","),
  "batted-ball opportunities,",
  format(nrow(advancement$opportunities), big.mark = ","),
  "runner-advancement opportunities,",
  nrow(gold_glove),
  "Gold Glove Watch rows and",
  nrow(play_of_day),
  "daily play selections.\n"
)
