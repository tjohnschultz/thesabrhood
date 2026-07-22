#' Standardize an active roster returned by BaseballR
#'
#' @param rosters One or more results from `baseballr::mlb_rosters()`.
#' @param as_of_date Date the roster was acquired.
#'
#' @return One row per rostered player with stable identifiers and pitcher flags.
#' @export
standardize_baseballr_rosters <- function(rosters, as_of_date = Sys.Date()) {
  if (!is.data.frame(rosters) || nrow(rosters) == 0L) {
    stop("`rosters` must be a non-empty data frame.", call. = FALSE)
  }
  player_id <- as.character(.column_or_default(rosters, c("person_id", "person.id", "player_id", "id")))
  if (all(is.na(player_id) | player_id == "")) {
    stop("Roster data must contain an MLBAM player identifier.", call. = FALSE)
  }
  position_abbreviation <- toupper(as.character(.column_or_default(
    rosters, c("position_abbreviation", "position.abbreviation", "position", "position_code")
  )))
  position_type <- as.character(.column_or_default(
    rosters, c("position_type", "position.type", "position_name", "position.name")
  ))
  status <- as.character(.column_or_default(
    rosters, c("status_description", "status.description", "status", "roster_type"), "Active"
  ))
  roster_type <- as.character(.column_or_default(rosters, c("roster_type", "rosterType"), "active"))
  status_key <- tolower(trimws(paste(status, roster_type)))
  is_pitcher <- position_abbreviation %in% c("P", "SP", "RP", "CP", "RHP", "LHP") |
    grepl("pitch", position_type, ignore.case = TRUE)

  tibble::tibble(
    player_id = player_id,
    player_name = as.character(.column_or_default(
      rosters, c("person_full_name", "person.fullName", "player_full_name", "player_name", "full_name")
    )),
    team_id = as.character(.column_or_default(rosters, c("team_id", "team.id"))),
    team_name = as.character(.column_or_default(rosters, c("team_name", "team.name"))),
    position_abbreviation = position_abbreviation,
    position_type = position_type,
    roster_status = status,
    roster_type = roster_type,
    is_active = grepl("active", status_key) & !grepl("inactive", status_key),
    is_pitcher = is_pitcher,
    roster_pitcher_role = dplyr::case_when(
      position_abbreviation == "SP" ~ "starter",
      position_abbreviation %in% c("RP", "CP") ~ "reliever",
      is_pitcher ~ "pitcher_unspecified",
      TRUE ~ "position_player"
    ),
    roster_verified_at = as.Date(as_of_date),
    roster_method = "baseballr_mlb_rosters_active_v1"
  ) |>
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "") |>
    dplyr::distinct(.data$team_id, .data$player_id, .keep_all = TRUE)
}

#' Restrict a workload board to active-roster bullpen pitchers
#'
#' @param availability Output from [build_bullpen_availability()].
#' @param active_rosters Output from [standardize_baseballr_rosters()].
#' @param probable_starter_ids MLBAM identifiers to exclude from the bullpen.
#'
#' @return Availability rows verified against the active roster.
#' @export
filter_active_roster_bullpen <- function(
    availability,
    active_rosters,
    probable_starter_ids = character()) {
  if (!is.data.frame(availability) || nrow(availability) == 0L) {
    stop("`availability` must be a non-empty data frame.", call. = FALSE)
  }
  if (!"pitcher_id" %in% names(availability)) {
    stop("Availability data must contain `pitcher_id`.", call. = FALSE)
  }
  required <- c("player_id", "is_active", "is_pitcher")
  missing <- setdiff(required, names(active_rosters))
  if (length(missing)) {
    stop("Active rosters are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  roster <- active_rosters |>
    dplyr::filter(.data$is_active, .data$is_pitcher) |>
    dplyr::mutate(player_id = as.character(.data$player_id)) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE)
  output <- availability |>
    dplyr::mutate(pitcher_id = as.character(.data$pitcher_id)) |>
    dplyr::inner_join(roster, by = c("pitcher_id" = "player_id"), suffix = c("", "_roster")) |>
    dplyr::filter(!.data$pitcher_id %in% as.character(probable_starter_ids))
  output$active_roster_verified <- TRUE
  output$bullpen_pool_method <- "workload_plus_baseballr_active_roster_v1"
  dplyr::arrange(output, .data$team, dplyr::desc(.data$availability_score))
}

#' Assemble schedule, probable starter, and batting-order inputs
#'
#' @param schedule A BaseballR `mlb_schedule()` result, usually filtered to one date.
#' @param probables Bound `mlb_probables()` results.
#' @param batting_orders Bound `mlb_batting_orders()` results.
#'
#' @return A list containing normalized `games`, `probables`, and `lineups` tables.
#' @export
assemble_baseballr_game_inputs <- function(schedule, probables = NULL, batting_orders = NULL) {
  if (!is.data.frame(schedule) || nrow(schedule) == 0L) {
    stop("`schedule` must be a non-empty data frame.", call. = FALSE)
  }
  game_pk <- as.character(.column_or_default(schedule, c("game_pk", "gamePk")))
  if (all(is.na(game_pk) | game_pk == "")) stop("Schedule data must contain `game_pk`.", call. = FALSE)
  games <- tibble::tibble(
    game_id = game_pk,
    game_date = as.Date(.column_or_default(schedule, c("official_date", "officialDate", "date"))),
    game_time_utc = as.character(.column_or_default(schedule, c("game_date", "gameDate"))),
    away_team_id = as.character(.column_or_default(schedule, c(
      "teams_away_team_id", "teams.away.team.id", "away_team_id", "away_id"
    ))),
    away_team = as.character(.column_or_default(schedule, c(
      "teams_away_team_name", "teams.away.team.name", "away_team_name", "away_name"
    ))),
    home_team_id = as.character(.column_or_default(schedule, c(
      "teams_home_team_id", "teams.home.team.id", "home_team_id", "home_id"
    ))),
    home_team = as.character(.column_or_default(schedule, c(
      "teams_home_team_name", "teams.home.team.name", "home_team_name", "home_name"
    ))),
    venue_name = as.character(.column_or_default(schedule, c(
      "venue_name", "venue.name", "teams_home_team_venue_name"
    ))),
    game_status = as.character(.column_or_default(schedule, c(
      "status_detailed_state", "status.detailedState", "status_abstract_game_state"
    )))
  ) |>
    dplyr::distinct(.data$game_id, .keep_all = TRUE)

  empty_probables <- tibble::tibble(
    game_id = character(), starter_id = character(), starter_name = character(),
    team_id = character(), team_name = character()
  )
  normalized_probables <- empty_probables
  if (is.data.frame(probables) && nrow(probables)) {
    normalized_probables <- tibble::tibble(
      game_id = as.character(.column_or_default(probables, c("game_pk", "gamePk"))),
      starter_id = as.character(.column_or_default(probables, c("id", "person_id", "player_id"))),
      starter_name = as.character(.column_or_default(probables, c("fullName", "full_name", "player_name"))),
      team_id = as.character(.column_or_default(probables, c("team_id", "team.id"))),
      team_name = as.character(.column_or_default(probables, c("team", "team_name", "team.name")))
    ) |>
      dplyr::filter(!is.na(.data$game_id), .data$game_id != "") |>
      dplyr::distinct(.data$game_id, .data$team_id, .keep_all = TRUE)
  }

  empty_lineups <- tibble::tibble(
    game_id = character(), team_side = character(), team_id = character(), team_name = character(),
    batting_order = integer(), player_id = character(), player_name = character(), position = character()
  )
  normalized_lineups <- empty_lineups
  if (is.data.frame(batting_orders) && nrow(batting_orders)) {
    normalized_lineups <- tibble::tibble(
      game_id = as.character(.column_or_default(batting_orders, c("game_pk", "gamePk"))),
      team_side = tolower(as.character(.column_or_default(batting_orders, c("team", "side")))),
      team_id = as.character(.column_or_default(batting_orders, c("teamID", "team_id", "team.id"))),
      team_name = as.character(.column_or_default(batting_orders, c("teamName", "team_name", "team.name"))),
      batting_order = .integer_value(.column_or_default(batting_orders, c("batting_order", "battingOrder"))),
      player_id = as.character(.column_or_default(batting_orders, c("id", "person_id", "player_id"))),
      player_name = as.character(.column_or_default(batting_orders, c("fullName", "full_name", "player_name"))),
      position = as.character(.column_or_default(batting_orders, c("abbreviation", "position_abbreviation")))
    ) |>
      dplyr::filter(.data$team_side %in% c("away", "home"), !is.na(.data$player_id)) |>
      dplyr::arrange(.data$game_id, .data$team_side, .data$batting_order)
  }

  probable_side <- function(side) {
    team_column <- paste0(side, "_team_id")
    match_index <- match(
      paste(games$game_id, games[[team_column]], sep = "\034"),
      paste(normalized_probables$game_id, normalized_probables$team_id, sep = "\034")
    )
    list(
      id = normalized_probables$starter_id[match_index],
      name = normalized_probables$starter_name[match_index]
    )
  }
  away_starter <- probable_side("away")
  home_starter <- probable_side("home")
  games$away_starter_id <- away_starter$id
  games$away_starter_name <- away_starter$name
  games$home_starter_id <- home_starter$id
  games$home_starter_name <- home_starter$name

  lineup_counts <- normalized_lineups |>
    dplyr::count(.data$game_id, .data$team_side, name = "lineup_count")
  lineup_count <- function(side) {
    index <- match(
      paste(games$game_id, side, sep = "\034"),
      paste(lineup_counts$game_id, lineup_counts$team_side, sep = "\034")
    )
    count <- lineup_counts$lineup_count[index]
    count[is.na(count)] <- 0L
    count
  }
  games$away_lineup_count <- lineup_count("away")
  games$home_lineup_count <- lineup_count("home")
  games$away_lineup_status <- ifelse(games$away_lineup_count >= 9L, "confirmed", "missing")
  games$home_lineup_status <- ifelse(games$home_lineup_count >= 9L, "confirmed", "missing")
  games$input_method <- "baseballr_schedule_probables_batting_orders_v1"

  list(games = games, probables = normalized_probables, lineups = normalized_lineups)
}

#' Build an Open-Meteo hourly forecast URL for a ballpark
#'
#' @param latitude,longitude Ballpark coordinates.
#' @param start_date,end_date Inclusive forecast dates.
#' @param timezone IANA timezone or `auto`.
#'
#' @return A complete Open-Meteo forecast URL.
#' @export
build_open_meteo_url <- function(
    latitude,
    longitude,
    start_date,
    end_date = start_date,
    timezone = "auto") {
  latitude <- .numeric_value(latitude)[[1L]]
  longitude <- .numeric_value(longitude)[[1L]]
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (!is.finite(latitude) || latitude < -90 || latitude > 90) stop("Invalid latitude.", call. = FALSE)
  if (!is.finite(longitude) || longitude < -180 || longitude > 180) stop("Invalid longitude.", call. = FALSE)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) stop("Invalid weather date range.", call. = FALSE)
  paste0(
    "https://api.open-meteo.com/v1/forecast?latitude=", latitude,
    "&longitude=", longitude,
    "&hourly=temperature_2m,apparent_temperature,precipitation_probability,precipitation,weather_code,wind_speed_10m,wind_direction_10m,wind_gusts_10m",
    "&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch",
    "&timezone=", utils::URLencode(as.character(timezone), reserved = TRUE),
    "&start_date=", format(start_date, "%Y-%m-%d"),
    "&end_date=", format(end_date, "%Y-%m-%d")
  )
}

