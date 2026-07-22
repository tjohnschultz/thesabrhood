workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages({
  library(sabrhoodR)
  library(baseballr)
})

output_dir <- file.path(workspace, "data", "derived")
games_path <- file.path(output_dir, "daily-game-inputs.csv")
if (!file.exists(games_path)) stop("Morning daily-game-inputs.csv is required.", call. = FALSE)

games <- utils::read.csv(games_path, stringsAsFactors = FALSE, check.names = FALSE)
prior_lineups_path <- file.path(output_dir, "daily-batting-orders.csv")
prior_probables_path <- file.path(output_dir, "daily-probable-starters.csv")
prior_lineups <- if (file.exists(prior_lineups_path)) {
  utils::read.csv(prior_lineups_path, stringsAsFactors = FALSE, check.names = FALSE)
} else data.frame()
prior_probables <- if (file.exists(prior_probables_path)) {
  utils::read.csv(prior_probables_path, stringsAsFactors = FALSE, check.names = FALSE)
} else data.frame()
target_date <- as.Date(Sys.getenv(
  "SABRHOOD_DATE",
  unset = format(Sys.time(), tz = "America/New_York", format = "%Y-%m-%d")
))
if (is.na(target_date)) stop("SABRHOOD_DATE must use YYYY-MM-DD.", call. = FALSE)
if (!nrow(games) || !all(suppressWarnings(as.Date(games$game_date)) == target_date)) {
  stop("The morning game-input file does not match the requested lineup date.", call. = FALSE)
}

bind_safely <- function(items) {
  items <- items[vapply(items, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(items)) return(data.frame())
  dplyr::bind_rows(items)
}

game_pks <- unique(as.character(games$game_id))
cat("Checking probables and batting orders for", length(game_pks), "games on", as.character(target_date), "...\n")
probables <- bind_safely(lapply(game_pks, function(game_pk) {
  result <- tryCatch(baseballr::mlb_probables(game_pk), error = function(error) {
    message("Probables unavailable for ", game_pk, ": ", conditionMessage(error))
    data.frame()
  })
  if (is.data.frame(result) && nrow(result) && !"game_pk" %in% names(result)) result$game_pk <- game_pk
  result
}))
orders <- bind_safely(lapply(game_pks, function(game_pk) {
  result <- tryCatch(baseballr::mlb_batting_orders(game_pk, type = "starting"), error = function(error) {
    message("Batting order unavailable for ", game_pk, ": ", conditionMessage(error))
    data.frame()
  })
  if (is.data.frame(result) && nrow(result) && !"game_pk" %in% names(result)) result$game_pk <- game_pk
  result
}))

schedule_contract <- data.frame(
  game_pk = games$game_id,
  official_date = games$game_date,
  game_date = games$game_time_utc,
  teams_away_team_id = games$away_team_id,
  teams_away_team_name = games$away_team,
  teams_home_team_id = games$home_team_id,
  teams_home_team_name = games$home_team,
  venue_name = games$venue_name,
  status_detailed_state = games$game_status,
  stringsAsFactors = FALSE
)
refreshed <- assemble_baseballr_game_inputs(schedule_contract, probables, orders)
fresh_lineups <- refreshed$lineups
fresh_probables <- refreshed$probables

normalize_lineups <- function(value) {
  columns <- c("game_id", "team_side", "team_id", "team_name", "batting_order", "player_id", "player_name", "position")
  for (column in setdiff(columns, names(value))) value[[column]] <- rep(NA, nrow(value))
  value <- value[, columns, drop = FALSE]
  character_columns <- setdiff(columns, "batting_order")
  value[character_columns] <- lapply(value[character_columns], as.character)
  value$batting_order <- suppressWarnings(as.integer(value$batting_order))
  value
}
normalize_probables <- function(value) {
  columns <- c("game_id", "starter_id", "starter_name", "team_id", "team_name")
  for (column in setdiff(columns, names(value))) value[[column]] <- rep(NA, nrow(value))
  value <- value[, columns, drop = FALSE]
  value[] <- lapply(value, as.character)
  value
}
prior_lineups <- normalize_lineups(prior_lineups)
fresh_lineups <- normalize_lineups(fresh_lineups)
prior_probables <- normalize_probables(prior_probables)
fresh_probables <- normalize_probables(fresh_probables)

if (nrow(prior_lineups)) {
  prior_lineups$game_id <- as.character(prior_lineups$game_id)
  prior_lineups$team_side <- as.character(prior_lineups$team_side)
}
if (nrow(fresh_lineups)) {
  fresh_lineups$game_id <- as.character(fresh_lineups$game_id)
  fresh_lineups$team_side <- as.character(fresh_lineups$team_side)
}
fresh_complete_keys <- character()
if (nrow(fresh_lineups)) {
  fresh_counts <- fresh_lineups |>
    dplyr::count(.data$game_id, .data$team_side, name = "rows") |>
    dplyr::filter(.data$rows >= 9L)
  fresh_complete_keys <- paste(fresh_counts$game_id, fresh_counts$team_side, sep = "\034")
}
if (nrow(prior_lineups) && length(fresh_complete_keys)) {
  prior_key <- paste(prior_lineups$game_id, prior_lineups$team_side, sep = "\034")
  prior_lineups <- prior_lineups[!prior_key %in% fresh_complete_keys, , drop = FALSE]
}
final_lineups <- dplyr::bind_rows(prior_lineups, fresh_lineups) |>
  dplyr::distinct(.data$game_id, .data$team_side, .data$batting_order, .keep_all = TRUE) |>
  dplyr::arrange(.data$game_id, .data$team_side, .data$batting_order)

final_probables <- dplyr::bind_rows(fresh_probables, prior_probables) |>
  dplyr::filter(!is.na(.data$game_id), .data$game_id != "") |>
  dplyr::distinct(.data$game_id, .data$team_id, .keep_all = TRUE) |>
  dplyr::arrange(.data$game_id, .data$team_id)

same_table <- function(left, right) {
  if (!identical(names(left), names(right)) || nrow(left) != nrow(right)) return(FALSE)
  isTRUE(all.equal(as.data.frame(left), as.data.frame(right), check.attributes = FALSE))
}
lineup_content_changed <- !same_table(prior_lineups, final_lineups)
probable_content_changed <- !same_table(prior_probables, final_probables)

fresh_games <- refreshed$games
probable_side <- function(side) {
  team_column <- paste0(side, "_team_id")
  index <- match(
    paste(fresh_games$game_id, fresh_games[[team_column]], sep = "\034"),
    paste(final_probables$game_id, final_probables$team_id, sep = "\034")
  )
  list(id = final_probables$starter_id[index], name = final_probables$starter_name[index])
}
away_starter <- probable_side("away")
home_starter <- probable_side("home")
fresh_games$away_starter_id <- away_starter$id
fresh_games$away_starter_name <- away_starter$name
fresh_games$home_starter_id <- home_starter$id
fresh_games$home_starter_name <- home_starter$name
lineup_counts <- final_lineups |>
  dplyr::count(.data$game_id, .data$team_side, name = "lineup_count")
lineup_count <- function(side) {
  index <- match(
    paste(fresh_games$game_id, side, sep = "\034"),
    paste(lineup_counts$game_id, lineup_counts$team_side, sep = "\034")
  )
  count <- lineup_counts$lineup_count[index]
  count[is.na(count)] <- 0L
  count
}
fresh_games$away_lineup_count <- lineup_count("away")
fresh_games$home_lineup_count <- lineup_count("home")
fresh_games$away_lineup_status <- ifelse(fresh_games$away_lineup_count >= 9L, "confirmed", "missing")
fresh_games$home_lineup_status <- ifelse(fresh_games$home_lineup_count >= 9L, "confirmed", "missing")
game_index <- match(as.character(games$game_id), as.character(fresh_games$game_id))

refresh_columns <- c(
  "away_starter_id", "away_starter_name", "home_starter_id", "home_starter_name",
  "away_lineup_count", "home_lineup_count", "away_lineup_status", "home_lineup_status"
)
for (column in refresh_columns) games[[column]] <- fresh_games[[column]][game_index]
games$input_method <- "baseballr_morning_slate_plus_intraday_orders_v1"
prior_refresh_time <- if ("lineup_refreshed_at_utc" %in% names(games)) games$lineup_refreshed_at_utc else rep("", nrow(games))
if (lineup_content_changed || probable_content_changed || all(is.na(prior_refresh_time) | prior_refresh_time == "")) {
  games$lineup_refreshed_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
} else {
  games$lineup_refreshed_at_utc <- prior_refresh_time
}
games <- validate_projection_game_inputs(games, allow_projected_lineups = TRUE)

utils::write.csv(games, games_path, row.names = FALSE, na = "")
utils::write.csv(final_lineups, prior_lineups_path, row.names = FALSE, na = "")
utils::write.csv(final_probables, prior_probables_path, row.names = FALSE, na = "")

complete_teams <- sum(c(games$away_lineup_status, games$home_lineup_status) == "confirmed", na.rm = TRUE)
cat(
  "Lineup refresh retained", nrow(final_lineups), "batting-order rows and",
  complete_teams, "complete team lineups out of", 2L * nrow(games), ".\n"
)
