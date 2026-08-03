site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
derived_dir <- Sys.getenv(
  "SABRHOOD_DERIVED_DIR",
  unset = file.path(site_root, "data", "derived")
)

read_product <- function(name) {
  path <- file.path(derived_dir, name)
  if (!file.exists(path)) stop("Missing required product: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

write_product <- function(data, name) {
  utils::write.csv(data, file.path(derived_dir, name), row.names = FALSE, na = "")
}

empty_series_context <- function() {
  data.frame(
    current_game_id = character(), current_game_date = character(),
    team = character(), opponent = character(), series_start = character(),
    completed_series_games = integer(), series_game_number = integer(),
    last_game_date = character(), last_game_pk = character(),
    played_yesterday = logical(), method = character(), report_date = character(),
    source_through = character(), generated_at_utc = character(),
    stringsAsFactors = FALSE
  )
}

empty_series_players <- function() {
  data.frame(
    current_game_id = character(), team = character(), opponent = character(),
    role = character(), player_id = character(), player_name = character(),
    games = integer(), plate_appearances = numeric(), hits = numeric(),
    doubles = numeric(), triples = numeric(), home_runs = numeric(),
    runs_batted_in = numeric(), walks = numeric(), strikeouts = numeric(),
    innings_outs = numeric(), report_date = character(), source_through = character(),
    generated_at_utc = character(), method = character(),
    stringsAsFactors = FALSE
  )
}

empty_recent_games <- function() {
  data.frame(
    current_game_id = character(), team = character(), opponent = character(),
    game_pk = character(), game_date = character(), recency_label = character(),
    role = character(), player_id = character(), player_name = character(),
    stat_line = character(), performance_score = numeric(), report_date = character(),
    source_through = character(), generated_at_utc = character(),
    stringsAsFactors = FALSE
  )
}

games <- read_product("daily-game-inputs.csv")
hitters <- read_product("current-season-hitter-game-lines.csv")
pitchers <- read_product("current-season-pitcher-game-lines.csv")
if (!nrow(games)) {
  write_product(empty_series_context(), "daily-series-context.csv")
  write_product(empty_series_players(), "daily-series-player-lines.csv")
  write_product(empty_recent_games(), "daily-recent-game-lines.csv")
  quit(save = "no", status = 0L)
}

site_report_date <- max(as.Date(games$game_date), na.rm = TRUE)
generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
hitters$game_date <- as.Date(hitters$game_date)
pitchers$game_date <- as.Date(pitchers$game_date)
team_games <- unique(hitters[c("game_pk", "game_date", "team", "opponent")])
team_games <- team_games[
  !is.na(team_games$team) & nzchar(team_games$team) &
    !is.na(team_games$opponent) & nzchar(team_games$opponent),
  ,
  drop = FALSE
]
team_games <- team_games[order(team_games$team, team_games$game_date, team_games$game_pk), , drop = FALSE]

sum_column <- function(data, column) {
  if (!column %in% names(data)) return(NA_real_)
  values <- suppressWarnings(as.numeric(data[[column]]))
  if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
}

context_rows <- list()
series_player_rows <- list()
recent_rows <- list()

for (game_index in seq_len(nrow(games))) {
  game <- games[game_index, , drop = FALSE]
  report_date <- as.Date(game$game_date[[1L]])
  pair <- c(game$away_team[[1L]], game$home_team[[1L]])
  for (team_index in seq_along(pair)) {
    team <- pair[[team_index]]
    opponent <- pair[[3L - team_index]]
    prior <- team_games[
      team_games$team == team & team_games$game_date < report_date,
      ,
      drop = FALSE
    ]
    prior <- prior[order(prior$game_date, prior$game_pk, decreasing = TRUE), , drop = FALSE]
    series_ids <- character()
    if (nrow(prior)) {
      for (row_index in seq_len(nrow(prior))) {
        if (!identical(as.character(prior$opponent[[row_index]]), as.character(opponent))) break
        series_ids <- c(series_ids, as.character(prior$game_pk[[row_index]]))
      }
    }
    series_dates <- prior$game_date[as.character(prior$game_pk) %in% series_ids]
    previous <- if (nrow(prior)) prior[1L, , drop = FALSE] else data.frame()
    context_rows[[length(context_rows) + 1L]] <- data.frame(
      current_game_id = as.character(game$game_id[[1L]]),
      current_game_date = as.character(report_date),
      team = team,
      opponent = opponent,
      series_start = as.character(if (length(series_dates)) min(series_dates) else report_date),
      completed_series_games = length(series_ids),
      series_game_number = length(series_ids) + 1L,
      last_game_date = if (nrow(previous)) as.character(previous$game_date[[1L]]) else NA_character_,
      last_game_pk = if (nrow(previous)) as.character(previous$game_pk[[1L]]) else NA_character_,
      played_yesterday = if (nrow(previous)) previous$game_date[[1L]] == report_date - 1L else FALSE,
      method = "consecutive_same_opponent_v1",
      stringsAsFactors = FALSE
    )

    if (length(series_ids)) {
      hitter_series <- hitters[
        hitters$team == team & as.character(hitters$game_pk) %in% series_ids,
        ,
        drop = FALSE
      ]
      if (nrow(hitter_series)) {
        groups <- split(hitter_series, as.character(hitter_series$player_id))
        for (player in groups) {
          series_player_rows[[length(series_player_rows) + 1L]] <- data.frame(
            current_game_id = as.character(game$game_id[[1L]]),
            team = team,
            opponent = opponent,
            role = "Hitter",
            player_id = as.character(player$player_id[[1L]]),
            player_name = as.character(player$player_name[[1L]]),
            games = length(unique(player$game_pk)),
            plate_appearances = sum_column(player, "plate_appearances"),
            hits = sum_column(player, "hits"),
            doubles = sum_column(player, "doubles"),
            triples = sum_column(player, "triples"),
            home_runs = sum_column(player, "home_runs"),
            runs_batted_in = sum_column(player, "runs_batted_in"),
            walks = sum_column(player, "walks"),
            strikeouts = sum_column(player, "strikeouts"),
            innings_outs = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
      pitcher_series <- pitchers[
        pitchers$team == team & as.character(pitchers$game_pk) %in% series_ids,
        ,
        drop = FALSE
      ]
      if (nrow(pitcher_series)) {
        groups <- split(pitcher_series, as.character(pitcher_series$player_id))
        for (player in groups) {
          series_player_rows[[length(series_player_rows) + 1L]] <- data.frame(
            current_game_id = as.character(game$game_id[[1L]]),
            team = team,
            opponent = opponent,
            role = "Pitcher",
            player_id = as.character(player$player_id[[1L]]),
            player_name = as.character(player$player_name[[1L]]),
            games = length(unique(player$game_pk)),
            plate_appearances = NA_real_,
            hits = NA_real_,
            doubles = NA_real_,
            triples = NA_real_,
            home_runs = NA_real_,
            runs_batted_in = NA_real_,
            walks = sum_column(player, "walks_allowed"),
            strikeouts = sum_column(player, "strikeouts"),
            innings_outs = sum_column(player, "innings_outs"),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (nrow(previous)) {
      previous_id <- as.character(previous$game_pk[[1L]])
      hitter_previous <- hitters[
        hitters$team == team & as.character(hitters$game_pk) == previous_id,
        ,
        drop = FALSE
      ]
      if (nrow(hitter_previous)) {
        hitter_previous$performance_score <-
          2 * suppressWarnings(as.numeric(hitter_previous$hits)) +
          3 * suppressWarnings(as.numeric(hitter_previous$home_runs)) +
          suppressWarnings(as.numeric(hitter_previous$runs_batted_in))
        hitter_previous <- utils::head(
          hitter_previous[order(-hitter_previous$performance_score, hitter_previous$player_name), , drop = FALSE],
          3L
        )
        for (row_index in seq_len(nrow(hitter_previous))) {
          row <- hitter_previous[row_index, , drop = FALSE]
          recent_rows[[length(recent_rows) + 1L]] <- data.frame(
            current_game_id = as.character(game$game_id[[1L]]),
            team = team,
            opponent = as.character(previous$opponent[[1L]]),
            game_pk = previous_id,
            game_date = as.character(previous$game_date[[1L]]),
            recency_label = if (previous$game_date[[1L]] == report_date - 1L) "Yesterday" else "Previous game",
            role = "Hitter",
            player_id = as.character(row$player_id[[1L]]),
            player_name = as.character(row$player_name[[1L]]),
            stat_line = paste0(
              row$hits[[1L]], " H, ", row$home_runs[[1L]], " HR, ",
              row$doubles[[1L]], " 2B, ", row$runs_batted_in[[1L]], " RBI"
            ),
            performance_score = row$performance_score[[1L]],
            stringsAsFactors = FALSE
          )
        }
      }
      pitcher_previous <- pitchers[
        pitchers$team == team & as.character(pitchers$game_pk) == previous_id,
        ,
        drop = FALSE
      ]
      if (nrow(pitcher_previous)) {
        pitcher_previous$performance_score <-
          suppressWarnings(as.numeric(pitcher_previous$innings_outs)) / 3 +
          suppressWarnings(as.numeric(pitcher_previous$strikeouts))
        pitcher_previous <- utils::head(
          pitcher_previous[order(-pitcher_previous$performance_score, pitcher_previous$player_name), , drop = FALSE],
          2L
        )
        for (row_index in seq_len(nrow(pitcher_previous))) {
          row <- pitcher_previous[row_index, , drop = FALSE]
          recent_rows[[length(recent_rows) + 1L]] <- data.frame(
            current_game_id = as.character(game$game_id[[1L]]),
            team = team,
            opponent = as.character(previous$opponent[[1L]]),
            game_pk = previous_id,
            game_date = as.character(previous$game_date[[1L]]),
            recency_label = if (previous$game_date[[1L]] == report_date - 1L) "Yesterday" else "Previous game",
            role = "Pitcher",
            player_id = as.character(row$player_id[[1L]]),
            player_name = as.character(row$player_name[[1L]]),
            stat_line = paste0(
              floor(as.numeric(row$innings_outs[[1L]]) / 3), ".",
              as.numeric(row$innings_outs[[1L]]) %% 3, " IP, ",
              row$strikeouts[[1L]], " K"
            ),
            performance_score = row$performance_score[[1L]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}

bind_rows <- function(rows, empty) if (length(rows)) do.call(rbind, rows) else empty
context <- bind_rows(context_rows, empty_series_context())
series_players <- bind_rows(series_player_rows, empty_series_players())
recent <- bind_rows(recent_rows, empty_recent_games())
source_dates <- c(hitters$game_date, pitchers$game_date)
source_dates <- source_dates[!is.na(source_dates)]
source_through <- if (length(source_dates)) max(source_dates) else as.Date(NA)
for (product_name in c("context", "series_players", "recent")) {
  product <- get(product_name)
  if (nrow(product)) {
    product$report_date <- as.character(site_report_date)
    product$source_through <- as.character(source_through)
    product$generated_at_utc <- generated_at_utc
  }
  assign(product_name, product)
}
if (nrow(series_players)) {
  series_players$method <- "consecutive_same_opponent_player_totals_v1"
  series_players <- series_players[order(
    series_players$current_game_id, series_players$team, series_players$role,
    -ifelse(series_players$role == "Hitter", series_players$hits, series_players$strikeouts)
  ), , drop = FALSE]
}
write_product(context, "daily-series-context.csv")
write_product(series_players, "daily-series-player-lines.csv")
write_product(recent, "daily-recent-game-lines.csv")
cat("Built", nrow(context), "team-series rows,", nrow(series_players), "series player rows, and", nrow(recent), "recent-game highlights.\n")
