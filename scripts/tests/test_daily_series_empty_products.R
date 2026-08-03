test_dir <- tempfile("sabrhood-series-contract-")
dir.create(test_dir, recursive = TRUE)
on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)

previous_derived_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = NA_character_)
on.exit({
  if (is.na(previous_derived_dir)) {
    Sys.unsetenv("SABRHOOD_DERIVED_DIR")
  } else {
    Sys.setenv(SABRHOOD_DERIVED_DIR = previous_derived_dir)
  }
}, add = TRUE)
Sys.setenv(SABRHOOD_DERIVED_DIR = test_dir)

write_product <- function(data, name) {
  utils::write.csv(data, file.path(test_dir, name), row.names = FALSE, na = "")
}

write_product(
  data.frame(
    game_id = "today-1", game_date = "2026-08-03",
    away_team = "Away", home_team = "Home",
    stringsAsFactors = FALSE
  ),
  "daily-game-inputs.csv"
)
write_product(
  data.frame(
    game_pk = "prior-1", game_date = "2026-08-02", team = "Away",
    opponent = "Previous Opponent", player_id = "hitter-1",
    player_name = "Example Hitter", plate_appearances = 4,
    hits = 2, doubles = 1, triples = 0, home_runs = 0,
    runs_batted_in = 1, walks = 0, strikeouts = 1,
    stringsAsFactors = FALSE
  ),
  "current-season-hitter-game-lines.csv"
)
write_product(
  data.frame(
    game_pk = character(), game_date = character(), team = character(),
    opponent = character(), player_id = character(), player_name = character(),
    innings_outs = numeric(), strikeouts = numeric(), walks_allowed = numeric(),
    stringsAsFactors = FALSE
  ),
  "current-season-pitcher-game-lines.csv"
)

source(file.path("scripts", "build_daily_series_context.R"), local = new.env(parent = globalenv()))

empty_path <- file.path(test_dir, "daily-series-player-lines.csv")
empty_product <- utils::read.csv(
  empty_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
expected_columns <- c(
  "current_game_id", "team", "opponent", "role", "player_id", "player_name",
  "games", "plate_appearances", "hits", "doubles", "triples", "home_runs",
  "runs_batted_in", "walks", "strikeouts", "innings_outs", "report_date",
  "source_through", "generated_at_utc", "method"
)
stopifnot(nrow(empty_product) == 0L)
stopifnot(identical(names(empty_product), expected_columns))

source(file.path("scripts", "update_derived_manifest.R"), local = new.env(parent = globalenv()))
manifest <- utils::read.csv(
  file.path(test_dir, "manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
manifest_row <- manifest[manifest$file == "daily-series-player-lines.csv", , drop = FALSE]
stopifnot(nrow(manifest_row) == 1L)
stopifnot(manifest_row$rows[[1L]] == 0L)
stopifnot(manifest_row$columns[[1L]] == length(expected_columns))

cat("Empty daily-series products retain their CSV schema and manifest cleanly.\n")
