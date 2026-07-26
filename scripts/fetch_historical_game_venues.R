workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

pbp_path <- file.path(workspace, ".private-data", "pbp", "2026", "current.rds")
if (!file.exists(pbp_path)) stop("Missing current PBP cache: ", pbp_path, call. = FALSE)
pbp <- readRDS(pbp_path)
game_dates <- as.Date(pbp$game_date)
end_date <- max(game_dates, na.rm = TRUE)
start_date <- as.Date(sprintf("%s-03-01", format(end_date, "%Y")))
season <- format(end_date, "%Y")

endpoint <- paste0(
  "https://statsapi.mlb.com/api/v1/schedule?",
  "sportId=1&startDate=", format(start_date, "%Y-%m-%d"),
  "&endDate=", format(end_date, "%Y-%m-%d"),
  "&hydrate=venue"
)
payload <- jsonlite::fromJSON(endpoint, simplifyVector = FALSE)

rows <- list()
for (date_group in payload$dates) {
  for (game in date_group$games) {
    rows[[length(rows) + 1L]] <- data.frame(
      game_pk = as.character(game$gamePk),
      official_date = as.character(game$officialDate %||% date_group$date),
      game_date_utc = as.character(game$gameDate %||% NA_character_),
      away_team = as.character(game$teams$away$team$name %||% NA_character_),
      home_team = as.character(game$teams$home$team$name %||% NA_character_),
      venue_id = as.character(game$venue$id %||% NA_character_),
      venue_name = as.character(game$venue$name %||% NA_character_),
      game_status = as.character(game$status$detailedState %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }
}
venues <- do.call(rbind, rows)
if (is.null(venues) || !nrow(venues)) {
  stop("MLB schedule response contained no games.", call. = FALSE)
}
venues <- venues[!duplicated(venues$game_pk), , drop = FALSE]

private_dir <- file.path(workspace, ".private-data", "reference")
dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
output_path <- file.path(private_dir, paste0("mlb-game-venues-", season, ".rds"))
saveRDS(venues, output_path)

cat(
  "Cached", nrow(venues), "MLB game/venue rows from",
  format(start_date), "through", format(end_date), "at", output_path, "\n"
)
