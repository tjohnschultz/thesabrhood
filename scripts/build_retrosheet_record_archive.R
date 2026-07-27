workspace_root <- normalizePath(file.path(".."), winslash = "/", mustWork = TRUE)
local_library <- file.path(workspace_root, "sabrhoodR", ".Rlib")
.libPaths(c(local_library, .libPaths()))
suppressPackageStartupMessages({
  library(data.table)
  library(sabrhoodR)
})

release_season <- as.integer(Sys.getenv("SABRHOOD_RETROSHEET_RELEASE", unset = "2025"))
history_start <- as.integer(Sys.getenv("SABRHOOD_RECORD_HISTORY_START", unset = "1898"))
history_end <- as.integer(Sys.getenv(
  "SABRHOOD_RECORD_HISTORY_END",
  unset = as.character(release_season)
))
source_root <- Sys.getenv(
  "SABRHOOD_RETROSHEET_SOURCE",
  unset = file.path(
    workspace_root, "progress", "private-source", "retrosheet",
    as.character(release_season)
  )
)
output_root <- Sys.getenv(
  "SABRHOOD_RETROSHEET_RECORD_ARCHIVE",
  unset = file.path(
    workspace_root, "progress", "private-history", "retrosheet-records",
    as.character(release_season)
  )
)

if (
  !is.finite(history_start) || !is.finite(history_end) ||
    history_start < 1898L || history_start > history_end ||
    history_end > release_season
) {
  stop("The requested record-history range is invalid.", call. = FALSE)
}

batting_path <- file.path(source_root, "raw", "batting", "batting.csv")
pitching_path <- file.path(source_root, "raw", "pitching", "pitching.csv")
bio_path <- file.path(source_root, "raw", "biodata", "biofile0.csv")
team_path <- file.path(source_root, "raw", "biodata", "teams0.csv")
required_paths <- c(batting_path, pitching_path, bio_path, team_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths)) {
  stop(
    "The Retrosheet aggregate snapshot is incomplete: ",
    paste(missing_paths, collapse = ", "),
    call. = FALSE
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)

players <- fread(
  bio_path,
  select = c("id", "usename", "lastname", "fullname"),
  na.strings = c("", "NA"),
  showProgress = FALSE
)
players[, display_name := trimws(paste(usename, lastname))]
players[, player_name := fifelse(
  !is.na(display_name) & nzchar(display_name),
  display_name,
  fullname
)]
teams <- fread(
  team_path,
  select = c("team", "city", "nickname"),
  na.strings = c("", "NA"),
  showProgress = FALSE
)
teams[, team_name := trimws(paste(city, nickname))]

batting_columns <- c(
  "gid", "id", "team", "stattype", "date", "opp", "vishome",
  "gametype", "box", "b_pa", "b_ab", "b_r", "b_h", "b_d", "b_t",
  "b_hr", "b_rbi", "b_sh", "b_sf", "b_hbp", "b_w", "b_iw", "b_k",
  "b_sb", "b_cs", "b_gdp", "b_xi", "b_roe"
)
pitching_columns <- c(
  "gid", "id", "team", "stattype", "date", "opp", "vishome",
  "gametype", "box", "p_ipouts", "p_bfp", "p_h", "p_d", "p_t",
  "p_hr", "p_r", "p_er", "p_w", "p_iw", "p_k", "p_hbp", "p_wp",
  "p_bk", "p_sh", "p_sf", "p_sb", "p_cs", "p_pb", "wp", "lp",
  "save", "p_gs", "p_gf", "p_cg"
)

cat("Reading the Retrosheet batting record archive...\n")
batting_raw <- fread(
  batting_path,
  select = batting_columns,
  na.strings = c("", "NA"),
  showProgress = TRUE
)
batting_raw <- batting_raw[
  tolower(stattype) == "value" &
    tolower(gametype) == "regular" &
    as.integer(substr(as.character(date), 1L, 4L)) >= history_start &
    as.integer(substr(as.character(date), 1L, 4L)) <= history_end
]
hitters <- as.data.table(standardize_retrosheet_batting_games(
  batting_raw,
  regular_season_only = TRUE
))
hitters[, player_name := players$player_name[match(player_id, players$id)]]
hitters[, team_name := teams$team_name[match(team_id, teams$team)]]
hitter_rare <- hitters[
  home_runs >= 3 |
    hits >= 5 |
    runs_batted_in >= 7 |
    doubles >= 4 |
    triples >= 3 |
    stolen_bases >= 5 |
    total_bases >= 12 |
    (hits >= 4 & total_bases >= 10)
]
setorderv(hitter_rare, c("game_date", "game_id", "player_id"))
saveRDS(
  hitter_rare,
  file.path(output_root, "rare-hitter-player-games.rds"),
  compress = "gzip"
)
batting_rows <- nrow(hitters)
rm(batting_raw, hitters)
invisible(gc())

cat("Reading the Retrosheet pitching record archive...\n")
pitching_raw <- fread(
  pitching_path,
  select = pitching_columns,
  na.strings = c("", "NA"),
  showProgress = TRUE
)
pitching_raw <- pitching_raw[
  tolower(stattype) == "value" &
    tolower(gametype) == "regular" &
    as.integer(substr(as.character(date), 1L, 4L)) >= history_start &
    as.integer(substr(as.character(date), 1L, 4L)) <= history_end
]
pitchers <- as.data.table(standardize_retrosheet_pitching_games(
  pitching_raw,
  regular_season_only = TRUE
))
pitchers[, player_name := players$player_name[match(player_id, players$id)]]
pitchers[, team_name := teams$team_name[match(team_id, teams$team)]]
pitcher_rare <- pitchers[
  (complete_games >= 1 & outs_recorded >= 27 & hits_allowed <= 1) |
    strikeouts >= 15 |
    (complete_games >= 1 & earned_runs == 0 & strikeouts >= 12) |
    (outs_recorded >= 27 & earned_runs == 0 & strikeouts >= 10) |
    (games_started == 0 & strikeouts >= 6)
]
setorderv(pitcher_rare, c("game_date", "game_id", "player_id"))
saveRDS(
  pitcher_rare,
  file.path(output_root, "rare-pitcher-player-games.rds"),
  compress = "gzip"
)
pitching_rows <- nrow(pitchers)
rm(pitching_raw, pitchers)
invisible(gc())

manifest <- data.table(
  release_season = release_season,
  history_start = history_start,
  history_end = history_end,
  hitter_player_game_rows_scanned = batting_rows,
  pitcher_player_game_rows_scanned = pitching_rows,
  rare_hitter_rows = nrow(hitter_rare),
  rare_pitcher_rows = nrow(pitcher_rare),
  built_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  method = "sabrhood_retrosheet_record_only_archive_v1",
  model_boundary = paste(
    "Record-comparison archive only.",
    "Career trajectory models remain pinned to the separate 1974-2025 history index."
  )
)
fwrite(manifest, file.path(output_root, "record-history-manifest.csv"))

cat(
  "Record-only history ready for", history_start, "through", history_end, "with",
  format(nrow(hitter_rare), big.mark = ","), "rare hitter games and",
  format(nrow(pitcher_rare), big.mark = ","), "rare pitcher games.\n"
)
