suppressPackageStartupMessages(library(dplyr))

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
report_date <- as.Date(Sys.getenv("SABRHOOD_DATE", unset = as.character(Sys.Date())))
workspace_root <- normalizePath(file.path(".."), winslash = "/", mustWork = TRUE)
history_root <- Sys.getenv(
  "SABRHOOD_RETROSHEET_HISTORY",
  unset = file.path(workspace_root, "progress", "private-history", "retrosheet", "2025")
)
record_root <- Sys.getenv(
  "SABRHOOD_RETROSHEET_RECORD_ARCHIVE",
  unset = file.path(
    workspace_root, "progress", "private-history", "retrosheet-records", "2025"
  )
)
output_path <- file.path(output_dir, "daily-retrosheet-history.csv")
record_manifest_path <- file.path(record_root, "record-history-manifest.csv")
record_hitter_path <- file.path(record_root, "rare-hitter-player-games.rds")
record_pitcher_path <- file.path(record_root, "rare-pitcher-player-games.rds")
use_record_archive <- all(file.exists(c(
  record_manifest_path, record_hitter_path, record_pitcher_path
)))
if (!use_record_archive && !dir.exists(history_root)) {
  if (file.exists(output_path)) {
    message("Private Retrosheet history is unavailable; preserving the last approved daily history product.")
    quit(save = "no", status = 0L)
  }
  stop("Private Retrosheet history index is missing: ", history_root, call. = FALSE)
}

history_start <- NA_integer_
history_end <- NA_integer_
season_dirs <- character()
if (use_record_archive) {
  record_manifest <- utils::read.csv(record_manifest_path, stringsAsFactors = FALSE)
  history_start <- suppressWarnings(as.integer(record_manifest$history_start[[1L]]))
  history_end <- suppressWarnings(as.integer(record_manifest$history_end[[1L]]))
} else {
  season_dirs <- list.dirs(history_root, recursive = FALSE, full.names = TRUE)
  season_dirs <- season_dirs[grepl("season=[0-9]{4}$", season_dirs)]
  if (!length(season_dirs)) stop("No Retrosheet season partitions were found.", call. = FALSE)
  seasons <- suppressWarnings(as.integer(sub("^.*season=", "", season_dirs)))
  history_start <- min(seasons, na.rm = TRUE)
  history_end <- max(seasons, na.rm = TRUE)
}
history_range <- paste0(history_start, "-", history_end)
month_day <- format(report_date, "%m-%d")

hitter_rules <- list(
  list(id = "four_hr", label = "four-home-run game", test = function(x) x$home_runs >= 4),
  list(id = "six_hits", label = "six-hit game", test = function(x) x$hits >= 6),
  list(id = "eight_rbi", label = "eight-RBI game", test = function(x) x$runs_batted_in >= 8),
  list(id = "four_doubles", label = "four-double game", test = function(x) x$doubles >= 4),
  list(id = "three_triples", label = "three-triple game", test = function(x) x$triples >= 3),
  list(id = "five_steals", label = "five-steal game", test = function(x) x$stolen_bases >= 5),
  list(id = "twelve_total_bases", label = "12-total-base game", test = function(x) x$total_bases >= 12),
  list(id = "three_hr", label = "three-home-run game", test = function(x) x$home_runs >= 3),
  list(id = "seven_rbi", label = "seven-RBI game", test = function(x) x$runs_batted_in >= 7),
  list(id = "five_hits", label = "five-hit game", test = function(x) x$hits >= 5),
  list(id = "ten_tb_four_hits", label = "four-hit, 10-total-base game", test = function(x) x$hits >= 4 & x$total_bases >= 10)
)
pitcher_rules <- list(
  list(id = "perfect_game", label = "perfect game", test = function(x) x$complete_games >= 1 & x$outs_recorded >= 27 & x$hits_allowed == 0 & x$walks_allowed == 0 & x$hit_batters == 0 & x$batters_faced <= 27),
  list(id = "no_hitter", label = "complete-game no-hitter", test = function(x) x$complete_games >= 1 & x$outs_recorded >= 27 & x$hits_allowed == 0),
  list(id = "eighteen_strikeouts", label = "18-strikeout game", test = function(x) x$strikeouts >= 18),
  list(id = "one_hit_complete", label = "complete-game one-hitter", test = function(x) x$complete_games >= 1 & x$outs_recorded >= 27 & x$hits_allowed <= 1),
  list(id = "fifteen_strikeouts", label = "15-strikeout game", test = function(x) x$strikeouts >= 15),
  list(id = "cg_shutout_twelve_k", label = "complete-game shutout with 12 strikeouts", test = function(x) x$complete_games >= 1 & x$earned_runs == 0 & x$strikeouts >= 12),
  list(id = "nine_scoreless_ten_k", label = "nine scoreless innings with 10 strikeouts", test = function(x) x$outs_recorded >= 27 & x$earned_runs == 0 & x$strikeouts >= 10),
  list(id = "relief_six_k", label = "six-strikeout relief appearance", test = function(x) x$games_started == 0 & x$strikeouts >= 6)
)

rule_counts <- function(rules) stats::setNames(integer(length(rules)), vapply(rules, `[[`, character(1), "id"))
hitter_counts <- rule_counts(hitter_rules)
pitcher_counts <- rule_counts(pitcher_rules)
today_hitters <- list()
today_pitchers <- list()

if (use_record_archive) {
  hitters <- readRDS(record_hitter_path)
  pitchers <- readRDS(record_pitcher_path)
  for (rule in hitter_rules) {
    hitter_counts[[rule$id]] <- sum(rule$test(hitters), na.rm = TRUE)
  }
  for (rule in pitcher_rules) {
    pitcher_counts[[rule$id]] <- sum(rule$test(pitchers), na.rm = TRUE)
  }
  hitter_keep <- format(as.Date(hitters$game_date), "%m-%d") == month_day
  pitcher_keep <- format(as.Date(pitchers$game_date), "%m-%d") == month_day
  if (any(hitter_keep)) today_hitters[[1L]] <- hitters[hitter_keep, , drop = FALSE]
  if (any(pitcher_keep)) today_pitchers[[1L]] <- pitchers[pitcher_keep, , drop = FALSE]
  rm(hitters, pitchers)
} else {
  for (directory in season_dirs) {
    hitter_path <- file.path(directory, "hitter-player-games.rds")
    pitcher_path <- file.path(directory, "pitcher-player-games.rds")
    if (file.exists(hitter_path)) {
      hitters <- readRDS(hitter_path)
      for (rule in hitter_rules) hitter_counts[[rule$id]] <- hitter_counts[[rule$id]] + sum(rule$test(hitters), na.rm = TRUE)
      keep <- format(as.Date(hitters$game_date), "%m-%d") == month_day
      if (any(keep)) today_hitters[[length(today_hitters) + 1L]] <- hitters[keep, , drop = FALSE]
      rm(hitters)
    }
    if (file.exists(pitcher_path)) {
      pitchers <- readRDS(pitcher_path)
      for (rule in pitcher_rules) pitcher_counts[[rule$id]] <- pitcher_counts[[rule$id]] + sum(rule$test(pitchers), na.rm = TRUE)
      keep <- format(as.Date(pitchers$game_date), "%m-%d") == month_day
      if (any(keep)) today_pitchers[[length(today_pitchers) + 1L]] <- pitchers[keep, , drop = FALSE]
      rm(pitchers)
    }
  }
}

profiles_path <- file.path(output_dir, "historical-player-profiles.csv")
profiles <- if (file.exists(profiles_path)) utils::read.csv(profiles_path, stringsAsFactors = FALSE) else data.frame()
profile_keys <- if (nrow(profiles)) tolower(gsub("[^a-z0-9]", "", profiles$player_name)) else character()
recognition <- function(player_name) {
  if (!nrow(profiles)) return(35)
  key <- tolower(gsub("[^a-z0-9]", "", player_name))
  index <- match(key, profile_keys)
  value <- suppressWarnings(as.numeric(profiles$career_significance_score[index]))
  value[!is.finite(value)] <- 35
  value
}

select_rule <- function(data, rules, counts) {
  candidates <- lapply(rules, function(rule) {
    hit <- rule$test(data)
    ifelse(hit, counts[[rule$id]], Inf)
  })
  candidate_matrix <- do.call(cbind, candidates)
  candidate_matrix[is.na(candidate_matrix)] <- Inf
  chosen <- max.col(-candidate_matrix, ties.method = "first")
  occurrence <- candidate_matrix[cbind(seq_len(nrow(data)), chosen)]
  valid <- is.finite(occurrence) & occurrence <= 250
  list(
    valid = valid,
    rule_id = vapply(rules[chosen], `[[`, character(1), "id"),
    rule_label = vapply(rules[chosen], `[[`, character(1), "label"),
    occurrence_count = occurrence
  )
}

hitter_rows <- if (length(today_hitters)) bind_rows(today_hitters) else data.frame()
pitcher_rows <- if (length(today_pitchers)) bind_rows(today_pitchers) else data.frame()
output <- list()
if (nrow(hitter_rows)) {
  selected <- select_rule(hitter_rows, hitter_rules, hitter_counts)
  if (any(selected$valid)) {
    data <- hitter_rows[selected$valid, , drop = FALSE]
    selected <- lapply(selected, function(value) value[selected$valid])
    output[[1L]] <- data.frame(
      role = "Hitter", game_date = as.Date(data$game_date), player_id = data$player_id,
      player_name = data$player_name, team = data$team_name, opponent = data$opponent_id,
      rarity_type = selected$rule_id, rarity_label = selected$rule_label,
      occurrence_count = selected$occurrence_count,
      stat_line = paste0(
        data$hits, "-for-", data$at_bats, ", ", data$home_runs, " HR, ",
        data$runs_batted_in, " RBI, ", data$total_bases, " TB"
      ),
      stringsAsFactors = FALSE
    )
  }
}
if (nrow(pitcher_rows)) {
  selected <- select_rule(pitcher_rows, pitcher_rules, pitcher_counts)
  if (any(selected$valid)) {
    data <- pitcher_rows[selected$valid, , drop = FALSE]
    selected <- lapply(selected, function(value) value[selected$valid])
    output[[2L]] <- data.frame(
      role = "Pitcher", game_date = as.Date(data$game_date), player_id = data$player_id,
      player_name = data$player_name, team = data$team_name, opponent = data$opponent_id,
      rarity_type = selected$rule_id, rarity_label = selected$rule_label,
      occurrence_count = selected$occurrence_count,
      stat_line = paste0(
        format(round(data$innings_pitched, 1), nsmall = 1), " IP, ", data$hits_allowed,
        " H, ", data$earned_runs, " ER, ", data$walks_allowed, " BB, ", data$strikeouts, " K"
      ),
      stringsAsFactors = FALSE
    )
  }
}

daily <- if (length(output)) bind_rows(output) else data.frame()
if (nrow(daily)) {
  daily$recognition_score <- recognition(daily$player_name)
  rarity_score <- pmax(0, 100 - 22 * log10(pmax(daily$occurrence_count, 1)))
  recency_score <- pmax(0, 100 - (as.integer(format(report_date, "%Y")) - as.integer(format(daily$game_date, "%Y"))) * 0.8)
  daily$story_score <- round(0.65 * rarity_score + 0.25 * daily$recognition_score + 0.10 * recency_score, 1)
  daily$headline <- paste0(
    daily$player_name, "'s ", daily$rarity_label, " has occurred ",
    format(daily$occurrence_count, big.mark = ","), " times in the ",
    history_range, " Retrosheet index"
  )
  daily <- daily[order(daily$role, -daily$story_score, daily$occurrence_count), , drop = FALSE]
  daily <- bind_rows(
    head(daily[daily$role == "Hitter", , drop = FALSE], 3L),
    head(daily[daily$role == "Pitcher", , drop = FALSE], 3L)
  )
  daily$report_date <- as.character(report_date)
  daily$history_universe <- paste0(
    "Retrosheet regular-season player games, ", history_range
  )
  daily$history_archive_method <- if (use_record_archive) {
    "sabrhood_retrosheet_record_only_archive_v1"
  } else {
    "sabrhood_retrosheet_partition_fallback_v1"
  }
  daily$source_note <- paste(
    "The information used here was obtained free of charge from and is copyrighted by Retrosheet.",
    "Interested parties may contact Retrosheet at www.retrosheet.org."
  )
  daily$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(daily, output_path, row.names = FALSE, na = "")
cat("Built", nrow(daily), "rare Retrosheet on-this-day game notes for", format(report_date, "%B %d"), ".\n")
