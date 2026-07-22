.history_sum_by_player <- function(data, columns, prefix = "") {
  if (is.null(data) || !is.data.frame(data) || !nrow(data) || !"playerID" %in% names(data)) {
    return(data.frame(playerID = character(), stringsAsFactors = FALSE))
  }
  columns <- intersect(columns, names(data))
  if (!length(columns)) return(data.frame(playerID = unique(as.character(data$playerID)), stringsAsFactors = FALSE))
  values <- lapply(data[columns], function(value) suppressWarnings(as.numeric(value)))
  values$playerID <- as.character(data$playerID)
  output <- stats::aggregate(values[columns], list(playerID = values$playerID), function(value) sum(value, na.rm = TRUE))
  names(output)[-1L] <- paste0(prefix, names(output)[-1L])
  output
}

.history_left_join <- function(x, y) {
  if (!nrow(y)) return(x)
  order_key <- seq_len(nrow(x))
  x$.history_order <- order_key
  output <- merge(x, y, by = "playerID", all.x = TRUE, sort = FALSE)
  output <- output[order(output$.history_order), , drop = FALSE]
  output$.history_order <- NULL
  output
}

.history_name <- function(people) {
  first <- if ("nameFirst" %in% names(people)) as.character(people$nameFirst) else rep("", nrow(people))
  last <- if ("nameLast" %in% names(people)) as.character(people$nameLast) else rep("", nrow(people))
  trimws(paste(first, last))
}

.normalize_player_name <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  value <- vapply(value, function(item) {
    repeat {
      hit <- regexpr("<U[+]([0-9A-Fa-f]{4,6})>", item, perl = TRUE)
      if (hit[[1L]] < 0L) break
      token <- regmatches(item, hit)
      code <- sub("^<U\\+", "", sub(">$", "", token))
      replacement <- intToUtf8(strtoi(code, base = 16L))
      start <- hit[[1L]]
      end <- start + attr(hit, "match.length") - 1L
      item <- paste0(substr(item, 1L, start - 1L), replacement, substr(item, end + 1L, nchar(item)))
    }
    transliterated <- iconv(item, from = "", to = "ASCII//TRANSLIT")
    if (is.na(transliterated)) item else transliterated
  }, character(1), USE.NAMES = FALSE)
  value <- tolower(value)
  value <- gsub("\\b(jr|sr|ii|iii|iv)\\b", " ", value, perl = TRUE)
  value <- gsub("[^a-z0-9]+", " ", value)
  trimws(gsub("[[:space:]]+", " ", value))
}

.history_clip <- function(value) pmax(pmin(value, 1), 0)

.career_summary <- function(profile) {
  name <- profile$player_name[[1L]]
  batting_value <- max(c(profile$career_H, profile$career_HR * 5, profile$career_RBI * 1.5), na.rm = TRUE)
  pitching_value <- max(c(profile$career_W * 10, profile$career_SO_pitch, profile$career_SV * 5), na.rm = TRUE)
  pieces <- character()
  if (is.finite(profile$allstar_selections[[1L]]) && profile$allstar_selections[[1L]] > 0) {
    pieces <- c(pieces, paste(profile$allstar_selections[[1L]], "All-Star selections"))
  }
  if (batting_value >= pitching_value && profile$career_AB[[1L]] > 0) {
    pieces <- c(
      pieces,
      paste(format(profile$career_H[[1L]], big.mark = ","), "hits"),
      paste(format(profile$career_HR[[1L]], big.mark = ","), "home runs")
    )
  } else if (profile$career_IPouts[[1L]] > 0) {
    pieces <- c(
      pieces,
      paste(format(profile$career_W[[1L]], big.mark = ","), "wins"),
      paste(format(profile$career_SO_pitch[[1L]], big.mark = ","), "strikeouts")
    )
  }
  pieces <- pieces[nzchar(pieces)]
  if (!length(pieces)) return(paste0(name, " appeared in Major League Baseball."))
  if (isTRUE(profile$hof_inducted[[1L]])) {
    paste0(name, " was a Hall of Famer and finished with ", paste(pieces, collapse = ", "), ".")
  } else {
    paste0(name, " finished with ", paste(pieces, collapse = ", "), ".")
  }
}

#' Build historical player career and recognition profiles
#'
#' @param people Lahman-compatible People data.
#' @param batting Lahman-compatible Batting data.
#' @param pitching Lahman-compatible Pitching data.
#' @param awards_players Optional AwardsPlayers data.
#' @param allstar Optional AllstarFull data.
#' @param hall_of_fame Optional HallOfFame data.
#' @param batting_post Optional BattingPost data.
#' @param pitching_post Optional PitchingPost data.
#' @param war Optional data frame containing `playerID` and a WAR column named
#'   `WAR`, `war`, `bWAR`, or `fWAR`. WAR is never inferred from Lahman.
#'
#' @return One row per player with career totals, accolade counts, ranks,
#'   recognition tier, and a transparent significance score.
#' @export
build_player_career_profiles <- function(
    people,
    batting,
    pitching,
    awards_players = NULL,
    allstar = NULL,
    hall_of_fame = NULL,
    batting_post = NULL,
    pitching_post = NULL,
    war = NULL) {
  if (!is.data.frame(people) || !"playerID" %in% names(people)) {
    stop("`people` must be a data frame containing `playerID`.", call. = FALSE)
  }
  profiles <- data.frame(
    playerID = as.character(people$playerID),
    player_name_raw = .history_name(people),
    debut = if ("debut" %in% names(people)) as.Date(people$debut) else as.Date(NA),
    finalGame = if ("finalGame" %in% names(people)) as.Date(people$finalGame) else as.Date(NA),
    birthYear = if ("birthYear" %in% names(people)) suppressWarnings(as.integer(people$birthYear)) else NA_integer_,
    bats = if ("bats" %in% names(people)) as.character(people$bats) else NA_character_,
    throws = if ("throws" %in% names(people)) as.character(people$throws) else NA_character_,
    stringsAsFactors = FALSE
  )
  profiles$player_name_key <- .normalize_player_name(profiles$player_name_raw)
  profiles$player_name <- profiles$player_name_raw

  batting_totals <- .history_sum_by_player(
    batting, c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB", "SO"), "career_"
  )
  pitching_totals <- .history_sum_by_player(
    pitching, c("G", "GS", "W", "L", "SV", "IPouts", "H", "ER", "HR", "BB", "SO"), "career_pitch_"
  )
  names(pitching_totals) <- sub("career_pitch_SO$", "career_SO_pitch", names(pitching_totals))
  names(pitching_totals) <- sub("career_pitch_W$", "career_W", names(pitching_totals))
  names(pitching_totals) <- sub("career_pitch_L$", "career_L", names(pitching_totals))
  names(pitching_totals) <- sub("career_pitch_SV$", "career_SV", names(pitching_totals))
  names(pitching_totals) <- sub("career_pitch_IPouts$", "career_IPouts", names(pitching_totals))
  names(pitching_totals) <- sub("career_pitch_ER$", "career_ER_pitch", names(pitching_totals))
  profiles <- .history_left_join(profiles, batting_totals)
  profiles <- .history_left_join(profiles, pitching_totals)

  numeric_totals <- grep("^career_", names(profiles), value = TRUE)
  for (column in numeric_totals) profiles[[column]][is.na(profiles[[column]])] <- 0
  required_totals <- c("career_G", "career_AB", "career_R", "career_H", "career_X2B", "career_X3B", "career_HR", "career_RBI", "career_SB", "career_BB", "career_SO", "career_pitch_G", "career_pitch_GS", "career_W", "career_L", "career_SV", "career_IPouts", "career_SO_pitch", "career_ER_pitch")
  for (column in setdiff(required_totals, names(profiles))) profiles[[column]] <- 0
  duplicate_name <- duplicated(profiles$player_name) | duplicated(profiles$player_name, fromLast = TRUE)
  duplicate_name <- duplicate_name & nzchar(profiles$player_name) & !is.na(profiles$birthYear)
  profiles$player_name[duplicate_name] <- paste0(profiles$player_name[duplicate_name], " (born ", profiles$birthYear[duplicate_name], ")")

  if (!is.null(awards_players) && is.data.frame(awards_players) && "playerID" %in% names(awards_players)) {
    award_name <- if ("awardID" %in% names(awards_players)) as.character(awards_players$awardID) else rep("", nrow(awards_players))
    award_key <- paste(awards_players$playerID, award_name, if ("yearID" %in% names(awards_players)) awards_players$yearID else seq_len(nrow(awards_players)))
    awards_unique <- awards_players[!duplicated(award_key), , drop = FALSE]
    award_name <- if ("awardID" %in% names(awards_unique)) as.character(awards_unique$awardID) else rep("", nrow(awards_unique))
    major_pattern <- "Most Valuable Player|Cy Young|Rookie of the Year|Gold Glove|Silver Slugger|Triple Crown|Pitching Triple Crown"
    award_counts <- stats::aggregate(
      list(awards_count = rep(1L, nrow(awards_unique)), major_awards = grepl(major_pattern, award_name, ignore.case = TRUE)),
      list(playerID = as.character(awards_unique$playerID)), sum
    )
    profiles <- .history_left_join(profiles, award_counts)
  }
  if (!"awards_count" %in% names(profiles)) profiles$awards_count <- 0
  if (!"major_awards" %in% names(profiles)) profiles$major_awards <- 0
  profiles$awards_count[is.na(profiles$awards_count)] <- 0
  profiles$major_awards[is.na(profiles$major_awards)] <- 0

  if (!is.null(allstar) && is.data.frame(allstar) && all(c("playerID", "yearID") %in% names(allstar))) {
    allstar_key <- unique(data.frame(playerID = as.character(allstar$playerID), yearID = allstar$yearID))
    allstar_counts <- stats::aggregate(list(allstar_selections = rep(1L, nrow(allstar_key))), list(playerID = allstar_key$playerID), sum)
    profiles <- .history_left_join(profiles, allstar_counts)
  }
  if (!"allstar_selections" %in% names(profiles)) profiles$allstar_selections <- 0
  profiles$allstar_selections[is.na(profiles$allstar_selections)] <- 0

  if (!is.null(hall_of_fame) && is.data.frame(hall_of_fame) && all(c("playerID", "inducted") %in% names(hall_of_fame))) {
    hall <- stats::aggregate(
      list(hof_inducted = toupper(as.character(hall_of_fame$inducted)) == "Y"),
      list(playerID = as.character(hall_of_fame$playerID)), any
    )
    profiles <- .history_left_join(profiles, hall)
  }
  if (!"hof_inducted" %in% names(profiles)) profiles$hof_inducted <- FALSE
  profiles$hof_inducted[is.na(profiles$hof_inducted)] <- FALSE

  post_bat <- .history_sum_by_player(batting_post, c("AB", "H", "HR", "RBI", "BB"), "post_")
  post_pitch <- .history_sum_by_player(pitching_post, c("IPouts", "W", "SV", "SO"), "post_pitch_")
  profiles <- .history_left_join(profiles, post_bat)
  profiles <- .history_left_join(profiles, post_pitch)
  post_columns <- grep("^post_", names(profiles), value = TRUE)
  for (column in post_columns) profiles[[column]][is.na(profiles[[column]])] <- 0

  profiles$career_WAR <- NA_real_
  war_method <- "not_supplied"
  if (!is.null(war) && is.data.frame(war) && "playerID" %in% names(war)) {
    war_column <- intersect(c("WAR", "war", "bWAR", "fWAR"), names(war))
    if (!length(war_column)) stop("`war` must contain WAR, war, bWAR, or fWAR.", call. = FALSE)
    war_totals <- stats::aggregate(
      list(career_WAR = suppressWarnings(as.numeric(war[[war_column[[1L]]]]))),
      list(playerID = as.character(war$playerID)),
      function(value) sum(value, na.rm = TRUE)
    )
    profiles$career_WAR <- NULL
    profiles <- .history_left_join(profiles, war_totals)
    war_method <- paste0("supplied_", war_column[[1L]])
  }

  debut_year <- suppressWarnings(as.integer(format(profiles$debut, "%Y")))
  final_year <- suppressWarnings(as.integer(format(profiles$finalGame, "%Y")))
  profiles$career_years <- ifelse(is.finite(debut_year) & is.finite(final_year), pmax(final_year - debut_year + 1L, 1L), 0L)
  profiles$career_ERA <- ifelse(profiles$career_IPouts > 0, 27 * profiles$career_ER_pitch / profiles$career_IPouts, NA_real_)

  profiles$rank_career_H <- rank(-profiles$career_H, ties.method = "min")
  profiles$rank_career_HR <- rank(-profiles$career_HR, ties.method = "min")
  profiles$rank_career_W <- rank(-profiles$career_W, ties.method = "min")
  profiles$rank_career_SO_pitch <- rank(-profiles$career_SO_pitch, ties.method = "min")
  profiles$rank_career_SV <- rank(-profiles$career_SV, ties.method = "min")

  batter_value <- .history_clip(pmax(
    profiles$career_H / 3000,
    profiles$career_HR / 500,
    profiles$career_RBI / 1500,
    profiles$career_SB / 500,
    na.rm = TRUE
  ))
  pitcher_value <- .history_clip(pmax(
    profiles$career_W / 300,
    profiles$career_SO_pitch / 3000,
    profiles$career_SV / 500,
    na.rm = TRUE
  ))
  war_value <- .history_clip(profiles$career_WAR / 70)
  war_value[!is.finite(war_value)] <- 0
  career_value <- pmax(batter_value, pitcher_value, war_value)
  accolade_value <- .history_clip(
    0.45 * as.numeric(profiles$hof_inducted) +
      0.25 * pmin(profiles$major_awards / 3, 1) +
      0.20 * pmin(profiles$allstar_selections / 10, 1) +
      0.10 * pmin(profiles$awards_count / 10, 1)
  )
  longevity_value <- .history_clip(profiles$career_years / 15)
  post_volume <- if ("post_AB" %in% names(profiles)) profiles$post_AB else 0
  if ("post_pitch_IPouts" %in% names(profiles)) post_volume <- post_volume + profiles$post_pitch_IPouts / 3
  postseason_value <- .history_clip(post_volume / 300)
  record_value <- .history_clip(
    as.numeric(profiles$rank_career_H <= 10 | profiles$rank_career_HR <= 10 |
      profiles$rank_career_W <= 10 | profiles$rank_career_SO_pitch <= 10 | profiles$rank_career_SV <= 10)
  )

  significance <- 100 * (
    0.47 * career_value + 0.23 * accolade_value + 0.13 * longevity_value +
      0.07 * postseason_value + 0.10 * record_value
  )
  significance[profiles$hof_inducted] <- pmax(significance[profiles$hof_inducted], 85)
  profiles$career_significance_score <- round(.history_clip(significance / 100) * 100, 1)
  profiles$recognition_tier <- cut(
    profiles$career_significance_score,
    breaks = c(-Inf, 20, 40, 60, 80, Inf),
    labels = c("limited", "established", "notable", "star", "icon"),
    right = FALSE
  )
  profiles$recognition_tier <- as.character(profiles$recognition_tier)
  profiles$career_profile_method <- if (war_method == "not_supplied") {
    "sabrhood_lahman_prominence_no_war_v1"
  } else {
    paste0("sabrhood_lahman_prominence_with_", war_method, "_v1")
  }
  profiles$career_summary <- vapply(seq_len(nrow(profiles)), function(index) .career_summary(profiles[index, , drop = FALSE]), character(1))
  tibble::as_tibble(profiles)
}

#' Build evergreen career milestone and record notes
#'
#' @param career_profiles Output from [build_player_career_profiles()].
#' @param minimum_significance Minimum career significance score.
#' @param record_top_n Career leaderboard depth included in record notes.
#'
#' @return Ranked milestone and career-record story candidates.
#' @export
build_career_milestone_notes <- function(career_profiles, minimum_significance = 45, record_top_n = 10L) {
  if (!is.data.frame(career_profiles) || !all(c("playerID", "player_name", "career_significance_score") %in% names(career_profiles))) {
    stop("`career_profiles` must come from `build_player_career_profiles()`.", call. = FALSE)
  }
  milestones <- list(
    list(column = "career_H", label = "hits", club_label = "hit", thresholds = c(1500, 2000, 2500, 3000, 3500, 4000)),
    list(column = "career_HR", label = "home runs", club_label = "home-run", thresholds = c(300, 400, 500, 600, 700)),
    list(column = "career_RBI", label = "RBI", club_label = "RBI", thresholds = c(1000, 1500, 2000)),
    list(column = "career_SB", label = "stolen bases", club_label = "stolen-base", thresholds = c(300, 500, 700)),
    list(column = "career_W", label = "wins", club_label = "win", thresholds = c(150, 200, 250, 300, 400, 500)),
    list(column = "career_SO_pitch", label = "strikeouts", club_label = "strikeout", thresholds = c(1500, 2000, 2500, 3000, 4000, 5000)),
    list(column = "career_SV", label = "saves", club_label = "save", thresholds = c(300, 400, 500, 600))
  )
  output <- list()
  next_index <- 1L
  for (entry in milestones) {
    if (!entry$column %in% names(career_profiles)) next
    values <- suppressWarnings(as.numeric(career_profiles[[entry$column]]))
    for (index in which(values >= min(entry$thresholds) & career_profiles$career_significance_score >= minimum_significance)) {
      achieved <- max(entry$thresholds[entry$thresholds <= values[[index]]])
      rank_column <- paste0("rank_", entry$column)
      career_rank <- if (rank_column %in% names(career_profiles)) career_profiles[[rank_column]][[index]] else NA_real_
      output[[next_index]] <- data.frame(
        note_id = paste("career", career_profiles$playerID[[index]], entry$column, achieved, sep = "_"),
        category = "career_milestone",
        subject_id = career_profiles$playerID[[index]],
        subject_name = career_profiles$player_name[[index]],
        recognition_tier = career_profiles$recognition_tier[[index]],
        career_significance_score = career_profiles$career_significance_score[[index]],
        milestone_stat = entry$label,
        milestone_value = achieved,
        career_value = values[[index]],
        career_rank = career_rank,
        headline = paste0(career_profiles$player_name[[index]], " reached the ", format(achieved, big.mark = ","), "-", entry$club_label, " club"),
        body = career_profiles$career_summary[[index]],
        evidence_source = paste0("Lahman career aggregate: ", entry$column),
        stringsAsFactors = FALSE
      )
      next_index <- next_index + 1L
    }
  }
  if (!length(output)) return(tibble::tibble())
  notes <- dplyr::bind_rows(output)
  notes$record_flag <- is.finite(notes$career_rank) & notes$career_rank <= as.integer(record_top_n)
  notes$rarity <- .history_clip(0.45 + 0.35 * notes$record_flag + notes$milestone_value / pmax(notes$career_value, 1) * 0.15)
  notes$magnitude <- .history_clip(notes$career_significance_score / 100)
  notes$timeliness <- 0.55
  notes$relevance <- .history_clip(notes$career_significance_score / 100)
  notes$confidence <- 1
  notes$novelty <- 0.75
  notes$recognition <- .history_clip(notes$career_significance_score / 100)
  notes$historical_importance <- .history_clip(0.55 + 0.35 * notes$record_flag)
  notes$broadcast_value <- .history_clip(0.60 + 0.25 * notes$record_flag)
  notes <- score_story_candidates(
    notes,
    weights = c(
      rarity = 1.1, magnitude = 1.1, timeliness = 0.7, relevance = 1.0,
      confidence = 1.2, novelty = 0.8, recognition = 1.35,
      historical_importance = 1.25, broadcast_value = 1.1
    )
  )
  notes$score_method <- "sabrhood_historical_story_score_v2"
  dplyr::arrange(notes, dplyr::desc(.data$story_score), dplyr::desc(.data$career_significance_score))
}
