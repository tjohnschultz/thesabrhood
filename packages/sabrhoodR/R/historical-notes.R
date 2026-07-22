.person_name <- function(people) {
  first <- if ("nameFirst" %in% names(people)) as.character(people$nameFirst) else rep("", nrow(people))
  last <- if ("nameLast" %in% names(people)) as.character(people$nameLast) else rep("", nrow(people))
  trimws(paste(first, last))
}

.build_player_date_anniversary_notes <- function(
    people,
    report_date,
    date_column,
    category,
    event_label,
    minimum_years = 1L,
    career_profiles = NULL) {
  if (!is.data.frame(people)) stop("`people` must be a data frame.", call. = FALSE)
  required <- c("playerID", date_column)
  missing <- setdiff(required, names(people))
  if (length(missing) > 0L) stop("People data is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  report_date <- as.Date(report_date)
  event_date <- as.Date(people[[date_column]])
  anniversary_match <- !is.na(event_date) & format(event_date, "%m-%d") == format(report_date, "%m-%d")
  years_ago <- as.integer(format(report_date, "%Y")) - as.integer(format(event_date, "%Y"))
  keep <- anniversary_match & years_ago >= as.integer(minimum_years)

  if (!any(keep)) {
    return(tibble::tibble(
      note_id = character(), report_date = as.Date(character()), category = character(),
      subject_id = character(), subject_name = character(), historical_date = as.Date(character()),
      years_ago = integer(), headline = character(), body = character(), evidence_source = character(),
      rarity = numeric(), magnitude = numeric(), timeliness = numeric(), relevance = numeric(),
      confidence = numeric(), novelty = numeric(), recognition = numeric(),
      historical_importance = numeric(), broadcast_value = numeric(),
      career_significance_score = numeric(), recognition_tier = character(),
      career_summary = character(), story_score = numeric(), score_method = character()
    ))
  }

  selected <- people[keep, , drop = FALSE]
  selected_date <- event_date[keep]
  selected_years <- years_ago[keep]
  names <- .person_name(selected)
  year_word <- ifelse(selected_years == 1L, "year", "years")

  notes <- tibble::tibble(
    note_id = paste(category, selected$playerID, report_date, sep = "_"),
    report_date = report_date,
    category = category,
    subject_id = as.character(selected$playerID),
    subject_name = names,
    historical_date = selected_date,
    years_ago = selected_years,
    headline = paste0(names, ": ", selected_years, " ", year_word, " since ", event_label),
    body = paste0(names, "'s ", event_label, " came ", selected_years, " ", year_word, " ago today, on ", selected_date, "."),
    evidence_source = paste0("People$", date_column),
    rarity = pmin(0.45 + selected_years / 125, 1),
    magnitude = pmin(0.35 + selected_years / 100, 1),
    timeliness = 1,
    relevance = 0.55,
    confidence = 1,
    novelty = 0.80,
    career_significance_score = 0,
    recognition_tier = "unrated",
    career_summary = ""
  )
  if (!is.null(career_profiles)) {
    if (!is.data.frame(career_profiles) || !all(c("playerID", "player_name", "career_significance_score", "recognition_tier", "career_summary") %in% names(career_profiles))) {
      stop("`career_profiles` must come from `build_player_career_profiles()`.", call. = FALSE)
    }
    profile_index <- match(notes$subject_id, as.character(career_profiles$playerID))
    matched <- !is.na(profile_index)
    display_name <- as.character(career_profiles$player_name[profile_index[matched]])
    notes$subject_name[matched] <- display_name
    notes$career_significance_score[matched] <- career_profiles$career_significance_score[profile_index[matched]]
    notes$recognition_tier[matched] <- as.character(career_profiles$recognition_tier[profile_index[matched]])
    notes$career_summary[matched] <- as.character(career_profiles$career_summary[profile_index[matched]])
    matched_years <- notes$years_ago[matched]
    matched_word <- ifelse(matched_years == 1L, "year", "years")
    notes$headline[matched] <- paste0(display_name, ": ", matched_years, " ", matched_word, " since ", event_label)
    notes$body[matched] <- paste0(display_name, "'s ", event_label, " came ", matched_years, " ", matched_word,
                                  " ago today, on ", notes$historical_date[matched], ".")
    notes$body[matched & nzchar(notes$career_summary)] <- paste(notes$body[matched & nzchar(notes$career_summary)], notes$career_summary[matched & nzchar(notes$career_summary)])
  }
  notes$recognition <- pmax(pmin(notes$career_significance_score / 100, 1), 0.08)
  notes$historical_importance <- pmax(notes$recognition, pmin(0.35 + notes$years_ago / 125, 1))
  notes$broadcast_value <- pmax(pmin(0.35 + 0.65 * notes$recognition, 1), 0.35)
  notes$relevance <- pmax(notes$relevance, notes$recognition)
  notes$magnitude <- pmax(notes$magnitude, pmin(0.25 + 0.75 * notes$recognition, 1))
  notes <- score_story_candidates(
    notes,
    weights = c(
      rarity = 1.0, magnitude = 1.0, timeliness = 1.25, relevance = 1.0,
      confidence = 1.3, novelty = 0.75, recognition = 1.5,
      historical_importance = 1.25, broadcast_value = 1.15
    )
  )
  notes$score_method <- "sabrhood_historical_story_score_v2"
  notes
}

#' Build MLB debut anniversary notes
#' @param people A Lahman-compatible People table.
#' @param report_date Date for which notes are generated.
#' @param minimum_years Minimum anniversary age.
#' @param career_profiles Optional output from [build_player_career_profiles()].
#' @return Scored historical note candidates.
#' @export
build_debut_anniversary_notes <- function(people, report_date = Sys.Date(), minimum_years = 1L, career_profiles = NULL) {
  .build_player_date_anniversary_notes(
    people, report_date, "debut", "debut_anniversary", "MLB debut", minimum_years, career_profiles
  )
}

#' Build final MLB appearance anniversary notes
#' @param people A Lahman-compatible People table.
#' @param report_date Date for which notes are generated.
#' @param minimum_years Minimum anniversary age.
#' @param career_profiles Optional output from [build_player_career_profiles()].
#' @return Scored historical note candidates.
#' @export
build_final_game_anniversary_notes <- function(people, report_date = Sys.Date(), minimum_years = 1L, career_profiles = NULL) {
  .build_player_date_anniversary_notes(
    people, report_date, "finalGame", "final_game_anniversary", "final MLB appearance", minimum_years, career_profiles
  )
}

#' Build the daily historical anniversary candidate pool
#' @param people A Lahman-compatible People table.
#' @param report_date Date for which notes are generated.
#' @param minimum_years Minimum anniversary age.
#' @param career_profiles Optional output from [build_player_career_profiles()].
#' @return Debut and final-appearance anniversary candidates ranked by story score.
#' @export
build_historical_anniversary_notes <- function(people, report_date = Sys.Date(), minimum_years = 1L, career_profiles = NULL) {
  output <- dplyr::bind_rows(
    build_debut_anniversary_notes(people, report_date, minimum_years, career_profiles),
    build_final_game_anniversary_notes(people, report_date, minimum_years, career_profiles)
  )
  dplyr::arrange(
    output,
    dplyr::desc(.data$story_score),
    dplyr::desc(.data$career_significance_score),
    dplyr::desc(.data$years_ago)
  )
}
