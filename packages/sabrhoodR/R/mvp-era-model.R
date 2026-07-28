.mvp_pct <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  keep <- is.finite(value)
  output <- rep(NA_real_, length(value))
  if (sum(keep) == 1L) output[keep] <- 0.5
  if (sum(keep) > 1L) output[keep] <- (rank(value[keep], ties.method = "average") - 1) / (sum(keep) - 1)
  output
}

#' Profile the statistical identity of MVP winners by decade
#'
#' Uses league-season percentiles so a winner is evaluated against the ballot
#' environment of his own year. Lahman supplies the historical batting and MVP
#' winner records; WAR is retained as a modern-model anchor because Lahman does
#' not provide a season WAR field.
#'
#' @param batting Lahman batting table.
#' @param awards_players Lahman player-awards table.
#' @param minimum_pa Minimum plate appearances for each league-year comparison.
#' @return A list containing winner percentiles, decade profiles, and a modern
#'   scoring-weight table.
#' @export
build_mvp_era_profiles <- function(batting, awards_players, minimum_pa = 100L) {
  required_batting <- c("playerID", "yearID", "lgID", "G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB")
  if (!all(required_batting %in% names(batting))) stop("`batting` is missing MVP-era fields.", call. = FALSE)
  if (!all(c("playerID", "awardID", "yearID", "lgID") %in% names(awards_players))) {
    stop("`awards_players` is missing MVP winner fields.", call. = FALSE)
  }
  numeric_columns <- intersect(c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB", "HBP", "SF"), names(batting))
  batting <- batting[batting$lgID %in% c("AL", "NL"), , drop = FALSE]
  totals <- stats::aggregate(
    batting[, numeric_columns, drop = FALSE],
    list(playerID = batting$playerID, yearID = batting$yearID, lgID = batting$lgID),
    sum,
    na.rm = TRUE
  )
  if (!"HBP" %in% names(totals)) totals$HBP <- 0
  if (!"SF" %in% names(totals)) totals$SF <- 0
  totals$PA <- totals$AB + totals$BB + totals$HBP + totals$SF
  totals$AVG <- totals$H / pmax(totals$AB, 1)
  totals$OBP <- (totals$H + totals$BB + totals$HBP) / pmax(totals$AB + totals$BB + totals$HBP + totals$SF, 1)
  totals$TB <- totals$H + totals$X2B + 2 * totals$X3B + 3 * totals$HR
  totals$SLG <- totals$TB / pmax(totals$AB, 1)
  totals$OPS <- totals$OBP + totals$SLG
  totals <- totals[totals$PA >= minimum_pa, , drop = FALSE]

  metric_columns <- c("AVG", "OBP", "SLG", "OPS", "H", "HR", "RBI", "R", "SB")
  grouped <- split(seq_len(nrow(totals)), paste(totals$yearID, totals$lgID, sep = "\034"))
  for (metric in metric_columns) {
    percentile <- rep(NA_real_, nrow(totals))
    for (indices in grouped) percentile[indices] <- .mvp_pct(totals[[metric]][indices])
    totals[[paste0(tolower(metric), "_percentile")]] <- percentile
  }

  winner_filter <- tolower(trimws(as.character(awards_players$awardID))) == "most valuable player"
  winners <- unique(awards_players[winner_filter & awards_players$lgID %in% c("AL", "NL"),
    c("playerID", "yearID", "lgID"), drop = FALSE])
  key <- paste(totals$playerID, totals$yearID, totals$lgID, sep = "\034")
  winner_key <- paste(winners$playerID, winners$yearID, winners$lgID, sep = "\034")
  winner_rows <- totals[match(winner_key, key), , drop = FALSE]
  winner_rows <- winner_rows[stats::complete.cases(winner_rows[, c("playerID", "yearID", "lgID")]), , drop = FALSE]
  winner_rows$decade <- floor(winner_rows$yearID / 10) * 10

  percentile_columns <- paste0(tolower(metric_columns), "_percentile")
  profiles <- do.call(rbind, lapply(split(winner_rows, winner_rows$decade), function(rows) {
    means <- vapply(percentile_columns, function(column) mean(rows[[column]], na.rm = TRUE), numeric(1))
    data.frame(
      decade = rows$decade[[1L]],
      metric = sub("_percentile$", "", names(means)),
      winner_mean_percentile = round(100 * means, 1),
      winner_median_percentile = round(100 * vapply(percentile_columns, function(column) stats::median(rows[[column]], na.rm = TRUE), numeric(1)), 1),
      mvp_seasons = length(unique(rows$yearID)),
      winner_rows = nrow(rows),
      stringsAsFactors = FALSE
    )
  }))
  profiles$influence_score <- pmax(profiles$winner_mean_percentile - 50, 0)
  profiles <- profiles[order(profiles$decade, -profiles$influence_score), , drop = FALSE]

  modern_decade <- max(profiles$decade[profiles$decade <= 2020], na.rm = TRUE)
  modern <- profiles[profiles$decade == modern_decade, , drop = FALSE]
  modern$relative_weight <- modern$influence_score / sum(modern$influence_score)
  weights <- data.frame(
    metric = c("war", modern$metric),
    model_weight = c(0.35, 0.65 * modern$relative_weight),
    source_decade = modern_decade,
    basis = c("modern total-value anchor", rep("mean MVP league percentile above 50", nrow(modern))),
    stringsAsFactors = FALSE
  )
  weights$model_weight <- weights$model_weight / sum(weights$model_weight)
  list(winners = tibble::as_tibble(winner_rows), profiles = tibble::as_tibble(profiles), weights = tibble::as_tibble(weights))
}
