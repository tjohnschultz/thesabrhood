suppressPackageStartupMessages(library(dplyr))

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
configured_source_date <- trimws(Sys.getenv("SABRHOOD_PBP_END", unset = ""))
configured_edition_date <- trimws(Sys.getenv("SABRHOOD_DATE", unset = ""))
source_through <- as.Date(if (nzchar(configured_source_date)) configured_source_date else Sys.Date() - 1)
edition_date <- as.Date(if (nzchar(configured_edition_date)) configured_edition_date else Sys.Date())
generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

read_product <- function(name, required = FALSE) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) {
    if (required) stop("Missing newsletter input: ", path, call. = FALSE)
    return(data.frame())
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

number <- function(value) suppressWarnings(as.numeric(as.character(value)))
text <- function(value, fallback = "") {
  value <- as.character(value)
  value[is.na(value) | !nzchar(value)] <- fallback
  value
}
rate <- function(value, digits = 1L) {
  paste0(format(round(100 * number(value), digits), nsmall = digits), "%")
}
decimal <- function(value, digits = 3L) {
  format(round(number(value), digits), nsmall = digits)
}
integer <- function(value) format(round(number(value)), big.mark = ",", scientific = FALSE)

weekday <- as.integer(format(edition_date, "%u"))
themes <- data.frame(
  weekday = 1:7,
  theme_id = c(
    "standings", "contact", "pitching", "future",
    "awards", "game-day", "week-in-review"
  ),
  theme_title = c(
    "The shape of the races", "Contact that changes the scoreboard",
    "Arsenals and pitching pressure", "The next wave",
    "Awards and separation", "The game-day board", "The week in review"
  ),
  theme_deck = c(
    "Division movement, run differential, and the clubs changing their October position.",
    "Exit velocity, plate discipline, and the hitters creating the loudest tracked contact.",
    "Velocity, pitch shape, workload, and the arms changing how games are managed.",
    "Triple-A team strength, call-up readiness, and the parent-club openings that matter.",
    "The statistical cases driving MVP, Cy Young, rookie, and reliever races.",
    "Probabilities, posted information, and the matchups most likely to define today.",
    "A cross-section of the strongest signals produced by The SABRhood this week."
  ),
  stringsAsFactors = FALSE
)
theme <- themes[themes$weekday == weekday, , drop = FALSE]

story <- function(story_id, lane, subject, team, headline, deck, evidence,
                  priority, page_link, source_date = source_through) {
  data.frame(
    story_id = as.character(story_id),
    lane = as.character(lane),
    subject = as.character(subject),
    team = as.character(team),
    headline = as.character(headline),
    deck = as.character(deck),
    evidence = as.character(evidence),
    priority = pmax(0, pmin(100, number(priority))),
    page_link = as.character(page_link),
    source_date = as.character(source_date),
    stringsAsFactors = FALSE
  )
}

candidates <- list()
add_candidate <- function(value) {
  if (is.data.frame(value) && nrow(value)) candidates[[length(candidates) + 1L]] <<- value
}

movement <- read_product("mlb-standings-movement.csv")
if (nrow(movement)) {
  movement <- movement[order(-number(movement$movement_score)), , drop = FALSE]
  for (index in seq_len(min(4L, nrow(movement)))) {
    row <- movement[index, , drop = FALSE]
    rank_change <- number(row$division_rank_change)
    gap_change <- number(row$games_back_change)
    headline <- if (rank_change > 0) {
      paste0(row$team, " climbed ", integer(rank_change), " place",
        ifelse(rank_change == 1, "", "s"), " in the ", row$division_label, " race")
    } else if (gap_change > 0) {
      paste0(row$team, " erased ", decimal(gap_change, 1), " games in the ", row$division_label)
    } else {
      paste0("The ", row$team, " are changing the shape of the ", row$division_label)
    }
    add_candidate(story(
      paste0("standings-", row$team_id), "standings", row$team, row$team,
      headline,
      paste0(
        "The club is ", row$wins, "-", row$losses, " with a ",
        ifelse(number(row$run_differential) >= 0, "+", ""), row$run_differential,
        " run differential."
      ),
      paste0(
        "Seven-day run-differential change: ",
        ifelse(number(row$run_differential_change) >= 0, "+", ""),
        row$run_differential_change, "."
      ),
      72 + 4 * pmin(abs(number(row$movement_score)), 6),
      "standings.html",
      source_through
    ))
  }
}

hitter_tracking <- read_product("hitter-tracking-totals.csv")
if (nrow(hitter_tracking)) {
  hitter_tracking <- hitter_tracking[
    order(-number(hitter_tracking$batted_balls_100_plus)),
    ,
    drop = FALSE
  ]
  for (index in seq_len(min(3L, nrow(hitter_tracking)))) {
    row <- hitter_tracking[index, , drop = FALSE]
    add_candidate(story(
      paste0("contact-", row$batter_id), "contact", row$player_name, row$team,
      paste0(row$player_name, " has produced ", integer(row$batted_balls_100_plus), " batted balls at 100+ mph"),
      paste0(
        "That is ", rate(row$batted_balls_100_plus_rate),
        " of his tracked contact, with a ", decimal(row$max_exit_velocity, 1), " mph maximum."
      ),
      paste0(integer(row$tracked_batted_balls), " tracked batted balls."),
      75 + 20 * number(row$batted_balls_100_plus_rate),
      "leaderboards.html",
      source_through
    ))
  }
}

pitcher_tracking <- read_product("pitcher-tracking-totals.csv")
if (nrow(pitcher_tracking)) {
  pitcher_tracking <- pitcher_tracking[
    order(-number(pitcher_tracking$pitches_100_plus)),
    ,
    drop = FALSE
  ]
  for (index in seq_len(min(3L, nrow(pitcher_tracking)))) {
    row <- pitcher_tracking[index, , drop = FALSE]
    add_candidate(story(
      paste0("velocity-", row$pitcher_id), "pitching", row$player_name, row$team,
      paste0(row$player_name, " has crossed 100 mph ", integer(row$pitches_100_plus), " times"),
      paste0(
        "The right-now velocity ledger puts ", rate(row$pitches_100_plus_rate),
        " of his tracked pitches in triple digits."
      ),
      paste0("Maximum velocity: ", decimal(row$max_velocity, 1), " mph."),
      76 + 20 * number(row$pitches_100_plus_rate),
      "leaderboards.html",
      source_through
    ))
  }
}

aaa_callups <- read_product("aaa-call-up-radar.csv")
if (nrow(aaa_callups)) {
  aaa_callups <- aaa_callups[order(number(aaa_callups$callup_rank)), , drop = FALSE]
  for (index in seq_len(min(3L, nrow(aaa_callups)))) {
    row <- aaa_callups[index, , drop = FALSE]
    add_candidate(story(
      paste0("aaa-", row$player_id), "future", row$player_name, row$mlb_team,
      paste0(row$player_name, " is the new No. ", row$callup_rank, " name on Call-Up Radar"),
      paste0(
        "Age ", row$age, ", a ", decimal(row$performance_score, 1),
        " Triple-A performance score, and a possible ", row$mlb_need_label,
        " opening with ", row$mlb_team, " drive the case."
      ),
      paste0("Readiness score: ", decimal(row$callup_score, 1), "/100; descriptive, not a transaction probability."),
      70 + 0.2 * number(row$callup_score) + pmax(0, 4 - number(row$callup_rank)),
      "aaa.html",
      source_through
    ))
  }
}

aaa_teams <- read_product("aaa-team-rankings.csv")
if (nrow(aaa_teams)) {
  row <- aaa_teams[order(number(aaa_teams$team_rank)), , drop = FALSE][1L, , drop = FALSE]
  add_candidate(story(
    paste0("aaa-team-", row$team_id), "future", row$full_team, row$full_team,
    paste0(row$full_team, " owns the strongest combined Triple-A profile"),
    paste0(
      "A ", row$wins, "-", row$losses, " record and ",
      ifelse(number(row$run_differential) >= 0, "+", ""), row$run_differential,
      " run differential sit beside the top-five hitter and pitcher talent pools."
    ),
    paste0("Triple-A team score: ", decimal(row$team_strength_score, 1), "/100."),
    82,
    "aaa.html",
    source_through
  ))
}

milestones <- read_product("active-milestone-watch.csv")
if (nrow(milestones)) {
  milestones <- milestones[number(milestones$distance_to_milestone) > 0, , drop = FALSE]
  milestones <- milestones[order(number(milestones$distance_to_milestone), -number(milestones$story_score)), , drop = FALSE]
  for (index in seq_len(min(2L, nrow(milestones)))) {
    row <- milestones[index, , drop = FALSE]
    milestone_label <- gsub("_", " ", row$milestone_stat)
    if (number(row$distance_to_milestone) == 1) {
      singular_labels <- c(
        hits = "hit", runs = "run", home_runs = "home run",
        doubles = "double", rbi = "RBI", stolen_bases = "stolen base",
        strikeouts = "strikeout", wins = "win", saves = "save"
      )
      singular_match <- unname(singular_labels[[row$milestone_stat]])
      milestone_label <- if (is.null(singular_match)) sub("s$", "", milestone_label) else singular_match
    }
    add_candidate(story(
      paste0("milestone-", row$player_id, "-", row$milestone_stat), "history", row$player_name, row$team,
      paste0(row$player_name, " is ", integer(row$distance_to_milestone), " ", milestone_label, " from ", integer(row$milestone_target)),
      "The upcoming-landmark desk connects the current season to the full career record.",
      paste0("Career-to-date total: ", integer(row$career_to_date_value), "."),
      pmin(92, 70 + number(row$story_score) / 5),
      "history.html",
      source_through
    ))
  }
}

awards <- read_product("award-race-current-leaders.csv")
if (nrow(awards)) {
  awards <- awards[number(awards$rank) == 1, , drop = FALSE]
  for (index in seq_len(min(4L, nrow(awards)))) {
    row <- awards[index, , drop = FALSE]
    add_candidate(story(
      paste0("award-", row$league, "-", row$award), "awards", row$player_name, row$team,
      paste0(row$player_name, " holds the current ", row$league, " ", row$award, " performance lead"),
      paste0(row$evidence, "."),
      paste0("Race Rating: ", decimal(row$race_rating, 1), "/100."),
      72 + 0.15 * number(row$race_rating),
      "races.html",
      row$checkpoint_date
    ))
  }
}

projections <- read_product("daily-projections-live.csv")
if (nrow(projections)) {
  feature <- projections[as.logical(projections$feature_game), , drop = FALSE]
  if (!nrow(feature)) feature <- projections[order(-number(projections$winner_probability)), , drop = FALSE][1L, , drop = FALSE]
  feature <- feature[1L, , drop = FALSE]
  add_candidate(story(
    paste0("projection-", feature$game_id), "game-day",
    paste(feature$away_team, feature$home_team, sep = " at "), feature$projected_winner,
    paste0(feature$away_team, " at ", feature$home_team, " anchors today's simulation board"),
    paste0(
      feature$projected_winner, " carries a ", rate(feature$winner_probability),
      " development-model win share in a ", decimal(feature$mean_total_runs, 1), "-run environment."
    ),
    paste0("One-run probability: ", rate(feature$one_run_probability), "; calibration still pending."),
    86,
    "projections.html",
    feature$game_date
  ))
}

queue <- read_product("daily-story-queue.csv")
if (nrow(queue)) {
  queue <- queue[order(number(queue$queue_rank)), , drop = FALSE]
  for (index in seq_len(min(5L, nrow(queue)))) {
    row <- queue[index, , drop = FALSE]
    add_candidate(story(
      paste0("engine-", row$story_id), "intelligence", row$subject, row$team,
      row$headline, row$evidence, paste0("Story Engine score: ", decimal(row$story_score, 1), "/100."),
      0.80 * number(row$story_score),
      text(row$destination, "story-desk.html"),
      source_through
    ))
  }
}

stories <- dplyr::bind_rows(candidates)
if (!nrow(stories)) stop("No newsletter candidates were built.", call. = FALSE)
theme_lanes <- switch(
  theme$theme_id,
  standings = c("standings"),
  contact = c("contact"),
  pitching = c("pitching"),
  future = c("future"),
  awards = c("awards"),
  `game-day` = c("game-day"),
  `week-in-review` = c("standings", "contact", "pitching", "future", "awards", "game-day"),
  character()
)
stories$theme_match <- stories$lane %in% theme_lanes
stories$editorial_score <- round(pmin(100, stories$priority + ifelse(stories$theme_match, 12, 0)), 1)
stories <- stories[order(-stories$editorial_score, stories$lane, stories$subject), , drop = FALSE]
stories <- stories[!duplicated(tolower(stories$subject)), , drop = FALSE]

lane_count <- base::integer()
names(lane_count) <- character()
selected <- logical(nrow(stories))
for (index in seq_len(nrow(stories))) {
  lane <- stories$lane[[index]]
  current_count <- if (lane %in% names(lane_count)) lane_count[[lane]] else 0L
  if (current_count < 2L && sum(selected) < 12L) {
    selected[[index]] <- TRUE
    lane_count[[lane]] <- current_count + 1L
  }
}
stories <- stories[selected, , drop = FALSE]
stories$newsletter_rank <- seq_len(nrow(stories))
stories$placement <- ifelse(
  stories$newsletter_rank == 1L,
  "lead",
  ifelse(stories$newsletter_rank <= 4L, "feature", "brief")
)
stories$edition_date <- as.character(edition_date)
stories$theme_id <- theme$theme_id
stories$source_through <- as.character(source_through)
stories$generated_at_utc <- generated_at
stories$newsletter_method <- "deterministic_multi_lane_daily_editor_v1"

edition <- data.frame(
  edition_date = as.character(edition_date),
  weekday = format(edition_date, "%A"),
  theme_id = theme$theme_id,
  theme_title = theme$theme_title,
  theme_deck = theme$theme_deck,
  lead_headline = stories$headline[[1L]],
  lead_subject = stories$subject[[1L]],
  story_count = nrow(stories),
  lane_count = length(unique(stories$lane)),
  source_through = as.character(source_through),
  generated_at_utc = generated_at,
  newsletter_method = "deterministic_multi_lane_daily_editor_v1",
  stringsAsFactors = FALSE
)

utils::write.csv(stories, file.path(output_dir, "daily-newsletter-stories.csv"), row.names = FALSE, na = "")
utils::write.csv(edition, file.path(output_dir, "daily-newsletter-edition.csv"), row.names = FALSE, na = "")
cat(
  "Built", edition$weekday, "edition:", edition$theme_title, "with", nrow(stories),
  "stories across", edition$lane_count, "editorial lanes.\n"
)
