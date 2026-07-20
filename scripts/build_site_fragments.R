site_root <- normalizePath(".", winslash = "/")
data_dir <- file.path(site_root, "data", "derived")
include_dir <- file.path(site_root, "includes")
dir.create(include_dir, recursive = TRUE, showWarnings = FALSE)

read_product <- function(name) {
  path <- file.path(data_dir, name)
  if (!file.exists(path)) stop("Missing derived site product: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}

decode_unicode_tokens <- function(value) {
  value <- as.character(value)
  vapply(value, function(item) {
    if (is.na(item)) return(NA_character_)
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
    item
  }, character(1), USE.NAMES = FALSE)
}

html_escape <- function(value) {
  value <- decode_unicode_tokens(value)
  value[is.na(value)] <- ""
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

num <- function(value) suppressWarnings(as.numeric(value))
fmt_int <- function(value) format(round(num(value)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_rate <- function(value, digits = 1L) {
  ifelse(is.finite(num(value)), paste0(format(round(100 * num(value), digits), nsmall = digits), "%"), "—")
}
fmt_dec <- function(value, digits = 3L) {
  ifelse(is.finite(num(value)), format(round(num(value), digits), nsmall = digits), "—")
}
fmt_score <- function(value) {
  ifelse(is.finite(num(value)), format(round(num(value), 1L), nsmall = 1L), "—")
}
fmt_yes_no <- function(value) ifelse(as.logical(value), "Yes", "No")

write_fragment <- function(name, lines) {
  writeLines(enc2utf8(lines), file.path(include_dir, name), useBytes = TRUE)
}

slugify <- function(value) {
  value <- iconv(as.character(value), from = "", to = "ASCII//TRANSLIT")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("(^-|-$)", "", value)
}

rank_meter <- function(label, rank, detail) {
  width <- pmax(pmin(100 * (31 - num(rank)) / 30, 100), 3)
  paste0(
    '<div class="rank-meter"><div class="rank-meter__label"><span>', html_escape(label), '</span><strong>#',
    html_escape(fmt_int(rank)), '</strong></div><div class="rank-meter__track" role="img" aria-label="',
    html_escape(paste(label, "rank", fmt_int(rank), "of 30")), '"><span style="width:',
    base::format(round(width, 1), nsmall = 1), '%"></span></div><small>', html_escape(detail), '</small></div>'
  )
}

stat_card <- function(kicker, title, value, detail, tone = "navy") {
  paste0(
    '<article class="data-card data-card--', tone, '">',
    '<span class="eyebrow">', html_escape(kicker), '</span>',
    '<h3>', html_escape(title), '</h3>',
    '<div class="data-card__value">', html_escape(value), '</div>',
    '<p>', html_escape(detail), '</p>',
    '</article>'
  )
}

player_card <- function(kicker, name, team, headline, detail, score = NULL) {
  score_html <- if (is.null(score)) "" else paste0(
    '<span class="signal-score" aria-label="Signal score ', html_escape(score), '">',
    html_escape(score), '</span>'
  )
  paste0(
    '<article class="player-signal">',
    '<div class="player-signal__top"><span class="eyebrow">', html_escape(kicker), '</span>', score_html, '</div>',
    '<h3>', html_escape(name), '</h3>',
    '<p class="player-signal__team">', html_escape(team), '</p>',
    '<p><strong>', html_escape(headline), '</strong></p>',
    '<p class="muted">', html_escape(detail), '</p>',
    '</article>'
  )
}

meter_bar <- function(label, value, ceiling = 0.70, formatter = fmt_rate, highlight = FALSE) {
  width <- pmax(pmin(100 * num(value) / ceiling, 100), 0)
  paste0(
    '<div class="metric-bar', if (highlight) ' is-highlight' else '', '">',
    '<div class="metric-bar__label"><span>', html_escape(label), '</span><strong>', html_escape(formatter(value)), '</strong></div>',
    '<div class="metric-bar__track" role="img" aria-label="', html_escape(paste(label, formatter(value))), '">',
    '<span style="width:', base::format(round(width, 1), nsmall = 1), '%"></span></div></div>'
  )
}

pitch_identity_card <- function(row) {
  x <- pmax(pmin(50 + 2 * num(row$average_horizontal_break), 95), 5)
  y <- pmax(pmin(20 + 2.8 * num(row$average_induced_vertical_break), 95), 5)
  movement_label <- paste(
    fmt_dec(row$average_horizontal_break, 1L), "inches horizontal and",
    fmt_dec(row$average_induced_vertical_break, 1L), "inches induced vertical break"
  )
  paste0(
    '<article class="pitch-identity-card"><div class="pitch-identity-card__head"><div><span class="eyebrow">',
    html_escape(row$pitch_family), ' identity</span><h3>', html_escape(row$player_name), '</h3><p>',
    html_escape(row$team), ' | ', html_escape(row$pitch_name), '</p></div><span class="signal-score">',
    html_escape(fmt_score(row$pitch_quality_score)), '</span></div>',
    '<div class="pitch-identity-card__body"><div class="movement-plane" role="img" aria-label="', html_escape(movement_label), '">',
    '<span class="movement-plane__pitch" style="left:', format(round(x, 1), nsmall = 1), '%;bottom:', format(round(y, 1), nsmall = 1), '%"></span>',
    '<span class="movement-plane__x">Horizontal break</span><span class="movement-plane__y">IVB</span></div>',
    '<div class="pitch-metrics">',
    meter_bar("Whiff", row$whiff_rate, 0.70),
    meter_bar("Chase", row$chase_rate, 0.55),
    meter_bar("Usage", row$usage_rate, 0.60),
    '<div class="pitch-velo"><span>Average velocity</span><strong>', html_escape(fmt_dec(row$average_velocity, 1L)), ' mph</strong></div>',
    '</div></div></article>'
  )
}

matchup_edge_card <- function(row) {
  left_strong <- row$stronger_opponent_hand == "L"
  paste0(
    '<article class="matchup-edge-card"><div class="player-signal__top"><span class="eyebrow">',
    html_escape(paste(row$perspective, "edge")), '</span><span class="signal-score">', html_escape(fmt_score(row$matchup_edge_score)), '</span></div>',
    '<h3>', html_escape(row$player_name), '</h3><p class="player-signal__team">', html_escape(row$team), '</p>',
    '<p><strong>', html_escape(row$headline), '</strong></p>',
    '<div class="matchup-bars">',
    meter_bar("vs L", row$woba_vs_l, 0.55, fmt_dec, left_strong),
    meter_bar("vs R", row$woba_vs_r, 0.55, fmt_dec, !left_strong),
    '</div><p class="muted">', html_escape(row$evidence), '</p></article>'
  )
}

render_table <- function(data, columns, labels, formatters = list(), table_class = "data-table") {
  header <- paste0("<th>", html_escape(labels), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(i) {
    cells <- vapply(seq_along(columns), function(j) {
      column <- columns[[j]]
      value <- data[[column]][[i]]
      formatter <- formatters[[column]]
      shown <- if (is.null(formatter)) html_escape(value) else html_escape(formatter(value))
      paste0("<td>", shown, "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0(
    '<div class="table-scroll"><table class="', table_class, '"><thead><tr>', header,
    '</tr></thead><tbody>', paste0(rows, collapse = ""), '</tbody></table></div>'
  )
}

frontmatter_value <- function(lines, key, default = "") {
  hit <- grep(paste0("^", key, ":"), lines, value = TRUE)
  if (!length(hit)) return(default)
  value <- trimws(sub(paste0("^", key, ":"), "", hit[[1L]]))
  gsub('^\"|\"$', "", value)
}

article_descriptions <- c(
  "durbin_article" = "There is a heat wave in Boston, centralized entirely in Caleb Durbin's bat.",
  "bello_article_final" = "A pitch-level look at Brayan Bello's arsenal, results, and changing approach.",
  "A.J Ewing Gets the Call" = "Can an early-season call-up change the direction of a club searching for a spark?",
  "Series Recap Tigers Sox" = "A game-by-game research rundown of the matchups and turning points at Fenway Park.",
  "CleanPig" = "Garrett Crochet has not had his usual filth. The pitch traits show what changed.",
  "sorianopreseason26" = "Jose Soriano's arsenal and the risk embedded in the Angels' rotation.",
  "ceddannesnewgroove" = "How Ceddanne Rafaela's swing decisions and batted-ball profile were changing."
)
article_categories <- c(
  "durbin_article" = "Red Sox | Hitting",
  "bello_article_final" = "Red Sox | Pitching",
  "A.J Ewing Gets the Call" = "Prospects | Call-up",
  "Series Recap Tigers Sox" = "Red Sox | Series",
  "CleanPig" = "Red Sox | Pitching",
  "sorianopreseason26" = "Angels | Pitching",
  "ceddannesnewgroove" = "Red Sox | Hitting"
)
article_fallback_images <- c(
  "CleanPig" = "images/crochet.png",
  "Series Recap Tigers Sox" = "images/SABRHOODpng.png",
  "sorianopreseason26" = "images/Soriano.jpg"
)

build_article_index <- function() {
  paths <- list.files(file.path(site_root, "posts"), pattern = "\\.qmd$", full.names = TRUE)
  paths <- paths[basename(paths) != "fla 2025 v 2026 article.qmd"]
  rows <- lapply(paths, function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    closing <- which(lines == "---")
    front <- if (length(closing) >= 2L) lines[seq.int(2L, closing[[2L]] - 1L)] else lines
    stem <- tools::file_path_sans_ext(basename(path))
    image_path <- frontmatter_value(front, "image", article_fallback_images[[stem]])
    image_path <- sub("^\\.\\./", "", image_path)
    if (!nzchar(image_path) || !file.exists(file.path(site_root, image_path))) {
      image_path <- article_fallback_images[[stem]]
    }
    if (is.null(image_path) || !nzchar(image_path) || !file.exists(file.path(site_root, image_path))) {
      image_path <- "images/thesabrhood2clean.png"
    }
    description <- frontmatter_value(front, "description", article_descriptions[[stem]])
    if (is.null(description) || !nzchar(description)) description <- "Original baseball research from The SABRhood."
    category <- article_categories[[stem]]
    if (is.null(category)) category <- "Research"
    data.frame(
      stem = stem,
      title = frontmatter_value(front, "title", stem),
      date = as.Date(frontmatter_value(front, "date", "1900-01-01")),
      description = description,
      category = category,
      image = image_path,
      href = paste0("posts/", utils::URLencode(stem, reserved = TRUE), ".html"),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output[order(output$date, decreasing = TRUE), , drop = FALSE]
}

article_card <- function(article, featured = FALSE) {
  class_name <- if (isTRUE(featured)) "article-feature" else "article-card"
  paste0(
    '<article class="', class_name, '"><a class="article-card__image" href="', html_escape(article$href), '">',
    '<img src="', html_escape(article$image), '" alt="Artwork for ', html_escape(article$title), '" loading="lazy"></a>',
    '<div class="article-card__copy"><span class="article-card__category">', html_escape(article$category), '</span>',
    '<h2><a href="', html_escape(article$href), '">', html_escape(article$title), '</a></h2>',
    '<p>', html_escape(article$description), '</p>',
    '<div class="article-card__meta"><time datetime="', html_escape(as.character(article$date)), '">',
    html_escape(format(article$date, "%B %d, %Y")), '</time><a href="', html_escape(article$href), '">Read story <span aria-hidden="true">&rarr;</span></a></div>',
    '</div></article>'
  )
}

hitters <- read_product("hitter-performance-summary.csv")
pitchers <- read_product("pitcher-performance-summary.csv")
hitter_form <- read_product("hitter-recent-form.csv")
pitcher_form <- read_product("pitcher-recent-form.csv")
hitter_platoon <- read_product("hitter-platoon-summary.csv")
pitcher_platoon <- read_product("pitcher-platoon-summary.csv")
pitch_types <- read_product("pitch-type-summary.csv")
historical <- read_product("historical-anniversary-notes.csv")
historical_milestones <- read_product("historical-milestone-notes.csv")
historical_profiles <- read_product("historical-player-profiles.csv")
active_milestones <- read_product("active-milestone-watch.csv")
offensive_race <- read_product("offensive-race-board.csv")
prevention_race <- read_product("run-prevention-race-board.csv")
team_intelligence <- read_product("team-intelligence-summary.csv")
story_queue <- read_product("daily-story-queue.csv")
hitter_matchups <- read_product("hitter-matchup-edges.csv")
pitcher_matchups <- read_product("pitcher-matchup-edges.csv")
signature_pitches <- read_product("signature-pitch-board.csv")
team_broadcast_notes <- read_product("team-broadcast-notes.csv")
re24 <- read_product("run-expectancy-24.csv")
bullpen <- read_product("bullpen-availability.csv")
manager <- read_product("manager-data-summary.csv")
manager_model <- read_product("manager-hook-model.csv")
hook_validation <- read_product("manager-hook-validation-metrics.csv")
hook_calibration <- read_product("manager-hook-calibration.csv")
hook_scenarios <- read_product("manager-hook-scenarios.csv")
bullpen_matchups <- read_product("bullpen-matchup-selector.csv")
articles <- build_article_index()

updated_date <- max(as.Date(hitters$last_game), na.rm = TRUE)
updated_label <- format(updated_date, "%B %d, %Y")

top_hitter_form <- hitter_form[order(-num(hitter_form$form_score), -num(hitter_form$recent_pa)), ][1:6, ]
top_pitcher_form <- pitcher_form[order(-num(pitcher_form$form_score), -num(pitcher_form$recent_pa)), ][1:6, ]
ops_leaders <- hitters[order(-num(hitters$ops), -num(hitters$pa)), ][1:10, ]
woba_leaders <- hitters[order(-num(hitters$woba_estimate), -num(hitters$pa)), ][1:10, ]
pitcher_suppressors <- pitchers[order(num(pitchers$ops), -num(pitchers$pa)), ][1:10, ]
arsenal_whiffs <- pitch_types[num(pitch_types$swings) >= 50, ]
arsenal_whiffs <- arsenal_whiffs[order(-num(arsenal_whiffs$whiff_rate), -num(arsenal_whiffs$pitches)), ][1:10, ]

home_cards <- c(
  stat_card("Data through", "Season pulse", updated_label, paste(fmt_int(nrow(hitters)), "qualified hitter profiles"), "red"),
  stat_card("RE24", "Highest run state", "2.469", "Bases loaded, zero outs", "navy"),
  stat_card("Decision lab", "Observed hooks", fmt_int(manager$rows[manager$dataset == "observed_pitcher_hooks"]), "Pitching changes available for modeling", "steel"),
  stat_card("Pitch lab", "Arsenal rows", fmt_int(nrow(pitch_types)), "Pitch-type profiles with sequence context", "red")
)
home_signals <- c(
  player_card(
    "Hitter rising", top_hitter_form$player_name[[1]], top_hitter_form$team[[1]],
    paste("Recent OPS", fmt_dec(top_hitter_form$recent_ops[[1]])),
    paste("Prior baseline", fmt_dec(top_hitter_form$baseline_ops[[1]]), "across", fmt_int(top_hitter_form$baseline_pa[[1]]), "PA"),
    fmt_score(top_hitter_form$form_score[[1]])
  ),
  player_card(
    "Pitcher rising", top_pitcher_form$player_name[[1]], top_pitcher_form$team[[1]],
    paste("Recent OPS allowed", fmt_dec(top_pitcher_form$recent_ops[[1]])),
    paste("Prior baseline", fmt_dec(top_pitcher_form$baseline_ops[[1]]), "across", fmt_int(top_pitcher_form$baseline_pa[[1]]), "PA"),
    fmt_score(top_pitcher_form$form_score[[1]])
  ),
  player_card(
    "On this date", historical$subject_name[[1]], paste(historical$years_ago[[1]], "years ago"),
    historical$headline[[1]], historical$body[[1]], fmt_score(historical$story_score[[1]])
  ),
  player_card(
    "Milestone watch", active_milestones$player_name[[1]], active_milestones$team[[1]],
    active_milestones$headline[[1]],
    paste("Career estimate", fmt_int(active_milestones$career_to_date_value[[1]]), "|", active_milestones$milestone_stat[[1]]),
    fmt_score(active_milestones$story_score[[1]])
  )
)
write_fragment("home-snapshot.html", c(
  '<section class="section-heading"><span class="eyebrow">Live analytical snapshot</span><h2>What the numbers are saying</h2><p>Compact, auditable products generated from the SABRhood analytics engine.</p></section>',
  paste0('<div class="data-card-grid">', paste0(home_cards, collapse = ""), '</div>'),
  '<section class="section-heading section-heading--tight"><span class="eyebrow">Signal desk</span><h2>Three stories worth your attention</h2></section>',
  paste0('<div class="signal-grid">', paste0(home_signals, collapse = ""), '</div>')
))

write_fragment("article-listing.html", c(
  '<section class="article-desk">',
  article_card(articles[1, , drop = FALSE], featured = TRUE),
  '<div class="article-grid">',
  vapply(seq.int(2L, nrow(articles)), function(index) article_card(articles[index, , drop = FALSE]), character(1)),
  '</div></section>'
))

write_fragment("home-research.html", c(
  '<section class="section-heading"><span class="eyebrow">From the research desk</span><h2>Stories built from the numbers</h2><p>Original reporting, pitch studies, player development, and series analysis.</p></section>',
  '<div class="article-grid article-grid--home">',
  vapply(seq_len(min(3L, nrow(articles))), function(index) article_card(articles[index, , drop = FALSE]), character(1)),
  '</div>',
  '<div class="section-action"><a class="btn btn-metallic" href="blog.html">Open the full article archive</a></div>'
))

category_labels <- c(
  hitter_form = "Hitter form", pitcher_form = "Pitcher form",
  offensive_race = "Offensive race", run_prevention = "Run prevention",
  milestone = "Milestone", history = "On this date", team = "Team intelligence",
  pitch_identity = "Pitch identity"
)
story_shortlist <- story_queue[as.logical(story_queue$daily_shortlist), , drop = FALSE]
story_shortlist$lane_label <- unname(category_labels[story_shortlist$category])
story_lineup <- story_shortlist[!duplicated(story_shortlist$category), , drop = FALSE]
story_lineup <- story_lineup[seq_len(min(8L, nrow(story_lineup))), , drop = FALSE]
story_cards <- vapply(seq_len(nrow(story_lineup)), function(index) {
  player_card(
    category_labels[[story_lineup$category[[index]]]],
    story_lineup$subject[[index]], story_lineup$team[[index]],
    story_lineup$headline[[index]], story_lineup$evidence[[index]],
    fmt_score(story_lineup$story_score[[index]])
  )
}, character(1))
write_fragment("story-desk.html", c(
  '<div class="update-strip"><strong>Eight reporting lanes</strong><span>One leading candidate from each analytical category prevents a single metric from owning the news cycle.</span></div>',
  '<section class="section-heading"><span class="eyebrow">Editor\'s lineup</span><h2>The stories the data desk would assign today</h2><p>Form, races, milestones, history, team movement, and pitch identity compete inside their own lanes before the daily lineup is assembled.</p></section>',
  '<div class="signal-grid signal-grid--four">', story_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Shortlist</span><h2>The next 16 reporting leads</h2><p>Scores order research attention. Publication still requires editorial review and source verification.</p></div>',
  render_table(utils::head(story_shortlist, 16L), c("queue_rank", "lane_label", "subject", "team", "headline", "story_score", "confidence"),
    c("Queue", "Lane", "Subject", "Team", "Reporting lead", "Score", "Confidence"),
    list(queue_rank = fmt_int, story_score = fmt_score, confidence = fmt_rate), "data-table story-queue-table"),
  '</section>',
  '<div class="method-callout"><strong>Why this matters:</strong> the Story Engine is the bridge between statistical detection and journalism. It creates an assignment queue, not automated finished prose.</div>'
))
write_fragment("home-story-desk.html", c(
  '<section class="section-heading"><span class="eyebrow">The assignment desk</span><h2>Eight different ways to find tomorrow\'s baseball story</h2><p>The daily queue protects editorial variety while preserving the evidence behind every lead.</p></section>',
  '<div class="signal-grid signal-grid--four">', utils::head(story_cards, 4L), '</div>',
  '<div class="section-action"><a class="btn btn-metallic" href="story-desk.html">Open the complete Story Engine</a></div>'
))

hitter_matchup_spotlights <- hitter_matchups[seq_len(min(6L, nrow(hitter_matchups))), ]
pitcher_matchup_spotlights <- pitcher_matchups[seq_len(min(6L, nrow(pitcher_matchups))), ]
hitter_matchup_cards <- vapply(seq_len(nrow(hitter_matchup_spotlights)), function(index) matchup_edge_card(hitter_matchup_spotlights[index, , drop = FALSE]), character(1))
pitcher_matchup_cards <- vapply(seq_len(nrow(pitcher_matchup_spotlights)), function(index) matchup_edge_card(pitcher_matchup_spotlights[index, , drop = FALSE]), character(1))
write_fragment("matchup-edges.html", c(
  '<section class="section-heading"><span class="eyebrow">Hitter matchup edges</span><h2>Where the damage profile changes most</h2><p>Two-sided samples only: every card requires at least 40 plate appearances against both left- and right-handed pitching.</p></section>',
  '<div class="signal-grid">', hitter_matchup_cards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Pitcher matchup edges</span><h2>Which side is each arm suppressing?</h2><p>For pitchers, the highlighted side is the opponent hand with the lower estimated wOBA allowed.</p></section>',
  '<div class="signal-grid">', pitcher_matchup_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Full hitter board</span><h2>Largest qualified platoon gaps</h2></div>',
  render_table(utils::head(hitter_matchups, 20L), c("matchup_edge_rank", "player_name", "team", "pa_vs_l", "woba_vs_l", "pa_vs_r", "woba_vs_r", "woba_gap", "stronger_opponent_hand", "matchup_edge_score"),
    c("Rank", "Hitter", "Team", "PA vs L", "wOBA vs L", "PA vs R", "wOBA vs R", "Gap", "Edge vs", "Score"),
    list(matchup_edge_rank = fmt_int, pa_vs_l = fmt_int, woba_vs_l = fmt_dec, pa_vs_r = fmt_int, woba_vs_r = fmt_dec, woba_gap = fmt_dec, matchup_edge_score = fmt_score)),
  '</section>',
  '<div class="method-callout"><strong>Use the split, not the stereotype:</strong> handedness is a reporting starting point. Pitch type, release point, command, park, and sample stability still shape the actual matchup.</div>'
))

icon_profiles <- historical_profiles[order(-num(historical_profiles$career_significance_score)), ]
icon_profiles <- icon_profiles[seq_len(min(15L, nrow(icon_profiles))), ]
milestone_spotlights <- historical_milestones[!duplicated(historical_milestones$subject_id), ]
milestone_spotlights <- milestone_spotlights[seq_len(min(8L, nrow(milestone_spotlights))), ]
anniversary_spotlights <- historical[seq_len(min(8L, nrow(historical))), ]
active_milestone_spotlights <- active_milestones[seq_len(min(8L, nrow(active_milestones))), ]

anniversary_cards <- vapply(seq_len(nrow(anniversary_spotlights)), function(index) {
  player_card(
    paste(anniversary_spotlights$recognition_tier[[index]], "anniversary"),
    anniversary_spotlights$subject_name[[index]],
    paste(anniversary_spotlights$years_ago[[index]], "years ago"),
    anniversary_spotlights$headline[[index]],
    anniversary_spotlights$career_summary[[index]],
    fmt_score(anniversary_spotlights$story_score[[index]])
  )
}, character(1))

milestone_cards <- vapply(seq_len(nrow(milestone_spotlights)), function(index) {
  player_card(
    paste(milestone_spotlights$recognition_tier[[index]], "career"),
    milestone_spotlights$subject_name[[index]],
    if (isTRUE(milestone_spotlights$record_flag[[index]])) paste("Career rank", fmt_int(milestone_spotlights$career_rank[[index]])) else "Career milestone",
    milestone_spotlights$headline[[index]],
    milestone_spotlights$body[[index]],
    fmt_score(milestone_spotlights$story_score[[index]])
  )
}, character(1))

active_milestone_cards <- vapply(seq_len(nrow(active_milestone_spotlights)), function(index) {
  status <- if (active_milestone_spotlights$milestone_status[[index]] == "reached_this_season") "Reached in 2026" else "Closing in"
  player_card(
    paste(active_milestone_spotlights$role[[index]], "milestone"),
    active_milestone_spotlights$player_name[[index]],
    active_milestone_spotlights$team[[index]],
    active_milestone_spotlights$headline[[index]],
    paste(status, "| career estimate", fmt_int(active_milestone_spotlights$career_to_date_value[[index]])),
    fmt_score(active_milestone_spotlights$story_score[[index]])
  )
}, character(1))

write_fragment("history-desk.html", c(
  '<div class="history-scoreboard">',
  stat_card("Career profiles", "Recognizable players", fmt_int(nrow(historical_profiles)), "Historical careers above the public significance threshold.", "navy"),
  stat_card("Recognition", "Icons", fmt_int(sum(historical_profiles$recognition_tier == "icon")), "Hall of Famers, record holders, and historically dominant careers.", "red"),
  stat_card("Story inventory", "Milestone notes", fmt_int(nrow(historical_milestones)), "Career clubs and top-ten leaderboard context.", "steel"),
  stat_card("Today", "Anniversary candidates", fmt_int(nrow(historical)), "Debuts and final appearances ranked for editorial review.", "navy"),
  '</div>',
  '<section class="section-heading"><span class="eyebrow">On this date</span><h2>The recognizable names rise first</h2><p>Career significance, awards, Hall of Fame status, record standing, and broadcast value now influence the daily queue.</p></section>',
  '<div class="signal-grid signal-grid--four">', anniversary_cards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Active milestone watch</span><h2>Career landmarks moving right now</h2><p>Prior Lahman career totals through 2025 plus current 2026 summaries surface the players who have reached or are approaching recognizable clubs.</p></section>',
  '<div class="signal-grid signal-grid--four">', active_milestone_cards, '</div>',
  '<div class="method-callout"><strong>Identity note:</strong> this prototype uses unique normalized-name matching between MLBAM summaries and Lahman. Ambiguous duplicate names are rejected, and every published row carries the provisional match method.</div>',
  '<section class="section-heading"><span class="eyebrow">Milestone vault</span><h2>Career clubs and record context</h2><p>Evergreen research candidates built from Lahman career totals. True WAR remains a separate future input.</p></section>',
  '<div class="signal-grid signal-grid--four">', milestone_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Career prominence</span><h2>Historical icons in the recognition model</h2></div>',
  render_table(icon_profiles, c("player_name", "recognition_tier", "career_significance_score", "career_H", "career_HR", "career_W", "career_SO_pitch", "allstar_selections", "hof_inducted"),
    c("Player", "Tier", "Score", "Hits", "HR", "Wins", "Pitching SO", "All-Star", "Hall of Fame"),
    list(career_significance_score = fmt_score, career_H = fmt_int, career_HR = fmt_int, career_W = fmt_int, career_SO_pitch = fmt_int, allstar_selections = fmt_int, hof_inducted = fmt_yes_no)),
  '</section>',
  '<div class="method-callout"><strong>Recognition is context, not a verdict:</strong> the score exists to rank story candidates for broadcasters and editors. Lahman does not supply WAR, so the current version never presents its prominence score as WAR.</div>'
))

today_hitter_cards <- vapply(seq_len(min(4L, nrow(top_hitter_form))), function(i) {
  player_card(
    paste("Hitter", top_hitter_form$form_label[[i]]), top_hitter_form$player_name[[i]], top_hitter_form$team[[i]],
    paste(fmt_dec(top_hitter_form$recent_ops[[i]]), "recent OPS"),
    paste("Change:", fmt_dec(top_hitter_form$ops_delta[[i]]), "|", fmt_int(top_hitter_form$recent_pa[[i]]), "recent PA"),
    fmt_score(top_hitter_form$form_score[[i]])
  )
}, character(1))
today_pitcher_cards <- vapply(seq_len(min(4L, nrow(top_pitcher_form))), function(i) {
  player_card(
    paste("Pitcher", top_pitcher_form$form_label[[i]]), top_pitcher_form$player_name[[i]], top_pitcher_form$team[[i]],
    paste(fmt_dec(top_pitcher_form$recent_ops[[i]]), "recent OPS allowed"),
    paste("Change:", fmt_dec(top_pitcher_form$ops_delta[[i]]), "|", fmt_int(top_pitcher_form$recent_pa[[i]]), "recent PA"),
    fmt_score(top_pitcher_form$form_score[[i]])
  )
}, character(1))
history_cards <- vapply(seq_len(min(4L, nrow(historical))), function(i) {
  player_card(
    "On this date", historical$subject_name[[i]], as.character(historical$historical_date[[i]]),
    historical$headline[[i]], historical$body[[i]], fmt_score(historical$story_score[[i]])
  )
}, character(1))
write_fragment("today-dashboard.html", c(
  paste0('<div class="update-strip"><strong>Data through ', html_escape(updated_label), '</strong><span>Method-labeled signals, not black-box claims.</span></div>'),
  '<section class="section-heading"><span class="eyebrow">Recent form</span><h2>Hitters changing the conversation</h2><p>Fourteen-day performance compared with a non-overlapping prior-season baseline.</p></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(today_hitter_cards, collapse = ""), '</div>'),
  '<section class="section-heading"><span class="eyebrow">Run prevention</span><h2>Arms moving in the right direction</h2></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(today_pitcher_cards, collapse = ""), '</div>'),
  '<section class="section-heading"><span class="eyebrow">History desk</span><h2>On this date</h2><p>Daily Lahman anniversary candidates, ranked for editorial review.</p></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(history_cards, collapse = ""), '</div>')
))

offense_top <- offensive_race[seq_len(min(12L, nrow(offensive_race))), ]
prevention_top <- prevention_race[seq_len(min(12L, nrow(prevention_race))), ]
offense_race_cards <- vapply(seq_len(min(3L, nrow(offense_top))), function(index) {
  player_card(
    paste("Offense #", fmt_int(offense_top$race_rank[[index]])),
    offense_top$player_name[[index]], offense_top$team[[index]],
    paste(fmt_dec(offense_top$ops[[index]]), "OPS |", fmt_dec(offense_top$woba_estimate[[index]]), "estimated wOBA"),
    paste(fmt_int(offense_top$pa[[index]]), "PA |", fmt_rate(offense_top$hard_hit_rate[[index]]), "hard-hit rate"),
    fmt_score(offense_top$race_score[[index]])
  )
}, character(1))
prevention_race_cards <- vapply(seq_len(min(3L, nrow(prevention_top))), function(index) {
  player_card(
    paste("Run prevention #", fmt_int(prevention_top$race_rank[[index]])),
    prevention_top$player_name[[index]], prevention_top$team[[index]],
    paste(fmt_dec(prevention_top$ops[[index]]), "OPS allowed |", fmt_dec(prevention_top$woba_estimate[[index]]), "estimated wOBA"),
    paste(fmt_int(prevention_top$pa[[index]]), "BF |", fmt_rate(prevention_top$strikeout_rate[[index]]), "strikeout rate"),
    fmt_score(prevention_top$race_score[[index]])
  )
}, character(1))
write_fragment("league-races.html", c(
  '<div class="race-disclaimer"><strong>Performance race, not award prediction.</strong><span>These boards describe season-to-date statistical quality. They do not model ballots, position, defense, team record, or official award criteria.</span></div>',
  '<section class="section-heading"><span class="eyebrow">The offensive race</span><h2>Who has built the strongest hitting case?</h2><p>Estimated wOBA, OPS, run value per plate appearance, contact quality, and sample reliability form the transparent composite.</p></section>',
  '<div class="signal-grid">', offense_race_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Offensive board</span><h2>Top 12 season performances</h2></div>',
  render_table(offense_top, c("race_rank", "player_name", "team", "pa", "ops", "woba_estimate", "hard_hit_rate", "run_value_per_pa", "race_score"),
    c("Rank", "Player", "Team", "PA", "OPS", "wOBA est.", "Hard-hit", "RV/PA", "Score"),
    list(race_rank = fmt_int, pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, hard_hit_rate = fmt_rate, run_value_per_pa = fmt_dec, race_score = fmt_score)),
  '</section>',
  '<section class="section-heading"><span class="eyebrow">The run-prevention race</span><h2>Which pitchers are suppressing offense most completely?</h2><p>Opponent estimated wOBA and OPS lead the model, reinforced by strikeouts, contact suppression, and sample reliability.</p></section>',
  '<div class="signal-grid">', prevention_race_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Run-prevention board</span><h2>Top 12 season performances</h2></div>',
  render_table(prevention_top, c("race_rank", "player_name", "team", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate", "race_score"),
    c("Rank", "Pitcher", "Team", "BF", "OPS allowed", "wOBA est.", "K%", "Hard-hit", "Score"),
    list(race_rank = fmt_int, pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, strikeout_rate = fmt_rate, hard_hit_rate = fmt_rate, race_score = fmt_score)),
  '</section>'
))

newsletter_hitter <- top_hitter_form[1, ]
newsletter_pitcher <- top_pitcher_form[1, ]
newsletter_history <- historical[1:min(5L, nrow(historical)), ]
newsletter_history_list <- paste0(
  '<li><strong>', html_escape(newsletter_history$subject_name), '</strong><span>',
  html_escape(newsletter_history$headline), '</span></li>',
  collapse = ""
)
newsletter_radar <- story_lineup[seq_len(min(5L, nrow(story_lineup))), ]
newsletter_radar_list <- paste0(
  '<li><strong>', html_escape(category_labels[newsletter_radar$category]), ': ', html_escape(newsletter_radar$subject), '</strong><span>',
  html_escape(newsletter_radar$headline), '</span></li>',
  collapse = ""
)
write_fragment("newsletter-daily.html", c(
  paste0(
    '<section class="newsletter-mast"><div><span class="eyebrow">Daily edition · ', html_escape(updated_label),
    '</span><h2>The signals changing the MLB conversation</h2><p>A compact morning briefing generated from form, contact quality, pitch data, game context, and baseball history.</p></div>',
    '<div class="newsletter-mast__stamp"><strong>SABR</strong><span>hood daily</span></div></section>'
  ),
  '<div class="newsletter-layout"><main class="newsletter-main">',
  '<section class="newsletter-story newsletter-story--lead"><span class="eyebrow">Lead signal</span>',
  paste0('<h2>', html_escape(newsletter_hitter$player_name[[1]]), ' is forcing a new baseline</h2>'),
  paste0('<p class="newsletter-deck">The ', html_escape(newsletter_hitter$team[[1]]), ' hitter carries a ',
    html_escape(fmt_dec(newsletter_hitter$recent_ops[[1]])), ' OPS across ', html_escape(fmt_int(newsletter_hitter$recent_pa[[1]])),
    ' recent plate appearances, compared with ', html_escape(fmt_dec(newsletter_hitter$baseline_ops[[1]])), ' before the current window.</p>'),
  paste0('<div class="evidence-row"><span><small>Form score</small><strong>', html_escape(fmt_score(newsletter_hitter$form_score[[1]])),
    '</strong></span><span><small>OPS change</small><strong>', html_escape(fmt_dec(newsletter_hitter$ops_delta[[1]])),
    '</strong></span><span><small>Confidence</small><strong>', html_escape(fmt_dec(newsletter_hitter$form_score_confidence[[1]])), '</strong></span></div>'),
  '<p class="method-note">The recent window and prior baseline do not overlap. Form score is a cohort-relative signal, not a rest-of-season projection.</p></section>',
  '<section class="newsletter-story"><span class="eyebrow">Run prevention</span>',
  paste0('<h2>', html_escape(newsletter_pitcher$player_name[[1]]), ' has changed the quality of contact</h2>'),
  paste0('<p>The ', html_escape(newsletter_pitcher$team[[1]]), ' pitcher has allowed a ',
    html_escape(fmt_dec(newsletter_pitcher$recent_ops[[1]])), ' OPS in the recent window. The prior baseline was ',
    html_escape(fmt_dec(newsletter_pitcher$baseline_ops[[1]])), '.</p>'),
  paste0('<div class="evidence-row"><span><small>Recent BF</small><strong>', html_escape(fmt_int(newsletter_pitcher$recent_pa[[1]])),
    '</strong></span><span><small>RV/PA change</small><strong>', html_escape(fmt_dec(newsletter_pitcher$run_value_per_pa_delta[[1]])),
    '</strong></span><span><small>Form score</small><strong>', html_escape(fmt_score(newsletter_pitcher$form_score[[1]])), '</strong></span></div>'),
  '</section>',
  '</main><aside class="newsletter-side">',
  '<section class="newsletter-note"><span class="eyebrow">On this date</span><h2>History queue</h2><ul class="history-list">',
  newsletter_history_list,
  '</ul><p class="method-note">Candidates are ranked automatically and intended for editorial review.</p></section>',
  '<section class="newsletter-note"><span class="eyebrow">Editor\'s radar</span><h2>Five different reporting lanes</h2><ul class="history-list">',
  newsletter_radar_list,
  '</ul><p class="method-note">The queue deliberately selects across categories instead of simply taking the five highest raw scores.</p></section>',
  paste0('<section class="newsletter-note"><span class="eyebrow">Race and milestone watch</span><h2>',
    html_escape(offense_top$player_name[[1]]), ' leads the offensive board</h2><p>',
    html_escape(offense_top$player_name[[1]]), ' carries a ', html_escape(fmt_dec(offense_top$ops[[1]])),
    ' OPS and a ', html_escape(fmt_score(offense_top$race_score[[1]])), ' descriptive race score.</p><p><strong>',
    html_escape(active_milestones$player_name[[1]]), ':</strong> ', html_escape(active_milestones$headline[[1]]),
    '.</p><p class="method-note">The race score describes current statistical quality; it is not an award forecast.</p></section>'),
  '<section class="newsletter-note newsletter-note--dark"><span class="eyebrow">Today’s reading list</span><h2>Go deeper</h2>',
  '<a href="today.html">Open the signal desk <span>→</span></a>',
  '<a href="pitch-lab.html">Inspect the Pitch Lab <span>→</span></a>',
  '<a href="methodology.html">Audit the methods <span>→</span></a>',
  '</section></aside></div>'
))

write_fragment("player-leaders.html", c(
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Offense</span><h2>OPS leaders</h2><p>Minimum 100 plate appearances in this build.</p></div>',
  render_table(ops_leaders, c("player_name", "team", "pa", "ops", "woba_estimate", "hard_hit_rate", "run_value_per_pa"),
    c("Player", "Team", "PA", "OPS", "wOBA est.", "Hard-hit", "RV/PA"),
    list(pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, hard_hit_rate = fmt_rate, run_value_per_pa = function(x) fmt_dec(x, 3L))),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Quality of offense</span><h2>Estimated wOBA leaders</h2></div>',
  render_table(woba_leaders, c("player_name", "team", "pa", "woba_estimate", "walk_rate", "strikeout_rate", "pa_reliability"),
    c("Player", "Team", "PA", "wOBA est.", "BB%", "K%", "Reliability"),
    list(pa = fmt_int, woba_estimate = fmt_dec, walk_rate = fmt_rate, strikeout_rate = fmt_rate, pa_reliability = fmt_dec)),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Results allowed</span><h2>Pitcher suppression board</h2><p>Lower opponent OPS is better; minimum 75 batters faced.</p></div>',
  render_table(pitcher_suppressors, c("player_name", "team", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate"),
    c("Pitcher", "Team", "BF", "OPS allowed", "wOBA est.", "K%", "Hard-hit"),
    list(pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, strikeout_rate = fmt_rate, hard_hit_rate = fmt_rate)),
  '</section>'
))

signature_spotlights <- signature_pitches[seq_len(min(6L, nrow(signature_pitches))), ]
signature_cards <- vapply(seq_len(nrow(signature_spotlights)), function(index) pitch_identity_card(signature_spotlights[index, , drop = FALSE]), character(1))
write_fragment("pitch-lab.html", c(
  '<section class="section-heading"><span class="eyebrow">Pitch identities</span><h2>Six pitches with a visual fingerprint</h2><p>Movement location, velocity, bat-missing, chase, and usage combine into a transparent pitch-quality board.</p></section>',
  '<div class="pitch-identity-grid">', signature_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Arsenal leaderboard</span><h2>Highest whiff pitch types</h2><p>At least 50 swings and 100 total pitches in the underlying product.</p></div>',
  render_table(arsenal_whiffs, c("player_name", "team", "pitch_name", "pitches", "usage_rate", "average_velocity", "whiff_rate", "chase_rate", "pitch_change_rate"),
    c("Pitcher", "Team", "Pitch", "Pitches", "Usage", "Velo", "Whiff", "Chase", "Changed from prior"),
    list(pitches = fmt_int, usage_rate = fmt_rate, average_velocity = function(x) fmt_dec(x, 1L), whiff_rate = fmt_rate, chase_rate = fmt_rate, pitch_change_rate = fmt_rate)),
  '</section>',
  '<section class="method-grid">',
  stat_card("Pitch context", "Sequence change", "Previous → current", "Every pitch can be evaluated relative to the pitch before it.", "navy"),
  stat_card("Location", "Separation", "Feet at the plate", "Measures how far consecutive pitches move across the hitting window.", "steel"),
  stat_card("Contact", "Quality", "EV + launch angle", "Hard-hit and barrel-proxy fields remain explicitly labeled.", "red"),
  '</section>',
  '<div class="section-action"><a class="btn btn-metallic" href="matchups.html">Explore handedness matchup edges</a></div>'
))

factor_labels <- c(
  pitches_over_60 = "Workload beyond 60 pitches",
  bf_over_18 = "Batters faced beyond 18",
  third_time = "Third time through order",
  late_inning = "Later inning",
  close_game = "Score within two runs",
  trailing_badly = "Team trailing by four-plus",
  adverse_result = "Adverse plate-appearance result",
  starter_flag = "Starter role",
  reliever_flag = "Reliever role"
)
manager_factors <- manager_model[manager_model$term != "(Intercept)", , drop = FALSE]
manager_factors$factor <- unname(factor_labels[manager_factors$term])
manager_factors$direction <- ifelse(num(manager_factors$estimate) > 0, "Higher modeled likelihood", "Lower modeled likelihood")
manager_factors$coefficient <- num(manager_factors$estimate)
manager_factors <- manager_factors[order(-abs(manager_factors$coefficient)), , drop = FALSE]
scenario_cards <- vapply(seq_len(min(3L, nrow(hook_scenarios))), function(index) {
  player_card(
    paste(hook_scenarios$relative_likelihood[[index]], "hook likelihood"),
    hook_scenarios$scenario_label[[index]],
    paste(fmt_int(hook_scenarios$inning[[index]]), "inning | tied game"),
    paste(fmt_rate(hook_scenarios$hook_probability[[index]]), "modeled hook probability"),
    paste(fmt_int(hook_scenarios$pitches_in_appearance[[index]]), "pitches |", fmt_int(hook_scenarios$batters_faced_in_appearance[[index]]), "batters faced"),
    fmt_score(100 * num(hook_scenarios$hook_probability[[index]]))
  )
}, character(1))
boston_matchups <- bullpen_matchups[bullpen_matchups$team == "Boston Red Sox", , drop = FALSE]
write_fragment("projections-model.html", c(
  '<section class="section-heading"><span class="eyebrow">Manager decision model</span><h2>The first bullpen-entry layer is now measurable</h2><p>The pooled hook model is evaluated on later game dates that were not used for fitting. These are development diagnostics, not production probabilities.</p></section>',
  '<div class="data-card-grid">',
  stat_card("Out-of-time holdout", "Validation rows", fmt_int(hook_validation$validation_rows[[1]]), paste("Training through", hook_validation$train_through[[1]]), "navy"),
  stat_card("Discrimination", "ROC AUC", fmt_dec(hook_validation$roc_auc[[1]], 3L), "Higher means pulled and retained decisions are ranked more distinctly.", "red"),
  stat_card("Probability error", "Brier score", fmt_dec(hook_validation$brier_score[[1]], 3L), "Lower is better; calibration still requires improvement before publishing forecasts.", "steel"),
  stat_card("Observed vs predicted", "Hook rate", paste0(fmt_rate(hook_validation$observed_hook_rate[[1]]), " / ", fmt_rate(hook_validation$mean_predicted_hook_rate[[1]])), "Observed first, mean prediction second.", "navy"),
  '</div>',
  '<section class="section-heading"><span class="eyebrow">Situation ladder</span><h2>How workload changes the decision environment</h2><p>Representative tied-game contexts scored through the pooled model. These fixed scenarios isolate the shape of the model, not a specific manager.</p></section>',
  '<div class="signal-grid">', scenario_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Hook scenario board</span><h2>Starter and reliever decision points</h2></div>',
  render_table(hook_scenarios, c("hook_probability_rank", "scenario_label", "pitcher_role", "inning", "pitches_in_appearance", "batters_faced_in_appearance", "times_through_order_proxy", "hook_probability", "relative_likelihood"),
    c("Rank", "Situation", "Role", "Inn.", "Pitches", "BF", "TTO", "Hook prob.", "Relative"),
    list(hook_probability_rank = fmt_int, inning = fmt_int, pitches_in_appearance = fmt_int, batters_faced_in_appearance = fmt_int, times_through_order_proxy = fmt_int, hook_probability = fmt_rate)),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">What moves the model</span><h2>Observed manager-decision factors</h2><p>Coefficient direction is descriptive. Correlated game situations mean these are not isolated causal effects.</p></div>',
  render_table(manager_factors, c("factor", "coefficient", "direction"),
    c("Factor", "Coefficient", "Association"),
    list(coefficient = function(x) fmt_dec(x, 2L))),
  '</section>',
  '<section class="section-heading"><span class="eyebrow">Bullpen matchup selector</span><h2>A Boston proof of concept for the broadcast workflow</h2><p>Left- and right-handed matchup pockets combine recent availability, pitcher role, exact MLBAM identity, handedness, and current results allowed.</p></section>',
  '<section class="dashboard-block">',
  render_table(boston_matchups, c("upcoming_batter_side", "selection_rank", "pitcher_name", "throws", "pitcher_role", "availability_score", "performance_score_used", "matchup_score", "selection_score"),
    c("Next batter", "Rank", "Reliever", "Throws", "Role", "Available", "Performance", "Matchup", "Selector"),
    list(selection_rank = fmt_int, availability_score = fmt_rate, performance_score_used = fmt_rate, matchup_score = fmt_rate, selection_score = fmt_score)),
  '</section>',
  '<div class="method-callout"><strong>Roster limitation:</strong> the current selector uses a recent-appearance eligibility proxy because an authoritative active-roster feed is not yet connected. It is a research lead, not a live recommendation.</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Calibration audit</span><h2>Predicted probability against observed decisions</h2></div>',
  render_table(hook_calibration, c("calibration_bin", "rows", "minimum_predicted", "maximum_predicted", "mean_predicted", "observed_rate"),
    c("Bin", "Rows", "Min predicted", "Max predicted", "Mean predicted", "Observed hook rate"),
    list(calibration_bin = fmt_int, rows = fmt_int, minimum_predicted = fmt_rate, maximum_predicted = fmt_rate, mean_predicted = fmt_rate, observed_rate = fmt_rate)),
  '</section>'
))

team_pulse_cards <- vapply(seq_len(min(6L, nrow(team_intelligence))), function(index) {
  player_card(
    paste("Team intelligence #", fmt_int(team_intelligence$team_index_rank[[index]])),
    team_intelligence$team[[index]],
    paste("Top driver:", team_intelligence$top_signal[[index]]),
    paste("Offense #", fmt_int(team_intelligence$offense_rank[[index]]), "| run prevention #", fmt_int(team_intelligence$run_prevention_rank[[index]])),
    paste(fmt_int(team_intelligence$surging_signals[[index]]), "surging | bullpen", team_intelligence$bullpen_health[[index]]),
    fmt_score(team_intelligence$team_index[[index]])
  )
}, character(1))
write_fragment("team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Team dossiers</span><h2>Who has the most complete analytical profile?</h2><p>Offense, run prevention, recent player form, and bullpen readiness now travel together instead of living in separate tables.</p></section>',
  '<div class="signal-grid">', team_pulse_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Full league board</span><h2>Thirty-team intelligence index</h2><p>This is a descriptive reporting index, not a standings projection. Component ranks make each placement auditable.</p></div>',
  render_table(team_intelligence, c("team_index_rank", "team", "team_index", "offense_rank", "run_prevention_rank", "form_rank", "bullpen_rank", "surging_signals", "cooling_signals", "bullpen_health", "top_signal"),
    c("Rank", "Team", "Index", "Offense", "Run prev.", "Form", "Bullpen", "Surging", "Cooling", "Pen state", "Top signal"),
    list(team_index_rank = fmt_int, team_index = fmt_score, offense_rank = fmt_int, run_prevention_rank = fmt_int, form_rank = fmt_int, bullpen_rank = fmt_int, surging_signals = fmt_int, cooling_signals = fmt_int)),
  '</section>',
  '<div class="method-callout"><strong>How to read it:</strong> offense and run prevention each carry 35% of the index, recent form carries 20%, and bullpen readiness carries 10%. The visible ranks expose why a club moves.</div>'
))
write_fragment("home-team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Around the league</span><h2>The most complete team profiles</h2><p>A composite of season quality, current movement, and bullpen readiness built to lead reporters toward the next question.</p></section>',
  '<section class="dashboard-block home-pulse-board">',
  render_table(utils::head(team_intelligence, 8L), c("team_index_rank", "team", "team_index", "offense_rank", "run_prevention_rank", "surging_signals", "bullpen_health", "top_signal"),
    c("Rank", "Team", "Index", "Offense", "Run prev.", "Surging", "Pen", "Top driver"),
    list(team_index_rank = fmt_int, team_index = fmt_score, offense_rank = fmt_int, run_prevention_rank = fmt_int, surging_signals = fmt_int)),
  '</section>',
  '<div class="section-action"><a class="btn btn-metallic" href="teams.html">Open all 30 team dossiers</a></div>'
))

team_source_dir <- file.path(site_root, "team-dossiers")
team_include_dir <- file.path(include_dir, "team-dossiers")
dir.create(team_source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(team_include_dir, recursive = TRUE, showWarnings = FALSE)

team_dossier_links <- vapply(seq_len(nrow(team_intelligence)), function(index) {
  team <- team_intelligence$team[[index]]
  slug <- slugify(team)
  paste0(
    '<a class="team-dossier-link" href="team-dossiers/', html_escape(slug), '.html">',
    '<span class="team-dossier-link__rank">#', html_escape(fmt_int(team_intelligence$team_index_rank[[index]])), '</span>',
    '<span><strong>', html_escape(team), '</strong><small>Index ', html_escape(fmt_score(team_intelligence$team_index[[index]])),
    ' | offense #', html_escape(fmt_int(team_intelligence$offense_rank[[index]])),
    ' | run prevention #', html_escape(fmt_int(team_intelligence$run_prevention_rank[[index]])), '</small></span>',
    '<span aria-hidden="true">&rarr;</span></a>'
  )
}, character(1))
write_fragment("team-dossier-index.html", c(
  '<section class="section-heading"><span class="eyebrow">Thirty club research rooms</span><h2>Choose a team dossier</h2><p>Every page uses the same auditable framework so strengths, weaknesses, player movement, pitches, and bullpen decisions can be compared consistently.</p></section>',
  '<div class="team-dossier-directory">', team_dossier_links, '</div>',
  '<div class="method-callout"><strong>Dossier status:</strong> these are automated research foundations through June 14, 2026. Editorial notes, roster confirmation, and opponent-specific context are the next layer before broadcast use.</div>'
))

form_card_for_team <- function(row) {
  role_label <- if (row$perspective[[1L]] == "pitcher") "Pitcher form" else "Hitter form"
  metric_label <- if (row$perspective[[1L]] == "pitcher") "recent OPS allowed" else "recent OPS"
  player_card(
    role_label, row$player_name[[1L]], row$team[[1L]],
    paste(fmt_dec(row$recent_ops[[1L]]), metric_label),
    paste("Prior", fmt_dec(row$baseline_ops[[1L]]), "| change", fmt_dec(row$ops_delta[[1L]])),
    fmt_score(row$form_score[[1L]])
  )
}

team_names <- as.character(team_intelligence$team)
for (index in seq_along(team_names)) {
  team <- team_names[[index]]
  slug <- slugify(team)
  team_row <- team_intelligence[team_intelligence$team == team, , drop = FALSE][1L, ]
  team_hitters <- hitters[hitters$team == team, , drop = FALSE]
  team_hitters <- team_hitters[order(-num(team_hitters$woba_estimate), -num(team_hitters$pa)), , drop = FALSE]
  team_pitchers <- pitchers[pitchers$team == team, , drop = FALSE]
  team_pitchers <- team_pitchers[order(num(team_pitchers$woba_estimate), -num(team_pitchers$pa)), , drop = FALSE]
  team_form <- rbind(hitter_form[hitter_form$team == team, , drop = FALSE], pitcher_form[pitcher_form$team == team, , drop = FALSE])
  team_form <- team_form[order(-num(team_form$form_score), -num(team_form$recent_pa)), , drop = FALSE]
  team_pitches <- signature_pitches[signature_pitches$team == team, , drop = FALSE]
  team_pitches <- team_pitches[order(num(team_pitches$pitch_quality_rank)), , drop = FALSE]
  team_matchups <- rbind(hitter_matchups[hitter_matchups$team == team, , drop = FALSE], pitcher_matchups[pitcher_matchups$team == team, , drop = FALSE])
  team_matchups <- team_matchups[order(-num(team_matchups$matchup_edge_score)), , drop = FALSE]
  team_bullpen <- bullpen_matchups[bullpen_matchups$team == team, , drop = FALSE]
  team_notes <- team_broadcast_notes[team_broadcast_notes$team == team, , drop = FALSE]
  team_stories <- story_queue[story_queue$team == team, , drop = FALSE]
  team_stories <- team_stories[order(-num(team_stories$story_score)), , drop = FALSE]

  prior_index <- if (index == 1L) length(team_names) else index - 1L
  next_index <- if (index == length(team_names)) 1L else index + 1L
  prior_slug <- slugify(team_names[[prior_index]])
  next_slug <- slugify(team_names[[next_index]])
  dossier_nav <- paste0(
    '<nav class="team-dossier-nav" aria-label="Team dossier navigation">',
    '<a href="', prior_slug, '.html">&larr; ', html_escape(team_names[[prior_index]]), '</a>',
    '<a href="../team-dossiers.html">All teams</a>',
    '<a href="', next_slug, '.html">', html_escape(team_names[[next_index]]), ' &rarr;</a></nav>'
  )

  notes_html <- paste0(
    '<li><span>', html_escape(fmt_int(team_notes$note_order)), '</span><div><small>', html_escape(gsub("_", " ", team_notes$note_category)),
    '</small><h3>', html_escape(team_notes$headline), '</h3><p>', html_escape(team_notes$evidence), '</p></div></li>',
    collapse = ""
  )
  form_cards <- if (nrow(team_form)) {
    vapply(seq_len(min(4L, nrow(team_form))), function(row_index) form_card_for_team(team_form[row_index, , drop = FALSE]), character(1))
  } else character()
  pitch_cards <- if (nrow(team_pitches)) {
    vapply(seq_len(min(2L, nrow(team_pitches))), function(row_index) pitch_identity_card(team_pitches[row_index, , drop = FALSE]), character(1))
  } else character()
  matchup_cards <- if (nrow(team_matchups)) {
    vapply(seq_len(min(3L, nrow(team_matchups))), function(row_index) matchup_edge_card(team_matchups[row_index, , drop = FALSE]), character(1))
  } else character()

  dossier_lines <- c(
    dossier_nav,
    '<section class="team-dossier-hero"><div><span class="eyebrow">SABRhood team dossier</span><h1>', html_escape(team), '</h1><p>', html_escape(team_row$team_story[[1L]]), '</p></div>',
    '<div class="team-dossier-hero__score"><small>League intelligence</small><strong>#', html_escape(fmt_int(team_row$team_index_rank[[1L]])), '</strong><span>', html_escape(fmt_score(team_row$team_index[[1L]])), ' index</span></div></section>',
    '<section class="team-fingerprint" aria-label="Team component ranks">',
    rank_meter("Offensive quality", team_row$offense_rank[[1L]], paste("Weighted estimated wOBA", fmt_dec(team_row$offense_woba[[1L]]))),
    rank_meter("Run prevention", team_row$run_prevention_rank[[1L]], paste("Opponent estimated wOBA", fmt_dec(team_row$opponent_woba[[1L]]))),
    rank_meter("Recent form", team_row$form_rank[[1L]], paste(fmt_int(team_row$surging_signals[[1L]]), "surging signals")),
    rank_meter("Bullpen readiness", team_row$bullpen_rank[[1L]], paste("Current state", team_row$bullpen_health[[1L]])),
    '</section>',
    '<section class="broadcast-three"><div class="section-heading section-heading--tight"><span class="eyebrow">Broadcast three</span><h2>Three notes to take on air</h2><p>Short leads with an evidence trail, ready for verification and expansion.</p></div><ol>', notes_html, '</ol></section>',
    '<section class="section-heading"><span class="eyebrow">Current movement</span><h2>The players changing the team conversation</h2></section>',
    if (length(form_cards)) paste0('<div class="signal-grid signal-grid--four">', paste0(form_cards, collapse = ""), '</div>') else '<div class="method-callout">No players met both recent and baseline form thresholds.</div>',
    '<section class="dossier-split-grid"><div class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Offense</span><h2>Leading hitter profiles</h2></div>',
    if (nrow(team_hitters)) render_table(utils::head(team_hitters, 5L), c("player_name", "pa", "ops", "woba_estimate", "hard_hit_rate", "strikeout_rate", "walk_rate"), c("Hitter", "PA", "OPS", "wOBA est.", "Hard-hit", "K%", "BB%"), list(pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, hard_hit_rate = fmt_rate, strikeout_rate = fmt_rate, walk_rate = fmt_rate)) else '<p>No qualified hitters in the current build.</p>',
    '</div><div class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Run prevention</span><h2>Leading pitcher profiles</h2></div>',
    if (nrow(team_pitchers)) render_table(utils::head(team_pitchers, 5L), c("player_name", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate"), c("Pitcher", "BF", "OPS allowed", "wOBA est.", "K%", "Hard-hit"), list(pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, strikeout_rate = fmt_rate, hard_hit_rate = fmt_rate)) else '<p>No qualified pitchers in the current build.</p>',
    '</div></section>',
    '<section class="section-heading"><span class="eyebrow">Pitch identity</span><h2>The arsenal signatures worth knowing</h2></section>',
    if (length(pitch_cards)) paste0('<div class="pitch-identity-grid pitch-identity-grid--compact">', paste0(pitch_cards, collapse = ""), '</div>') else '<div class="method-callout">No pitch types met the signature-pitch thresholds.</div>',
    '<section class="section-heading"><span class="eyebrow">Matchup intelligence</span><h2>The largest qualified handedness edges</h2></section>',
    if (length(matchup_cards)) paste0('<div class="signal-grid">', paste0(matchup_cards, collapse = ""), '</div>') else '<div class="method-callout">No players met the two-sided matchup threshold.</div>',
    '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Bullpen decision board</span><h2>Top options against the next left- or right-handed batter</h2><p>Development selector using recent-appearance roster proxy, workload, role, matchup, and performance.</p></div>',
    if (nrow(team_bullpen)) render_table(team_bullpen, c("upcoming_batter_side", "selection_rank", "pitcher_name", "throws", "availability_score", "performance_score_used", "selection_score"), c("Next batter", "Rank", "Reliever", "Throws", "Available", "Performance", "Selector"), list(selection_rank = fmt_int, availability_score = fmt_rate, performance_score_used = fmt_rate, selection_score = fmt_score)) else '<p>No eligible bullpen candidates.</p>',
    '</section>',
    '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Story leads</span><h2>What this team could become next</h2></div>',
    if (nrow(team_stories)) render_table(utils::head(team_stories, 6L), c("category", "subject", "headline", "story_score"), c("Lane", "Subject", "Reporting lead", "Score"), list(story_score = fmt_score), "data-table story-queue-table") else '<p>No current story candidates.</p>',
    '</section>',
    '<div class="method-callout"><strong>Research status:</strong> this automated dossier is a reporting foundation, not a finished scouting report. Confirm active rosters, injuries, probable pitchers, and opponent context before broadcast use.</div>',
    dossier_nav
  )
  writeLines(enc2utf8(dossier_lines), file.path(team_include_dir, paste0(slug, ".html")), useBytes = TRUE)
  qmd_lines <- c(
    "---",
    paste0('title: "', team, ' Team Dossier"'),
    paste0('description: "Automated SABRhood research dossier for the ', team, '."'),
    "---",
    "",
    paste0("{{< include ../includes/team-dossiers/", slug, ".html >}}")
  )
  writeLines(enc2utf8(qmd_lines), file.path(team_source_dir, paste0(slug, ".qmd")), useBytes = TRUE)
}

re_empty <- re24[re24$outs_before == 0 & re24$base_state_before == 0, ]
re_loaded <- re24[re24$outs_before == 0 & re24$base_state_before == 7, ]
write_fragment("methodology-data.html", c(
  '<div class="method-grid">',
  stat_card("Canonical grain", "Pitch", fmt_int(sum(read_product("data-contract-summary.csv")$rows[read_product("data-contract-summary.csv")$dataset == "pitch_view"])), "One row per pitch; no full PBP shipped to the browser.", "navy"),
  stat_card("Run environment", "Empty, 0 outs", fmt_dec(re_empty$expected_runs[[1]]), "Empirical expected runs to the end of the inning.", "steel"),
  stat_card("Run environment", "Loaded, 0 outs", fmt_dec(re_loaded$expected_runs[[1]]), "Estimated from the same season-level state table.", "red"),
  '</div>',
  '<div class="method-callout"><strong>Interpretation rule:</strong> fixed-weight wOBA is labeled as an estimate, barrel classification is labeled as a proxy, and the pressure measure is labeled as a leverage proxy. These fields are designed for transparent reporting, not to impersonate proprietary or official metrics.</div>'
))

cat("Generated site fragments in", include_dir, "\n")
