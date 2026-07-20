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
    '<span class="signal-score" aria-label="Form score ', html_escape(score), '">',
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
re24 <- read_product("run-expectancy-24.csv")
bullpen <- read_product("bullpen-availability.csv")
manager <- read_product("manager-data-summary.csv")
hook_validation <- read_product("manager-hook-validation-metrics.csv")
hook_calibration <- read_product("manager-hook-calibration.csv")
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

icon_profiles <- historical_profiles[order(-num(historical_profiles$career_significance_score)), ]
icon_profiles <- icon_profiles[seq_len(min(15L, nrow(icon_profiles))), ]
milestone_spotlights <- historical_milestones[!duplicated(historical_milestones$subject_id), ]
milestone_spotlights <- milestone_spotlights[seq_len(min(8L, nrow(milestone_spotlights))), ]
anniversary_spotlights <- historical[seq_len(min(8L, nrow(historical))), ]

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

write_fragment("history-desk.html", c(
  '<div class="history-scoreboard">',
  stat_card("Career profiles", "Recognizable players", fmt_int(nrow(historical_profiles)), "Historical careers above the public significance threshold.", "navy"),
  stat_card("Recognition", "Icons", fmt_int(sum(historical_profiles$recognition_tier == "icon")), "Hall of Famers, record holders, and historically dominant careers.", "red"),
  stat_card("Story inventory", "Milestone notes", fmt_int(nrow(historical_milestones)), "Career clubs and top-ten leaderboard context.", "steel"),
  stat_card("Today", "Anniversary candidates", fmt_int(nrow(historical)), "Debuts and final appearances ranked for editorial review.", "navy"),
  '</div>',
  '<section class="section-heading"><span class="eyebrow">On this date</span><h2>The recognizable names rise first</h2><p>Career significance, awards, Hall of Fame status, record standing, and broadcast value now influence the daily queue.</p></section>',
  '<div class="signal-grid signal-grid--four">', anniversary_cards, '</div>',
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

newsletter_hitter <- top_hitter_form[1, ]
newsletter_pitcher <- top_pitcher_form[1, ]
newsletter_history <- historical[1:min(5L, nrow(historical)), ]
newsletter_history_list <- paste0(
  '<li><strong>', html_escape(newsletter_history$subject_name), '</strong><span>',
  html_escape(newsletter_history$headline), '</span></li>',
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

write_fragment("pitch-lab.html", c(
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Arsenal leaderboard</span><h2>Highest whiff pitch types</h2><p>At least 50 swings and 100 total pitches in the underlying product.</p></div>',
  render_table(arsenal_whiffs, c("player_name", "team", "pitch_name", "pitches", "usage_rate", "average_velocity", "whiff_rate", "chase_rate", "pitch_change_rate"),
    c("Pitcher", "Team", "Pitch", "Pitches", "Usage", "Velo", "Whiff", "Chase", "Changed from prior"),
    list(pitches = fmt_int, usage_rate = fmt_rate, average_velocity = function(x) fmt_dec(x, 1L), whiff_rate = fmt_rate, chase_rate = fmt_rate, pitch_change_rate = fmt_rate)),
  '</section>',
  '<section class="method-grid">',
  stat_card("Pitch context", "Sequence change", "Previous → current", "Every pitch can be evaluated relative to the pitch before it.", "navy"),
  stat_card("Location", "Separation", "Feet at the plate", "Measures how far consecutive pitches move across the hitting window.", "steel"),
  stat_card("Contact", "Quality", "EV + launch angle", "Hard-hit and barrel-proxy fields remain explicitly labeled.", "red"),
  '</section>'
))

write_fragment("projections-model.html", c(
  '<section class="section-heading"><span class="eyebrow">Manager decision model</span><h2>The first bullpen-entry layer is now measurable</h2><p>The pooled hook model is evaluated on later game dates that were not used for fitting. These are development diagnostics, not production probabilities.</p></section>',
  '<div class="data-card-grid">',
  stat_card("Out-of-time holdout", "Validation rows", fmt_int(hook_validation$validation_rows[[1]]), paste("Training through", hook_validation$train_through[[1]]), "navy"),
  stat_card("Discrimination", "ROC AUC", fmt_dec(hook_validation$roc_auc[[1]], 3L), "Higher means pulled and retained decisions are ranked more distinctly.", "red"),
  stat_card("Probability error", "Brier score", fmt_dec(hook_validation$brier_score[[1]], 3L), "Lower is better; calibration still requires improvement before publishing forecasts.", "steel"),
  stat_card("Observed vs predicted", "Hook rate", paste0(fmt_rate(hook_validation$observed_hook_rate[[1]]), " / ", fmt_rate(hook_validation$mean_predicted_hook_rate[[1]])), "Observed first, mean prediction second.", "navy"),
  '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Calibration audit</span><h2>Predicted probability against observed decisions</h2></div>',
  render_table(hook_calibration, c("calibration_bin", "rows", "minimum_predicted", "maximum_predicted", "mean_predicted", "observed_rate"),
    c("Bin", "Rows", "Min predicted", "Max predicted", "Mean predicted", "Observed hook rate"),
    list(calibration_bin = fmt_int, rows = fmt_int, minimum_predicted = fmt_rate, maximum_predicted = fmt_rate, mean_predicted = fmt_rate, observed_rate = fmt_rate)),
  '</section>'
))

team_form <- rbind(
  data.frame(team = hitter_form$team, form_score = num(hitter_form$form_score), type = "Hitter", player = hitter_form$player_name),
  data.frame(team = pitcher_form$team, form_score = num(pitcher_form$form_score), type = "Pitcher", player = pitcher_form$player_name)
)
team_split <- split(team_form, team_form$team)
team_pulse <- do.call(rbind, lapply(names(team_split), function(team_name) {
  rows <- team_split[[team_name]]
  top <- rows[order(-rows$form_score), ][1, ]
  data.frame(
    team = team_name,
    average_form = mean(rows$form_score, na.rm = TRUE),
    surging = sum(rows$form_score >= 65, na.rm = TRUE),
    cooling = sum(rows$form_score <= 35, na.rm = TRUE),
    top_signal = paste(top$player, paste0("(", top$type, ")")),
    stringsAsFactors = FALSE
  )
}))
team_pulse <- team_pulse[order(-team_pulse$average_form), ][1:min(30L, nrow(team_pulse)), ]
team_pulse_cards <- vapply(seq_len(min(6L, nrow(team_pulse))), function(index) {
  player_card(
    paste("League pulse #", index),
    team_pulse$team[[index]],
    paste("Top driver:", team_pulse$top_signal[[index]]),
    paste(fmt_int(team_pulse$surging[[index]]), "surging player signals"),
    paste(fmt_int(team_pulse$cooling[[index]]), "cooling signals | average form", fmt_score(team_pulse$average_form[[index]])),
    fmt_score(team_pulse$average_form[[index]])
  )
}, character(1))
write_fragment("team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Team spotlights</span><h2>Who is driving the league pulse?</h2><p>The six strongest team-level concentrations, with the top individual signal attached.</p></section>',
  '<div class="signal-grid">', team_pulse_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Organization pulse</span><h2>Which clubs have the strongest current signals?</h2><p>Average recent-form score across qualifying hitters and pitchers. This is a roster-signal board, not a projected standings table.</p></div>',
  render_table(team_pulse, c("team", "average_form", "surging", "cooling", "top_signal"),
    c("Team", "Avg. form", "Surging", "Cooling", "Top signal"),
    list(average_form = fmt_score, surging = fmt_int, cooling = fmt_int)),
  '</section>'
))
write_fragment("home-team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Around the league</span><h2>The clubs carrying the most surge signals</h2><p>Recent player movement aggregated into a team-level reporting lead.</p></section>',
  '<section class="dashboard-block home-pulse-board">',
  render_table(utils::head(team_pulse, 8L), c("team", "average_form", "surging", "cooling", "top_signal"),
    c("Team", "Avg. form", "Surging", "Cooling", "Top driver"),
    list(average_form = fmt_score, surging = fmt_int, cooling = fmt_int)),
  '</section>',
  '<div class="section-action"><a class="btn btn-metallic" href="teams.html">See all 30 team signals</a></div>'
))

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
