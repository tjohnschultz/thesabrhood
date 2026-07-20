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
  ifelse(is.finite(num(value)), paste0(format(round(100 * num(value), digits), nsmall = digits), "%"), "-")
}
fmt_dec <- function(value, digits = 3L) {
  ifelse(is.finite(num(value)), format(round(num(value), digits), nsmall = digits), "-")
}
fmt_score <- function(value) {
  ifelse(is.finite(num(value)), format(round(num(value), 1L), nsmall = 1L), "-")
}
fmt_z <- function(value) {
  value <- num(value)
  ifelse(is.finite(value), paste0(ifelse(value > 0, "+", ""), format(round(value, 1L), nsmall = 1L), " SD"), "-")
}
fmt_ordinal <- function(value) {
  value <- round(num(value))
  remainder_100 <- value %% 100
  suffix <- ifelse(remainder_100 %in% 11:13, "th", ifelse(value %% 10 == 1, "st", ifelse(value %% 10 == 2, "nd", ifelse(value %% 10 == 3, "rd", "th"))))
  paste0(value, suffix)
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

context_metric_labels <- c(
  ops = "OPS", woba_estimate = "Est. wOBA", strikeout_rate = "Strikeout rate",
  walk_rate = "Walk rate", hard_hit_rate = "Hard-hit rate", run_value_per_pa = "Run value / PA"
)

context_metric_label <- function(metric, perspective = "batter") {
  if (perspective != "pitcher") return(context_metric_labels[[metric]])
  pitcher_labels <- c(
    ops = "OPS allowed", woba_estimate = "Est. wOBA allowed", strikeout_rate = "Strikeout rate",
    walk_rate = "Walk rate allowed", hard_hit_rate = "Hard-hit rate allowed", run_value_per_pa = "Run value allowed / PA"
  )
  pitcher_labels[[metric]]
}

fmt_context_metric <- function(metric, value) {
  if (metric %in% c("strikeout_rate", "walk_rate", "hard_hit_rate")) fmt_rate(value) else fmt_dec(value)
}

percentile_ruler <- function(label, percentile, detail = "") {
  position <- pmax(pmin(num(percentile), 100), 0)
  paste0(
    '<div class="percentile-ruler"><div class="percentile-ruler__label"><span>', html_escape(label),
    '</span><strong>', html_escape(fmt_ordinal(percentile)), '</strong></div>',
    '<div class="percentile-ruler__track" role="img" aria-label="',
    html_escape(paste(label, fmt_int(percentile), "MLB percentile")), '">',
    '<span style="width:', format(round(position, 1), nsmall = 1), '%"></span>',
    '<i style="left:', format(round(position, 1), nsmall = 1), '%"></i></div>',
    if (nzchar(detail)) paste0('<small>', html_escape(detail), '</small>') else '', '</div>'
  )
}

change_z_row <- function(row, metric) {
  z <- num(row[[paste0(metric, "_change_z")]][[1L]])
  width <- pmin(50, 50 * abs(z) / 4)
  direction <- if (z >= 0) "positive" else "negative"
  position_style <- if (z >= 0) "left:50%" else "right:50%"
  recent <- row[[paste0("recent_", metric)]][[1L]]
  baseline <- row[[paste0("baseline_", metric)]][[1L]]
  label <- context_metric_label(metric, row$perspective[[1L]])
  paste0(
    '<div class="change-z-row"><div class="change-z-row__label"><span>', html_escape(label),
    '</span><strong>', html_escape(fmt_z(z)), '</strong></div>',
    '<div class="change-z-track" role="img" aria-label="',
    html_escape(paste(label, fmt_z(z), "recent", fmt_context_metric(metric, recent), "baseline", fmt_context_metric(metric, baseline))), '">',
    '<span class="change-z-zero"></span><span class="change-z-fill is-', direction, '" style="', position_style,
    ';width:', format(round(width, 1), nsmall = 1), '%"></span></div>',
    '<small>Recent ', html_escape(fmt_context_metric(metric, recent)), ' <span aria-hidden="true">vs</span> baseline ',
    html_escape(fmt_context_metric(metric, baseline)), '</small></div>'
  )
}

player_context_card <- function(row, compact = FALSE) {
  perspective <- row$perspective[[1L]]
  role <- if (perspective == "pitcher") "Pitcher change profile" else "Hitter change profile"
  metrics <- names(context_metric_labels)
  percentile_metrics <- if (isTRUE(compact)) c("ops", "woba_estimate", "hard_hit_rate") else metrics
  percentile_html <- vapply(percentile_metrics, function(metric) {
    percentile_ruler(
      context_metric_label(metric, perspective),
      row[[paste0("season_", metric, "_percentile")]][[1L]],
      fmt_context_metric(metric, row[[paste0("season_", metric)]][[1L]])
    )
  }, character(1))
  change_metrics <- if (isTRUE(compact)) row$dominant_change_stat[[1L]] else metrics
  change_html <- vapply(change_metrics, function(metric) change_z_row(row, metric), character(1))
  direction_class <- paste0("is-", row$dominant_change_direction[[1L]])
  sample_noun <- if (perspective == "pitcher") "BF" else "PA"
  paste0(
    '<article class="player-context-card', if (compact) ' player-context-card--compact' else '', '">',
    '<header class="player-context-card__head"><div><span class="eyebrow">', html_escape(role), '</span><h3>',
    html_escape(row$player_name[[1L]]), '</h3><p>', html_escape(row$team[[1L]]), ' | ',
    html_escape(row$hand[[1L]]), if (perspective == "pitcher") 'HP' else 'HB', '</p></div>',
    '<span class="signal-score" aria-label="Context signal score ', html_escape(fmt_score(row$change_signal_score[[1L]])), '">',
    html_escape(fmt_score(row$change_signal_score[[1L]])), '</span></header>',
    '<div class="player-context-strip"><span><small>Season ', sample_noun, '</small><strong>', html_escape(fmt_int(row$season_pa[[1L]])),
    '</strong></span><span><small>OPS', if (perspective == "pitcher") ' allowed' else '', '</small><strong>', html_escape(fmt_dec(row$season_ops[[1L]])),
    '</strong></span><span><small>Est. wOBA', if (perspective == "pitcher") ' allowed' else '', '</small><strong>', html_escape(fmt_dec(row$season_woba_estimate[[1L]])),
    '</strong></span><span><small>Sample</small><strong>', html_escape(row$season_sample_label[[1L]]), '</strong></span></div>',
    '<div class="change-banner ', direction_class, '"><span>Biggest change</span><strong>',
    html_escape(row$dominant_change_label[[1L]]), ' ', html_escape(fmt_z(row$dominant_change_z[[1L]])),
    '</strong><p>', html_escape(row$change_context[[1L]]), '</p></div>',
    '<div class="player-context-card__body"><section><div class="context-subhead"><span>Season context</span><small>MLB percentile</small></div>',
    '<div class="percentile-stack">', paste0(percentile_html, collapse = ""), '</div>',
    '<div class="percentile-axis" aria-hidden="true"><span>0</span><span>League midpoint</span><span>100</span></div></section>',
    '<section><div class="context-subhead"><span>What changed</span><small>Recent vs earlier season</small></div>',
    '<div class="change-z-stack">', paste0(change_html, collapse = ""), '</div>',
    '<div class="change-z-axis" aria-hidden="true"><span>Cooling</span><span>0 SD</span><span>Improving</span></div></section></div>',
    '<footer class="player-context-card__note"><strong>Broadcast read:</strong> ', html_escape(row$change_context[[1L]]),
    ' Recent sample: ', html_escape(fmt_int(row$recent_pa[[1L]])), ' ', sample_noun,
    '; prior sample: ', html_escape(fmt_int(row$baseline_pa[[1L]])), ' ', sample_noun, '.</footer></article>'
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
hitter_changes <- read_product("hitter-change-profiles.csv")
pitcher_changes <- read_product("pitcher-change-profiles.csv")
re24 <- read_product("run-expectancy-24.csv")
bullpen <- read_product("bullpen-availability.csv")
manager <- read_product("manager-data-summary.csv")
manager_model <- read_product("manager-hook-model.csv")
hook_validation <- read_product("manager-hook-validation-metrics.csv")
hook_calibration <- read_product("manager-hook-calibration.csv")
hook_scenarios <- read_product("manager-hook-scenarios.csv")
bullpen_matchups <- read_product("bullpen-matchup-selector.csv")
daily_projections <- read_product("daily-projection-demo.csv")
projection_margins <- read_product("daily-projection-margin.csv")
projection_scorelines <- read_product("daily-projection-scorelines.csv")
projection_drivers <- read_product("daily-projection-drivers.csv")
projection_inputs <- read_product("projection-input-readiness.csv")
bullpen_chains <- read_product("bullpen-chain-demo.csv")
projection_hook_path <- read_product("projection-hook-path.csv")
articles <- build_article_index()

updated_date <- max(as.Date(hitters$last_game), na.rm = TRUE)
updated_label <- format(updated_date, "%B %d, %Y")

top_hitter_form <- hitter_form[order(-num(hitter_form$form_score), -num(hitter_form$recent_pa)), ][1:6, ]
top_pitcher_form <- pitcher_form[order(-num(pitcher_form$form_score), -num(pitcher_form$recent_pa)), ][1:6, ]
all_changes <- rbind(hitter_changes, pitcher_changes)
all_changes <- all_changes[order(-num(all_changes$change_signal_score), -num(all_changes$dominant_change_abs_z)), , drop = FALSE]
change_spotlights <- do.call(rbind, lapply(c("batter", "pitcher"), function(perspective) {
  do.call(rbind, lapply(c("improving", "declining"), function(direction) {
    rows <- all_changes[all_changes$perspective == perspective & all_changes$dominant_change_direction == direction, , drop = FALSE]
    utils::head(rows, 2L)
  }))
}))
change_spotlights <- change_spotlights[order(-num(change_spotlights$change_signal_score)), , drop = FALSE]
ops_leaders <- hitters[order(-num(hitters$ops), -num(hitters$pa)), ][1:10, ]
woba_leaders <- hitters[order(-num(hitters$woba_estimate), -num(hitters$pa)), ][1:10, ]
pitcher_suppressors <- pitchers[order(num(pitchers$ops), -num(pitchers$pa)), ][1:10, ]
arsenal_whiffs <- pitch_types[num(pitch_types$swings) >= 50, ]
arsenal_whiffs <- arsenal_whiffs[order(-num(arsenal_whiffs$whiff_rate), -num(arsenal_whiffs$pitches)), ][1:10, ]
daily_projections <- daily_projections[order(num(daily_projections$display_order)), , drop = FALSE]
feature_projection <- daily_projections[as.logical(daily_projections$feature_game), , drop = FALSE][1L, ]
feature_projection_input <- projection_inputs[projection_inputs$game_id == feature_projection$game_id[[1L]], , drop = FALSE][1L, ]

projection_lean <- function(probability) {
  probability <- num(probability)
  if (probability < 0.54) "Near toss-up" else if (probability < 0.60) "Narrow lean" else "Clear lean"
}

projection_game_card <- function(row) {
  away_width <- round(100 * num(row$away_win_probability[[1L]]), 1L)
  home_width <- round(100 * num(row$home_win_probability[[1L]]), 1L)
  paste0(
    '<article class="projection-game-card"><header><span class="eyebrow">Game ', html_escape(fmt_int(row$display_order[[1L]])),
    ' &middot; team baseline</span><span class="projection-lean">', html_escape(projection_lean(row$winner_probability[[1L]])), '</span></header>',
    '<div class="projection-team-row"><span><small>Away</small><strong>', html_escape(row$away_team[[1L]]),
    '</strong></span><b>', html_escape(fmt_rate(row$away_win_probability[[1L]])), '</b></div>',
    '<div class="projection-team-row"><span><small>Home</small><strong>', html_escape(row$home_team[[1L]]),
    '</strong></span><b>', html_escape(fmt_rate(row$home_win_probability[[1L]])), '</b></div>',
    '<div class="projection-win-track" role="img" aria-label="',
    html_escape(paste(row$away_team[[1L]], fmt_rate(row$away_win_probability[[1L]]), row$home_team[[1L]], fmt_rate(row$home_win_probability[[1L]]))),
    '"><span class="is-away" style="width:', away_width, '%"></span><span class="is-home" style="width:', home_width, '%"></span></div>',
    '<div class="projection-score-strip"><span><small>Mean score</small><strong>', html_escape(fmt_dec(row$away_mean_runs[[1L]], 1L)),
    ' &ndash; ', html_escape(fmt_dec(row$home_mean_runs[[1L]], 1L)), '</strong></span><span><small>Model total</small><strong>',
    html_escape(fmt_dec(row$mean_total_runs[[1L]], 1L)), '</strong></span><span><small>One-run game</small><strong>',
    html_escape(fmt_rate(row$one_run_probability[[1L]])), '</strong></span></div>',
    '<footer><strong>', html_escape(row$projected_winner[[1L]]), ' ', html_escape(fmt_rate(row$winner_probability[[1L]])),
    '</strong><span>20,000 draws &middot; starters/lineups pending</span></footer></article>'
  )
}

projection_feature <- function(row) {
  game_id <- row$game_id[[1L]]
  margins <- projection_margins[projection_margins$game_id == game_id, , drop = FALSE]
  scores <- projection_scorelines[projection_scorelines$game_id == game_id, , drop = FALSE]
  drivers <- projection_drivers[projection_drivers$game_id == game_id, , drop = FALSE]
  max_margin <- max(num(margins$probability))
  margin_bars <- vapply(seq_len(nrow(margins)), function(index) {
    width <- 100 * num(margins$probability[[index]]) / max_margin
    paste0(
      '<div class="margin-row"><span>', html_escape(margins$margin_group[[index]]), '</span>',
      '<div class="margin-row__track"><i style="width:', format(round(width, 1), nsmall = 1), '%"></i></div>',
      '<strong>', html_escape(fmt_rate(margins$probability[[index]])), '</strong></div>'
    )
  }, character(1))
  score_cards <- vapply(seq_len(nrow(scores)), function(index) {
    paste0('<span><small>#', index, ' exact score</small><strong>',
      html_escape(fmt_int(scores$away_runs[[index]])), ' &ndash; ', html_escape(fmt_int(scores$home_runs[[index]])),
      '</strong><em>', html_escape(fmt_rate(scores$probability[[index]])), '</em></span>')
  }, character(1))
  driver_cards <- vapply(seq_len(nrow(drivers)), function(index) {
    paste0('<li><span>', html_escape(drivers$driver_label[[index]]), '</span><strong>',
      html_escape(drivers$advantage_team[[index]]), '</strong><small>', html_escape(drivers$driver_detail[[index]]), '</small></li>')
  }, character(1))
  bullpen_swing <- num(row$bullpen_home_win_swing[[1L]])
  bullpen_direction <- if (bullpen_swing >= 0) "adds" else "subtracts"
  paste0(
    '<section class="projection-feature"><div class="projection-feature__head"><div><span class="eyebrow">Feature simulation &middot; ',
    html_escape(fmt_int(row$simulations[[1L]])), ' draws</span><h2>', html_escape(row$away_team[[1L]]),
    ' at ', html_escape(row$home_team[[1L]]), '</h2><p>The closest matchup on the representative slate shows how uncertainty, bullpen context, and competing team strengths will be reported.</p></div>',
    '<div class="projection-feature__call"><small>Model lean</small><strong>', html_escape(row$projected_winner[[1L]]),
    '</strong><span>', html_escape(fmt_rate(row$winner_probability[[1L]])), '</span></div></div>',
    '<div class="projection-matchup-score"><div><span>', html_escape(row$away_team[[1L]]), '</span><strong>',
    html_escape(fmt_dec(row$away_mean_runs[[1L]], 1L)), '</strong><small>80% range ', html_escape(fmt_int(row$away_runs_p10[[1L]])),
    '&ndash;', html_escape(fmt_int(row$away_runs_p90[[1L]])), '</small></div><i aria-hidden="true">at</i><div><span>',
    html_escape(row$home_team[[1L]]), '</span><strong>', html_escape(fmt_dec(row$home_mean_runs[[1L]], 1L)),
    '</strong><small>80% range ', html_escape(fmt_int(row$home_runs_p10[[1L]])), '&ndash;', html_escape(fmt_int(row$home_runs_p90[[1L]])), '</small></div></div>',
    '<div class="projection-feature__grid"><section><div class="context-subhead"><span>Winning-margin distribution</span><small>Away &larr; outcome &rarr; Home</small></div>',
    '<div class="margin-distribution">', paste0(margin_bars, collapse = ''), '</div></section>',
    '<section><div class="context-subhead"><span>Why the model lands here</span><small>League ranks</small></div>',
    '<ol class="projection-driver-list">', paste0(driver_cards, collapse = ''), '</ol></section></div>',
    '<div class="projection-outcome-grid"><span><small>Mean total</small><strong>', html_escape(fmt_dec(row$mean_total_runs[[1L]], 1L)),
    '</strong><em>80% range ', html_escape(fmt_int(row$total_runs_p10[[1L]])), '&ndash;', html_escape(fmt_int(row$total_runs_p90[[1L]])),
    '</em></span><span><small>One-run game</small><strong>', html_escape(fmt_rate(row$one_run_probability[[1L]])),
    '</strong><em>Close-game likelihood</em></span><span><small>Extras</small><strong>', html_escape(fmt_rate(row$extra_innings_probability[[1L]])),
    '</strong><em>Regulation tie rate</em></span><span><small>Bullpen effect</small><strong>',
    html_escape(paste0(ifelse(bullpen_swing >= 0, "+", ""), fmt_dec(100 * bullpen_swing, 1L), " pts")),
    '</strong><em>Home win probability ', bullpen_direction, '</em></span></div>',
    '<div class="projection-scorelines"><div class="context-subhead"><span>Most common exact scores</span><small>Away &ndash; home</small></div><div>',
    paste0(score_cards, collapse = ''), '</div></div></section>'
  )
}

projection_input_board <- function(row) {
  gates <- data.frame(
    label = c("Demo slate shell", "Probable starters", "Lineups", "Park factor", "Weather", "Active rosters"),
    ready = c(row$schedule_ready, row$starters_ready, row$lineups_ready, row$park_ready, row$weather_ready, row$rosters_ready),
    detail = c(
      "Representative teams and date are present; authoritative schedule is not connected.",
      "Both starters must be identified before pitcher-specific run expectations.",
      "Projected lineups permit a conditional run; confirmed lineups unlock publication.",
      "A neutral demonstration factor is present; production uses the actual venue.",
      "Roof and weather context are still missing.",
      "Recent-appearance eligibility remains a proxy, not an active-roster feed."
    ),
    stringsAsFactors = FALSE
  )
  gate_html <- vapply(seq_len(nrow(gates)), function(index) {
    is_ready <- as.logical(gates$ready[[index]])
    paste0(
      '<article class="projection-input-gate ', if (is_ready) 'is-ready' else 'is-missing', '">',
      '<span>', if (is_ready) 'Ready' else 'Missing', '</span><h3>', html_escape(gates$label[[index]]),
      '</h3><p>', html_escape(gates$detail[[index]]), '</p></article>'
    )
  }, character(1))
  paste0(
    '<section class="projection-readiness"><div class="projection-readiness__head"><div><span class="eyebrow">Publication gate</span>',
    '<h2>Why this remains a development forecast</h2><p>Every game must pass the same six-part input contract. Missing information is exposed instead of silently replaced by a confident-looking number.</p></div>',
    '<div class="projection-readiness__score"><strong>', html_escape(fmt_rate(row$input_completeness[[1L]])),
    '</strong><span>demo inputs complete</span></div></div><div class="projection-input-grid">',
    paste0(gate_html, collapse = ''), '</div></section>'
  )
}

projection_hook_visual <- function(path) {
  nodes <- vapply(seq_len(nrow(path)), function(index) {
    width <- pmax(3, 100 * num(path$hook_probability[[index]]))
    paste0(
      '<article class="hook-path-node"><header><span>Inning ', html_escape(fmt_int(path$inning[[index]])),
      '</span><strong>', html_escape(fmt_rate(path$hook_probability[[index]])), '</strong></header><h3>',
      html_escape(path$decision_label[[index]]), '</h3><p>', html_escape(fmt_int(path$pitches_in_appearance[[index]])),
      ' pitches &middot; ', html_escape(fmt_int(path$batters_faced_in_appearance[[index]])), ' BF &middot; ',
      html_escape(fmt_int(path$times_through_order_proxy[[index]])), 'x through</p><div class="hook-path-track" role="img" aria-label="Hook probability ',
      html_escape(fmt_rate(path$hook_probability[[index]])), '"><i style="width:', format(round(width, 1), nsmall = 1),
      '%"></i></div></article>'
    )
  }, character(1))
  paste0(
    '<section class="hook-path-section"><div class="section-heading section-heading--tight"><span class="eyebrow">Starter decision path</span>',
    '<h2>The handoff becomes part of every game simulation</h2><p>A fixed tied-game workload path shows when the pooled hook model begins shifting probability toward the bullpen.</p></div>',
    '<div class="hook-path">', paste0(nodes, collapse = ''), '</div><p class="method-note">Descriptive pooled-model scenarios, not causal manager tendencies or a forecast for a named starter.</p></section>'
  )
}

bullpen_chain_visual <- function(chains) {
  team_names <- unique(as.character(chains$defense_team))
  chain_cards <- vapply(team_names, function(team) {
    rows <- chains[chains$defense_team == team, , drop = FALSE]
    rows <- rows[order(num(rows$chain_step)), , drop = FALSE]
    steps <- vapply(seq_len(nrow(rows)), function(index) {
      paste0(
        '<li><div class="bullpen-chain-step__inning"><span>', html_escape(fmt_int(rows$inning[[index]])), '</span><small>inning</small></div>',
        '<div class="bullpen-chain-step__body"><header><span>', html_escape(rows$pocket_label[[index]]), ' &middot; vs ',
        html_escape(rows$upcoming_batter_side[[index]]), 'HB</span><strong>', html_escape(fmt_score(rows$selection_score[[index]])),
        '</strong></header><h3>', html_escape(rows$pitcher_name[[index]]), ' <small>', html_escape(rows$throws[[index]]), 'HP</small></h3>',
        '<p>Availability ', html_escape(fmt_rate(rows$availability_entering[[index]])), ' &middot; matchup ',
        html_escape(fmt_rate(rows$matchup_score[[index]])), ' &middot; planned ', html_escape(fmt_int(rows$estimated_pitches[[index]])),
        ' pitches</p><footer>Alternatives: ', html_escape(rows$alternatives[[index]]), '</footer></div></li>'
      )
    }, character(1))
    paste0(
      '<article class="bullpen-chain"><header><div><span class="eyebrow">Defending ', html_escape(rows$offense_team[[1L]]),
      '</span><h3>', html_escape(team), ' bullpen path</h3></div><span>Scenario</span></header><ol>',
      paste0(steps, collapse = ''), '</ol></article>'
    )
  }, character(1))
  paste0(
    '<section class="bullpen-chain-section"><div class="section-heading section-heading--tight"><span class="eyebrow">Reliever chain planner</span>',
    '<h2>Handedness, leverage, and fatigue change the next arm</h2><p>Once a starter exits, each three-batter pocket reranks the available bullpen and carries planned workload into the next decision.</p></div>',
    '<div class="bullpen-chain-grid">', paste0(chain_cards, collapse = ''), '</div>',
    '<div class="method-callout"><strong>Selector score, not probability:</strong> the displayed score transparently combines availability, role fit, performance, handedness, and leverage. Active rosters and confirmed batting order remain required before operational use.</div></section>'
  )
}

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

home_change_cards <- vapply(seq_len(min(4L, nrow(change_spotlights))), function(index) {
  player_context_card(change_spotlights[index, , drop = FALSE], compact = TRUE)
}, character(1))
write_fragment("home-player-change.html", c(
  '<section class="section-heading"><span class="eyebrow">Change Engine</span><h2>The number moved. Here is the context.</h2><p>Every signal pairs a recent-versus-prior z-score with the player\'s full-season MLB percentile, so a hot stretch is never mistaken for an elite season.</p></section>',
  '<div class="player-context-grid player-context-grid--compact">', home_change_cards, '</div>',
  '<div class="section-action"><a class="btn btn-metallic" href="player-change-engine.html">Open the full Player Change Engine</a></div>'
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

newsletter_hitter <- change_spotlights[change_spotlights$perspective == "batter" & change_spotlights$dominant_change_direction == "improving", , drop = FALSE][1L, ]
newsletter_pitcher <- change_spotlights[change_spotlights$perspective == "pitcher" & change_spotlights$dominant_change_direction == "improving", , drop = FALSE][1L, ]
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
    '<section class="newsletter-mast"><div><span class="eyebrow">Daily edition &middot; ', html_escape(updated_label),
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
    '</strong></span><span><small>Biggest shift</small><strong>', html_escape(fmt_z(newsletter_hitter$dominant_change_z[[1]])),
    '</strong></span><span><small>Season context</small><strong>', html_escape(paste0(fmt_ordinal(newsletter_hitter$dominant_season_percentile[[1]]), " pct.")), '</strong></span></div>'),
  '<p class="method-note">The recent window and prior baseline do not overlap. Form score is a cohort-relative signal, not a rest-of-season projection.</p></section>',
  '<section class="newsletter-story"><span class="eyebrow">Run prevention</span>',
  paste0('<h2>', html_escape(newsletter_pitcher$player_name[[1]]), ' has changed the quality of contact</h2>'),
  paste0('<p>The ', html_escape(newsletter_pitcher$team[[1]]), ' pitcher has allowed a ',
    html_escape(fmt_dec(newsletter_pitcher$recent_ops[[1]])), ' OPS in the recent window. The prior baseline was ',
    html_escape(fmt_dec(newsletter_pitcher$baseline_ops[[1]])), '.</p>'),
  paste0('<div class="evidence-row"><span><small>Recent BF</small><strong>', html_escape(fmt_int(newsletter_pitcher$recent_pa[[1]])),
    '</strong></span><span><small>Biggest shift</small><strong>', html_escape(fmt_z(newsletter_pitcher$dominant_change_z[[1]])),
    '</strong></span><span><small>Season context</small><strong>', html_escape(paste0(fmt_ordinal(newsletter_pitcher$dominant_season_percentile[[1]]), " pct.")), '</strong></span></div>'),
  paste0('<p class="method-note"><strong>Context read:</strong> ', html_escape(newsletter_pitcher$change_context[[1]]), '</p>'),
  '</section>',
  paste0('<section class="newsletter-story"><span class="eyebrow">Simulation center preview</span><h2>',
    html_escape(feature_projection$away_team[[1L]]), ' at ', html_escape(feature_projection$home_team[[1L]]),
    ' profiles as a ', html_escape(projection_lean(feature_projection$winner_probability[[1L]])), '</h2><p>The demonstration score layer gives ',
    html_escape(feature_projection$projected_winner[[1L]]), ' a ', html_escape(fmt_rate(feature_projection$winner_probability[[1L]])),
    ' win share, with a ', html_escape(fmt_dec(feature_projection$mean_total_runs[[1L]], 1L)),
    '-run mean total and a ', html_escape(fmt_rate(feature_projection$one_run_probability[[1L]])),
    ' chance of a one-run result.</p><div class="evidence-row"><span><small>Away mean</small><strong>',
    html_escape(fmt_dec(feature_projection$away_mean_runs[[1L]], 1L)), '</strong></span><span><small>Home mean</small><strong>',
    html_escape(fmt_dec(feature_projection$home_mean_runs[[1L]], 1L)), '</strong></span><span><small>Extras</small><strong>',
    html_escape(fmt_rate(feature_projection$extra_innings_probability[[1L]])), '</strong></span></div><p class="method-note">Representative matchup only; starters, lineups, park, and weather are not connected yet. <a href="projections.html">Open the simulation center.</a></p></section>'),
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
  '<section class="newsletter-note newsletter-note--dark"><span class="eyebrow">Today&rsquo;s reading list</span><h2>Go deeper</h2>',
  '<a href="today.html">Open the signal desk <span>&rarr;</span></a>',
  '<a href="pitch-lab.html">Inspect the Pitch Lab <span>&rarr;</span></a>',
  '<a href="methodology.html">Audit the methods <span>&rarr;</span></a>',
  '</section></aside></div>'
))

change_profile_cards <- vapply(seq_len(nrow(change_spotlights)), function(index) {
  player_context_card(change_spotlights[index, , drop = FALSE], compact = FALSE)
}, character(1))
change_board <- utils::head(all_changes, 30L)
write_fragment("player-change-cards.html", c(
  '<div class="change-method-strip"><strong>Two lenses, one read</strong><span><b>Change z-score</b> compares each player\'s recent shift with the MLB cohort. <b>Season percentile</b> compares the player\'s full-season level with league peers.</span></div>',
  '<section class="section-heading"><span class="eyebrow">Balanced change radar</span><h2>Eight players whose underlying conversation moved</h2><p>The board deliberately includes hitters, pitchers, improvements, and declines. Signal priority rewards unusual movement while accounting for recent-sample reliability.</p></section>',
  '<div class="player-context-grid">', change_profile_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">League change board</span><h2>The 30 strongest context signals</h2><p>The selected stat is the largest absolute direction-aware z-score across OPS, estimated wOBA, strikeout rate, walk rate, hard-hit rate, and run value per plate appearance.</p></div>',
  render_table(change_board, c("player_name", "team", "perspective", "dominant_change_label", "dominant_change_z", "dominant_season_percentile", "recent_pa", "baseline_pa", "change_signal_score"),
    c("Player", "Team", "Role", "Biggest change", "Change z", "Season pct.", "Recent", "Prior", "Signal"),
    list(dominant_change_z = fmt_z, dominant_season_percentile = fmt_ordinal, recent_pa = fmt_int, baseline_pa = fmt_int, change_signal_score = fmt_score), "data-table change-board-table"),
  '</section>',
  '<div class="method-callout"><strong>Interpretation boundary:</strong> a +2 SD change means the direction of movement is unusual among qualified peers; it does not mean the player is two standard deviations above league quality. Season percentiles answer the quality question separately. Neither is a projection.</div>'
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
  stat_card("Pitch context", "Sequence change", "Previous to current", "Every pitch can be evaluated relative to the pitch before it.", "navy"),
  stat_card("Location", "Separation", "Feet at the plate", "Measures how far consecutive pitches move across the hitting window.", "steel"),
  stat_card("Contact", "Quality", "EV + launch angle", "Hard-hit and barrel-proxy fields remain explicitly labeled.", "red"),
  '</section>',
  '<div class="section-action"><a class="btn btn-metallic" href="matchups.html">Explore handedness matchup edges</a></div>'
))

projection_cards <- vapply(seq_len(nrow(daily_projections)), function(index) {
  projection_game_card(daily_projections[index, , drop = FALSE])
}, character(1))
write_fragment("daily-projections.html", c(
  '<div class="projection-status-strip"><span><strong>Development interface</strong> Representative matchups, not today&rsquo;s MLB schedule</span><span>20,000 simulations per game &middot; team-strength inputs only</span></div>',
  '<section class="section-heading"><span class="eyebrow">Daily projection board</span><h2>The whole slate in one scan</h2><p>When the automated run finishes, each game card will show the win split, score center, uncertainty, close-game chance, and input status before a reader opens the deeper matchup view.</p></section>',
  '<div class="projection-slate-grid">', projection_cards, '</div>',
  projection_feature(feature_projection),
  projection_input_board(feature_projection_input),
  projection_hook_visual(projection_hook_path),
  bullpen_chain_visual(bullpen_chains),
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Slate table</span><h2>Every game, every uncertainty signal</h2><p>The published version will replace this representative slate after schedule, probable pitcher, confirmed lineup, park, and weather gates pass.</p></div>',
  render_table(daily_projections, c("away_team", "home_team", "away_win_probability", "home_win_probability", "away_mean_runs", "home_mean_runs", "mean_total_runs", "one_run_probability", "extra_innings_probability", "projected_winner"),
    c("Away", "Home", "Away win", "Home win", "Away runs", "Home runs", "Total", "One-run", "Extras", "Model lean"),
    list(away_win_probability = fmt_rate, home_win_probability = fmt_rate, away_mean_runs = function(x) fmt_dec(x, 1L), home_mean_runs = function(x) fmt_dec(x, 1L), mean_total_runs = function(x) fmt_dec(x, 1L), one_run_probability = fmt_rate, extra_innings_probability = fmt_rate)),
  '</section>',
  '<section class="projection-model-card"><div><span class="eyebrow">Model card &middot; score layer v1</span><h2>What this demonstration does &mdash; and does not &mdash; know</h2></div><div class="projection-model-grid"><span><small>Included now</small><strong>Team offense, run prevention, form, bullpen readiness, home field</strong></span><span><small>Next inputs</small><strong>Probable starters, confirmed lineups, platoons, park, weather, reliever chain</strong></span><span><small>Publication gate</small><strong>Time-based calibration and daily input-completeness checks</strong></span></div></section>',
  '<div class="method-callout"><strong>Interpretation boundary:</strong> these matchups are an interface demonstration generated from the June 14 team snapshot. The probabilities are real Monte Carlo outputs from an uncalibrated team-strength score layer, but they are not today&rsquo;s schedule, betting advice, or public forecasts.</div>'
))

write_fragment("home-projections.html", c(
  '<section class="section-heading"><span class="eyebrow">Simulation center preview</span><h2>Probabilities with the uncertainty left in</h2><p>The future daily board will report a lean, a score range, and the factors behind the number &mdash; never a naked percentage.</p></section>',
  '<div class="projection-slate-grid projection-slate-grid--home">', projection_cards[seq_len(min(3L, length(projection_cards)))], '</div>',
  '<div class="section-action"><a class="btn btn-metallic" href="projections.html">Preview the complete projection board</a></div>'
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

team_names <- as.character(team_intelligence$team)
for (index in seq_along(team_names)) {
  team <- team_names[[index]]
  slug <- slugify(team)
  team_row <- team_intelligence[team_intelligence$team == team, , drop = FALSE][1L, ]
  team_hitters <- hitters[hitters$team == team, , drop = FALSE]
  team_hitters <- team_hitters[order(-num(team_hitters$woba_estimate), -num(team_hitters$pa)), , drop = FALSE]
  team_pitchers <- pitchers[pitchers$team == team, , drop = FALSE]
  team_pitchers <- team_pitchers[order(num(team_pitchers$woba_estimate), -num(team_pitchers$pa)), , drop = FALSE]
  team_changes <- all_changes[all_changes$team == team, , drop = FALSE]
  team_changes <- team_changes[order(-num(team_changes$change_signal_score), -num(team_changes$dominant_change_abs_z)), , drop = FALSE]
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
  form_cards <- if (nrow(team_changes)) {
    vapply(seq_len(min(2L, nrow(team_changes))), function(row_index) player_context_card(team_changes[row_index, , drop = FALSE], compact = FALSE), character(1))
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
    '<section class="section-heading"><span class="eyebrow">Why it changed</span><h2>Player movement with league context attached</h2><p>The recent-window shift and the full-season league standing are separate, so a surge never floats free of the player\'s actual level.</p></section>',
    if (length(form_cards)) paste0('<div class="player-context-grid player-context-grid--dossier">', paste0(form_cards, collapse = ""), '</div>') else '<div class="method-callout">No players met both recent and baseline form thresholds.</div>',
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
