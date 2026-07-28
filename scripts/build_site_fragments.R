site_root <- normalizePath(".", winslash = "/")
data_dir <- file.path(site_root, "data", "derived")
include_dir <- file.path(site_root, "includes")
dir.create(include_dir, recursive = TRUE, showWarnings = FALSE)

read_product <- function(name) {
  path <- file.path(data_dir, name)
  if (!file.exists(path)) stop("Missing derived site product: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}

read_optional_product <- function(name, columns) {
  path <- file.path(data_dir, name)
  if (file.exists(path)) {
    return(utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      encoding = "UTF-8"
    ))
  }
  output <- as.data.frame(
    stats::setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
  output
}

decode_unicode_tokens <- function(value) {
  value <- as.character(value)
  vapply(value, function(item) {
    if (is.na(item)) return(NA_character_)
    if (grepl("Ã|Â", item)) {
      repaired <- suppressWarnings(iconv(item, from = "latin1", to = "UTF-8"))
      if (!is.na(repaired)) item <- repaired
    }
    if (grepl("\u00C3|\u00C2", item)) {
      repaired <- suppressWarnings(iconv(item, from = "latin1", to = "UTF-8"))
      if (!is.na(repaired)) item <- repaired
    }
    byte_fallbacks <- c("C)" = "é", "C!" = "á", "C-" = "í", "C1" = "ñ", "C3" = "ó", "C:" = "ú")
    for (token in names(byte_fallbacks)) item <- gsub(token, byte_fallbacks[[token]], item, fixed = TRUE)
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
fmt_signed <- function(value, digits = 1L) {
  value <- num(value)
  ifelse(
    is.finite(value),
    paste0(ifelse(value > 0, "+", ""), format(round(value, digits), nsmall = digits)),
    "-"
  )
}
fmt_z <- function(value) {
  value <- num(value)
  ifelse(is.finite(value), fmt_score(pmin(pmax(50 + 15 * value, 0), 100)), "-")
}
fmt_standard_z <- function(value) {
  value <- num(value)
  ifelse(
    is.finite(value),
    paste0(ifelse(value > 0, "+", ""), format(round(value, 2L), nsmall = 2L), " z"),
    "-"
  )
}
fmt_ordinal <- function(value) {
  value <- round(num(value))
  remainder_100 <- value %% 100
  suffix <- ifelse(remainder_100 %in% 11:13, "th", ifelse(value %% 10 == 1, "st", ifelse(value %% 10 == 2, "nd", ifelse(value %% 10 == 3, "rd", "th"))))
  paste0(value, suffix)
}
fmt_yes_no <- function(value) ifelse(as.logical(value), "Yes", "No")

graphics_cache_token <- ""
write_fragment <- function(name, lines) {
  if (nzchar(graphics_cache_token)) {
    lines <- gsub(
      "(images/graphics-feed/[^\\\"'?[:space:]]+[.]png)(?![?])",
      paste0("\\1?v=", graphics_cache_token),
      lines,
      perl = TRUE
    )
  }
  writeLines(enc2utf8(lines), file.path(include_dir, name), useBytes = TRUE)
}

slugify <- function(value) {
  value <- iconv(as.character(value), from = "", to = "ASCII//TRANSLIT")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("(^-|-$)", "", value)
}

team_theme_path <- file.path(site_root, "config", "mlb-team-themes.csv")
if (!file.exists(team_theme_path)) stop("Missing MLB team theme configuration.", call. = FALSE)
team_themes <- utils::read.csv(team_theme_path, stringsAsFactors = FALSE, check.names = FALSE)
required_theme_columns <- c("team", "slug", "primary", "secondary", "accent")
if (!all(required_theme_columns %in% names(team_themes))) {
  stop("MLB team theme configuration is missing required columns.", call. = FALSE)
}
theme_css <- c(
  "/* Generated from config/mlb-team-themes.csv. */",
  vapply(seq_len(nrow(team_themes)), function(index) {
    row <- team_themes[index, , drop = FALSE]
    paste0(
      ".article-theme--", row$slug, ", .theme-", row$slug, " {",
      "--team-primary:", row$primary, ";",
      "--team-secondary:", row$secondary, ";",
      "--team-accent:", row$accent, ";",
      "}"
    )
  }, character(1)),
  vapply(seq_len(nrow(team_themes)), function(index) {
    row <- team_themes[index, , drop = FALSE]
    paste0(
      "body:has(.theme-", row$slug, ")::before {",
      "background:radial-gradient(circle at 12% 8%,color-mix(in srgb,", row$secondary,
      " 20%,transparent),transparent 30rem),",
      "linear-gradient(180deg,color-mix(in srgb,", row$primary,
      " 12%,white),#F6F9FC 50%,color-mix(in srgb,", row$accent, " 24%,white));}"
    )
  }, character(1))
)
writeLines(theme_css, file.path(include_dir, "article-team-themes.css"), useBytes = TRUE)

rank_meter <- function(label, rank, detail) {
  width <- pmax(pmin(100 * (31 - num(rank)) / 30, 100), 3)
  paste0(
    '<div class="rank-meter"><div class="rank-meter__label"><span>', html_escape(label), '</span><strong>#',
    html_escape(fmt_int(rank)), '</strong></div><div class="rank-meter__track" role="img" aria-label="',
    html_escape(paste(label, "rank", fmt_int(rank), "of 30")), '"><span style="width:',
    base::format(round(width, 1), nsmall = 1), '%"></span></div><small>', html_escape(detail), '</small></div>'
  )
}

positional_war_cell <- function(row) {
  status <- as.character(row$status[[1L]])
  status_label <- if (status == "strength") "Strength" else if (status == "need") "Need" else "Middle"
  paste0(
    '<article class="position-war-cell is-', html_escape(status), '">',
    '<header><span>', html_escape(row$position[[1L]]), '</span><small>', html_escape(status_label), '</small></header>',
    '<h3>', html_escape(row$position_label[[1L]]), '</h3>',
    '<div><strong>', html_escape(fmt_dec(row$war[[1L]], 1L)), '</strong><span>WAR</span><b>#',
    html_escape(fmt_int(row$mlb_rank[[1L]])), '<small> of ', html_escape(fmt_int(row$teams_ranked[[1L]])), '</small></b></div>',
    '<footer><span>Top value</span><strong>', html_escape(row$top_player[[1L]]), '</strong><small>',
    html_escape(fmt_dec(row$top_player_war[[1L]], 1L)), ' WAR</small></footer></article>'
  )
}

arsenal_spotlight_card <- function(rows) {
  featured <- rows[rows$pitch_type == rows$featured_pitch_type[[1L]], , drop = FALSE]
  if (!nrow(featured)) featured <- rows[1L, , drop = FALSE]
  rows <- rows[order(-num(rows$usage_rate)), , drop = FALSE]
  arsenal_rows <- vapply(seq_len(nrow(rows)), function(index) {
    is_featured <- rows$pitch_type[[index]] == rows$featured_pitch_type[[1L]]
    paste0(
      '<li class="', if (is_featured) 'is-featured' else '', '"><span>', html_escape(rows$pitch_name[[index]]),
      '</span><i><b style="width:', html_escape(fmt_score(100 * num(rows$usage_rate[[index]]))), '%"></b></i><strong>',
      html_escape(fmt_rate(rows$usage_rate[[index]])), '</strong><small>', html_escape(fmt_rate(rows$whiff_rate[[index]])), ' whiff</small></li>'
    )
  }, character(1))
  paste0(
    '<article class="arsenal-spotlight-card"><header><div><span class="eyebrow">Emerging weapon #',
    html_escape(fmt_int(rows$spotlight_rank[[1L]])), '</span><h3>', html_escape(rows$player_name[[1L]]),
    '</h3><p>', html_escape(rows$team[[1L]]), ' &middot; throws ', html_escape(rows$hand[[1L]]),
    '</p></div><strong>', html_escape(rows$featured_pitch_name[[1L]]), '</strong></header>',
    '<div class="arsenal-spotlight-card__signal"><span><small>Usage change</small><strong>+',
    html_escape(fmt_dec(rows$featured_usage_delta_pp[[1L]], 1L)), ' pts</strong></span><span><small>Whiff change</small><strong>',
    html_escape(ifelse(num(rows$featured_whiff_delta[[1L]]) > 0, "+", "")), html_escape(fmt_rate(rows$featured_whiff_delta[[1L]])),
    '</strong></span><span><small>Recent pitch sample</small><strong>', html_escape(fmt_int(rows$featured_recent_pitches[[1L]])),
    '</strong></span><span><small>Earlier pitch sample</small><strong>', html_escape(fmt_int(rows$featured_baseline_pitches[[1L]])),
    '</strong></span><span><small>Shape</small><strong>', html_escape(fmt_dec(featured$average_horizontal_break[[1L]], 1L)),
    ' HB / ', html_escape(fmt_dec(featured$average_induced_vertical_break[[1L]], 1L)), ' IVB</strong></span></div>',
    '<ol class="arsenal-mix">', paste0(arsenal_rows, collapse = ""), '</ol></article>'
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
    '</span><strong>', html_escape(fmt_standard_z(z)), '</strong></div>',
    '<div class="change-z-track" role="img" aria-label="',
    html_escape(paste(label, fmt_standard_z(z), "recent", fmt_context_metric(metric, recent), "baseline", fmt_context_metric(metric, baseline))), '">',
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
    html_escape(row$dominant_change_label[[1L]]), ' ', html_escape(fmt_standard_z(row$dominant_change_z[[1L]])),
    '</strong><p>', html_escape(row$change_context[[1L]]), '</p></div>',
    '<div class="player-context-card__body"><section><div class="context-subhead"><span>Season context</span><small>MLB percentile</small></div>',
    '<div class="percentile-stack">', paste0(percentile_html, collapse = ""), '</div>',
    '<div class="percentile-axis" aria-hidden="true"><span>0</span><span>League midpoint</span><span>100</span></div></section>',
    '<section><div class="context-subhead"><span>What changed</span><small>Recent vs earlier season</small></div>',
    '<div class="change-z-stack">', paste0(change_html, collapse = ""), '</div>',
    '<div class="change-z-axis" aria-hidden="true"><span>Cooling</span><span>50</span><span>Improving</span></div></section></div>',
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
    html_escape(row$team), ' | ', html_escape(ifelse(row$hand == "L", "LHP", "RHP")), ' | ',
    html_escape(row$pitch_name), '</p></div><span class="signal-score">',
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

pitch_peer_percentile <- function(row, metric, peer_pool, lower_is_better = FALSE) {
  value <- num(row[[metric]][[1L]])
  peers <- peer_pool[
    peer_pool$pitch_type == row$pitch_type[[1L]] &
      num(peer_pool$pitches) >= 50,
    ,
    drop = FALSE
  ]
  peer_values <- num(peers[[metric]])
  peer_values <- peer_values[is.finite(peer_values)]
  if (!is.finite(value) || !length(peer_values)) return(NA_real_)
  100 * if (isTRUE(lower_is_better)) {
    mean(peer_values >= value)
  } else {
    mean(peer_values <= value)
  }
}

arsenal_metric_cell <- function(row, metric, formatter, peer_pool, lower_is_better = FALSE) {
  value <- row[[metric]][[1L]]
  percentile <- pitch_peer_percentile(row, metric, peer_pool, lower_is_better)
  percentile_class <- if (!is.finite(percentile)) {
    "is-neutral"
  } else if (percentile >= 80) {
    "is-elite"
  } else if (percentile >= 60) {
    "is-strong"
  } else if (percentile < 35) {
    "is-low"
  } else {
    "is-neutral"
  }
  paste0(
    '<td><span class="arsenal-value">', html_escape(formatter(value)),
    if (is.finite(percentile)) paste0(
      '<small class="arsenal-percentile ', percentile_class, '">',
      html_escape(fmt_ordinal(percentile)), ' pct</small>'
    ) else '',
    '</span></td>'
  )
}

team_ace_arsenal_card <- function(ace, arsenal, peer_pool) {
  arsenal <- arsenal[order(-num(arsenal$usage_rate), -num(arsenal$pitches)), , drop = FALSE]
  arsenal_rows <- vapply(seq_len(nrow(arsenal)), function(index) {
    row <- arsenal[index, , drop = FALSE]
    paste0(
      '<tr><td class="arsenal-pitch-name"><strong>', html_escape(row$pitch_name[[1L]]),
      '</strong><small>', html_escape(row$pitch_type[[1L]]), ' &middot; ',
      html_escape(fmt_int(row$pitches[[1L]])), ' pitches</small></td>',
      '<td>', html_escape(fmt_rate(row$usage_rate[[1L]])), '</td>',
      arsenal_metric_cell(row, "average_velocity", function(value) fmt_dec(value, 1L), peer_pool),
      arsenal_metric_cell(row, "whiff_rate", fmt_rate, peer_pool),
      arsenal_metric_cell(row, "chase_rate", fmt_rate, peer_pool),
      arsenal_metric_cell(row, "zone_rate", fmt_rate, peer_pool),
      arsenal_metric_cell(row, "putaway_rate", fmt_rate, peer_pool),
      arsenal_metric_cell(row, "hard_hit_rate", fmt_rate, peer_pool, TRUE),
      '</tr>'
    )
  }, character(1))
  hand <- if (arsenal$hand[[1L]] == "L") "LHP" else "RHP"
  paste0(
    '<article class="team-ace-card"><header><div><span class="eyebrow">Staff ace &middot; ',
    html_escape(hand), '</span><h3>', html_escape(ace$player_name[[1L]]), '</h3><p>',
    html_escape(fmt_dec(ace$innings_display[[1L]], 1L)), ' IP &middot; ',
    html_escape(fmt_dec(ace$era[[1L]], 2L)), ' ERA &middot; ',
    html_escape(fmt_dec(ace$war[[1L]], 1L)), ' WAR</p></div><strong>',
    html_escape(fmt_int(sum(num(arsenal$pitches), na.rm = TRUE))), '<small> tracked pitches</small></strong></header>',
    '<div class="table-scroll"><table class="data-table ace-arsenal-table"><thead><tr>',
    '<th>Pitch</th><th>Use</th><th>Velo</th><th>Whiff</th><th>Chase</th><th>Zone</th><th>Putaway</th><th>Hard hit</th>',
    '</tr></thead><tbody>', paste0(arsenal_rows, collapse = ""), '</tbody></table></div>',
    '<footer>Percentiles compare each pitch only with the same MLB pitch type and a minimum 50-pitch peer sample. Hard-hit percentile is reversed so a higher percentile is better.</footer></article>'
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
      cell_class <- ""
      if (column == "legacy_context") {
        cell_class <- if (grepl("Hall bound", as.character(value), fixed = TRUE)) " class=\"context-positive\"" else " class=\"context-negative\""
      }
      if (column == "confidence") {
        cell_class <- if (tolower(as.character(value)) %in% c("high", "strong", "excellent")) " class=\"context-positive\"" else " class=\"context-negative\""
      }
      paste0("<td", cell_class, ">", shown, "</td>")
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
article_teams <- c(
  "durbin_article" = "Boston Red Sox",
  "bello_article_final" = "Boston Red Sox",
  "A.J Ewing Gets the Call" = "New York Mets",
  "Series Recap Tigers Sox" = "Boston Red Sox",
  "CleanPig" = "Boston Red Sox",
  "sorianopreseason26" = "Los Angeles Angels",
  "ceddannesnewgroove" = "Boston Red Sox"
)
article_topics <- c(
  "durbin_article" = "Hitting",
  "bello_article_final" = "Pitching",
  "A.J Ewing Gets the Call" = "Prospects",
  "Series Recap Tigers Sox" = "Series",
  "CleanPig" = "Pitching",
  "sorianopreseason26" = "Pitching",
  "ceddannesnewgroove" = "Hitting"
)
article_fallback_images <- c(
  "CleanPig" = "images/crochet.png",
  "Series Recap Tigers Sox" = "images/SABRHOODpng.png",
  "sorianopreseason26" = "images/Soriano.jpg"
)

build_article_index <- function() {
  article_directories <- file.path(site_root, c("posts", "articles"))
  article_directories <- article_directories[dir.exists(article_directories)]
  paths <- unlist(lapply(article_directories, list.files, pattern = "\\.qmd$", full.names = TRUE), use.names = FALSE)
  paths <- paths[!startsWith(basename(paths), "_")]
  paths <- paths[basename(paths) != "fla 2025 v 2026 article.qmd"]
  rows <- lapply(paths, function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    closing <- which(lines == "---")
    front <- if (length(closing) >= 2L) lines[seq.int(2L, closing[[2L]] - 1L)] else lines
    if (tolower(frontmatter_value(front, "draft", "false")) == "true") return(NULL)
    stem <- tools::file_path_sans_ext(basename(path))
    source_directory <- basename(dirname(path))
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
    team <- frontmatter_value(front, "team", article_teams[[stem]])
    if (is.null(team) || !nzchar(team)) team <- "League"
    topic <- frontmatter_value(front, "topic", article_topics[[stem]])
    if (is.null(topic) || !nzchar(topic)) topic <- "Research"
    category <- paste(team, topic, sep = " | ")
    data.frame(
      stem = stem,
      title = frontmatter_value(front, "title", stem),
      date = as.Date(frontmatter_value(front, "date", "1900-01-01")),
      description = description,
      category = category,
      team = team,
      theme_slug = slugify(team),
      image = image_path,
      href = paste0(source_directory, "/", utils::URLencode(stem, reserved = TRUE), ".html"),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  output <- do.call(rbind, rows)
  output[order(output$date, decreasing = TRUE), , drop = FALSE]
}

article_card <- function(article, featured = FALSE) {
  class_name <- paste(
    if (isTRUE(featured)) "article-feature" else "article-card",
    paste0("article-theme--", article$theme_slug)
  )
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
history_match_notes <- read_product("history-match-notes.csv")
daily_retrosheet_history <- read_product("daily-retrosheet-history.csv")
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
career_trajectories <- read_product("career-trajectory-projections.csv")
career_comparables <- read_product("career-trajectory-comparables.csv")
career_three_season <- read_product("career-three-season-forecasts.csv")
career_model_card <- read_product("career-trajectory-model-card.csv")
career_backtest <- read_product("career-trajectory-backtest-summary.csv")
career_calibration <- read_product(
  "career-trajectory-probability-calibration.csv"
)
career_holdout <- read_product("career-trajectory-holdout-validation.csv")
career_rate_validation <- read_product(
  "career-trajectory-rate-validation.csv"
)
career_weight_tuning <- read_product("career-trajectory-weight-tuning.csv")
career_tuning_evaluation <- read_product(
  "career-trajectory-tuning-evaluation.csv"
)
career_weight_profiles <- read_product(
  "career-trajectory-weight-profiles.csv"
)
re24 <- read_product("run-expectancy-24.csv")
bullpen <- read_product("bullpen-availability.csv")
manager <- read_product("manager-data-summary.csv")
manager_model <- read_product("manager-hook-model.csv")
hook_validation <- read_product("manager-hook-validation-metrics.csv")
hook_calibration <- read_product("manager-hook-calibration.csv")
hook_scenarios <- read_product("manager-hook-scenarios.csv")
bullpen_matchups <- read_product("active-roster-bullpen-selector.csv")
daily_projections <- read_product("daily-projections-live.csv")
projection_margins <- read_product("daily-projection-margin-live.csv")
projection_scorelines <- read_product("daily-projection-scorelines-live.csv")
projection_drivers <- read_product("daily-projection-drivers-live.csv")
projection_components <- read_product("daily-projection-components-live.csv")
projection_publication <- read_product("projection-publication-readiness.csv")
projection_inputs <- read_product("projection-input-readiness.csv")
bullpen_chains <- read_product("bullpen-chain-demo.csv")
projection_hook_path <- read_product("projection-hook-path.csv")
pitch_usage_changes <- read_product("pitch-usage-change-board.csv")
daily_game_inputs <- read_product("daily-game-inputs.csv")
daily_batting_orders <- read_product("daily-batting-orders.csv")
daily_probables <- read_product("daily-probable-starters.csv")
active_rosters <- read_product("active-rosters.csv")
active_roster_bullpens <- read_product("active-roster-bullpens.csv")
daily_park_weather <- read_product("daily-park-weather.csv")
daily_series_context <- read_optional_product(
  "daily-series-context.csv",
  c(
    "current_game_id", "team", "opponent", "series_game_number",
    "series_start", "played_yesterday"
  )
)
daily_series_players <- read_optional_product(
  "daily-series-player-lines.csv",
  c(
    "current_game_id", "team", "role", "player_name", "hits",
    "home_runs", "runs_batted_in", "innings_outs", "strikeouts"
  )
)
daily_recent_games <- read_optional_product(
  "daily-recent-game-lines.csv",
  c(
    "current_game_id", "team", "opponent", "role", "player_name",
    "stat_line", "performance_score"
  )
)
daily_slate_status <- read_product("daily-slate-status.csv")
aaa_hitters <- read_product("aaa-hitter-watch.csv")
aaa_pitchers <- read_product("aaa-pitcher-watch.csv")
aaa_callups <- read_product("aaa-call-up-radar.csv")
aaa_standings <- read_product("aaa-standings-current.csv")
aaa_standings_movement <- read_product("aaa-standings-movement.csv")
aaa_team_rankings <- read_product("aaa-team-rankings.csv")
hitter_tracking <- read_product("hitter-tracking-totals.csv")
pitcher_tracking <- read_product("pitcher-tracking-totals.csv")
team_tracking <- read_product("team-tracking-totals.csv")
mlb_standings <- read_product("mlb-standings-current.csv")
mlb_standings_movement <- read_product("mlb-standings-movement.csv")
newsletter_stories <- read_product("daily-newsletter-stories.csv")
newsletter_edition <- read_product("daily-newsletter-edition.csv")
fangraphs_hitters <- read_product("fangraphs-season-hitters.csv")
fangraphs_pitchers <- read_product("fangraphs-season-pitchers.csv")
player_market_groups <- read_product("player-market-groups.csv")
player_market_players <- read_product("player-market-players.csv")
award_races <- read_product("award-race-board.csv")
award_race_history <- read_product("award-race-history.csv")
award_race_display <- read_product("award-race-display.csv")
award_race_events <- read_product("award-race-events.csv")
award_race_current <- read_product("award-race-current-leaders.csv")
mvp_era_profiles <- read_product("mvp-era-stat-profiles.csv")
mvp_modern_weights <- read_product("mvp-modern-model-weights.csv")
graphics_manifest <- read_product("graphics-feed-manifest.csv")
graphics_cache_token <- gsub(
  "[^0-9]",
  "",
  as.character(max(graphics_manifest$source_acquired_at_utc, na.rm = TRUE))
)
player_probabilities <- read_product("daily-player-probabilities.csv")
player_simulations <- read_product("daily-player-simulations.csv")
player_simulation_model <- read_product("daily-player-simulation-model-card.csv")
matchup_event_probabilities <- read_product("daily-matchup-event-probabilities.csv")
matchup_event_diagnostics <- read_product("daily-matchup-event-diagnostics.csv")
matchup_event_model <- read_product("daily-matchup-event-model-card.csv")
state_simulation_games <- read_product("daily-state-simulation-games.csv")
state_simulation_hitters <- read_product("daily-state-simulation-hitters.csv")
state_simulation_bullpens <- read_product("daily-state-simulation-bullpen-inputs.csv")
state_simulation_model <- read_product("daily-state-simulation-model-card.csv")
state_simulation_relievers <- read_product("daily-state-simulation-relievers.csv")
state_simulation_events <- read_product("daily-state-simulation-events.csv")
state_calibration_status <- read_product("state-simulation-calibration-status.csv")
baserunning_league <- read_product("baserunning-league-rates.csv")
baserunning_runners <- read_product("baserunning-runner-profiles.csv")
baserunning_pitcher_hold <- read_product("baserunning-pitcher-hold-profiles.csv")
baserunning_parks <- read_product("baserunning-park-factors.csv")
baserunning_model <- read_product("baserunning-model-card.csv")
run_game_pitchers <- read_product("run-game-pitcher-ratings.csv")
run_game_catchers <- read_product("run-game-catcher-ratings.csv")
run_game_runners <- read_product("run-game-runner-ratings.csv")
run_game_counts <- read_product("run-game-count-windows.csv")
run_game_notes <- read_product("run-game-notes.csv")
run_game_model <- read_product("run-game-model-card.csv")
catcher_framing <- read_product("catcher-framing-ratings.csv")
catcher_framing_model <- read_product("catcher-framing-model-card.csv")
abs_challenges <- read_product("abs-challenge-leaderboard.csv")
abs_model <- read_product("abs-challenge-model-card.csv")
official_fielding <- read_product("official-fielding-run-value.csv")
official_team_fielding <- read_product("official-team-fielding-run-value.csv")
fielding_players <- read_product("fielding-player-ratings.csv")
fielding_teams <- read_product("fielding-team-ratings.csv")
advancement_fielders <- read_product("runner-advancement-fielding-ratings.csv")
advancement_teams <- read_product("runner-advancement-team-ratings.csv")
fielding_play_day <- read_product("fielding-play-of-day.csv")
gold_glove_watch <- read_product("gold-glove-watch.csv")
fielding_model <- read_product("fielding-model-card.csv")
game_backtest_metrics <- read_product("game-projection-backtest-metrics.csv")
game_backtest_calibration <- read_product("game-projection-calibration.csv")
game_score_model_card <- read_product("game-score-model-card.csv")
projection_ledger_status <- read_product("projection-ledger-status.csv")
projection_feedback_metrics <- read_product("projection-feedback-metrics.csv")
projection_feedback_ledger <- read_product("projection-feedback-ledger.csv")
player_feedback_metrics <- read_product("player-projection-feedback-metrics.csv")
rolling_pitch_usage <- read_product("rolling-league-pitch-usage.csv")
rolling_production <- read_product("rolling-league-production.csv")
rolling_pitch_quality <- read_product("rolling-league-pitch-quality.csv")
rolling_batted_ball <- read_product("rolling-league-batted-ball.csv")
rolling_workload <- read_product("rolling-league-workload.csv")
insane_awards <- read_product("insane-baseball-awards.csv")
team_positional_war <- read_product("team-positional-war.csv")
hitter_discipline <- read_product("hitter-discipline-profiles.csv")
arsenal_spotlights <- read_product("arsenal-spotlights.csv")
pull_rate_batted_balls <- read_product("pull-rate-leader-batted-balls.csv")
articles <- build_article_index()

team_full_to_abbr <- c(
  "Arizona Diamondbacks" = "ARI", "Athletics" = "ATH", "Atlanta Braves" = "ATL",
  "Baltimore Orioles" = "BAL", "Boston Red Sox" = "BOS", "Chicago Cubs" = "CHC",
  "Chicago White Sox" = "CHW", "Cincinnati Reds" = "CIN", "Cleveland Guardians" = "CLE",
  "Colorado Rockies" = "COL", "Detroit Tigers" = "DET", "Houston Astros" = "HOU",
  "Kansas City Royals" = "KCR", "Los Angeles Angels" = "LAA", "Los Angeles Dodgers" = "LAD",
  "Miami Marlins" = "MIA", "Milwaukee Brewers" = "MIL", "Minnesota Twins" = "MIN",
  "New York Mets" = "NYM", "New York Yankees" = "NYY", "Philadelphia Phillies" = "PHI",
  "Pittsburgh Pirates" = "PIT", "San Diego Padres" = "SDP", "San Francisco Giants" = "SFG",
  "Seattle Mariners" = "SEA", "St. Louis Cardinals" = "STL", "Tampa Bay Rays" = "TBR",
  "Texas Rangers" = "TEX", "Toronto Blue Jays" = "TOR", "Washington Nationals" = "WSN"
)

updated_date <- max(as.Date(hitters$last_game), na.rm = TRUE)
updated_label <- format(updated_date, "%B %d, %Y")
slate_report_date <- suppressWarnings(as.Date(daily_slate_status$report_date[[1L]]))
site_reference_date <- as.Date(Sys.getenv("SABRHOOD_DATE", unset = as.character(Sys.Date())))
is_off_day <- nrow(daily_slate_status) > 0L &&
  identical(as.character(daily_slate_status$slate_state[[1L]]), "no_games_scheduled") &&
  !is.na(slate_report_date) && identical(slate_report_date, site_reference_date)

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
projection_time_match <- match(daily_projections$game_id, daily_game_inputs$game_id)
daily_projections$game_time_utc <- daily_game_inputs$game_time_utc[projection_time_match]
daily_projections <- daily_projections[order(daily_projections$game_time_utc, num(daily_projections$display_order)), , drop = FALSE]
daily_projections$display_order <- seq_len(nrow(daily_projections))
feature_projection <- daily_projections[as.logical(daily_projections$feature_game), , drop = FALSE][1L, ]
daily_game_inputs <- daily_game_inputs[order(daily_game_inputs$game_time_utc), , drop = FALSE]
weather_index <- match(daily_game_inputs$game_id, daily_park_weather$game_id)
daily_game_inputs$temperature_f <- daily_park_weather$temperature_f[weather_index]
daily_game_inputs$wind_mph <- daily_park_weather$wind_mph[weather_index]
daily_game_inputs$precipitation_probability <- daily_park_weather$precipitation_probability[weather_index]
daily_game_inputs$conditions <- daily_park_weather$conditions[weather_index]

pitch_change_candidates <- pitch_usage_changes[order(-num(pitch_usage_changes$change_signal_score), -abs(num(pitch_usage_changes$usage_delta_pp))), , drop = FALSE]
pitch_change_spotlights <- pitch_change_candidates[!duplicated(pitch_change_candidates$pitch_type), , drop = FALSE]
pitch_change_spotlights <- utils::head(pitch_change_spotlights, 6L)
pitch_context_groups <- split(
  pitch_change_candidates[!is.na(pitch_change_candidates$pitch_name) & nzchar(pitch_change_candidates$pitch_name), , drop = FALSE],
  pitch_change_candidates$pitch_name[!is.na(pitch_change_candidates$pitch_name) & nzchar(pitch_change_candidates$pitch_name)]
)
pitch_context_board <- if (length(pitch_context_groups)) {
  do.call(rbind, lapply(pitch_context_groups, function(rows) utils::head(rows, 4L)))
} else {
  pitch_change_candidates[0, , drop = FALSE]
}
pitch_context_board <- pitch_context_board[
  order(-num(pitch_context_board$change_signal_score), -abs(num(pitch_context_board$usage_delta_pp))),
  ,
  drop = FALSE
]
pitch_context_board <- utils::head(pitch_context_board, 25L)
pitch_context_board$context_rank <- seq_len(nrow(pitch_context_board))

aaa_young_hitters <- aaa_hitters[aaa_hitters$age_lens == "age-qualified watch", , drop = FALSE]
aaa_young_hitters <- aaa_young_hitters[order(-num(aaa_young_hitters$performance_score), num(aaa_young_hitters$age)), , drop = FALSE]
aaa_young_pitchers <- aaa_pitchers[aaa_pitchers$age_lens == "age-qualified watch", , drop = FALSE]
aaa_young_pitchers <- aaa_young_pitchers[order(-num(aaa_young_pitchers$performance_score), num(aaa_young_pitchers$age)), , drop = FALSE]

projection_lean <- function(probability) {
  probability <- num(probability)
  if (probability < 0.54) "Near toss-up" else if (probability < 0.60) "Narrow lean" else "Clear lean"
}

projection_game_card <- function(row) {
  away_width <- round(100 * num(row$away_win_probability[[1L]]), 1L)
  home_width <- round(100 * num(row$home_win_probability[[1L]]), 1L)
  paste0(
    '<article class="projection-game-card"><header><span class="eyebrow">Game ', html_escape(fmt_int(row$display_order[[1L]])),
    ' &middot; scheduled slate</span><span class="projection-lean">', html_escape(projection_lean(row$winner_probability[[1L]])), '</span></header>',
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
    '</strong><span>', html_escape(fmt_int(row$simulations[[1L]])), ' draws &middot; ',
    html_escape(row$lineup_status[[1L]]), ' orders &middot; ', html_escape(row$starter_status[[1L]]), ' starters</span></footer></article>'
  )
}

projection_feature <- function(row) {
  game_id <- row$game_id[[1L]]
  margins <- projection_margins[projection_margins$game_id == game_id, , drop = FALSE]
  drivers <- projection_drivers[projection_drivers$game_id == game_id, , drop = FALSE]
  standout_ids <- c(
    "batter_hr_2plus", "pitcher_k_10plus", "batter_tb_3plus",
    "pitcher_k_7plus", "batter_hit_2plus", "batter_hr_1plus"
  )
  standouts <- player_simulations[
    player_simulations$game_id == game_id & player_simulations$metric_id %in% standout_ids,
    ,
    drop = FALSE
  ]
  if (nrow(standouts)) {
    standouts$standout_order <- match(standouts$metric_id, standout_ids)
    standouts <- do.call(rbind, lapply(split(standouts, standouts$metric_id), function(rows) {
      rows <- rows[order(-num(rows$probability), -num(rows$expected_value)), , drop = FALSE]
      utils::head(rows, 1L)
    }))
    standouts <- standouts[order(standouts$standout_order, -num(standouts$probability)), , drop = FALSE]
    standouts <- utils::head(standouts, 4L)
  }
  max_margin <- max(num(margins$probability))
  margin_bars <- vapply(seq_len(nrow(margins)), function(index) {
    width <- 100 * num(margins$probability[[index]]) / max_margin
    paste0(
      '<div class="margin-row"><span>', html_escape(margins$margin_group[[index]]), '</span>',
      '<div class="margin-row__track"><i style="width:', format(round(width, 1), nsmall = 1), '%"></i></div>',
      '<strong>', html_escape(fmt_rate(margins$probability[[index]])), '</strong></div>'
    )
  }, character(1))
  driver_cards <- vapply(seq_len(nrow(drivers)), function(index) {
    paste0('<li><span>', html_escape(drivers$driver_label[[index]]), '</span><strong>',
      html_escape(drivers$advantage_team[[index]]), '</strong><small>', html_escape(drivers$driver_detail[[index]]), '</small></li>')
  }, character(1))
  standout_cards <- if (nrow(standouts)) {
    paste0(vapply(seq_len(nrow(standouts)), function(index) {
      standout <- standouts[index, , drop = FALSE]
      paste0(
        '<article class="projection-standout-card"><span>', html_escape(standout$metric_label[[1L]]),
        '</span><h3>', html_escape(standout$player_name[[1L]]), '</h3><strong>',
        html_escape(fmt_rate(standout$probability[[1L]])), '</strong><small>',
        html_escape(standout$team[[1L]]), ' vs. ', html_escape(standout$opponent[[1L]]),
        ' &middot; expected ', html_escape(fmt_dec(standout$expected_value[[1L]], 2L)),
        '</small></article>'
      )
    }, character(1)), collapse = '')
  } else {
    '<div class="method-callout"><strong>Lineup gate:</strong> standout-performance probabilities appear when current batting orders and probable starters have completed their player-level simulation.</div>'
  }
  paste0(
    '<section class="projection-feature"><div class="projection-feature__head"><div><span class="eyebrow">Feature simulation &middot; ',
    html_escape(fmt_int(row$simulations[[1L]])), ' draws</span><h2>', html_escape(row$away_team[[1L]]),
    ' at ', html_escape(row$home_team[[1L]]), '</h2><p>The closest matchup on today&rsquo;s scheduled slate pairs the score distribution with the players most likely to produce a defining performance.</p></div>',
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
    '<div class="projection-standout-room"><div class="context-subhead"><span>Projected standout performances</span><small>Highest-upside player outcomes in this matchup</small></div><div class="projection-standout-grid">',
    standout_cards, '</div></div></section>'
  )
}

projection_public_audit <- function(game_metrics, player_metrics) {
  if (!nrow(game_metrics)) {
    return('<section class="projection-public-audit"><div class="method-callout"><strong>Model audit:</strong> grading begins after archived pregame forecasts settle. No live result is used to rewrite a prediction that was already published.</div></section>')
  }
  game <- game_metrics[1L, , drop = FALSE]
  prop_count <- if (nrow(player_metrics)) sum(num(player_metrics$settled_predictions), na.rm = TRUE) else 0
  prop_gap <- if (nrow(player_metrics) && prop_count > 0) {
    stats::weighted.mean(
      abs(num(player_metrics$calibration_bias)),
      w = pmax(num(player_metrics$settled_predictions), 0),
      na.rm = TRUE
    )
  } else {
    NA_real_
  }
  paste0(
    '<section class="projection-public-audit"><div class="section-heading section-heading--tight"><span class="eyebrow">Public model audit</span>',
    '<h2>What the archived forecasts have actually done</h2><p>Every number below comes from a prediction saved before first pitch and graded after the game became final.</p></div>',
    '<div class="projection-audit-grid">',
    '<article><span>Correct winner</span><strong>', html_escape(fmt_rate(game$classification_accuracy[[1L]])), '</strong><small>',
    html_escape(fmt_int(game$settled_games[[1L]])), ' settled games</small></article>',
    '<article><span>Final-total error</span><strong>', html_escape(fmt_dec(game$total_runs_mae[[1L]], 2L)), '</strong><small>Mean absolute runs</small></article>',
    '<article><span>Player props graded</span><strong>', html_escape(fmt_int(prop_count)), '</strong><small>Across published event types</small></article>',
    '<article><span>Average prop gap</span><strong>', html_escape(ifelse(is.finite(prop_gap), fmt_rate(prop_gap), "-")), '</strong><small>Prediction vs. observed rate</small></article>',
    '</div><div class="method-callout"><strong>Reading the audit:</strong> accuracy describes winner selection, while the calibration gap asks whether an event projected at a given rate occurred at roughly that rate. The sample is still growing and is reported without hiding misses.</div></section>'
  )
}

projection_input_board <- function(row) {
  gates <- data.frame(
    label = c("Demo slate shell", "Probable starters", "Lineups", "Park factor", "Weather", "Active rosters"),
    ready = c(row$schedule_ready, row$starters_ready, row$lineups_ready, row$park_ready, row$weather_ready, row$rosters_ready),
    detail = c(
      "Representative teams and date are present; this demonstration row is not joined to the live schedule above.",
      "Both starters must be identified before pitcher-specific run expectations.",
      "Projected lineups permit a conditional run; confirmed lineups unlock publication.",
      "A neutral demonstration factor is present; production uses the actual venue.",
      "Roof and weather context are still missing.",
      "The active-roster feed is connected beside the model, but this demonstration row still uses its older proxy."
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
    '<h2>Why the demonstration probabilities remain separate</h2><p>Every modeled game must pass the same six-part input contract. The live slate above is assembled; the representative probability rows below have not yet been rebuilt from those inputs.</p></div>',
    '<div class="projection-readiness__score"><strong>', html_escape(fmt_rate(row$input_completeness[[1L]])),
    '</strong><span>demo probability inputs complete</span></div></div><div class="projection-input-grid">',
    paste0(gate_html, collapse = ''), '</div></section>'
  )
}

projection_publication_board <- function(row) {
  gates <- data.frame(
    label = c("Scheduled games", "Posted lineups", "Probable starters", "Active bullpens", "Current PBP usage", "Park factors", "Chronological calibration"),
    value = c(
      paste0(fmt_int(row$games[[1L]]), " games"),
      paste0(fmt_int(row$games_with_confirmed_lineups[[1L]]), "/", fmt_int(row$games[[1L]]), " confirmed"),
      paste0(fmt_int(row$games_with_matched_starters[[1L]]), "/", fmt_int(row$games[[1L]]), " matched"),
      if (as.logical(row$active_roster_bullpen_model[[1L]])) "Roster verified" else "Missing",
      if (as.logical(row$bullpen_usage_current[[1L]])) "Current" else paste0(fmt_int(row$bullpen_usage_age_days[[1L]]), " days old"),
      if (as.logical(row$empirical_park_factors[[1L]])) "Modeled" else "Neutral placeholder",
      if (as.logical(row$chronological_calibration_complete[[1L]])) "Passed" else "Not yet passed"
    ),
    ready = c(
      num(row$games[[1L]]) > 0,
      num(row$games_with_confirmed_lineups[[1L]]) == num(row$games[[1L]]),
      num(row$games_with_matched_starters[[1L]]) == num(row$games[[1L]]),
      as.logical(row$active_roster_bullpen_model[[1L]]),
      as.logical(row$bullpen_usage_current[[1L]]),
      as.logical(row$empirical_park_factors[[1L]]),
      as.logical(row$chronological_calibration_complete[[1L]])
    ),
    stringsAsFactors = FALSE
  )
  gate_html <- vapply(seq_len(nrow(gates)), function(index) {
    paste0(
      '<article class="projection-input-gate ', if (gates$ready[[index]]) 'is-ready' else 'is-missing', '">',
      '<span>', if (gates$ready[[index]]) 'Ready' else 'Open gate', '</span><h3>', html_escape(gates$label[[index]]),
      '</h3><p>', html_escape(gates$value[[index]]), '</p></article>'
    )
  }, character(1))
  readiness_fraction <- mean(gates$ready)
  paste0(
    '<section class="projection-readiness"><div class="projection-readiness__head"><div><span class="eyebrow">Publication gate &middot; ',
    html_escape(format(as.Date(row$game_date[[1L]]), "%B %d, %Y")), '</span><h2>The scheduled-game engine is running behind a calibration wall</h2>',
    '<p>The page can now simulate today&rsquo;s actual games. Development probabilities stay clearly labeled until current PBP workload, empirical park effects, and a chronological backtest pass.</p></div>',
    '<div class="projection-readiness__score"><strong>', html_escape(fmt_rate(readiness_fraction)),
    '</strong><span>activation gates closed</span></div></div><div class="projection-input-grid">',
    paste0(gate_html, collapse = ''), '</div></section>'
  )
}

projection_feedback_room <- function(metrics, model_card, ledger) {
  approved <- isTRUE(as.logical(metrics$deployment_approved[[1L]]))
  status <- if (approved) "Shadow-mode eligible" else "Withheld by holdout gate"
  fit_summary <- if (approved) {
    "The fitted score layer learned from prior-only rolling team production, then faced a completely later block of games. It improved the raw score scale and narrowly beat the simple home-team Brier baseline, earning shadow-mode evaluation without overwriting the public board."
  } else {
    "The fitted score layer learned from prior-only rolling team production, then faced a completely later block of games. It improved the raw score scale, but it did not beat the simple home-team Brier baseline, so the live board was not overwritten."
  }
  remaining <- pmax(0, 300 - num(ledger$eligible_unique_games[[1L]]))
  paste0(
    '<section class="projection-feedback-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Model fitting &middot; no-lookahead evaluation</span>',
    '<h2>Real outcomes change the model only after they earn the right</h2><p>', html_escape(fit_summary), '</p></div>',
    '<div class="projection-diagnostic-layout"><figure><img src="images/graphics-feed/projection-calibration-curve.png" alt="Calibration curve comparing historical predicted home-win probability with observed home-win rate"><figcaption>Points show non-empty probability bins; labels are holdout game counts. The dashed line is perfect calibration.</figcaption></figure>',
    '<div class="projection-diagnostic-stats">',
    stat_card("Holdout", "Later games only", fmt_int(metrics$observations[[1L]]), paste("Training ended", format(as.Date(metrics$training_end[[1L]]), "%B %d")), "navy"),
    stat_card("Win probability", "Brier score", fmt_dec(metrics$brier_score[[1L]], 3L), paste("Naive home baseline", fmt_dec(metrics$naive_home_brier[[1L]], 3L), "-", status), "red"),
    stat_card("Run expectation", "Team-runs MAE", fmt_dec(metrics$mean_absolute_team_runs[[1L]], 2L), paste("Raw scale", fmt_dec(metrics$raw_mean_absolute_team_runs[[1L]], 2L), "before fitting"), "steel"),
    stat_card("Live feedback", "Eligible forecasts", fmt_int(ledger$eligible_unique_games[[1L]]), paste(fmt_int(remaining), "more game forecasts before live probability calibration"), "navy"),
    '</div></div>',
    '<div class="projection-feedback-steps"><article><span>01</span><h3>Freeze before first pitch</h3><p>Every game and player-event probability is archived with its inputs and model version. Late builds are permanently excluded.</p></article>',
    '<article><span>02</span><h3>Settle against final PBP</h3><p>Team scores, winners, hitter events, and starter strikeouts are joined by exact game and MLBAM player IDs.</p></article>',
    '<article><span>03</span><h3>Diagnose the misses</h3><p>Brier score, log loss, run MAE, calibration bins, and player-event residuals reveal whether the model is too aggressive or too conservative.</p></article>',
    '<article><span>04</span><h3>Refit in chronological blocks</h3><p>Only coefficients that improve later unseen games enter shadow mode; public probabilities require a second approval gate.</p></article></div>',
    '<div class="method-callout"><strong>Why today does not count:</strong> the first ledger snapshot was taken after first pitch, so all 15 games were correctly marked late and excluded. The automated daily job will archive future boards before games begin.</div></section>'
  )
}

pitch_usage_change_card <- function(row) {
  baseline_width <- pmax(2, pmin(100, 100 * num(row$baseline_usage[[1L]])))
  recent_width <- pmax(2, pmin(100, 100 * num(row$recent_usage[[1L]])))
  delta <- num(row$usage_delta_pp[[1L]])
  paste0(
    '<article class="pitch-change-card"><header><span class="eyebrow">', html_escape(row$direction[[1L]]),
    '</span><strong>', html_escape(paste0(ifelse(delta > 0, "+", ""), fmt_dec(delta, 1L), " pts")), '</strong></header>',
    '<h3>', html_escape(row$pitcher_name[[1L]]), '</h3><p class="pitch-change-card__team">',
    html_escape(row$team[[1L]]), ' &middot; ', html_escape(row$pitch_name[[1L]]), '</p>',
    '<div class="usage-compare" role="img" aria-label="Baseline usage ', html_escape(fmt_rate(row$baseline_usage[[1L]])),
    '; recent usage ', html_escape(fmt_rate(row$recent_usage[[1L]])), '">',
    '<div><span>Earlier season</span><i><b style="width:', format(round(baseline_width, 1), nsmall = 1), '%"></b></i><strong>',
    html_escape(fmt_rate(row$baseline_usage[[1L]])), '</strong></div>',
    '<div class="is-recent"><span>Last five games</span><i><b style="width:', format(round(recent_width, 1), nsmall = 1), '%"></b></i><strong>',
    html_escape(fmt_rate(row$recent_usage[[1L]])), '</strong></div></div>',
    '<footer><span>', html_escape(fmt_z(row$usage_change_z[[1L]])), ' vs same-pitch peers</span><span>',
    html_escape(fmt_int(row$recent_pitches[[1L]])), ' recent pitches</span></footer></article>'
  )
}

aaa_watch_card <- function(row, perspective = c("hitter", "pitcher")) {
  perspective <- match.arg(perspective)
  if (perspective == "hitter") {
    headline <- paste("OPS", fmt_dec(row$ops[[1L]]), "|", fmt_int(row$home_runs[[1L]]), "HR |", fmt_int(row$stolen_bases[[1L]]), "SB")
    detail <- paste(fmt_int(row$pa[[1L]]), "PA |", fmt_rate(row$walk_rate[[1L]]), "BB |", fmt_rate(row$strikeout_rate[[1L]]), "K")
  } else {
    headline <- paste(fmt_dec(row$era[[1L]], 2L), "ERA |", fmt_dec(row$whip[[1L]], 2L), "WHIP")
    detail <- paste(fmt_dec(row$innings[[1L]], 1L), "IP |", fmt_rate(row$k_minus_bb_rate[[1L]]), "K-BB")
  }
  player_card(
    paste0("Age ", fmt_int(row$age[[1L]]), " ", perspective, " watch"),
    row$player_name[[1L]], row$team[[1L]], headline, detail,
    fmt_score(row$performance_score[[1L]])
  )
}

live_input_board <- function(games) {
  ready_games <- sum(as.logical(games$projection_ready), na.rm = TRUE)
  lineup_games <- sum(games$away_lineup_status == "confirmed" & games$home_lineup_status == "confirmed", na.rm = TRUE)
  weather_games <- sum(games$weather_status %in% c("available", "indoors", "not_required"), na.rm = TRUE)
  weather_detail <- paste(weather_games, "of", nrow(games), "park environments resolved")
  paste0(
    '<section class="live-input-section"><div class="section-heading section-heading--tight"><span class="eyebrow">Live input assembly &middot; ',
    html_escape(format(as.Date(games$game_date[[1L]]), "%B %d, %Y")), '</span><h2>Today&rsquo;s projection inputs</h2>',
    '<p>BaseballR supplies the schedule, probable starters, posted orders, and active rosters. Park coordinates route each game to Open-Meteo: the displayed temperature, wind, rain chance, and conditions come from the forecast hour nearest first pitch, while a separate five-hour game window tracks changing rain and gust risk.</p></div>',
    '<div class="method-grid">',
    stat_card("Schedule", "MLB games", fmt_int(nrow(games)), "Game IDs, teams, first pitch, venue, and status.", "navy"),
    stat_card("Posted orders", "Complete games", paste0(lineup_games, "/", nrow(games)), paste(fmt_int(nrow(daily_batting_orders)), "confirmed batting-order rows."), "red"),
    stat_card("Active roster gate", "Verified players", fmt_int(nrow(active_rosters)), paste(fmt_int(nrow(active_roster_bullpens)), "workload-qualified bullpen rows remain after the roster join."), "steel"),
    stat_card("Park weather", "Resolved games", paste0(weather_games, "/", nrow(games)), weather_detail, "navy"),
    '</div><section class="dashboard-block">',
    render_table(games, c("away_team", "home_team", "away_starter_name", "home_starter_name", "away_lineup_status", "home_lineup_status", "weather_location", "temperature_f", "wind_mph", "precipitation_probability", "readiness_label"),
      c("Away", "Home", "Away starter", "Home starter", "Away order", "Home order", "Park weather", "Temp", "Wind", "Rain", "Input state"),
      list(temperature_f = function(x) ifelse(is.finite(num(x)), paste0(fmt_int(x), " F"), "Indoor"), wind_mph = function(x) ifelse(is.finite(num(x)), paste0(fmt_dec(x, 1L), " mph"), "-"), precipitation_probability = function(x) ifelse(is.finite(num(x)), paste0(fmt_int(x), "%"), "-"))),
    '</section><div class="method-callout"><strong>Current boundary:</strong> a resolved input row is not the same as a validated forecast. Starter, lineup, active-roster bullpen, and temperature effects now enter the score model; park factors, fresh reliever workload, and out-of-time calibration remain open gates.</div></section>'
  )
}

game_time_label <- function(value) {
  parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  if (is.na(parsed)) return("Time pending")
  paste0(trimws(format(parsed, "%l:%M %p", tz = "America/New_York")), " ET")
}

best_lineup_change <- function(game_id, team_name) {
  lineup <- daily_batting_orders[
    as.character(daily_batting_orders$game_id) == as.character(game_id) &
      daily_batting_orders$team_name == team_name,
    , drop = FALSE
  ]
  if (!nrow(lineup)) return(NULL)
  candidates <- hitter_changes[
    as.character(hitter_changes$player_id) %in% as.character(lineup$player_id),
    , drop = FALSE
  ]
  if (!nrow(candidates)) return(NULL)
  candidates <- candidates[order(-num(candidates$change_signal_score), -num(candidates$dominant_change_abs_z)), , drop = FALSE]
  candidates[1L, , drop = FALSE]
}

starter_change <- function(starter_id) {
  if (is.na(starter_id) || !nzchar(trimws(as.character(starter_id)))) return(NULL)
  candidates <- pitcher_changes[as.character(pitcher_changes$player_id) == as.character(starter_id), , drop = FALSE]
  candidates <- candidates[!is.na(candidates$player_id), , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  candidates[order(-num(candidates$change_signal_score)), , drop = FALSE][1L, , drop = FALSE]
}

team_game_context <- function(team_name) {
  row <- team_intelligence[team_intelligence$team == team_name, , drop = FALSE]
  if (!nrow(row)) return(NULL)
  row[1L, , drop = FALSE]
}

top_active_bullpen_option <- function(team_name) {
  candidates <- bullpen_matchups[bullpen_matchups$team == team_name & as.logical(bullpen_matchups$active_roster_verified), , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  candidates <- candidates[order(-num(candidates$selection_score), num(candidates$selection_rank)), , drop = FALSE]
  candidates <- candidates[!duplicated(candidates$pitcher_id), , drop = FALSE]
  candidates[1L, , drop = FALSE]
}

compact_change_signal <- function(row, fallback) {
  if (is.null(row) || !nrow(row)) return(paste0('<p class="slate-signal is-muted">', html_escape(fallback), '</p>'))
  paste0(
    '<p class="slate-signal"><strong>', html_escape(row$player_name[[1L]]), '</strong><span>',
    html_escape(row$dominant_change_label[[1L]]), ' ', html_escape(fmt_z(row$dominant_change_z[[1L]])),
    ' &middot; ', html_escape(row$dominant_change_direction[[1L]]), ' &middot; ',
    html_escape(fmt_ordinal(row$dominant_season_percentile[[1L]])), ' season percentile</span></p>'
  )
}

slate_team_block <- function(team_name, starter_name, starter_id, game_id) {
  team_row <- team_game_context(team_name)
  bullpen_row <- top_active_bullpen_option(team_name)
  lineup_signal <- best_lineup_change(game_id, team_name)
  starter_signal <- starter_change(starter_id)
  team_index <- if (is.null(team_row)) "Team index pending" else paste0("#", fmt_int(team_row$team_index_rank[[1L]]), " team index")
  bullpen_copy <- if (is.null(bullpen_row)) {
    "No roster-qualified reliever signal"
  } else {
    paste0("Roster-qualified selector: ", bullpen_row$pitcher_name[[1L]], " (", bullpen_row$throws[[1L]], ")")
  }
  paste0(
    '<section class="slate-team"><header><div><span>', html_escape(team_name), '</span><strong>',
    html_escape(starter_name), '</strong></div><small>', html_escape(team_index), '</small></header>',
    compact_change_signal(starter_signal, "Starter change sample not yet qualified."),
    compact_change_signal(lineup_signal, "No qualified change signal in the posted order."),
    '<p class="slate-bullpen">', html_escape(bullpen_copy), '</p></section>'
  )
}

daily_slate_card <- function(row) {
  ready <- isTRUE(as.logical(row$projection_ready[[1L]]))
  status_class <- if (ready) "is-ready" else "is-conditional"
  status_label <- if (ready) "Research inputs ready" else "Conditional inputs"
  weather <- if (row$weather_status[[1L]] == "indoors") {
    "Indoor environment"
  } else {
    paste0(
      row$conditions[[1L]], " &middot; ", fmt_int(row$temperature_f[[1L]]), " F &middot; ",
      fmt_dec(row$wind_mph[[1L]], 1L), " mph wind"
    )
  }
  paste0(
    '<article class="slate-brief ', status_class, '"><header><div><span class="eyebrow">',
    html_escape(game_time_label(row$game_time_utc[[1L]])), ' &middot; ', html_escape(row$venue_name[[1L]]),
    '</span><h3>', html_escape(row$away_team[[1L]]), ' at ', html_escape(row$home_team[[1L]]),
    '</h3></div><span class="slate-status">', html_escape(status_label), '</span></header>',
    '<div class="slate-brief__weather"><span>', weather, '</span><span>',
    html_escape(paste0(row$away_lineup_status[[1L]], " / ", row$home_lineup_status[[1L]], " orders")), '</span></div>',
    '<div class="slate-team-grid">',
    slate_team_block(row$away_team[[1L]], row$away_starter_name[[1L]], row$away_starter_id[[1L]], row$game_id[[1L]]),
    slate_team_block(row$home_team[[1L]], row$home_starter_name[[1L]], row$home_starter_id[[1L]], row$game_id[[1L]]),
    '</div><footer><span>Slate snapshot ', html_escape(format(as.Date(row$game_date[[1L]]), "%B %d")),
    '</span><span>Performance context through ', html_escape(updated_label), '</span></footer></article>'
  )
}

series_player_line <- function(row) {
  if (as.character(row$role[[1L]]) == "Hitter") {
    paste0(
      fmt_int(row$hits[[1L]]), " H &middot; ",
      fmt_int(row$home_runs[[1L]]), " HR &middot; ",
      fmt_int(row$runs_batted_in[[1L]]), " RBI"
    )
  } else {
    outs <- num(row$innings_outs[[1L]])
    paste0(
      floor(outs / 3), ".", outs %% 3, " IP &middot; ",
      fmt_int(row$strikeouts[[1L]]), " K"
    )
  }
}

daily_series_card <- function(game_id) {
  context <- daily_series_context[
    as.character(daily_series_context$current_game_id) == as.character(game_id),
    ,
    drop = FALSE
  ]
  if (!nrow(context)) return("")
  team_panels <- vapply(seq_len(nrow(context)), function(index) {
    row <- context[index, , drop = FALSE]
    recent <- daily_recent_games[
      as.character(daily_recent_games$current_game_id) == as.character(game_id) &
        daily_recent_games$team == row$team,
      ,
      drop = FALSE
    ]
    recent <- utils::head(recent[order(-num(recent$performance_score)), , drop = FALSE], 3L)
    series <- daily_series_players[
      as.character(daily_series_players$current_game_id) == as.character(game_id) &
        daily_series_players$team == row$team,
      ,
      drop = FALSE
    ]
    series_score <- if (nrow(series)) ifelse(
      series$role == "Hitter",
      2 * num(series$hits) + 3 * num(series$home_runs) + num(series$runs_batted_in),
      num(series$innings_outs) / 3 + num(series$strikeouts)
    ) else numeric()
    if (nrow(series)) series <- utils::head(series[order(-series_score), , drop = FALSE], 4L)
    recent_list <- if (nrow(recent)) paste0(vapply(seq_len(nrow(recent)), function(i) {
      paste0(
        '<li><span>', html_escape(recent$role[[i]]), '</span><strong>',
        html_escape(recent$player_name[[i]]), '</strong><b>',
        html_escape(recent$stat_line[[i]]), '</b></li>'
      )
    }, character(1)), collapse = "") else '<li class="is-empty">No prior-game line available.</li>'
    series_list <- if (nrow(series)) paste0(vapply(seq_len(nrow(series)), function(i) {
      paste0(
        '<li><span>', html_escape(series$role[[i]]), '</span><strong>',
        html_escape(series$player_name[[i]]), '</strong><b>',
        series_player_line(series[i, , drop = FALSE]), '</b></li>'
      )
    }, character(1)), collapse = "") else '<li class="is-empty">Series opener; totals begin after this game.</li>'
    paste0(
      '<section class="daily-series-team"><header><span>Game ',
      html_escape(fmt_int(row$series_game_number[[1L]])), ' of series</span><h3>',
      html_escape(row$team[[1L]]), '</h3><small>vs. ',
      html_escape(row$opponent[[1L]]), ' &middot; since ',
      html_escape(row$series_start[[1L]]), '</small></header>',
      '<h4>', if (isTRUE(as.logical(row$played_yesterday[[1L]]))) "Yesterday" else "Previous game",
      '</h4><ul>', recent_list, '</ul><h4>Current series</h4><ul>',
      series_list, '</ul></section>'
    )
  }, character(1))
  paste0('<article class="daily-series-card">', paste0(team_panels, collapse = ""), '</article>')
}

award_lane_card <- function(rows) {
  rows <- rows[order(num(rows$rank)), , drop = FALSE]
  leaders <- utils::head(rows, 3L)
  title <- paste(rows$league[[1L]], rows$award[[1L]])
  status <- if (rows$award[[1L]] == "ROTY watch") "Eligibility screen" else "Season performance"
  leader_rows <- vapply(seq_len(nrow(leaders)), function(index) {
    row <- leaders[index, , drop = FALSE]
    paste0(
      '<li><div><span>', html_escape(paste0("#", row$rank[[1L]])), '</span><strong>',
      html_escape(row$player_name[[1L]]), '</strong><small>', html_escape(row$team[[1L]]), ' &middot; ',
      html_escape(row$role[[1L]]), '</small></div><div class="award-score-track" role="img" aria-label="Award performance score ',
      html_escape(fmt_score(row$award_score[[1L]])), '"><i style="width:', html_escape(fmt_score(row$award_score[[1L]])),
      '%"></i></div><b>', html_escape(fmt_score(row$award_score[[1L]])), '</b></li>'
    )
  }, character(1))
  paste0(
    '<article class="award-lane"><header><span class="eyebrow">', html_escape(status), '</span><h3>',
    html_escape(title), '</h3></header><ol>', paste0(leader_rows, collapse = ""), '</ol><footer><strong>',
    html_escape(leaders$player_name[[1L]]), '</strong><span>', html_escape(leaders$evidence[[1L]]), '</span></footer></article>'
  )
}

graphics_feed_card <- function(row, compact = FALSE) {
  class_name <- if (isTRUE(compact)) "graphics-card graphics-card--compact" else "graphics-card"
  paste0(
    '<figure class="', class_name, '"><a class="graphics-card__image" href="', html_escape(row$image_path[[1L]]),
    '"><img src="', html_escape(row$image_path[[1L]]), '" alt="', html_escape(row$alt_text[[1L]]),
    '" loading="lazy"></a><figcaption><span class="eyebrow">', html_escape(row$page_group[[1L]]), '</span><h3>',
    html_escape(row$title[[1L]]), '</h3><p>', html_escape(row$subtitle[[1L]]), '</p><div><small>',
    html_escape(row$coverage_note[[1L]]), '</small><a href="', html_escape(row$image_path[[1L]]), '" download="',
    html_escape(row$file_name[[1L]]), '">Download PNG</a></div></figcaption></figure>'
  )
}

leaderboard_card <- function(data, title, value_col, formatter = fmt_int, subtitle = NULL, lower_is_better = FALSE, limit = 5L, eyebrow = "League leaders") {
  values <- num(data[[value_col]])
  keep <- is.finite(values)
  data <- data[keep, , drop = FALSE]
  values <- values[keep]
  order_index <- if (isTRUE(lower_is_better)) order(values) else order(-values)
  data <- data[order_index, , drop = FALSE]
  data <- utils::head(data, limit)
  if (!nrow(data)) return("")
  rows <- vapply(seq_len(nrow(data)), function(index) {
    paste0(
      '<li><span class="leaderboard-rank">', index, '</span><span class="leaderboard-player"><strong>',
      html_escape(data$player_name[[index]]), '</strong><small>', html_escape(data$team[[index]]),
      '</small></span><span class="leaderboard-value">', html_escape(formatter(data[[value_col]][[index]])), '</span></li>'
    )
  }, character(1))
  paste0(
    '<article class="leaderboard-card"><header><span class="eyebrow">', html_escape(eyebrow), '</span><h3>', html_escape(title),
    '</h3>', if (!is.null(subtitle)) paste0('<p>', html_escape(subtitle), '</p>') else '',
    '</header><ol>', paste0(rows, collapse = ""), '</ol></article>'
  )
}

ranked_board_card <- function(data, title, subtitle, value_col, formatter = fmt_score, limit = 5L, footer = NULL, extra_class = "") {
  data <- data[order(num(data$rank)), , drop = FALSE]
  data <- utils::head(data, limit)
  if (!nrow(data)) return("")
  rows <- vapply(seq_len(nrow(data)), function(index) {
    value <- data[[value_col]][[index]]
    paste0(
      '<li><span class="leaderboard-rank">', html_escape(fmt_int(data$rank[[index]])),
      '</span><span class="leaderboard-player"><strong>', html_escape(data$player_name[[index]]),
      '</strong><small>', html_escape(data$team[[index]]), if ("role" %in% names(data)) paste0(' &middot; ', html_escape(data$role[[index]])) else '',
      '</small></span><span class="leaderboard-value">', html_escape(formatter(value)), '</span></li>'
    )
  }, character(1))
  paste0(
    '<article class="leaderboard-card ', html_escape(extra_class), '"><header><span class="eyebrow">Ranked race</span><h3>',
    html_escape(title), '</h3><p>', html_escape(subtitle), '</p></header><ol>', paste0(rows, collapse = ""), '</ol>',
    if (!is.null(footer)) paste0('<footer class="leaderboard-card__footer">', html_escape(footer), '</footer>') else '', '</article>'
  )
}

player_probability_leaderboard <- function(metric_id, title, description) {
  rows <- player_simulations[player_simulations$metric_id == metric_id, , drop = FALSE]
  rows <- rows[order(num(rows$metric_rank)), , drop = FALSE]
  rows <- utils::head(rows, 10L)
  paste0(
    '<section class="player-prob-board"><header><span class="eyebrow">Starter + bullpen Monte Carlo</span><h3>',
    html_escape(title), '</h3><p>', html_escape(description), '</p></header>',
    render_table(rows, c("metric_rank", "player_name", "team", "opponent", "probability", "expected_value"),
      c("Rank", "Player", "Team", "Opponent", "Probability", "Mean"),
      list(metric_rank = fmt_int, probability = fmt_rate, expected_value = function(x) fmt_dec(x, 2L)), "data-table data-table--compact"),
    '</section>'
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
    '<div class="method-callout"><strong>Selector score, not probability:</strong> the displayed score transparently combines active-roster eligibility, availability, role fit, performance, handedness, and leverage. A confirmed batting order and same-day transaction check remain required before operational use.</div></section>'
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
  '<section class="section-heading"><span class="eyebrow">League pulse</span><h2>Four stories worth your attention</h2><p>The strongest movement, milestone, and history signals in the current league snapshot.</p></section>',
  paste0('<div class="signal-grid">', paste0(home_signals, collapse = ""), '</div>')
))

home_change_cards <- vapply(seq_len(min(4L, nrow(change_spotlights))), function(index) {
  player_context_card(change_spotlights[index, , drop = FALSE], compact = TRUE)
}, character(1))
write_fragment("home-player-change.html", c(
  '<section class="section-heading"><span class="eyebrow">The Trend Engine</span><h2>The winds have changed, where are these players heading now?</h2><p>Our engine searches for the players with the most movement against there season averages.</p></section>',
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
  '<section class="section-heading"><span class="eyebrow">From our writers</span><h2>Stories from the Sabrhood</h2><p>Original stories crafted from our own research engine.</p></section>',
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
  '<section class="editorial-shortlist"><div class="section-heading section-heading--tight"><span class="eyebrow">Next assignments</span><h2>Eight more leads for the newsroom</h2><p>A cleaner editorial queue: the idea, the supporting evidence, and the score that moved it forward.</p></div><ol>',
  vapply(seq_len(min(8L, nrow(story_shortlist))), function(index) {
    row <- story_shortlist[index, , drop = FALSE]
    paste0('<li><span class="editorial-shortlist__rank">', html_escape(fmt_int(row$queue_rank[[1L]])),
      '</span><div><small>', html_escape(row$lane_label[[1L]]), ' &middot; ', html_escape(row$team[[1L]]),
      '</small><h3>', html_escape(row$headline[[1L]]), '</h3><p>', html_escape(row$evidence[[1L]]),
      '</p></div><strong>', html_escape(fmt_score(row$story_score[[1L]])), '</strong></li>')
  }, character(1)),
  '</ol></section>',
  '<div class="method-callout"><strong>Why this matters:</strong> the Story Engine is the bridge between statistical detection and journalism. It creates an assignment queue, not automated finished prose.</div>'
))
write_fragment("home-story-desk.html", c(
  '<section class="section-heading"><span class="eyebrow">Headliners</span><h2>Find headlines from different angles every day.</h2><p>The engine hand picks new stories to keep things fresh.</p></section>',
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
  '<section class="section-heading"><span class="eyebrow">Pitcher matchup edges</span><h2>Which side does each arm handle best?</h2><p>For pitchers, the highlighted side is the opponent hand with the lower estimated wOBA allowed.</p></section>',
  '<div class="signal-grid">', pitcher_matchup_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Full hitter board</span><h2>Largest qualified platoon gaps</h2></div>',
  render_table(utils::head(hitter_matchups, 20L), c("matchup_edge_rank", "player_name", "team", "pa_vs_l", "woba_vs_l", "pa_vs_r", "woba_vs_r", "woba_gap", "stronger_opponent_hand", "matchup_edge_score"),
    c("Rank", "Hitter", "Team", "PA vs L", "wOBA vs L", "PA vs R", "wOBA vs R", "Gap", "Edge vs", "Score"),
    list(matchup_edge_rank = fmt_int, pa_vs_l = fmt_int, woba_vs_l = fmt_dec, pa_vs_r = fmt_int, woba_vs_r = fmt_dec, woba_gap = fmt_dec, matchup_edge_score = fmt_score)),
  '</section>',
  '<div class="method-callout"><strong>Use the split, not the stereotype:</strong> handedness is a reporting starting point. Pitch type, release point, command, park, and sample stability still shape the actual matchup.</div>'
))

hall_pool <- historical_profiles[as.logical(historical_profiles$hof_inducted), , drop = FALSE]
hall_final_year <- suppressWarnings(as.integer(format(as.Date(hall_pool$finalGame), "%Y")))
hall_years_since <- pmax(as.integer(format(Sys.Date(), "%Y")) - hall_final_year, 0)
hall_recency <- pmax(0.18, exp(-hall_years_since / 45))
hall_recency[!is.finite(hall_recency)] <- 0.35
hall_significance <- pmax(pmin(num(hall_pool$career_significance_score) / 100, 1), 0)
hall_weight <- 0.60 * hall_recency + 0.40 * hall_significance
hall_weight[!is.finite(hall_weight) | hall_weight <= 0] <- 0.35
history_seed_date <- if (nrow(historical) && "report_date" %in% names(historical)) {
  as.character(historical$report_date[[1L]])
} else {
  as.character(Sys.Date())
}
set.seed(sum(utf8ToInt(history_seed_date)))
hall_spotlight <- hall_pool[sample(seq_len(nrow(hall_pool)), 1L, prob = hall_weight), , drop = FALSE]

hall_of_fame_card <- function(row) {
  is_pitcher <- num(row$career_IPouts[[1L]]) > 0 &&
    num(row$career_IPouts[[1L]]) / 3 > num(row$career_AB[[1L]])
  stat_rows <- if (is_pitcher) {
    list(
      c(fmt_int(row$career_W[[1L]]), "Career wins"),
      c(fmt_int(row$career_SO_pitch[[1L]]), "Strikeouts"),
      c(fmt_dec(row$career_ERA[[1L]], 2L), "Career ERA")
    )
  } else {
    list(
      c(fmt_int(row$career_H[[1L]]), "Career hits"),
      c(fmt_int(row$career_HR[[1L]]), "Home runs"),
      c(fmt_int(row$career_RBI[[1L]]), "RBI")
    )
  }
  initials <- paste0(substr(strsplit(row$player_name_raw[[1L]], " +")[[1L]], 1L, 1L), collapse = "")
  teams <- if ("teams_played" %in% names(row) && !is.na(row$teams_played[[1L]]) && nzchar(row$teams_played[[1L]])) {
    row$teams_played[[1L]]
  } else {
    "Team history unavailable"
  }
  years <- paste0(
    format(as.Date(row$debut[[1L]]), "%Y"), " - ",
    format(as.Date(row$finalGame[[1L]]), "%Y")
  )
  stats_html <- paste0(vapply(stat_rows, function(item) {
    paste0("<span><strong>", html_escape(item[[1L]]), "</strong><small>", html_escape(item[[2L]]), "</small></span>")
  }, character(1)), collapse = "")
  paste0(
    '<article class="hof-spotlight"><div class="hof-spotlight__identity">',
    '<div class="hof-spotlight__monogram" aria-hidden="true">', html_escape(initials), '</div>',
    '<span>Hall of Fame spotlight</span></div><div class="hof-spotlight__copy">',
    '<span class="eyebrow">Today&rsquo;s weighted draw</span><h2>', html_escape(row$player_name[[1L]]), '</h2>',
    '<p>', html_escape(years), ' &middot; Bats ', html_escape(row$bats[[1L]]),
    ' &middot; Throws ', html_escape(row$throws[[1L]]), '</p>',
    '<p><strong>Teams:</strong> ', html_escape(teams), '</p>',
    '<p>', html_escape(row$career_summary[[1L]]), '</p>',
    '<div class="hof-spotlight__stats">', stats_html, '</div></div></article>'
  )
}

milestone_spotlights <- historical_milestones[!duplicated(historical_milestones$subject_id), ]
milestone_spotlights <- milestone_spotlights[seq_len(min(8L, nrow(milestone_spotlights))), ]
anniversary_spotlights <- historical[seq_len(min(8L, nrow(historical))), ]
upcoming_milestones <- active_milestones[active_milestones$milestone_status == "approaching", , drop = FALSE]
upcoming_milestones <- upcoming_milestones[
  order(num(upcoming_milestones$distance_to_milestone), -num(upcoming_milestones$story_score)),
  ,
  drop = FALSE
]
upcoming_milestone_spotlights <- utils::head(upcoming_milestones, 8L)
reached_date <- suppressWarnings(as.Date(active_milestones$reached_date))
history_reference_dates <- c(reached_date, suppressWarnings(as.Date(historical$report_date)))
history_reference_dates <- history_reference_dates[!is.na(history_reference_dates)]
history_reference_date <- if (length(history_reference_dates)) max(history_reference_dates) else Sys.Date()
recent_milestones <- active_milestones[
  active_milestones$milestone_status == "reached_this_season" &
    !is.na(reached_date) & reached_date >= history_reference_date - 30,
  ,
  drop = FALSE
]
recent_milestones$reached_date <- reached_date[
  active_milestones$milestone_status == "reached_this_season" &
    !is.na(reached_date) & reached_date >= history_reference_date - 30
]
recent_milestones <- recent_milestones[order(as.Date(recent_milestones$reached_date), decreasing = TRUE), , drop = FALSE]

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

upcoming_milestone_cards <- vapply(seq_len(nrow(upcoming_milestone_spotlights)), function(index) {
  player_card(
    paste(upcoming_milestone_spotlights$role[[index]], "landmark ahead"),
    upcoming_milestone_spotlights$player_name[[index]],
    upcoming_milestone_spotlights$team[[index]],
    upcoming_milestone_spotlights$headline[[index]],
    paste("Current career estimate", fmt_int(upcoming_milestone_spotlights$career_to_date_value[[index]]),
      "| target", fmt_int(upcoming_milestone_spotlights$milestone_target[[index]])),
    fmt_score(upcoming_milestone_spotlights$story_score[[index]])
  )
}, character(1))
recent_milestone_cards <- vapply(seq_len(min(6L, nrow(recent_milestones))), function(index) {
  player_card(
    paste(recent_milestones$role[[index]], "recent landmark"),
    recent_milestones$player_name[[index]],
    recent_milestones$team[[index]],
    recent_milestones$headline[[index]],
    paste("Reached", format(as.Date(recent_milestones$reached_date[[index]]), "%B %d, %Y"),
      "| current estimate", fmt_int(recent_milestones$career_to_date_value[[index]])),
    fmt_score(recent_milestones$story_score[[index]])
  )
}, character(1))
retrosheet_game_cards <- vapply(seq_len(nrow(daily_retrosheet_history)), function(index) {
  player_card(
    paste(daily_retrosheet_history$role[[index]], "rarity"),
    daily_retrosheet_history$player_name[[index]],
    paste(daily_retrosheet_history$team[[index]], "vs.", daily_retrosheet_history$opponent[[index]]),
    daily_retrosheet_history$stat_line[[index]],
    paste(
      format(as.Date(daily_retrosheet_history$game_date[[index]]), "%B %d, %Y"), "|",
      daily_retrosheet_history$rarity_label[[index]], "|",
      fmt_int(daily_retrosheet_history$occurrence_count[[index]]), "occurrences in the indexed era"
    ),
    fmt_score(daily_retrosheet_history$story_score[[index]])
  )
}, character(1))
retrosheet_universe <- if (
  nrow(daily_retrosheet_history) &&
    "history_universe" %in% names(daily_retrosheet_history)
) {
  as.character(daily_retrosheet_history$history_universe[[1L]])
} else {
  "the indexed Retrosheet regular-season universe"
}

write_fragment("history-desk.html", c(
  '<div class="history-scoreboard">',
  stat_card("Career profiles", "Recognizable players", fmt_int(nrow(historical_profiles)), "Historical careers above the public significance threshold.", "navy"),
  stat_card("Recognition", "Icons", fmt_int(sum(historical_profiles$recognition_tier == "icon")), "Hall of Famers, record holders, and historically dominant careers.", "red"),
  stat_card("Story inventory", "Milestone notes", fmt_int(nrow(historical_milestones)), "Career clubs and top-ten leaderboard context.", "steel"),
  stat_card("Today", "Anniversary candidates", fmt_int(nrow(historical)), "Debuts and final appearances ranked for editorial review.", "navy"),
  '</div>',
  '<section class="section-heading"><span class="eyebrow">On this date</span><h2>Happy anniversary to these players today!</h2><p></p></section>',
  '<div class="signal-grid signal-grid--four">', anniversary_cards, '</div>',
  paste0(
    '<section class="section-heading"><span class="eyebrow">The date in box-score history</span><h2>Happy anniversary to these record-setting games!</h2><p>The engine searches through data ',
    'from ', html_escape(retrosheet_universe),
    '. The data is provided from the Retrosheet online database at retrosheet.org</p></section>'
  ),
  '<div class="signal-grid signal-grid--three history-game-grid">', retrosheet_game_cards, '</div>',
  '<div class="method-callout"><strong>Retrosheet attribution:</strong> The information used was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at www.retrosheet.org.</div>',
  '<section class="section-heading"><span class="eyebrow">Landmarks ahead</span><h2>The next milestones and records</h2><p>Because records are meant to be broken.</p></section>',
  '<div class="signal-grid signal-grid--four">', upcoming_milestone_cards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Recently reached</span><h2>Who just made history?</h2><p>Milestones and records reached within the past 30 days.</p></section>',
  '<div class="signal-grid signal-grid--three">', recent_milestone_cards, '</div>',
  '<div class="method-callout"><strong>Identity note:</strong> this prototype uses unique normalized-name matching between MLBAM summaries and Lahman. Ambiguous duplicate names are rejected, and every published row carries the provisional match method.</div>',
  '<section class="section-heading"><span class="eyebrow">Milestone vault</span><h2>The G.O.A.T-yard</h2><p>Player profiles that are among the best in history.</p></section>',
  '<div class="signal-grid signal-grid--four">', milestone_cards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Hall of Famer spotlight</span><h2>HOFer of the Day</h2><p>Meet a new Hall of Famer everyday!.</p></section>',
  hall_of_fame_card(hall_spotlight)
))

history_occurrences <- num(history_match_notes$historical_occurrence_count)
history_frequency <- if ("historical_season_frequency" %in% names(history_match_notes)) {
  num(history_match_notes$historical_season_frequency)
} else {
  rep(NA_real_, nrow(history_match_notes))
}
history_match_notes <- history_match_notes[
  is.finite(history_occurrences) &
    history_occurrences <= 100 &
    (
      history_occurrences <= 25 |
        (is.finite(history_frequency) & history_frequency <= 0.35)
    ),
  ,
  drop = FALSE
]
history_match_notes <- history_match_notes[
  order(num(history_match_notes$note_score), decreasing = TRUE),
  ,
  drop = FALSE
]
history_match_cards <- if (nrow(history_match_notes)) {
  vapply(seq_len(nrow(history_match_notes)), function(index) {
    row <- history_match_notes[index, , drop = FALSE]
    product_label <- switch(
      as.character(row$history_product[[1L]]),
      hitter = "Hitter",
      pitcher = "Pitcher",
      team = "Team",
      "History"
    )
    population_label <- if (
      identical(as.character(row$subject_type[[1L]]), "team")
    ) {
      "historical team-seasons"
    } else {
      "historical players"
    }
    precedent <- if (
      !is.na(row$prior_subject_name[[1L]]) &&
        nzchar(as.character(row$prior_subject_name[[1L]]))
    ) {
      paste0(
        "Most recent precedent: ",
        html_escape(row$prior_subject_name[[1L]]), " (",
        html_escape(row$prior_season[[1L]]), ")"
      )
    } else {
      paste0(
        "No prior qualifier in the verified ",
        html_escape(row$history_start[[1L]]), "&ndash;",
        html_escape(row$history_end[[1L]]), " search"
      )
    }
    paste0(
      '<article class="history-match-public-card is-',
      html_escape(row$history_product[[1L]]), '">',
      '<header><span>', html_escape(product_label), " &middot; ",
      html_escape(row$window_label[[1L]]), '</span><small>',
      html_escape(row$profile_label[[1L]]), '</small></header>',
      '<h2>', html_escape(row$headline[[1L]]), '</h2>',
      '<p>', html_escape(row$sentence[[1L]]), '</p>',
      '<footer><span>', html_escape(fmt_int(
        row$historical_subject_count[[1L]]
      )), " ", html_escape(population_label), '</span><span>',
      precedent, '</span><span>Data through ',
      html_escape(row$source_through[[1L]]), '</span></footer></article>'
    )
  }, character(1))
} else {
  character()
}
history_match_body <- if (length(history_match_cards)) {
  c(
    '<div class="history-match-public-grid">',
    history_match_cards,
    '</div>'
  )
} else {
  c(
    '<article class="history-match-public-empty">',
    '<span class="eyebrow">Editorial gate active</span>',
    '<h2>No History Match note is public right now</h2>',
    '<p>The engine has current research candidates, but this page publishes only claims that a SABRhood producer has approved for the website. A changed threshold or historical precedent automatically removes a prior approval until it is checked again.</p>',
    '</article>'
  )
}
history_match_source_date <- if (nrow(history_match_notes)) {
  max(as.Date(history_match_notes$source_through), na.rm = TRUE)
} else {
  as.Date(NA)
}
write_fragment("history-match-desk.html", c(
  '<div class="history-match-summary-strip">',
  '<article><span>Approved matches</span><strong>',
  html_escape(fmt_int(nrow(history_match_notes))),
  '</strong><small>Every live claim passed editorial review</small></article>',
  '<article><span>Player matches</span><strong>',
  html_escape(fmt_int(sum(history_match_notes$subject_type == "player"))),
  '</strong><small>Hitter and pitcher profiles</small></article>',
  '<article><span>Team matches</span><strong>',
  html_escape(fmt_int(sum(history_match_notes$subject_type == "team"))),
  '</strong><small>Club-season profiles</small></article>',
  '<article><span>Evidence current through</span><strong>',
  if (is.na(history_match_source_date)) "Pending" else
    html_escape(format(history_match_source_date, "%b %d")),
  '</strong><small>Retrosheet archive through 2025</small></article>',
  '</div>',
  '<section class="section-heading"><span class="eyebrow">Reviewed claims</span><h2>Current performance, matched at a meaningful threshold</h2><p>The engine selects interpretable two-stat profiles, searches the requested historical universe, and retains the most recent precedent. Editorial review controls which claims reach this public feed.</p></section>',
  history_match_body,
  '<div class="method-callout"><strong>Retrosheet attribution:</strong> The historical information used in History Match was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at www.retrosheet.org.</div>',
  '<div class="method-callout"><strong>Publication rule:</strong> packet candidates may remain pending for producer verification, but this page accepts only website-approved claims whose stored claim fingerprint still matches the current threshold and precedent.</div>'
))

league_yesterday_hitters <- daily_recent_games[daily_recent_games$role == "Hitter", , drop = FALSE]
league_yesterday_hitters <- league_yesterday_hitters[
  order(-num(league_yesterday_hitters$performance_score)),
  ,
  drop = FALSE
]
league_yesterday_hitters <- league_yesterday_hitters[
  !duplicated(league_yesterday_hitters$player_name),
  ,
  drop = FALSE
]
league_yesterday_pitchers <- daily_recent_games[daily_recent_games$role == "Pitcher", , drop = FALSE]
league_yesterday_pitchers <- league_yesterday_pitchers[
  order(-num(league_yesterday_pitchers$performance_score)),
  ,
  drop = FALSE
]
league_yesterday_pitchers <- league_yesterday_pitchers[
  !duplicated(league_yesterday_pitchers$player_name),
  ,
  drop = FALSE
]
today_hitter_cards <- vapply(seq_len(min(4L, nrow(league_yesterday_hitters))), function(i) {
  player_card(
    "Yesterday&rsquo;s hitter", league_yesterday_hitters$player_name[[i]], league_yesterday_hitters$team[[i]],
    league_yesterday_hitters$stat_line[[i]],
    paste("vs.", league_yesterday_hitters$opponent[[i]]),
    fmt_score(league_yesterday_hitters$performance_score[[i]])
  )
}, character(1))
today_pitcher_cards <- vapply(seq_len(min(4L, nrow(league_yesterday_pitchers))), function(i) {
  player_card(
    "Yesterday&rsquo;s pitcher", league_yesterday_pitchers$player_name[[i]], league_yesterday_pitchers$team[[i]],
    league_yesterday_pitchers$stat_line[[i]],
    paste("vs.", league_yesterday_pitchers$opponent[[i]]),
    fmt_score(league_yesterday_pitchers$performance_score[[i]])
  )
}, character(1))
history_cards <- vapply(seq_len(min(4L, nrow(historical))), function(i) {
  player_card(
    "On this date", historical$subject_name[[i]], as.character(historical$historical_date[[i]]),
    historical$headline[[i]], historical$body[[i]], fmt_score(historical$story_score[[i]])
  )
}, character(1))
daily_slate_cards <- if (is_off_day) {
  paste0('<article class="method-callout"><strong>No MLB games are scheduled for ', html_escape(format(slate_report_date, "%B %d")), '.</strong> The daily intelligence, history, leaderboards, and graphics are still refreshed; the last game slate is not presented as current.</article>')
} else {
  vapply(seq_len(nrow(daily_game_inputs)), function(i) {
    daily_slate_card(daily_game_inputs[i, , drop = FALSE])
  }, character(1))
}
daily_series_cards <- if (is_off_day) {
  character()
} else {
  unique(vapply(as.character(daily_game_inputs$game_id), daily_series_card, character(1)))
}
daily_series_cards <- daily_series_cards[nzchar(daily_series_cards)]
write_fragment("today-dashboard.html", c(
  paste0('<div class="update-strip"><strong>Data through ', html_escape(updated_label), '</strong><span>Method-labeled signals, not black-box claims.</span></div>'),
  '<section class="slate-desk"><div class="section-heading section-heading--tight"><span class="eyebrow">Daily game research</span><h2>Every matchup gets an evidence-first briefing</h2><p>Probable starters, posted orders, park weather, team strength, active-roster bullpen options, and the strongest qualified change signal are assembled without pretending they are already a calibrated forecast.</p></div>',
  '<div class="slate-desk__legend"><span class="is-ready">Complete research inputs</span><span class="is-conditional">Lineup or starter still conditional</span><span>Change context uses the displayed performance cutoff</span></div>',
  '<div class="slate-brief-grid">', paste0(daily_slate_cards, collapse = ""), '</div></section>',
  '<section class="section-heading"><span class="eyebrow">Yesterday around MLB</span><h2>The performances worth carrying into today</h2><p>Top completed-game lines from around the league, presented with the same production-first approach used in the broadcast packets.</p></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(today_hitter_cards, collapse = ""), '</div>'),
  '<section class="section-heading"><span class="eyebrow">Yesterday&rsquo;s arms</span><h2>Pitchers who controlled the previous slate</h2></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(today_pitcher_cards, collapse = ""), '</div>'),
  '<section class="section-heading"><span class="eyebrow">History desk</span><h2>On this date</h2><p>Daily Lahman anniversary candidates, ranked for editorial review.</p></section>',
  paste0('<div class="signal-grid signal-grid--four">', paste0(history_cards, collapse = ""), '</div>')
))

offense_top <- offensive_race[seq_len(min(12L, nrow(offensive_race))), ]
prevention_top <- prevention_race[seq_len(min(12L, nrow(prevention_race))), ]
award_keys <- expand.grid(award = c("MVP", "Cy Young", "Reliever of the Year", "ROTY watch"), league = c("AL", "NL"), stringsAsFactors = FALSE)
award_cards <- vapply(seq_len(nrow(award_keys)), function(index) {
  key <- award_keys[index, , drop = FALSE]
  rows <- award_races[award_races$award == key$award & award_races$league == key$league, , drop = FALSE]
  label <- if (key$award == "ROTY watch") "Rookie of the Year watch" else key$award
  subtitle <- if (key$award == "ROTY watch") "Role-balanced hitter and pitcher score" else if (key$award == "Reliever of the Year") "Leverage, command, saves, and pitching performance" else "Transparent season performance score"
  footer <- if (key$award == "ROTY watch") {
    "Hitters: WAR 40, wRC+ 25, offense 15, PA 10, defense 5, baserunning 5. Pitchers: WAR 40, ERA 20, FIP 15, K-BB 15, IP 10."
  } else if (key$award == "MVP") {
    "Combined hitting and pitching WAR 30, wRC+ 18, HR 12, RBI 8, runs 7, offense 10, defense 5, PA 5, WPA 5."
  } else if (key$award == "Reliever of the Year") {
    "Relievers only: WAR 25, ERA 20, FIP 15, K-BB 15, WPA 10, saves 10, innings 5."
  } else {
    "WAR, ERA, FIP, K-BB rate, innings, and WHIP."
  }
  ranked_board_card(rows, paste(key$league, label), subtitle, "award_score", fmt_score, 5L, footer, "leaderboard-card--award")
}, character(1))
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
    paste("Pitching performance #", fmt_int(prevention_top$race_rank[[index]])),
    prevention_top$player_name[[index]], prevention_top$team[[index]],
    paste(fmt_dec(prevention_top$ops[[index]]), "OPS allowed |", fmt_dec(prevention_top$woba_estimate[[index]]), "estimated wOBA"),
    paste(fmt_int(prevention_top$pa[[index]]), "BF |", fmt_rate(prevention_top$strikeout_rate[[index]]), "strikeout rate"),
    fmt_score(prevention_top$race_score[[index]])
  )
}, character(1))
race_timeline_cards <- vapply(c("AL", "NL"), function(league_name) {
  current <- award_race_current[award_race_current$league == league_name, , drop = FALSE]
  current <- current[order(num(current$race_rank)), , drop = FALSE]
  leader <- current[1L, , drop = FALSE]
  runner <- current[2L, , drop = FALSE]
  gap <- num(leader$race_rating[[1L]]) - num(runner$race_rating[[1L]])
  risers <- award_race_events[award_race_events$league == league_name & award_race_events$event_type == "fastest_riser", , drop = FALSE]
  risers <- risers[order(as.Date(risers$checkpoint_date), decreasing = TRUE), , drop = FALSE]
  changes <- award_race_events[award_race_events$league == league_name & award_race_events$event_type == "new_leader", , drop = FALSE]
  changes <- changes[order(as.Date(changes$checkpoint_date), decreasing = TRUE), , drop = FALSE]
  change_note <- if (nrow(changes)) paste("Last lead change", format(as.Date(changes$checkpoint_date[[1L]]), "%B %d")) else "No recorded lead change after the first checkpoint"
  paste0(
    '<article class="race-timeline-card"><header><span class="eyebrow">', league_name,
    ' season path</span><strong>', html_escape(fmt_dec(leader$race_rating[[1L]], 1L)), '</strong></header><h3>',
    html_escape(leader$player_name[[1L]]), ' leads</h3><p>', html_escape(fmt_dec(gap, 1L)),
    ' points ahead of ', html_escape(runner$player_name[[1L]]), '.</p><footer><span>',
    html_escape(change_note), '</span><span>Latest fastest riser: ',
    html_escape(if (nrow(risers)) risers$player_name[[1L]] else "none"), '</span></footer></article>'
  )
}, character(1))
mvp_metric_labels <- c(
  war = "WAR", avg = "AVG", obp = "OBP", slg = "SLG", ops = "OPS",
  h = "Hits", hr = "HR", rbi = "RBI", r = "Runs", sb = "SB"
)
mvp_weight_text <- paste(
  paste0(
    unname(mvp_metric_labels[mvp_modern_weights$metric]),
    " ",
    round(100 * num(mvp_modern_weights$model_weight))
  ),
  collapse = " &middot; "
)
available_mvp_decades <- sort(unique(num(mvp_era_profiles$decade)))
mvp_comparison_pool <- setdiff(available_mvp_decades, 2020)
mvp_rotation_date <- max(as.Date(award_race_history$checkpoint_date), na.rm = TRUE)
mvp_rotation_decade <- mvp_comparison_pool[(as.integer(mvp_rotation_date) %% length(mvp_comparison_pool)) + 1L]
gold_glove_rosters <- gold_glove_watch[
  num(gold_glove_watch$position_rank) == 1 &
    num(gold_glove_watch$innings) >= 150,
  ,
  drop = FALSE
]
gold_glove_rosters <- gold_glove_rosters[
  order(
    gold_glove_rosters$league,
    match(
      gold_glove_rosters$primary_position,
      c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")
    )
  ),
  ,
  drop = FALSE
]
gold_glove_roster_tables <- vapply(c("AL", "NL"), function(league_name) {
  rows <- gold_glove_rosters[
    gold_glove_rosters$league == league_name,
    ,
    drop = FALSE
  ]
  roster_columns <- c(
    "primary_position", "player_name", "team", "innings",
    "fielding_runs", "gold_glove_score"
  )
  rows <- rows[, roster_columns, drop = FALSE]
  rows <- rows[
    order(match(
      rows$primary_position,
      c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")
    )),
    ,
    drop = FALSE
  ]
  paste0(
    '<div><h3>', if (league_name == "AL") "American League" else "National League", '</h3>',
    render_table(
      rows,
      c("primary_position", "player_name", "team", "innings", "fielding_runs", "gold_glove_score"),
      c("Pos.", "Projected selection", "Team", "Innings", "Official FRV", "Gold Glove Index"),
      list(
        innings = fmt_int,
        fielding_runs = function(value) fmt_signed(value, 1L),
        gold_glove_score = function(value) fmt_dec(value, 1L)
      ),
      "data-table fielding-table"
    ),
    '</div>'
  )
}, character(1))
write_fragment("league-races.html", c(
  '<section class="award-race-room"><div class="section-heading section-heading--tight"><span class="eyebrow">FanGraphs season award room</span><h2>MVP, Cy Young, Reliever of the Year, and the provisional rookie pool</h2><p>WAR, counting production, rate quality, volume, leverage, defense, baserunning, pitching performance, and command are translated into league-specific performance scores. Hitting and pitching WAR are combined by player ID so two-way value remains intact. The result is not presented as a ballot forecast.</p></div>',
  '<div class="award-lane-grid">', paste0(award_cards, collapse = ""), '</div>',
  paste0('<div class="award-method-strip"><span><strong>MVP &middot; 2020s ballot profile</strong> ', mvp_weight_text, '</span><span><strong>Cy Young</strong> WAR 25 &middot; ERA 20 &middot; FIP 15 &middot; K-BB 15 &middot; IP 15 &middot; WHIP 10</span><span><strong>Reliever</strong> WAR 25 &middot; ERA 20 &middot; FIP/K-BB 30 &middot; WPA 10 &middot; saves 10 &middot; IP 5</span><span><strong>ROTY</strong> Hitters and pitchers receive separate role-native weights before joining one race; official service days still require verification</span></div>'),
  '<section class="gold-glove-roster-room"><div class="section-heading section-heading--tight"><span class="eyebrow">League-specific defensive awards</span><h2>Gold Glove Index</h2><p>The American League and National League are ranked independently at every official FRV position. The utility placeholder and summary graphic have been removed for a cleaner comparison.</p></div>',
  '<div class="fielding-board-grid">', gold_glove_roster_tables, '</div>',
  '<div class="method-callout"><strong>Index boundary:</strong> selections use league-position standardized, playing-time-shrunk official Fielding Run Value. Pitcher defense appears on the Fielding page as a separately labeled SABRhood estimate.</div></section>',
  '<section class="award-history-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Seventeen date-bounded checkpoints</span><h2>How the MVP races reached today</h2><p>The current top eight are traced backward through weekly cumulative FanGraphs pulls. Each point is recalculated with only the statistics available through that checkpoint, with stronger playing-time reliability shrinkage early in the season.</p></div><div class="race-timeline-summary">', race_timeline_cards, '</div>',
  '<div class="race-graphics-grid race-graphics-grid--timelines"><figure><img src="images/graphics-feed/al-mvp-race.png" alt="American League MVP season-to-date Race Rating timeline for the current top eight"><figcaption>AL MVP season-to-date timeline</figcaption></figure><figure><img src="images/graphics-feed/nl-mvp-race.png" alt="National League MVP season-to-date Race Rating timeline for the current top eight"><figcaption>NL MVP season-to-date timeline</figcaption></figure></div></section>',
  paste0('<section class="mvp-era-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Daily ballot-history rotation</span><h2>What the ', html_escape(fmt_int(mvp_rotation_decade)), 's rewarded, compared with today</h2><p>Every historical MVP is placed back into his own league-season distribution. The chart asks which categories MVP winners most consistently dominated in each decade, then compares that profile with winners from the 2020s.</p></div><figure class="feature-graphic feature-graphic--wide"><img src="images/graphics-feed/mvp-era-rotation.png" alt="MVP winner league percentiles by statistic for a rotating historical decade compared with the 2020s"><figcaption><strong>Ballot DNA changes with the sport.</strong> The current model uses the 2020s statistical profile, with WAR retained as a 35% modern total-value anchor because Lahman does not contain season WAR.</figcaption></figure></section>'),
  '<div class="race-graphics-grid"><figure><img src="images/graphics-feed/al-cy-young-race.png" alt="American League Cy Young performance ladder"><figcaption>AL Cy Young performance ladder</figcaption></figure><figure><img src="images/graphics-feed/nl-cy-young-race.png" alt="National League Cy Young performance ladder"><figcaption>NL Cy Young performance ladder</figcaption></figure><figure><img src="images/graphics-feed/al-reliever-race.png" alt="American League reliever performance ladder"><figcaption>AL Reliever of the Year performance ladder</figcaption></figure><figure><img src="images/graphics-feed/nl-reliever-race.png" alt="National League reliever performance ladder"><figcaption>NL Reliever of the Year performance ladder</figcaption></figure></div></section>',
  '<div class="race-disclaimer"><strong>Two complementary lenses.</strong><span>The FanGraphs award room above includes season value and defensive components. The PBP-derived boards below isolate offensive and pitching performance without claiming to predict a ballot.</span></div>',
  '<section class="section-heading"><span class="eyebrow">The offensive race</span><h2>Who has built the strongest hitting case?</h2><p>Estimated wOBA, OPS, run value per plate appearance, contact quality, and sample reliability form the transparent composite.</p></section>',
  '<div class="signal-grid">', offense_race_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Offensive board</span><h2>Top 12 season performances</h2></div>',
  render_table(offense_top, c("race_rank", "player_name", "team", "pa", "ops", "woba_estimate", "hard_hit_rate", "run_value_per_pa", "race_score"),
    c("Rank", "Player", "Team", "PA", "OPS", "wOBA est.", "Hard-hit", "RV/PA", "Score"),
    list(race_rank = fmt_int, pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, hard_hit_rate = fmt_rate, run_value_per_pa = fmt_dec, race_score = fmt_score)),
  '</section>',
  '<section class="section-heading"><span class="eyebrow">The pitching performance race</span><h2>Which pitchers are limiting opponent production most completely?</h2><p>Opponent estimated wOBA and OPS lead the model, reinforced by strikeouts, contact quality allowed, and sample reliability.</p></section>',
  '<div class="signal-grid">', prevention_race_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Run-prevention board</span><h2>Top 12 season performances</h2></div>',
  render_table(prevention_top, c("race_rank", "player_name", "team", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate", "race_score"),
    c("Rank", "Pitcher", "Team", "BF", "OPS allowed", "wOBA est.", "K%", "Hard-hit", "Score"),
    list(race_rank = fmt_int, pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, strikeout_rate = fmt_rate, hard_hit_rate = fmt_rate, race_score = fmt_score)),
  '</section>'
))

newsletter_stories <- newsletter_stories[order(num(newsletter_stories$newsletter_rank)), , drop = FALSE]
newsletter_lead <- newsletter_stories[newsletter_stories$placement == "lead", , drop = FALSE][1L, ]
newsletter_features <- newsletter_stories[newsletter_stories$placement == "feature", , drop = FALSE]
newsletter_briefs <- newsletter_stories[newsletter_stories$placement == "brief", , drop = FALSE]
newsletter_feature_cards <- vapply(seq_len(nrow(newsletter_features)), function(index) {
  row <- newsletter_features[index, , drop = FALSE]
  paste0(
    '<article class="newsletter-story"><span class="eyebrow">',
    html_escape(gsub("-", " ", row$lane[[1L]])), '</span><h2>',
    html_escape(row$headline[[1L]]), '</h2><p>', html_escape(row$deck[[1L]]),
    '</p><div class="newsletter-evidence"><span>', html_escape(row$evidence[[1L]]),
    '</span><strong>', html_escape(fmt_score(row$editorial_score[[1L]])),
    '</strong></div><a class="newsletter-story__link" href="',
    html_escape(row$page_link[[1L]]), '">Open the full board <span aria-hidden="true">&rarr;</span></a></article>'
  )
}, character(1))
newsletter_brief_list <- vapply(seq_len(nrow(newsletter_briefs)), function(index) {
  row <- newsletter_briefs[index, , drop = FALSE]
  paste0(
    '<li><span>', html_escape(gsub("-", " ", row$lane[[1L]])), '</span><div><strong>',
    html_escape(row$headline[[1L]]), '</strong><small>', html_escape(row$evidence[[1L]]),
    '</small></div><a href="', html_escape(row$page_link[[1L]]),
    '" aria-label="Open ', html_escape(row$headline[[1L]]), '">&rarr;</a></li>'
  )
}, character(1))
write_fragment("newsletter-daily.html", c(
  paste0(
    '<section class="newsletter-mast"><div><span class="eyebrow">',
    html_escape(newsletter_edition$weekday[[1L]]), ' edition &middot; ', html_escape(updated_label),
    '</span><h2>', html_escape(newsletter_edition$theme_title[[1L]]), '</h2><p>',
    html_escape(newsletter_edition$theme_deck[[1L]]), '</p></div>',
    '<div class="newsletter-mast__stamp"><strong>SABR</strong><span>hood daily</span></div></section>'
  ),
  '<div class="newsletter-layout"><main class="newsletter-main">',
  paste0(
    '<section class="newsletter-story newsletter-story--lead"><span class="eyebrow">',
    html_escape(gsub("-", " ", newsletter_lead$lane[[1L]])), ' &middot; lead story</span><h1>',
    html_escape(newsletter_lead$headline[[1L]]), '</h1><p class="newsletter-deck">',
    html_escape(newsletter_lead$deck[[1L]]), '</p><div class="newsletter-lead-proof"><span>',
    html_escape(newsletter_lead$evidence[[1L]]), '</span><strong>Editorial score ',
    html_escape(fmt_score(newsletter_lead$editorial_score[[1L]])), '</strong></div><a class="btn btn-sabr-red" href="',
    html_escape(newsletter_lead$page_link[[1L]]), '">Read the complete analysis</a></section>'
  ),
  newsletter_feature_cards,
  '</main><aside class="newsletter-side">',
  '<section class="newsletter-note newsletter-briefing"><span class="eyebrow">Morning briefing</span><h2>The rest of today&rsquo;s board</h2><ol>',
  newsletter_brief_list,
  '</ol><p class="method-note">The edition limits each reporting lane to two stories, then applies the day&rsquo;s rotating editorial theme.</p></section>',
  paste0(
    '<section class="newsletter-note newsletter-edition-card"><span class="eyebrow">Edition audit</span><h2>',
    html_escape(fmt_int(newsletter_edition$story_count[[1L]])), ' selected stories</h2><p>',
    html_escape(fmt_int(newsletter_edition$lane_count[[1L]])),
    ' distinct reporting lanes are represented. Headlines and decks are assembled from published data fields and threshold-aware templates.</p><small>',
    html_escape(newsletter_edition$newsletter_method[[1L]]), '</small></section>'
  ),
  '<section class="newsletter-note newsletter-note--dark"><span class="eyebrow">Today&rsquo;s reading list</span><h2>Go deeper</h2>',
  '<a href="standings.html">Track the standings <span>&rarr;</span></a>',
  '<a href="leaderboards.html">Open tracking-data leaders <span>&rarr;</span></a>',
  '<a href="today.html">Open the signal desk <span>&rarr;</span></a>',
  '<a href="aaa.html">Visit Triple-A Watch <span>&rarr;</span></a>',
  '</section></aside></div>'
))

change_profile_cards <- vapply(seq_len(nrow(change_spotlights)), function(index) {
  player_context_card(change_spotlights[index, , drop = FALSE], compact = FALSE)
}, character(1))
change_board <- utils::head(all_changes, 30L)
write_fragment("player-change-cards.html", c(
  '<div class="change-method-strip"><strong>Two lenses, one read</strong><span><b>Change z-score</b> shows how many league standard deviations a player&rsquo;s recent movement sits from ordinary movement in that statistic. <b>Season percentile</b> compares the full-season level with league peers.</span></div>',
  '<section class="section-heading"><span class="eyebrow">Balanced change radar</span><h2>Eight players whose underlying conversation moved</h2><p>The board deliberately includes hitters, pitchers, improvements, and declines. Signal priority rewards unusual movement while accounting for recent-sample reliability.</p></section>',
  '<div class="player-context-grid">', change_profile_cards, '</div>',
  '<div class="method-callout"><strong>Interpretation boundary:</strong> a +2.00 z change is two league standard deviations above ordinary recent movement; a -2.00 z change is two below. Direction still depends on the statistic and player perspective. Season percentiles answer the quality question separately. Neither is a projection.</div>'
))

career_legacy_label <- function(value) {
  value <- tolower(as.character(value))
  ifelse(
    grepl("^strong", value),
    "Hall bound?",
    ifelse(
      grepl("^credible", value),
      "Hall bound?",
      ifelse(
        grepl("^fringe", value),
        "No Hall yet",
        "No Hall yet"
      )
    )
  )
}

career_legacy_context <- function(value) {
  label <- career_legacy_label(value)
  switch(
    label,
    "Hall bound?" =
      "This career shape frequently appears beside eventual Hall of Famers.",
    "The comparable group has not usually finished on a Hall-level path."
  )
}

career_forecast_strip <- function(player_id) {
  rows <- career_three_season[
    as.character(career_three_season$player_id) == as.character(player_id),
    ,
    drop = FALSE
  ]
  rows <- rows[order(num(rows$forecast_season)), , drop = FALSE]
  if (!nrow(rows)) {
    return(
      '<p class="career-season-empty">Three-season estimates are waiting for the next model build.</p>'
    )
  }
  paste0(
    '<div class="career-season-strip">',
    paste0(vapply(seq_len(min(3L, nrow(rows))), function(index) {
      row <- rows[index, , drop = FALSE]
      slash <- paste0(
        sub("^0", "", sprintf("%.3f", num(row$projected_batting_average[[1L]]))),
        " / ",
        sub("^0", "", sprintf("%.3f", num(row$projected_on_base_percentage[[1L]]))),
        " / ",
        sub("^0", "", sprintf("%.3f", num(row$projected_slugging_percentage[[1L]])))
      )
      paste0(
        '<article><span>', html_escape(row$forecast_season[[1L]]),
        ' estimate</span><strong>', html_escape(fmt_int(row$projected_home_runs[[1L]])),
        ' HR</strong><small>', html_escape(fmt_int(row$projected_plate_appearances[[1L]])),
        ' PA &middot; ', html_escape(slash), '</small><em>',
        html_escape(tools::toTitleCase(row$role_context[[1L]])),
        '</em></article>'
      )
    }, character(1)), collapse = ""),
    '</div>'
  )
}

career_trajectory_cards <- vapply(
  seq_len(min(8L, nrow(career_trajectories))),
  function(index) {
    row <- career_trajectories[index, , drop = FALSE]
    paste0(
      '<article class="career-trajectory-card"><header><span>',
      html_escape(row$team[[1L]]), ' &middot; ',
      html_escape(fmt_int(row$career_games[[1L]])), ' career games</span><strong>',
      html_escape(paste(
        tools::toTitleCase(row$confidence[[1L]]),
        "neighbor fit"
      )),
      '</strong></header><h3>', html_escape(row$player_name[[1L]]),
      '</h3><p class="career-trajectory-lede">',
      html_escape(row$broadcast_note[[1L]]), '</p>',
      '<div class="career-legacy-read"><span>Legacy Projection</span><strong>',
      html_escape(career_legacy_label(row$hof_path[[1L]])),
      '</strong><p>',
      html_escape(career_legacy_context(row$hof_path[[1L]])),
      '</p></div>',
      '<div class="career-trajectory-metrics"><span><small>Closest path</small><strong>',
      html_escape(row$top_comparable[[1L]]),
      '</strong></span><span><small>Projected AVG / OBP / SLG</small><strong>',
      html_escape(paste0(
        sub("^0", "", sprintf("%.3f", row$final_batting_average_p50[[1L]])),
        " / ",
        sub("^0", "", sprintf("%.3f", row$final_on_base_percentage_p50[[1L]])),
        " / ",
        sub("^0", "", sprintf("%.3f", row$final_slugging_percentage_p50[[1L]]))
      )),
      '</strong></span><span><small>Expected HR pace</small><strong>',
      html_escape(fmt_dec(
        row$future_home_runs_per_600_pa_p50[[1L]],
        1L
      )),
      ' per 600 PA</strong></span></div>',
      career_forecast_strip(row$player_id[[1L]]),
      '</article>'
    )
  },
  character(1)
)
career_projection_table <- utils::head(career_trajectories, 30L)
career_projection_table$projected_rate_line <- paste0(
  sub(
    "^0", "",
    sprintf("%.3f", career_projection_table$final_batting_average_p50)
  ),
  "/",
  sub(
    "^0", "",
    sprintf("%.3f", career_projection_table$final_on_base_percentage_p50)
  ),
  "/",
  sub(
    "^0", "",
    sprintf("%.3f", career_projection_table$final_slugging_percentage_p50)
  )
)
career_projection_table$three_year_read <- vapply(
  as.character(career_projection_table$player_id),
  function(player_id) {
    rows <- career_three_season[
      as.character(career_three_season$player_id) == player_id,
      ,
      drop = FALSE
    ]
    rows <- rows[order(num(rows$forecast_season)), , drop = FALSE]
    if (!nrow(rows)) return("Next model build")
    paste0(
      rows$forecast_season,
      ": ",
      fmt_int(rows$projected_plate_appearances),
      " PA / ",
      fmt_int(rows$projected_home_runs),
      " HR",
      collapse = " | "
    )
  },
  character(1)
)
career_projection_table$legacy_context <- career_legacy_label(
  career_projection_table$hof_path
)
career_backtest_overall <- career_backtest[
  career_backtest$scope == "overall",
  ,
  drop = FALSE
]
career_holdout_500 <- career_holdout[
  as.numeric(career_holdout$threshold_games) == 500,
  ,
  drop = FALSE
]
career_calibration_label <- if (
  nrow(career_model_card) &&
    identical(
      career_model_card$calibration_status[[1L]],
      "chronological_holdout_adopted"
    )
) {
  "Probability adjustment adopted"
} else {
  "Raw probabilities retained"
}
career_tuning_baseline <- career_tuning_evaluation[
  career_tuning_evaluation$model == "baseline",
  ,
  drop = FALSE
]
career_tuning_candidate <- career_tuning_evaluation[
  career_tuning_evaluation$model == "checkpoint_tuned",
  ,
  drop = FALSE
]
career_tuning_bias <- career_tuning_evaluation[
  career_tuning_evaluation$model == "active_bias_corrected",
  ,
  drop = FALSE
]
career_weight_label <- if (
  nrow(career_model_card) &&
    identical(
      career_model_card$weight_tuning_status[[1L]],
      "chronological_holdout_adopted"
    )
) {
  "Checkpoint weights adopted"
} else {
  "Baseline weights retained"
}
career_bias_label <- if (
  nrow(career_model_card) &&
    identical(
      career_model_card$bias_tuning_status[[1L]],
      "chronological_holdout_adopted"
    )
) {
  "Bias correction adopted"
} else {
  "Bias correction rejected"
}
write_fragment("career-trajectories.html", c(
  '<div class="change-method-strip"><strong>Comparable paths, translated for readers</strong><span>The engine aligns hitters by career games, age, offensive shape, and playing-time pace. The public view emphasizes what those careers suggest; calibration diagnostics remain in the methodology files.</span></div>',
  '<div class="career-reader-key">',
  '<article><span>Closest path</span><strong>Who had the most similar checkpoint?</strong><p>A comparison point, not a claim that the careers will finish alike.</p></article>',
  '<article><span>Three-season estimate</span><strong>Playing time, home runs, and slash line</strong><p>Full-season estimates blend the current player with how comparable careers occupied each future season.</p></article>',
  '<article><span>Legacy Projection</span><strong>Where comparable careers finished</strong><p>The label summarizes the completed careers in the neighbor group. It does not predict a vote.</p></article>',
  '</div>',
  '<section class="section-heading"><span class="eyebrow">Career trajectory watch</span><h2>How active batters careers may develop</h2><p>Player progression is analyzed and matched with similar careers thus far to estiamte where a career may lead.</p></section>',
  '<div class="career-trajectory-grid">', career_trajectory_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Active hitter board</span><h2>Career Outlook Table</h2><p>Compressed table for even more projections!</p></div>',
  render_table(
    career_projection_table,
    c(
      "player_name", "team", "age", "career_games", "top_comparable",
      "projected_rate_line", "legacy_context",
      "confidence"
    ),
    c(
      "Player", "Team", "Age", "Career G", "Closest comparison",
      "Career AVG/OBP/SLG", "Legacy Projection",
      "Level of fit"
    ),
    list(
      age = function(x) fmt_dec(x, 1L),
      career_games = fmt_int
    ),
    "data-table career-trajectory-table"
  ),
  '</section>',
  '<div class="method-callout"><strong>How to read it:</strong> the model was tested by rolling historical forecasts forward in time, and experimental adjustments that did not improve later unseen careers were rejected. Three-season estimates divide each comparable player&rsquo;s remaining games into future season segments, then apply the group&rsquo;s expected playing time and home-run pace. Hall labels summarize comparable career destinations and are not election probabilities. This release covers hitters and offensive career shape; defense, position, injuries, and minor-league history are not yet similarity features. Full validation diagnostics remain available on the methodology page.</div>'
))
write_fragment("home-career-trajectories.html", c(
  '<section class="section-heading"><span class="eyebrow">Career Path Engine</span><h2>Where have careers like this gone next?</h2><p>Games, age, offensive shape, and historical survival turn today&rsquo;s player line into a range of career paths.</p></section>',
  '<div class="career-trajectory-grid career-trajectory-grid--home">',
  utils::head(career_trajectory_cards, 3L),
  '</div><div class="section-action"><a class="btn btn-metallic" href="career-trajectories.html">Open the Career Path Engine</a></div>'
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
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Pitching leaders</span><h2>Pitcher Production Board</h2><p>Ranked by OPS allowed; minimum 75 batters faced.</p></div>',
  render_table(pitcher_suppressors, c("player_name", "team", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate"),
    c("Pitcher", "Team", "BF", "OPS allowed", "wOBA est.", "K%", "Hard-hit"),
    list(pa = fmt_int, ops = fmt_dec, woba_estimate = fmt_dec, strikeout_rate = fmt_rate, hard_hit_rate = fmt_rate)),
  '</section>'
))

player_market_groups <- player_market_groups[
  order(-num(player_market_groups$scarcity_index)),
  ,
  drop = FALSE
]
player_market_cards <- vapply(
  seq_len(nrow(player_market_groups)),
  function(index) {
    row <- player_market_groups[index, , drop = FALSE]
    paste0(
      '<article class="player-market-card is-',
      html_escape(row$market_read[[1L]]), '"><header><span>',
      html_escape(tools::toTitleCase(row$market_read[[1L]])),
      ' talent pool</span><strong>',
      html_escape(fmt_score(row$scarcity_index[[1L]])),
      ' scarcity</strong></header><h3>',
      html_escape(row$market_group[[1L]]), '</h3>',
      '<div class="player-market-card__numbers"><span><small>Roster slots / club</small><strong>',
      html_escape(fmt_dec(row$roster_slots_per_team[[1L]], 1L)),
      '</strong></span><span><small>Qualified coverage</small><strong>',
      html_escape(fmt_rate(row$qualified_slot_coverage[[1L]], 0L)),
      '</strong></span><span><small>Upper-half coverage</small><strong>',
      html_escape(fmt_rate(row$upper_half_slot_coverage[[1L]], 0L)),
      '</strong></span></div><p><b>Pool:</b> ',
      html_escape(fmt_int(row$player_supply[[1L]])), ' qualified players for ',
      html_escape(fmt_int(row$league_roster_slots[[1L]])),
      ' estimated MLB roster jobs; ',
      html_escape(fmt_int(row$teams_with_need[[1L]])),
      ' clubs currently grade as needing help. <b>Quality bar:</b> ',
      html_escape(fmt_dec(row$median_quality_rate[[1L]], 1L)), ' ',
      html_escape(row$quality_rate_label[[1L]]), '</p><footer><span>Pool leaders</span><strong>',
      html_escape(row$top_players[[1L]]), '</strong></footer></article>'
    )
  },
  character(1)
)
starting_lhp <- player_market_players[
  player_market_players$market_group == "Starting LHP",
  ,
  drop = FALSE
]
starting_lhp <- starting_lhp[
  order(-num(starting_lhp$quality_rate), -num(starting_lhp$war)),
  ,
  drop = FALSE
]
write_fragment("player-market.html", c(
  '<div class="market-boundary"><span class="eyebrow">Development preview</span><h2>This is a talent-supply market, not yet a transaction market</h2><p>The first release measures how much credible MLB talent exists in each role and how many clubs currently grade as needing help there. Salary, service time, options, years of control, free-agent status, and trade availability will remain blank until a reliable contract and transaction source is connected.</p></div>',
  '<section class="section-heading"><span class="eyebrow">League role market</span><h2>Where quality is plentiful&mdash;and where teams are chasing a thin pool</h2><p>Every pool is scaled to the number of 26-man roster jobs it must fill. Five rotation places and eight bullpen places per club are divided by handedness; the remaining 13 roster places are allocated across catcher, infield, outfield, and designated hitter roles.</p></section>',
  '<div class="player-market-grid">', player_market_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Example market: starting left-handers</span><h2>Who establishes the current quality and workload bar?</h2><p>This is the available MLB talent pool in the analytical sense; it does not claim that any listed pitcher is obtainable.</p></div>',
  render_table(
    utils::head(starting_lhp, 20L),
    c(
      "player_name", "team", "age", "playing_time_label", "war",
      "quality_rate", "quality_rate_label"
    ),
    c("Pitcher", "Team", "Age", "Workload", "WAR", "Role-adjusted quality", "Scale"),
    list(
      age = function(x) fmt_dec(x, 1L),
      war = function(x) fmt_dec(x, 1L),
      quality_rate = function(x) fmt_dec(x, 1L)
    ),
    "data-table player-market-table"
  ),
  '</section>',
  '<div class="market-price-lock"><span>Future licensed layer</span><strong>What do these players go for?</strong><p>That answer needs contract value, remaining control, option status, transaction history, and actual availability. Once sourced, the page can estimate dollars per projected win, acquisition cost by role, and the premium created by scarcity without confusing performance with price.</p></div>',
  '<div class="method-callout"><strong>Roster-adjusted formula:</strong> qualified pools use at least 100 PA for hitters, 30 IP for starters, and 15 IP for relievers. Role quality is WAR normalized to 600 PA, 180 starter innings, or 65 relief innings. Qualified and upper-half supply are divided by estimated league roster jobs, while club need is multiplied by the number of roster slots required for that role. The scarcity score combines the qualified-player gap, the upper-half talent gap, and need-weighted slot pressure.</div>'
))

signature_spotlights <- signature_pitches[seq_len(min(6L, nrow(signature_pitches))), ]
signature_cards <- vapply(seq_len(nrow(signature_spotlights)), function(index) pitch_identity_card(signature_spotlights[index, , drop = FALSE]), character(1))
pitch_change_cards <- vapply(seq_len(nrow(pitch_change_spotlights)), function(index) pitch_usage_change_card(pitch_change_spotlights[index, , drop = FALSE]), character(1))
arsenal_spotlight_ids <- unique(as.character(arsenal_spotlights$player_id))
arsenal_spotlight_cards <- vapply(arsenal_spotlight_ids, function(player_id) {
  arsenal_spotlight_card(arsenal_spotlights[as.character(arsenal_spotlights$player_id) == player_id, , drop = FALSE])
}, character(1))
write_fragment("pitch-lab.html", c(
  '<section class="section-heading"><span class="eyebrow">Pitch identities</span><h2>Six pitches with a visual fingerprint</h2><p>Movement location, velocity, bat-missing, chase, and usage combine into a transparent pitch-quality board.</p></section>',
  '<div class="pitch-identity-grid">', signature_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Arsenal leaderboard</span><h2>Highest whiff pitch types</h2><p>At least 50 swings and 100 total pitches in the underlying product.</p></div>',
  render_table(arsenal_whiffs, c("player_name", "team", "pitch_name", "pitches", "usage_rate", "average_velocity", "whiff_rate", "chase_rate", "pitch_change_rate"),
    c("Pitcher", "Team", "Pitch", "Pitches", "Usage", "Velo", "Whiff", "Chase", "Changed from prior"),
    list(pitches = fmt_int, usage_rate = fmt_rate, average_velocity = function(x) fmt_dec(x, 1L), whiff_rate = fmt_rate, chase_rate = fmt_rate, pitch_change_rate = fmt_rate)),
  '</section>',
  '<section class="section-heading"><span class="eyebrow">Expanded arsenal spotlights</span><h2>When a pitch earns a larger job and better results</h2><p>An emerging weapon must gain at least three percentage points of usage, appear at least 20 times in the pitcher&rsquo;s latest five games, and improve its whiff rate versus the non-overlapping earlier-season sample. The complete arsenal stays visible because a pitch succeeds in relation to everything thrown beside it.</p></section>',
  '<div class="arsenal-spotlight-grid">', arsenal_spotlight_cards, '</div>',
  '<figure class="feature-graphic"><img src="images/graphics-feed/arsenal-takeover-spotlights.png" alt="Pitch movement maps for four pitchers whose featured offering gained usage and whiffs"><figcaption><strong>Emerging-weapon map.</strong> Every panel uses the same axes centered at zero. Color identifies pitch type, point size identifies season usage, and the red outline identifies the offering gaining both trust and bat-missing success.</figcaption></figure>',
  '<section class="section-heading"><span class="eyebrow">Arsenal change detector</span><h2>The pitches whose jobs changed most</h2><p>The final five appearances are compared with the non-overlapping earlier season. Percentage-point movement is paired with a reader-friendly Change Index, where 50 is ordinary movement among pitchers throwing the same pitch.</p></section>',
  '<div class="pitch-change-grid">', pitch_change_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Full context board</span><h2>Usage, velocity, and shape moved together</h2><p>Large usage shifts lead the ranking; velocity, horizontal break, induced vertical break, and whiff changes show what else moved.</p></div>',
  render_table(pitch_context_board, c("context_rank", "pitcher_name", "team", "pitch_name", "baseline_pitches", "recent_pitches", "baseline_usage", "recent_usage", "usage_delta_pp", "usage_change_z", "velocity_delta", "horizontal_break_delta", "ivb_delta", "whiff_delta"),
    c("Rank", "Pitcher", "Team", "Pitch", "Earlier N", "Recent N", "Earlier use", "Recent use", "Delta", "Change Index", "Velo delta", "HB delta", "IVB delta", "Whiff delta"),
    list(context_rank = fmt_int, baseline_pitches = fmt_int, recent_pitches = fmt_int, baseline_usage = fmt_rate, recent_usage = fmt_rate, usage_delta_pp = function(x) paste0(ifelse(num(x) > 0, "+", ""), fmt_dec(x, 1L), " pts"), usage_change_z = fmt_z, velocity_delta = function(x) paste0(ifelse(num(x) > 0, "+", ""), fmt_dec(x, 1L)), horizontal_break_delta = function(x) paste0(ifelse(num(x) > 0, "+", ""), fmt_dec(x, 1L)), ivb_delta = function(x) paste0(ifelse(num(x) > 0, "+", ""), fmt_dec(x, 1L)), whiff_delta = function(x) paste0(ifelse(num(x) > 0, "+", ""), fmt_rate(x)))),
  '</section>',
  '<section class="discipline-lab"><div class="section-heading section-heading--tight"><span class="eyebrow">Hitter recognition layer</span><h2>Chase and contact belong beside the arsenal</h2><p>Pitch-level discipline separates hitters who avoid expanding the zone from hitters who make exceptional contact when they do swing.</p></div><div class="discipline-lab__layout"><figure><img src="images/graphics-feed/hitter-discipline-frontier.png" alt="Qualified hitters plotted by chase rate and zone contact rate"><figcaption>Low chase and high zone contact define the upper discipline frontier.</figcaption></figure>',
  render_table(utils::head(hitter_discipline[order(num(hitter_discipline$discipline_rank)), , drop = FALSE], 12L), c("discipline_rank", "player_name", "team", "pitches", "swing_rate", "whiff_rate", "chase_rate", "zone_contact_rate", "discipline_score"),
    c("Rank", "Hitter", "Team", "Pitches", "Swing", "Whiff", "Chase", "Zone contact", "Score"),
    list(discipline_rank = fmt_int, pitches = fmt_int, swing_rate = fmt_rate, whiff_rate = fmt_rate, chase_rate = fmt_rate, zone_contact_rate = fmt_rate, discipline_score = fmt_score)),
  '</div></section>',
  '<div class="method-callout"><strong>Reporter&rsquo;s rule:</strong> the board finds the largest changes; it does not assign a cause. Confirm role, health, opponent mix, and pitch-classification stability before turning a signal into a story.</div>',
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
slate_date <- as.Date(daily_game_inputs$game_date[[1L]])
player_model_date <- suppressWarnings(as.Date(player_simulation_model$game_date[[1L]]))
player_board_current <- nrow(player_simulations) > 0L && !is.na(player_model_date) && identical(player_model_date, slate_date)
matchup_model_date <- suppressWarnings(as.Date(matchup_event_model$game_date[[1L]]))
matchup_board_current <- nrow(matchup_event_probabilities) > 0L &&
  !is.na(matchup_model_date) && identical(matchup_model_date, slate_date)
state_model_date <- suppressWarnings(as.Date(state_simulation_model$game_date[[1L]]))
state_board_current <- nrow(state_simulation_games) > 0L &&
  nrow(state_simulation_hitters) > 0L &&
  !is.na(state_model_date) && identical(state_model_date, slate_date)
if (matchup_board_current) {
  matchup_spotlights <- matchup_event_probabilities[
    order(-num(matchup_event_probabilities$estimated_woba), -num(matchup_event_probabilities$matchup_reliability)),
    ,
    drop = FALSE
  ]
  matchup_probability_section <- paste0(
    '<section class="projection-matchup-lab"><div class="section-heading section-heading--tight"><span class="eyebrow">Simulation Phase 1 &middot; shadow model</span><h2>Every confirmed hitter now has a starter-specific event distribution</h2><p>The new layer combines batter and pitcher results with multinomial log5, shrinks small samples, applies the appropriate handedness splits, and makes restrained recent-form adjustments. These probabilities are being audited beside the current simulator before they are allowed to drive it.</p></div>',
    '<div class="method-grid">',
    stat_card("Matchup coverage", "Confirmed rows", fmt_int(matchup_event_model$matchup_rows[[1L]]), paste(fmt_int(matchup_event_model$scheduled_games[[1L]]), "games"), "navy"),
    stat_card("Handedness", "Complete splits", fmt_rate(matchup_event_model$complete_platoon_match_rate[[1L]]), "Both the hitter and starter have the required split.", "steel"),
    stat_card("Reliability", "Average score", fmt_score(matchup_event_model$mean_matchup_reliability[[1L]]), "Overall and split sample sizes determine the visible confidence.", "red"),
    '</div><section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Highest modeled production against today&rsquo;s starter</span><h3>The probability components remain visible</h3><p>A strong matchup can come from contact, power, patience, or avoiding strikeouts. Reliability is shown so an extreme number cannot hide its sample quality.</p></div>',
    render_table(
      utils::head(matchup_spotlights, 12L),
      c("batter_name", "batter_team", "pitcher_name", "batter_hand", "pitcher_hand", "p_K", "p_BB", "p_hit", "p_HR", "estimated_woba", "matchup_reliability"),
      c("Hitter", "Team", "Starter", "Bat", "Throws", "K", "BB", "Hit", "HR", "Est. wOBA", "Reliability"),
      list(
        p_K = fmt_rate, p_BB = fmt_rate, p_hit = fmt_rate, p_HR = fmt_rate,
        estimated_woba = fmt_dec, matchup_reliability = fmt_score
      )
    ),
    '</section><div class="method-callout"><strong>Probability contract:</strong> walk, hit-by-pitch, strikeout, single, double, triple, home run, and other out are mutually exclusive. The maximum sum error in today&rsquo;s ', html_escape(fmt_int(matchup_event_model$matchup_rows[[1L]])), ' rows is ', html_escape(format(num(matchup_event_model$max_probability_sum_error[[1L]]), scientific = TRUE)), '.</div></section>'
  )
} else {
  matchup_probability_section <- '<section class="projection-matchup-lab"><div class="method-callout"><strong>Matchup layer waiting:</strong> the Phase 1 batter-starter distributions are withheld until the current slate has posted batting orders.</div></section>'
}
if (state_board_current) {
  state_game_board <- state_simulation_games[
    order(-num(state_simulation_games$one_run_probability), -num(state_simulation_games$extra_innings_probability)),
    ,
    drop = FALSE
  ]
  state_hitter_board <- state_simulation_hitters[
    order(-num(state_simulation_hitters$prob_1plus_hr), -num(state_simulation_hitters$prob_1plus_hit)),
    ,
    drop = FALSE
  ]
  state_reliever_board <- state_simulation_relievers[
    order(-num(state_simulation_relievers$appearance_probability), -num(state_simulation_relievers$mean_selection_likelihood)),
    ,
    drop = FALSE
  ]
  today_runner_ids <- unique(as.character(state_simulation_hitters$batter_id))
  running_game_board <- baserunning_runners[
    as.character(baserunning_runners$runner_id) %in% today_runner_ids &
      as.character(baserunning_runners$opportunity_type) == "steal_second",
    ,
    drop = FALSE
  ]
  running_game_board <- running_game_board[
    order(-num(running_game_board$shrunk_rate), -num(running_game_board$reliability)),
    ,
    drop = FALSE
  ]
  today_starter_ids <- unique(c(
    as.character(state_simulation_games$away_starter_id),
    as.character(state_simulation_games$home_starter_id)
  ))
  hold_board <- baserunning_pitcher_hold[
    as.character(baserunning_pitcher_hold$pitcher_id) %in% today_starter_ids &
      as.character(baserunning_pitcher_hold$opportunity_type) == "steal_second",
    ,
    drop = FALSE
  ]
  hold_board <- hold_board[
    order(num(hold_board$attempt_suppression_index), -num(hold_board$hold_reliability)),
    ,
    drop = FALSE
  ]
  today_venues <- unique(as.character(daily_game_inputs$venue_name))
  park_board <- baserunning_parks[
    as.character(baserunning_parks$venue_name) %in% today_venues &
      as.character(baserunning_parks$opportunity_type) %in%
        c("single_second_scores", "single_first_to_third", "double_first_scores"),
    ,
    drop = FALSE
  ]
  park_board <- park_board[
    order(-abs(num(park_board$empirical_multiplier) - 1), -num(park_board$reliability)),
    ,
    drop = FALSE
  ]
  calibration_message <- if (isTRUE(as.logical(state_calibration_status$deployment_approved[[1L]]))) {
    "The chronological holdout gate approved a calibration layer for shadow application. Public promotion remains separately blocked."
  } else if (
    "model_version" %in% names(state_calibration_status) &&
      !identical(
        as.character(state_calibration_status$model_version[[1L]]),
        as.character(state_simulation_model$model_version[[1L]])
      )
  ) {
    paste(
      "The current Phase 5 version starts a separate chronological ledger with",
      "its next eligible pregame archive. Prior-version status:",
      as.character(state_calibration_status$status[[1L]])
    )
  } else {
    as.character(state_calibration_status$status[[1L]])
  }
  state_simulation_section <- paste0(
    '<section class="projection-state-lab"><div class="section-heading section-heading--tight"><span class="eyebrow">Simulation Phase 5 &middot; learned manager hook</span><h2>The engine now decides when the starter leaves, then chooses the next arm</h2><p>Every draw maintains inning, outs, named runners, score, batting-order position, pitcher identity, workload, park and weather context, and available bullpen arms. The time-validated manager model reassesses the hook after every plate appearance using pitch count, batters faced, times through the order, inning, score, and the latest result.</p></div>',
    '<div class="method-grid">',
    stat_card("Detailed slate", "Games simulated", fmt_int(state_simulation_model$games_simulated[[1L]]), paste(fmt_int(state_simulation_model$simulations_per_game[[1L]]), "shadow draws per game"), "navy"),
    stat_card("Bullpen path", "Named arms used", fmt_dec(state_simulation_model$mean_relievers_used[[1L]], 1L), paste(fmt_rate(state_simulation_model$mean_bullpen_pa_share[[1L]]), "of plate appearances"), "steel"),
    stat_card("Manager hook", "Later-date ROC AUC", fmt_dec(state_simulation_model$manager_hook_roc_auc[[1L]], 3L), paste(fmt_int(state_simulation_model$manager_hook_validation_rows[[1L]]), "validation decisions"), "red"),
    stat_card("Starter workload", "Expected BF", paste0(fmt_dec(state_simulation_model$mean_away_starter_expected_bf[[1L]], 1L), " / ", fmt_dec(state_simulation_model$mean_home_starter_expected_bf[[1L]], 1L)), "Season workload blended with the five most recent starter-like appearances.", "navy"),
    stat_card("Run environment", "Park + weather", fmt_dec(state_simulation_model$mean_run_environment_multiplier[[1L]], 3L), "Tilts on-base and extra-base event odds before the first pitch.", "steel"),
    stat_card("Ball in play", "Double plays", fmt_dec(state_simulation_model$mean_double_plays[[1L]], 2L), paste(fmt_dec(state_simulation_model$mean_productive_out_runs[[1L]], 2L), "runs per game from productive outs"), "red"),
    stat_card("Running game", "Steal attempts", fmt_dec(state_simulation_model$mean_steal_attempts[[1L]], 2L), paste(fmt_dec(state_simulation_model$mean_stolen_bases[[1L]], 2L), "successful steals per game"), "navy"),
    stat_card("Historical foundation", "Runner windows", fmt_int(baserunning_model$opportunities[[1L]]), paste(fmt_int(baserunning_model$games[[1L]]), "completed games"), "steel"),
    stat_card("Profile coverage", "Today's hitters", fmt_rate(state_simulation_model$mean_runner_profile_coverage[[1L]]), "Missing players fall back to the league prior.", "red"),
    '</div><section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Phase 5 shadow game board</span><h3>Workload, game state, environment, and bullpen decisions shape the score</h3><p>Starter BF, the probability of a modeled hook, bullpen share, relievers used, double plays, steals, and empirical park advancement all come from the same simulation.</p></div>',
    render_table(
      state_game_board,
      c("away_team", "home_team", "away_win_probability", "home_win_probability", "away_mean_runs", "home_mean_runs", "one_run_probability", "away_starter_mean_bf", "home_starter_mean_bf", "away_starter_hook_probability", "home_starter_hook_probability", "mean_relievers_used", "game_run_environment_multiplier", "mean_double_plays", "mean_stolen_bases"),
      c("Away", "Home", "Away win", "Home win", "Away runs", "Home runs", "One-run", "Away SP BF", "Home SP BF", "Away hook", "Home hook", "Relievers", "Run env.", "DP", "SB"),
      list(
        away_win_probability = fmt_rate, home_win_probability = fmt_rate,
        away_mean_runs = function(x) fmt_dec(x, 1L), home_mean_runs = function(x) fmt_dec(x, 1L),
        one_run_probability = fmt_rate, away_starter_mean_bf = function(x) fmt_dec(x, 1L),
        home_starter_mean_bf = function(x) fmt_dec(x, 1L),
        away_starter_hook_probability = fmt_rate,
        home_starter_hook_probability = fmt_rate,
        mean_relievers_used = function(x) fmt_dec(x, 1L),
        game_run_environment_multiplier = function(x) fmt_dec(x, 3L),
        mean_double_plays = function(x) fmt_dec(x, 2L),
        mean_stolen_bases = function(x) fmt_dec(x, 2L)
      )
    ),
    '</section><section class="dashboard-block phase4-running-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Running-game intelligence</span><h3>Who creates pressure, who suppresses it, and where does the outfield change the route?</h3><p>Attempt rates use every eligible runner window. Success and advancement rates are shrunk toward league context so a handful of plays cannot masquerade as a stable skill.</p></div><div class="projection-phase4-grid"><div><h4>Today&rsquo;s most aggressive runners</h4>',
    render_table(
      utils::head(running_game_board, 10L),
      c("runner_name", "opportunities", "attempts", "shrunk_rate", "shrunk_success_rate", "reliability"),
      c("Runner", "Windows", "Attempts", "Attempt rate", "Success", "Reliability"),
      list(
        opportunities = fmt_int, attempts = fmt_int, shrunk_rate = fmt_rate,
        shrunk_success_rate = fmt_rate, reliability = fmt_rate
      )
    ),
    '</div><div><h4>Probable-starter hold profile</h4>',
    render_table(
      utils::head(hold_board, 10L),
      c("pitcher_name", "fielding_team", "runner_windows", "attempts", "attempt_suppression_index", "shrunk_success_rate_allowed", "hold_reliability"),
      c("Pitcher", "Team", "Windows", "Attempts", "Attempt index", "Success allowed", "Reliability"),
      list(
        runner_windows = fmt_int, attempts = fmt_int,
        attempt_suppression_index = fmt_int,
        shrunk_success_rate_allowed = fmt_rate, hold_reliability = fmt_rate
      )
    ),
    '</div></div><div><h4>Today&rsquo;s strongest empirical park movement signals</h4>',
    render_table(
      utils::head(park_board, 12L),
      c("venue_name", "opportunity_type", "opportunities", "observed_rate", "league_rate", "empirical_multiplier", "reliability"),
      c("Venue", "Runner event", "Opps", "Observed", "League", "Multiplier", "Reliability"),
      list(
        opportunities = fmt_int, observed_rate = fmt_rate, league_rate = fmt_rate,
        empirical_multiplier = function(x) paste0(fmt_dec(100 * x, 0L), " index"),
        reliability = fmt_rate
      )
    ),
    '</div></section><section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Manager likelihood board</span><h3>Who is most likely to enter, and when?</h3><p>Appearance probability is produced across complete games. Selection likelihood is the arm&rsquo;s average probability whenever that reliever was still available at a decision point.</p></div>',
    render_table(
      utils::head(state_reliever_board, 20L),
      c("pitcher_name", "team", "opponent", "throws", "availability_score", "appearance_probability", "mean_selection_likelihood", "mean_bf_per_appearance", "mean_entry_inning", "high_leverage_entry_share"),
      c("Reliever", "Team", "Opponent", "Throws", "Available", "Appearance", "At decision", "BF / app.", "Entry inn.", "High leverage"),
      list(
        availability_score = fmt_rate, appearance_probability = fmt_rate,
        mean_selection_likelihood = fmt_rate,
        mean_bf_per_appearance = function(x) fmt_dec(x, 1L),
        mean_entry_inning = function(x) fmt_dec(x, 1L),
        high_leverage_entry_share = fmt_rate
      )
    ),
    '</section><section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Expanded player outcomes</span><h3>Runs, RBI, steals, sacrifice flies, and double-play risk now retain player identity</h3><p>Each occupied base stores the lineup spot that owns the runner, allowing advancement and outs to return to the appropriate player line.</p></div>',
    render_table(
      utils::head(state_hitter_board, 15L),
      c("batter_name", "team", "batting_order", "expected_pa", "expected_hits", "expected_hr", "expected_runs", "expected_rbi", "expected_sb", "expected_sf", "expected_gdp", "prob_1plus_hit", "prob_1plus_hr", "prob_1plus_run", "prob_1plus_rbi", "prob_1plus_sb"),
      c("Hitter", "Team", "Order", "PA", "H", "HR", "R", "RBI", "SB", "SF", "GDP", "1+ H", "1+ HR", "1+ R", "1+ RBI", "1+ SB"),
      list(
        batting_order = fmt_int, expected_pa = function(x) fmt_dec(x, 2L),
        expected_hits = function(x) fmt_dec(x, 2L), expected_hr = function(x) fmt_dec(x, 2L),
        expected_runs = function(x) fmt_dec(x, 2L), expected_rbi = function(x) fmt_dec(x, 2L),
        expected_sb = function(x) fmt_dec(x, 2L), expected_sf = function(x) fmt_dec(x, 2L),
        expected_gdp = function(x) fmt_dec(x, 2L), prob_1plus_hit = fmt_rate,
        prob_1plus_hr = fmt_rate, prob_1plus_run = fmt_rate,
        prob_1plus_rbi = fmt_rate, prob_1plus_sb = fmt_rate
      )
    ),
    '</section><div class="method-callout"><strong>Chronological calibration gate:</strong> ', html_escape(calibration_message), ' Phase 5 starts a new version-specific ledger. Calibration requires 300 settled forecasts, fits only the earliest 70%, and must improve both win-probability Brier score and team-runs MAE on the later 30% before shadow application.</div>',
    '<div class="method-callout"><strong>Current Phase 5 boundary:</strong> the manager hook is a validated pooled league model, not yet a manager-specific model. Park baserunning effects are empirical and shrunk; the run environment still needs a multi-season park model and wind direction aligned to each field. Catcher identity, outfielder conversion skill, errors, pinch hitters, injuries, and transaction-aware bench roles remain future inputs.</div></section>'
  )
} else {
  state_simulation_section <- '<section class="projection-state-lab"><div class="method-callout"><strong>Phase 5 waiting:</strong> the detailed state engine requires two complete nine-man orders, active bullpens, current hitter-reliever probability matrices, current starter workload, weather, and the daily empirical baserunning products.</div></section>'
}
if (player_board_current) {
  player_probability_boards <- c(
    player_probability_leaderboard("batter_hr_1plus", "Home-run probability leaders", "Full-game hitter draws include posted batting-order opportunity, the probable starter, the active-roster bullpen quality layer, and the run environment."),
    player_probability_leaderboard("batter_hit_1plus", "Hit probability leaders", "Each hitter continues through the same batting order when the simulation moves from the starter phase to the bullpen."),
    player_probability_leaderboard("batter_xbh_1plus", "Extra-base-hit probability leaders", "Doubles, triples, and home runs are simulated as mutually exclusive plate-appearance outcomes."),
    player_probability_leaderboard("batter_tb_2plus", "Two-plus total-base leaders", "The total-base board preserves each simulated event mix instead of treating every hit as equivalent."),
    player_probability_leaderboard("pitcher_k_5plus", "Five-plus strikeout leaders", "Probable starters draw a game-specific batters-faced distribution against the posted opposing order."),
    player_probability_leaderboard("pitcher_k_7plus", "Seven-plus strikeout leaders", "The upper-tail board responds to starter workload, the opposing order, and both pitcher and hitter strikeout rates.")
  )
  player_projection_section <- paste0(
    '<section class="player-probability-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Player simulation board &middot; ', html_escape(fmt_int(player_simulation_model$simulations_per_lineup[[1L]])), ' draws per lineup</span><h2>Player outcomes now live inside the game simulation</h2><p>Confirmed orders are simulated through two continuous phases: the probable starter, then the opposing active-roster bullpen. The same draws produce full-game hitter lines and starter strikeout distributions.</p></div><div class="player-probability-grid">',
    paste0(player_probability_boards, collapse = ''),
    '</div><div class="method-callout"><strong>How to read it:</strong> each percentage is the share of simulated games in which the player reached that threshold. Posted batting-order opportunity, starter matchup, bullpen quality, park, and weather all move the distribution.</div></section>'
  )
} else {
  player_projection_section <- paste0(
    '<section class="player-probability-room"><div class="section-heading section-heading--tight"><span class="eyebrow">Player simulation board &middot; lineup gate</span><h2>Player probabilities wait for posted batting orders</h2><p>The team slate can update in the morning, but hitter and starter-event boards are withheld until current-date orders provide real batting-order opportunity. Yesterday&rsquo;s player board is never relabeled as today&rsquo;s.</p></div><div class="method-callout"><strong>Current state:</strong> ',
    html_escape(fmt_int(nrow(daily_batting_orders))), ' posted batting-order rows are available for ', html_escape(format(slate_date, "%B %d")), '.</div></section>'
  )
}
if (is_off_day) {
  write_fragment("daily-projections.html", c(
    paste0('<div class="projection-status-strip"><span><strong>MLB off-day</strong> No games are scheduled for ', html_escape(format(slate_report_date, "%B %d, %Y")), '.</span><span>The reporting and model-feedback engines still refreshed today.</span></div>'),
    '<section class="section-heading"><span class="eyebrow">Simulation center</span><h2>The board rests when the league rests</h2><p>No probabilities are published without a current scheduled slate. The archived forecast record remains available below.</p></section>',
    projection_public_audit(projection_feedback_metrics, player_feedback_metrics),
    '<div class="method-callout"><strong>Publication rule:</strong> the previous game board is retained in the data archive but never presented as today&rsquo;s forecast.</div>'
  ))
} else write_fragment("daily-projections.html", c(
  '<div class="projection-status-strip"><span><strong>Daily forecast board</strong> Today&rsquo;s slate, ordered by first pitch</span><span>Probable starters, posted orders, active rosters, park, first-pitch weather, recent workload, and bullpen availability</span></div>',
  live_input_board(daily_game_inputs),
  player_projection_section,
  '<section class="section-heading"><span class="eyebrow">Game projections</span><h2>The entire slate in one scan</h2><p>Each game card shows the simulated win split, score center, uncertainty, and current input status.</p></section>',
  '<div class="projection-slate-grid">', projection_cards, '</div>',
  projection_feature(feature_projection),
  projection_public_audit(projection_feedback_metrics, player_feedback_metrics),
  '<div class="method-callout"><strong>Forecast boundary:</strong> these are probabilistic research outputs, not betting advice. The public audit is updated from forecasts archived before first pitch.</div>'
))

standings_movers <- mlb_standings_movement[
  order(-abs(num(mlb_standings_movement$movement_score))),
  ,
  drop = FALSE
]
standings_mover_cards <- vapply(seq_len(min(6L, nrow(standings_movers))), function(index) {
  row <- standings_movers[index, , drop = FALSE]
  direction <- if (num(row$movement_score[[1L]]) > 0) "up" else if (num(row$movement_score[[1L]]) < 0) "down" else "steady"
  paste0(
    '<article class="standings-mover standings-mover--', direction, '"><header><span>',
    html_escape(row$division_label[[1L]]), '</span><strong>',
    html_escape(fmt_signed(row$movement_score[[1L]], 1L)), '</strong></header><h3>',
    html_escape(row$team[[1L]]), '</h3><p>', html_escape(fmt_int(row$wins[[1L]])),
    '&ndash;', html_escape(fmt_int(row$losses[[1L]])), ' &middot; ',
    html_escape(fmt_signed(row$run_differential[[1L]], 0L)), ' run differential</p><footer><span>',
    html_escape(fmt_signed(row$division_rank_change[[1L]], 0L)), ' division places</span><span>',
    html_escape(fmt_signed(row$games_back_change[[1L]], 1L)), ' games gained</span></footer></article>'
  )
}, character(1))
division_order <- c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")
division_tables <- vapply(division_order, function(division_name) {
  rows <- mlb_standings_movement[mlb_standings_movement$division_label == division_name, , drop = FALSE]
  rows <- rows[order(num(rows$division_rank)), , drop = FALSE]
  paste0(
    '<section class="standings-division"><header><span class="eyebrow">Division board</span><h2>',
    html_escape(division_name), '</h2></header>',
    render_table(
      rows,
      c("division_rank", "team", "wins", "losses", "winning_percentage", "games_back", "run_differential", "division_rank_change", "games_back_change", "streak_code"),
      c("#", "Club", "W", "L", "Pct.", "GB", "Run diff.", "Rank move", "Games gained", "Streak"),
      list(
        division_rank = fmt_int, wins = fmt_int, losses = fmt_int,
        winning_percentage = fmt_dec, games_back = function(x) fmt_dec(x, 1L),
        run_differential = function(x) fmt_signed(x, 0L),
        division_rank_change = function(x) fmt_signed(x, 0L),
        games_back_change = function(x) fmt_signed(x, 1L)
      ),
      "data-table data-table--compact standings-table"
    ),
    '</section>'
  )
}, character(1))
write_fragment("standings-desk.html", c(
  '<section class="standings-mast"><div><span class="eyebrow">Standings movement desk</span><h2>The table tells you where they are. Movement tells you how they got there.</h2><p>Every division is paired with a seven-day change layer built from rank, games back, winning percentage, and run differential.</p></div><div class="standings-mast__key"><span><i class="is-up"></i>Gaining ground</span><span><i class="is-down"></i>Losing ground</span><small>Through ',
  html_escape(format(max(as.Date(mlb_standings$source_through)), "%B %d, %Y")), '</small></div></section>',
  '<section class="section-heading"><span class="eyebrow">Seven-day pressure board</span><h2>The largest standings moves</h2><p>The score is descriptive, and a positive result always means movement toward first place.</p></section>',
  '<div class="standings-mover-grid">', standings_mover_cards, '</div>',
  '<div class="standings-division-grid">', division_tables, '</div>',
  '<div class="method-callout"><strong>Movement method:</strong> division-rank gains and games gained receive the clearest weight, with changes in winning percentage and run differential supplying context. Ties and MLB tiebreaker rules are displayed as provided by the standings source.</div>'
))

aaa_callup_spotlights <- utils::head(aaa_callups[order(num(aaa_callups$callup_rank)), , drop = FALSE], 8L)
aaa_callup_cards <- vapply(seq_len(nrow(aaa_callup_spotlights)), function(index) {
  row <- aaa_callup_spotlights[index, , drop = FALSE]
  paste0(
    '<article class="callup-card"><header><span class="eyebrow">Call-Up Radar #',
    html_escape(fmt_int(row$callup_rank[[1L]])), '</span><strong>',
    html_escape(fmt_score(row$callup_score[[1L]])), '</strong></header><h3>',
    html_escape(row$player_name[[1L]]), '</h3><p>', html_escape(row$team[[1L]]),
    ' &rarr; ', html_escape(row$mlb_team[[1L]]), ' &middot; age ', html_escape(fmt_int(row$age[[1L]])),
    '</p><div class="callup-card__components">',
    percentile_ruler("Triple-A production", row$performance_score[[1L]], "35% of score"),
    percentile_ruler("Age at level", row$age_score[[1L]], "30% of score"),
    percentile_ruler("Triple-A experience", row$experience_score[[1L]], "15% of score"),
    percentile_ruler("Parent-club need", row$mlb_need_percentile[[1L]], paste(row$mlb_need_label[[1L]], "\u00b7 20% of score")),
    '</div><footer>', html_escape(row$callup_reason[[1L]]), '</footer></article>'
  )
}, character(1))
aaa_value_columns <- c(
  "player_name", "team", "sabrhood_war", "estimated_running_runs",
  "estimated_fielding_runs", "estimated_catching_runs"
)
aaa_total_value <- rbind(
  aaa_hitters[, aaa_value_columns, drop = FALSE],
  aaa_pitchers[, aaa_value_columns, drop = FALSE]
)
aaa_leaderboards <- c(
  leaderboard_card(aaa_total_value, "sWAR prototype", "sabrhood_war", function(x) fmt_dec(x, 1L), "Batting, running, fielding, catching, and pitching runs divided by 10", FALSE, 5L, "Triple-A total value"),
  leaderboard_card(aaa_total_value, "Running-game value", "estimated_running_runs", function(x) fmt_signed(x, 1L), "Runner value for hitters; hold and pickoff value for pitchers", FALSE, 5L, "Triple-A running game"),
  leaderboard_card(aaa_total_value, "Fielding value", "estimated_fielding_runs", function(x) fmt_signed(x, 1L), "Position-adjusted fielding percentage and chances, reliability-shrunk", FALSE, 5L, "Triple-A fielding"),
  leaderboard_card(aaa_hitters[aaa_hitters$position == "C", , drop = FALSE], "Catching value", "estimated_catching_runs", function(x) fmt_signed(x, 1L), "Caught-stealing value, pickoffs, passed balls, and interference", FALSE, 5L, "Triple-A catching"),
  leaderboard_card(aaa_hitters, "OPS", "ops", fmt_dec, "Overall on-base plus slugging", FALSE, 5L, "Triple-A hitters"),
  leaderboard_card(aaa_hitters, "Home runs", "home_runs", fmt_int, "Qualified power totals", FALSE, 5L, "Triple-A hitters"),
  leaderboard_card(aaa_hitters, "Walk rate", "walk_rate", fmt_rate, "Patience and zone control", FALSE, 5L, "Triple-A hitters"),
  leaderboard_card(aaa_hitters, "Lowest strikeout rate", "strikeout_rate", fmt_rate, "Qualified contact discipline", TRUE, 5L, "Triple-A hitters"),
  leaderboard_card(aaa_pitchers, "Lowest ERA", "era", function(x) fmt_dec(x, 2L), "Qualified pitching results", TRUE, 5L, "Triple-A pitchers"),
  leaderboard_card(aaa_pitchers, "K-BB rate", "k_minus_bb_rate", fmt_rate, "Bat missing minus free passes", FALSE, 5L, "Triple-A pitchers"),
  leaderboard_card(aaa_pitchers, "Strikeout rate", "strikeout_rate", fmt_rate, "Strikeouts per estimated batter faced", FALSE, 5L, "Triple-A pitchers"),
  leaderboard_card(aaa_pitchers, "Lowest WHIP", "whip", function(x) fmt_dec(x, 2L), "Traffic allowed per inning", TRUE, 5L, "Triple-A pitchers")
)
aaa_team_leaders <- aaa_team_rankings[order(num(aaa_team_rankings$team_rank)), , drop = FALSE]
aaa_team_leader_cards <- vapply(seq_len(min(6L, nrow(aaa_team_leaders))), function(index) {
  row <- aaa_team_leaders[index, , drop = FALSE]
  paste0(
    '<article class="aaa-team-card"><header><span>#', html_escape(fmt_int(row$team_rank[[1L]])),
    ' Triple-A</span><strong>', html_escape(fmt_score(row$team_strength_score[[1L]])),
    '</strong></header><h3>', html_escape(row$full_team[[1L]]), '</h3><p>',
    html_escape(fmt_int(row$wins[[1L]])), '&ndash;', html_escape(fmt_int(row$losses[[1L]])),
    ' &middot; ', html_escape(fmt_signed(row$run_differential[[1L]], 0L)),
    ' run differential</p><div><span><small>Hitter pool</small><strong>',
    html_escape(fmt_score(row$hitter_talent_score[[1L]])), '</strong></span><span><small>Pitcher pool</small><strong>',
    html_escape(fmt_score(row$pitcher_talent_score[[1L]])), '</strong></span><span><small>7-day state</small><strong>',
    html_escape(tools::toTitleCase(row$movement_label[[1L]])), '</strong></span></div></article>'
  )
}, character(1))
aaa_league_tables <- vapply(c("International League", "Pacific Coast League"), function(league_name) {
  rows <- aaa_team_rankings[aaa_team_rankings$league_label == league_name, , drop = FALSE]
  rows <- rows[order(-num(rows$winning_percentage), -num(rows$run_differential)), , drop = FALSE]
  paste0(
    '<section class="aaa-standings-board"><header><span class="eyebrow">Triple-A standings</span><h2>',
    html_escape(league_name), '</h2></header>',
    render_table(
      rows,
      c("full_team", "wins", "losses", "winning_percentage", "run_differential", "team_rank", "movement_label"),
      c("Club", "W", "L", "Pct.", "Run diff.", "Power rank", "7-day state"),
      list(
        wins = fmt_int, losses = fmt_int, winning_percentage = fmt_dec,
        run_differential = function(x) fmt_signed(x, 0L), team_rank = fmt_int
      ),
      "data-table data-table--compact standings-table"
    ),
    '</section>'
  )
}, character(1))
write_fragment("aaa-watch.html", c(
  '<section class="aaa-scoreboard"><div><span class="eyebrow">Triple-A performance watch</span><h2>A narrow minor-league desk with an MLB purpose</h2><p>The leaderboard identifies production. The Call-Up Radar asks the next question: does age, Triple-A experience, and a parent club&rsquo;s weakness create a plausible opening?</p></div><div class="aaa-scoreboard__stats"><span><strong>', html_escape(fmt_int(nrow(aaa_hitters))), '</strong><small>qualified hitters</small></span><span><strong>', html_escape(fmt_int(nrow(aaa_pitchers))), '</strong><small>qualified pitchers</small></span><span><strong>28</strong><small>public radar age cap</small></span></div></section>',
  '<section class="section-heading"><span class="eyebrow">Prospect-pool homepage</span><h2>The quickest read on who is separating</h2><p>Familiar production, plate discipline, bat-missing ability, age context, and estimated total-run value are kept visible so one composite never hides the reason a player surfaced.</p></section>',
  '<div class="leaderboard-grid aaa-leaderboard-grid">', aaa_leaderboards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Organizational power board</span><h2>Which Triple-A clubs combine winning and promotable talent?</h2><p>The ranking weights record 35%, run differential 20%, the top five qualified hitter performances 20%, the top five pitcher performances 20%, and the young/readiness pipeline 5%.</p></section>',
  '<div class="aaa-team-grid">', aaa_team_leader_cards, '</div>',
  '<div class="aaa-standings-grid">', aaa_league_tables, '</div>',
  '<section class="section-heading"><span class="eyebrow">Call-Up Radar</span><h2>Who has performance, timing, and an organizational opening?</h2><p>The model weights Triple-A production 35%, age 30%, experience 15%, and parent-club positional need 20%. A strong score is a reporting lead, not a transaction probability.</p></section>',
  '<div class="callup-grid">', aaa_callup_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Complete readiness board</span><h2>Every component remains visible</h2><p>Players older than 28 are excluded from the public radar. Pitchers are mapped to starting or relief need from their Triple-A start share; hitters are mapped by listed position.</p></div>',
  render_table(utils::head(aaa_callups, 25L), c("callup_rank", "player_name", "role", "age", "team", "mlb_team", "need_position", "performance_score", "age_score", "experience_score", "mlb_need_percentile", "callup_score"),
    c("Rank", "Player", "Role", "Age", "Triple-A club", "Parent club", "Need", "Production", "Age", "Experience", "MLB need", "Radar"),
    list(callup_rank = fmt_int, age = fmt_int, performance_score = fmt_score, age_score = fmt_score, experience_score = fmt_score, mlb_need_percentile = fmt_score, callup_score = fmt_score)),
  '</section>',
  '<div class="method-callout"><strong>sWAR boundary:</strong> Triple-A tracking does not provide MLB-quality Statcast defense. Fielding runs are estimated from position-specific fielding percentage and chances; catching uses caught-stealing, passed-ball, pickoff, and interference events; pitcher running value uses steals allowed, caught stealing, pickoffs, balks, and wild pitches. All components share a runs scale and use 10 runs per win, but this remains a transparent prototype.</div>',
  '<div class="method-callout"><strong>Call-Up Radar boundary:</strong> the radar is not yet trained on historical promotions. Options, 40-man status, injuries, service-time strategy, scouting grades, defensive fit, and multiple seasons of minor-league development will be added before it can become a calibrated predictor.</div>'
))

insane_award_ids <- unique(as.character(insane_awards$award_id))
insane_award_summary <- do.call(rbind, lapply(insane_award_ids, function(award_id) {
  insane_awards[insane_awards$award_id == award_id, , drop = FALSE][1L, , drop = FALSE]
}))
insane_award_summary <- insane_award_summary[order(-num(insane_award_summary$showcase_score)), , drop = FALSE]
insane_cards <- vapply(seq_len(nrow(insane_award_summary)), function(index) {
  summary_row <- insane_award_summary[index, , drop = FALSE]
  rows <- insane_awards[insane_awards$award_id == summary_row$award_id[[1L]], , drop = FALSE]
  footer <- paste0(summary_row$formula[[1L]], " | Leader separation ", fmt_dec(summary_row$leader_separation_z[[1L]], 2L),
    " separation index | Race closeness ", fmt_score(summary_row$race_closeness[[1L]]), "/100 | ", summary_row$eligibility[[1L]])
  ranked_board_card(rows, summary_row$award_name[[1L]], summary_row$description[[1L]], "display_value", identity, 5L, footer,
    if (as.logical(summary_row$featured[[1L]])) "insane-award-card is-featured" else "insane-award-card")
}, character(1))
write_fragment("insane-awards.html", c(
  '<section class="insane-awards-masthead"><div><span class="eyebrow">The SABRhood originals</span><h2>Baseball deserves awards for the wonderfully specific</h2><p>Every candidate board begins with an explainable formula or an unusual game context. The rotating showcase favors races with a leader far from the field, a tight fight at the top, or both.</p></div><div><strong>',
  html_escape(fmt_int(nrow(insane_award_summary))), '</strong><span>candidate awards</span><strong>',
  html_escape(fmt_int(sum(as.logical(insane_award_summary$featured)))), '</strong><span>featured this cycle</span></div></section>',
  '<div class="leaderboard-grid insane-awards-grid">', insane_cards, '</div>',
  '<div class="graphics-feed-method"><strong>Rotation rule</strong><span>Showcase score is 55% leader separation from the eligible field and 45% closeness between first and second. That lets a dominant oddity and a genuinely dramatic race both earn the front page.</span></div>'
))

league_trend_graphics <- graphics_manifest[graphics_manifest$page_group == "League trends", , drop = FALSE]
league_trend_graphics <- league_trend_graphics[
  league_trend_graphics$graphic_id != "league-starter-bullpen-workload",
  ,
  drop = FALSE
]
league_trend_cards <- if (nrow(league_trend_graphics)) vapply(seq_len(nrow(league_trend_graphics)), function(index) {
  graphics_feed_card(league_trend_graphics[index, , drop = FALSE])
}, character(1)) else character()
latest_pitch_date <- max(as.Date(rolling_pitch_usage$date), na.rm = TRUE)
comparison_date <- max(as.Date(rolling_pitch_usage$date)[as.Date(rolling_pitch_usage$date) <= latest_pitch_date - 28], na.rm = TRUE)
latest_pitch <- rolling_pitch_usage[as.Date(rolling_pitch_usage$date) == latest_pitch_date, , drop = FALSE]
prior_pitch <- rolling_pitch_usage[as.Date(rolling_pitch_usage$date) == comparison_date, c("pitch_type", "usage_rate_rolling"), drop = FALSE]
names(prior_pitch)[[2L]] <- "prior_usage"
pitch_movement <- merge(latest_pitch, prior_pitch, by = "pitch_type", all.x = TRUE)
pitch_movement$usage_change <- num(pitch_movement$usage_rate_rolling) - num(pitch_movement$prior_usage)
pitch_movement <- pitch_movement[order(-abs(pitch_movement$usage_change)), , drop = FALSE]
pitch_movement <- utils::head(pitch_movement, 5L)
pitch_movement_rows <- vapply(seq_len(nrow(pitch_movement)), function(index) {
  paste0('<li><span>', html_escape(pitch_movement$pitch_name[[index]]), '</span><strong>',
    html_escape(paste0(ifelse(pitch_movement$usage_change[[index]] > 0, "+", ""), fmt_dec(100 * pitch_movement$usage_change[[index]], 1L), " pp")),
    '</strong><small>versus 28 days earlier</small></li>')
}, character(1))
write_fragment("league-trends.html", c(
  '<section class="league-trend-summary"><div><span class="eyebrow">League movement desk</span><h2>Stay in touch with the top trends!</h2><p>Sway along with the ebbs and flows of the season.</p></div><ol>',
  pitch_movement_rows, '</ol></section>',
  '<div class="graphics-feed-grid league-trend-graphics">', league_trend_cards, '</div>',
  '<div class="graphics-feed-method"><strong>Trend boundary</strong><span>These are descriptive league rates through ', html_escape(format(latest_pitch_date, "%B %d, %Y")), '. Schedule mix, weather, park distribution, and rule changes can move the league line; the graphic identifies the question before the reporting explains it.</span></div>'
))

graphics_manifest <- graphics_manifest[order(num(graphics_manifest$display_order)), , drop = FALSE]
graphics_cards <- vapply(seq_len(nrow(graphics_manifest)), function(index) {
  graphics_feed_card(graphics_manifest[index, , drop = FALSE])
}, character(1))
graphics_groups <- unique(graphics_manifest$page_group)
graphics_group_sections <- vapply(graphics_groups, function(group_name) {
  rows <- graphics_manifest[graphics_manifest$page_group == group_name, , drop = FALSE]
  cards <- vapply(seq_len(nrow(rows)), function(index) graphics_feed_card(rows[index, , drop = FALSE]), character(1))
  paste0(
    '<section class="graphics-feed-group"><div class="section-heading section-heading--tight"><span class="eyebrow">Downloadable analysis</span><h2>',
    html_escape(group_name), '</h2></div><div class="graphics-feed-grid">', paste0(cards, collapse = ""), '</div></section>'
  )
}, character(1))
write_fragment("graphics-feed.html", c(
  '<section class="graphics-feed-hero"><div><span class="eyebrow">The SABRhood visual desk</span><h2>The league&rsquo;s biggest changes, turned into pictures</h2><p>The feed carries award races, defensive alignments, player movement, pitch changes, and team separation into downloadable visual formats for social, video, articles, and broadcast.</p></div><div class="graphics-feed-hero__stats"><span><strong>',
  html_escape(fmt_int(nrow(graphics_manifest))), '</strong><small>rendered graphics</small></span><span><strong>',
  html_escape(fmt_int(length(graphics_groups))), '</strong><small>organized collections</small></span><span><strong>PNG</strong><small>current download format</small></span></div></section>',
  '<nav class="graphics-feed-jump" aria-label="Graphics feed collections">',
  paste0('<a href="#', slugify(graphics_groups), '">', html_escape(graphics_groups), '</a>', collapse = ""), '</nav>',
  vapply(seq_along(graphics_groups), function(index) {
    sub('<section class="graphics-feed-group">', paste0('<section id="', slugify(graphics_groups[[index]]), '" class="graphics-feed-group">'), graphics_group_sections[[index]], fixed = TRUE)
  }, character(1)),
  '<div class="graphics-feed-method"><strong>How a graphic earns a place here</strong><span>Recent change is ranked by standardized movement against the MLB change distribution. Season separation is shown with league percentiles. Team graphics compare clubs on shared scales. Every visual keeps its sample and interpretation boundary attached.</span></div>'
))

newsletter_graphics <- utils::head(graphics_manifest[graphics_manifest$featured, , drop = FALSE], 3L)
newsletter_graphic_cards <- vapply(seq_len(nrow(newsletter_graphics)), function(index) {
  graphics_feed_card(newsletter_graphics[index, , drop = FALSE], compact = TRUE)
}, character(1))
write_fragment("newsletter-graphics.html", c(
  '<section class="newsletter-visual-edition"><div class="section-heading section-heading--tight"><span class="eyebrow">Visual edition</span><h2>Three graphics to carry into the conversation</h2><p>The newsletter selects a few explanatory graphics rather than repeating the complete daily slate. Each one is downloadable for reporting, social publishing, or broadcast prep.</p></div>',
  '<div class="graphics-feed-grid graphics-feed-grid--newsletter">', paste0(newsletter_graphic_cards, collapse = ""), '</div>',
  '<div class="section-action"><a class="btn btn-sabr-navy" href="graphics-feed.html">Browse the full graphics feed</a></div></section>'
))

qualified_hitters <- fangraphs_hitters[num(fangraphs_hitters$pa) >= 250, , drop = FALSE]
qualified_pitchers <- fangraphs_pitchers[num(fangraphs_pitchers$innings_outs) >= 240, , drop = FALSE]
qualified_contact_tracking <- hitter_tracking[num(hitter_tracking$tracked_batted_balls) >= 100, , drop = FALSE]
qualified_pitch_tracking <- pitcher_tracking[num(pitcher_tracking$tracked_pitches) >= 500, , drop = FALSE]
qualified_contact_allowed <- pitcher_tracking[num(pitcher_tracking$tracked_batted_balls_allowed) >= 100, , drop = FALSE]
qualified_pitch_shapes <- pitch_types[num(pitch_types$pitches) >= 250, , drop = FALSE]
qualified_pitch_shapes$player_name <- paste(qualified_pitch_shapes$player_name, qualified_pitch_shapes$pitch_name, sep = " | ")
team_tracking_board <- team_tracking
team_tracking_board$player_name <- team_tracking_board$team
team_tracking_board$team <- "MLB club total"
tracking_hitter_boards <- c(
  leaderboard_card(hitter_tracking, "100+ mph batted balls", "batted_balls_100_plus", fmt_int, "Season total from tracked contact", FALSE, 5L, "Contact tracking"),
  leaderboard_card(qualified_contact_tracking, "100+ mph contact rate", "batted_balls_100_plus_rate", fmt_rate, "Minimum 100 tracked batted balls", FALSE, 5L, "Contact tracking"),
  leaderboard_card(qualified_contact_tracking, "Maximum exit velocity", "max_exit_velocity", function(x) paste0(fmt_dec(x, 1L), " mph"), "Hardest tracked batted ball", FALSE, 5L, "Contact tracking"),
  leaderboard_card(qualified_contact_tracking, "Average exit velocity", "average_exit_velocity", function(x) paste0(fmt_dec(x, 1L), " mph"), "Minimum 100 tracked batted balls", FALSE, 5L, "Contact tracking")
)
tracking_pitcher_boards <- c(
  leaderboard_card(pitcher_tracking, "Pitches at 100+ mph", "pitches_100_plus", fmt_int, "Season total across every pitch type", FALSE, 5L, "Pitch tracking"),
  leaderboard_card(qualified_pitch_tracking, "100+ mph pitch rate", "pitches_100_plus_rate", fmt_rate, "Minimum 500 tracked pitches", FALSE, 5L, "Pitch tracking"),
  leaderboard_card(pitcher_tracking, "Maximum pitch velocity", "max_velocity", function(x) paste0(fmt_dec(x, 1L), " mph"), "Hardest tracked pitch", FALSE, 5L, "Pitch tracking"),
  leaderboard_card(qualified_contact_allowed, "Lowest 100+ contact rate allowed", "batted_balls_100_plus_allowed_rate", fmt_rate, "Minimum 100 tracked batted balls allowed", TRUE, 5L, "Contact allowed"),
  leaderboard_card(qualified_pitch_shapes, "Pitch-type velocity", "average_velocity", function(x) paste0(fmt_dec(x, 1L), " mph"), "Pitch-type line, minimum 250 pitches", FALSE, 5L, "Pitch shape"),
  leaderboard_card(qualified_pitch_shapes, "Pitch-type whiff rate", "whiff_rate", fmt_rate, "Minimum 250 pitches", FALSE, 5L, "Pitch shape")
)
tracking_team_boards <- c(
  leaderboard_card(team_tracking_board, "Team 100+ mph contact", "batted_balls_100_plus", fmt_int, "Batted-ball total", FALSE, 5L, "Club tracking"),
  leaderboard_card(team_tracking_board, "Team 100+ mph contact rate", "batted_balls_100_plus_rate", fmt_rate, "Share of tracked contact", FALSE, 5L, "Club tracking"),
  leaderboard_card(team_tracking_board, "Team 100+ mph pitches", "pitches_100_plus", fmt_int, "Pitch total", FALSE, 5L, "Club tracking"),
  leaderboard_card(team_tracking_board, "Lowest 100+ contact allowed", "batted_balls_100_plus_allowed_rate", fmt_rate, "Share of tracked contact allowed", TRUE, 5L, "Club tracking")
)
hitter_boards <- c(
  leaderboard_card(qualified_hitters, "Batting average", "avg", fmt_dec, "Minimum 250 PA"),
  leaderboard_card(qualified_hitters, "On-base percentage", "obp", fmt_dec, "Minimum 250 PA"),
  leaderboard_card(qualified_hitters, "Slugging percentage", "slg", fmt_dec, "Minimum 250 PA"),
  leaderboard_card(fangraphs_hitters, "Home runs", "home_runs", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Runs batted in", "rbi", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Runs scored", "runs", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Hits", "hits", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Doubles", "doubles", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Triples", "triples", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Stolen bases", "stolen_bases", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Walks", "walks", fmt_int, "Season total"),
  leaderboard_card(fangraphs_hitters, "Strikeouts", "strikeouts", fmt_int, "Season total")
)
pitcher_boards <- c(
  leaderboard_card(qualified_pitchers, "Earned run average", "era", function(x) fmt_dec(x, 2L), "Minimum 80 IP", TRUE),
  leaderboard_card(qualified_pitchers, "WHIP", "whip", function(x) fmt_dec(x, 2L), "Minimum 80 IP", TRUE),
  leaderboard_card(fangraphs_pitchers, "Innings pitched", "innings_display", function(x) fmt_dec(x, 1L), "Season total"),
  leaderboard_card(fangraphs_pitchers, "Strikeouts", "strikeouts", fmt_int, "Season total"),
  leaderboard_card(fangraphs_pitchers, "Wins", "wins", fmt_int, "Season total"),
  leaderboard_card(fangraphs_pitchers, "Saves", "saves", fmt_int, "Season total"),
  leaderboard_card(qualified_pitchers, "K minus BB rate", "k_minus_bb_rate", fmt_rate, "Minimum 80 IP"),
  leaderboard_card(fangraphs_pitchers, "Pitching WAR", "war", function(x) fmt_dec(x, 1L), "Season value")
)
qualified_running_leaders <- run_game_runners[
  num(run_game_runners$attempts) >= 5 & is.finite(num(run_game_runners$stealing_runs)),
  ,
  drop = FALSE
]
qualified_pitcher_hold_leaders <- run_game_pitchers[
  num(run_game_pitchers$eligible_pitches) >= 100 & is.finite(num(run_game_pitchers$stealing_runs_saved)),
  ,
  drop = FALSE
]
qualified_catcher_throwing_leaders <- run_game_catchers[
  num(run_game_catchers$eligible_pitches) >= 200 & is.finite(num(run_game_catchers$stealing_runs_saved)),
  ,
  drop = FALSE
]
qualified_framing_leaders <- catcher_framing[
  num(catcher_framing$called_pitches) >= 1000,
  ,
  drop = FALSE
]
qualified_framing_leaders$player_name <- qualified_framing_leaders$catcher_name
qualified_defense_leaders <- official_fielding[
  num(official_fielding$innings) >= 150,
  ,
  drop = FALSE
]
qualified_advancement_leaders <- advancement_fielders[
  num(advancement_fielders$opportunities) >= 50,
  ,
  drop = FALSE
]
run_game_and_defense_boards <- c(
  leaderboard_card(qualified_running_leaders, "Baserunning runs", "stealing_runs", function(x) fmt_signed(x, 2L), "Minimum five modeled attempts", FALSE, 5L, "Running game"),
  leaderboard_card(qualified_pitcher_hold_leaders, "Pitcher running-game control", "stealing_runs_saved", function(x) fmt_signed(x, 2L), "Estimated runs saved through attempts and success allowed", FALSE, 5L, "Pitcher hold"),
  leaderboard_card(qualified_catcher_throwing_leaders, "Catcher throwing value", "stealing_runs_saved", function(x) fmt_signed(x, 2L), "Pitcher- and runner-adjusted run-game estimate", FALSE, 5L, "Catching"),
  leaderboard_card(qualified_framing_leaders, "Catcher framing runs", "framing_runs_estimate", function(x) fmt_signed(x, 1L), "Location- and count-adjusted estimate", FALSE, 5L, "Catching"),
  leaderboard_card(qualified_defense_leaders, "Official Fielding Run Value", "fielding_runs", function(x) fmt_signed(x, 1L), "Minimum 150 innings", FALSE, 5L, "Fielding"),
  leaderboard_card(qualified_advancement_leaders, "Advancement runs saved", "advancement_runs_saved", function(x) fmt_signed(x, 2L), "Minimum 50 advancement opportunities", FALSE, 5L, "Fielding")
)
write_fragment("league-leaderboards.html", c(
  '<div class="leaderboard-note"><strong>Tracked events now sit beside baseball&rsquo;s familiar totals.</strong><span>A 100+ mph count measures volume; the paired rate measures how often the event occurs when tracking is available. MLBAM release speed and launch speed are used directly.</span></div>',
  '<section class="section-heading"><span class="eyebrow">Contact tracking</span><h2>The loudest contact in the league</h2><p>Counts reward repeated impact. Rate and average boards require at least 100 tracked batted balls.</p></section>',
  '<div class="leaderboard-grid leaderboard-grid--tracking">', tracking_hitter_boards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Pitch tracking</span><h2>Triple digits, pitch shape, and contact allowed</h2><p>100+ mph pitch totals combine every pitch type. Pitch-shape boards retain their own minimum samples.</p></section>',
  '<div class="leaderboard-grid leaderboard-grid--tracking">', tracking_pitcher_boards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Team tracking</span><h2>Which clubs create and withstand the most force?</h2><p>The same measures are aggregated to the team level so club pages and the league board tell the same story.</p></section>',
  '<div class="leaderboard-grid leaderboard-grid--tracking">', tracking_team_boards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Runs beyond the batter&rsquo;s box</span><h2>Baserunning, catcher value, and fielding leaders</h2><p>Official Fielding Run Value sits beside clearly labeled SABRhood estimates for running, pitcher hold, catcher throwing, framing, and runner-advancement prevention.</p></section>',
  '<div class="leaderboard-grid">', run_game_and_defense_boards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Hitting</span><h2>Traditional league leaderboards</h2><p>Rate boards require 250 PA; counting-stat boards rank season totals.</p></section>',
  '<div class="leaderboard-grid">', hitter_boards, '</div>',
  '<section class="section-heading"><span class="eyebrow">Pitching</span><h2>Workload, results, and whiffs</h2><p>Traditional pitching lines paired with some of the new.</p></section>',
  '<div class="leaderboard-grid">', pitcher_boards, '</div>'
))

if (is_off_day) {
  write_fragment("home-projections.html", c(
    '<section class="section-heading"><span class="eyebrow">Simulation center preview</span><h2>No MLB games are scheduled today</h2><p>The model board pauses on league off-days; daily intelligence and the feedback ledger continue to update.</p></section>',
    '<div class="section-action"><a class="btn btn-metallic" href="projections.html">Explore the simulation center</a></div>'
  ))
} else write_fragment("home-projections.html", c(
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
  '<div class="method-callout"><strong>Roster gate connected:</strong> candidates now must appear on the BaseballR active roster and survive the pregame workload screen. The selector remains a research lead, not a live recommendation.</div>',
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
    paste("Recent riser:", team_intelligence$recent_riser[[index]]),
    paste("Run generation #", fmt_int(team_intelligence$offense_rank[[index]]), "| pitching #", fmt_int(team_intelligence$pitching_rank[[index]])),
    paste(fmt_int(team_intelligence$surging_signals[[index]]), "surging | bullpen", team_intelligence$bullpen_health[[index]]),
    fmt_score(team_intelligence$team_index[[index]])
  )
}, character(1))
write_fragment("team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Team reports</span><h2>Who has the most complete analytical profile?</h2><p>The core index balances a broad Run Generation Score and Pitching Score. Recent player form and bullpen condition travel beside it as separate, visible context.</p></section>',
  '<div class="signal-grid">', team_pulse_cards, '</div>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Full league board</span><h2>Thirty-team intelligence index</h2><p>This is a descriptive reporting index, not a standings projection. Component ranks make each placement auditable.</p></div>',
  render_table(team_intelligence, c("team_index_rank", "team", "team_index", "run_generation_score", "pitching_score", "offense_rank", "pitching_rank", "form_rank", "bullpen_rank", "bullpen_health", "recent_riser"),
    c("Rank", "Team", "Index", "Run gen.", "Pitching", "Run rank", "Pitch rank", "Recent play", "Bullpen", "Pen state", "Recent riser"),
    list(team_index_rank = fmt_int, team_index = fmt_score, run_generation_score = fmt_score, pitching_score = fmt_score, offense_rank = fmt_int, pitching_rank = fmt_int, form_rank = fmt_int, bullpen_rank = fmt_int)),
  '</section>',
  '<div class="method-callout"><strong>How to read it:</strong> Run Generation combines estimated wOBA, OPS, walk rate, strikeout avoidance, and hard-hit rate. Pitching Score combines opponent wOBA, opponent OPS, strikeouts, walk avoidance, and hard-hit rate allowed. The team index weights those two broad scores equally; form and bullpen state remain separate context.</div>'
))
write_fragment("home-team-pulse.html", c(
  '<section class="section-heading"><span class="eyebrow">Around the league</span><h2>Complete team reports</h2><p>Taking you to every clubhouse around the league.</p></section>',
  '<section class="dashboard-block home-pulse-board">',
  render_table(utils::head(team_intelligence, 8L), c("team_index_rank", "team", "team_index", "run_generation_score", "pitching_score", "surging_signals", "bullpen_health", "recent_riser"),
    c("Rank", "Team", "Index", "Run gen.", "Pitching", "Risers", "Pen", "Recent riser"),
    list(team_index_rank = fmt_int, team_index = fmt_score, run_generation_score = fmt_score, pitching_score = fmt_score, surging_signals = fmt_int)),
  '</section>',
  '<div class="section-action"><a class="btn btn-metallic" href="teams.html">Open all 30 team reports</a></div>'
))

team_source_dir <- file.path(site_root, "team-reports")
team_include_dir <- file.path(include_dir, "team-reports")
dir.create(team_source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(team_include_dir, recursive = TRUE, showWarnings = FALSE)

team_report_links <- vapply(seq_len(nrow(team_intelligence)), function(index) {
  team <- team_intelligence$team[[index]]
  slug <- slugify(team)
  paste0(
    '<a class="team-report-link" href="team-reports/', html_escape(slug), '.html">',
    '<span class="team-report-link__rank">#', html_escape(fmt_int(team_intelligence$team_index_rank[[index]])), '</span>',
    '<span><strong>', html_escape(team), '</strong><small>Index ', html_escape(fmt_score(team_intelligence$team_index[[index]])),
    ' | run generation #', html_escape(fmt_int(team_intelligence$offense_rank[[index]])),
    ' | pitching #', html_escape(fmt_int(team_intelligence$pitching_rank[[index]])), '</small></span>',
    '<span aria-hidden="true">&rarr;</span></a>'
  )
}, character(1))
team_report_through <- if ("source_through" %in% names(team_intelligence)) {
  max(as.Date(team_intelligence$source_through), na.rm = TRUE)
} else {
  NA
}
write_fragment("team-report-index.html", c(
  '<section class="section-heading"><span class="eyebrow">Thirty club research rooms</span><h2>Choose a team report</h2><p>Every page uses the same auditable framework so strengths, weaknesses, player movement, pitches, and bullpen decisions can be compared consistently.</p></section>',
  '<figure class="feature-graphic feature-graphic--wide"><img src="images/graphics-feed/mlb-positional-war-map.png" alt="All MLB teams ranked by FanGraphs WAR at catcher, infield, outfield, designated hitter, starting pitcher, and relief pitcher"><figcaption><strong>The league&rsquo;s roster-value map.</strong> Each cell shows a club&rsquo;s MLB rank at that position. Open a report to see the WAR total and the player creating the most value.</figcaption></figure>',
  '<div class="team-report-directory">', team_report_links, '</div>',
  '<div class="method-callout"><strong>Report status:</strong> automated research foundation through ',
  html_escape(ifelse(is.finite(team_report_through), format(team_report_through, "%B %d, %Y"), "the current source snapshot")),
  '. Editorial notes, injury confirmation, same-day transactions, and opponent-specific context remain the final broadcast layer.</div>'
))

bullpen_decision_cards <- function(rows) {
  if (!nrow(rows)) return('<p>No eligible active-roster bullpen candidates.</p>')
  cards <- vapply(c("L", "R"), function(side) {
    candidates <- rows[rows$upcoming_batter_side == side, , drop = FALSE]
    candidates <- candidates[order(num(candidates$selection_rank), -num(candidates$selection_score)), , drop = FALSE]
    if (!nrow(candidates)) return("")
    pick <- candidates[1L, , drop = FALSE]
    side_label <- if (side == "L") "Left-handed batter" else "Right-handed batter"
    paste0(
      '<article class="bullpen-decision-card"><header><span>', side_label,
      '</span><strong>', html_escape(fmt_score(pick$selection_score[[1L]])), '</strong></header>',
      '<h3>', html_escape(pick$pitcher_name[[1L]]), '</h3>',
      '<p>', html_escape(pick$throws[[1L]]), 'HP &middot; ',
      html_escape(pick$availability_status[[1L]]), ' &middot; ',
      html_escape(fmt_int(pick$days_rest[[1L]])), ' days rest</p>',
      '<div class="bullpen-decision-metrics">',
      '<span><small>Availability</small><strong>', html_escape(fmt_rate(pick$availability_score[[1L]])), '</strong></span>',
      '<span><small>Matchup</small><strong>', html_escape(fmt_rate(pick$matchup_score[[1L]])), '</strong></span>',
      '<span><small>Performance</small><strong>', html_escape(fmt_rate(pick$performance_score_used[[1L]])), '</strong></span>',
      '</div></article>'
    )
  }, character(1))
  paste0('<div class="bullpen-decision-grid">', paste0(cards[nzchar(cards)], collapse = ""), '</div>')
}

team_names <- as.character(team_intelligence$team)
for (index in seq_along(team_names)) {
  team <- team_names[[index]]
  slug <- slugify(team)
  team_abbr <- unname(team_full_to_abbr[[team]])
  team_row <- team_intelligence[team_intelligence$team == team, , drop = FALSE][1L, ]
  team_hitters <- hitters[hitters$team == team, , drop = FALSE]
  team_hitters <- team_hitters[order(-num(team_hitters$woba_estimate), -num(team_hitters$pa)), , drop = FALSE]
  team_pitchers <- pitchers[pitchers$team == team, , drop = FALSE]
  team_pitchers <- team_pitchers[order(num(team_pitchers$woba_estimate), -num(team_pitchers$pa)), , drop = FALSE]
  qualified_team_hitters <- team_hitters[num(team_hitters$pa) >= 30, , drop = FALSE]
  qualified_team_pitchers <- team_pitchers[num(team_pitchers$pa) >= 30, , drop = FALSE]
  team_hitter_tracking <- hitter_tracking[hitter_tracking$team == team, , drop = FALSE]
  team_pitcher_tracking <- pitcher_tracking[pitcher_tracking$team == team, , drop = FALSE]
  qualified_team_contact_tracking <- team_hitter_tracking[
    num(team_hitter_tracking$tracked_batted_balls) >= 30,
    ,
    drop = FALSE
  ]
  qualified_team_pitch_tracking <- team_pitcher_tracking[
    num(team_pitcher_tracking$tracked_pitches) >= 100,
    ,
    drop = FALSE
  ]
  team_changes <- all_changes[all_changes$team == team, , drop = FALSE]
  team_changes <- team_changes[order(-num(team_changes$change_signal_score), -num(team_changes$dominant_change_abs_z)), , drop = FALSE]
  team_ace_pool <- fangraphs_pitchers[
    fangraphs_pitchers$team == team_abbr &
      num(fangraphs_pitchers$starts) >= 5 &
      as.character(fangraphs_pitchers$player_id) %in% as.character(pitch_types$player_id),
    ,
    drop = FALSE
  ]
  team_ace_pool <- team_ace_pool[
    order(-num(team_ace_pool$war), num(team_ace_pool$era), -num(team_ace_pool$innings_outs)),
    ,
    drop = FALSE
  ]
  if (!nrow(team_ace_pool)) {
    fallback_pitch <- signature_pitches[signature_pitches$team == team, , drop = FALSE]
    fallback_pitch <- fallback_pitch[order(num(fallback_pitch$pitch_quality_rank)), , drop = FALSE]
    if (nrow(fallback_pitch)) {
      team_ace_pool <- fangraphs_pitchers[
        as.character(fangraphs_pitchers$player_id) == as.character(fallback_pitch$player_id[[1L]]),
        ,
        drop = FALSE
      ]
    }
  }
  team_ace <- utils::head(team_ace_pool, 1L)
  team_ace_arsenal <- if (nrow(team_ace)) {
    pitch_types[
      as.character(pitch_types$player_id) == as.character(team_ace$player_id[[1L]]) &
        num(pitch_types$pitches) >= 5,
      ,
      drop = FALSE
    ]
  } else {
    pitch_types[0, , drop = FALSE]
  }
  team_matchups <- rbind(hitter_matchups[hitter_matchups$team == team, , drop = FALSE], pitcher_matchups[pitcher_matchups$team == team, , drop = FALSE])
  team_matchups <- team_matchups[order(-num(team_matchups$matchup_edge_score)), , drop = FALSE]
  team_bullpen <- bullpen_matchups[bullpen_matchups$team == team, , drop = FALSE]
  team_notes <- team_broadcast_notes[team_broadcast_notes$team == team, , drop = FALSE]
  team_stories <- story_queue[story_queue$team == team, , drop = FALSE]
  team_stories <- team_stories[order(-num(team_stories$story_score)), , drop = FALSE]
  team_position_rows <- team_positional_war[team_positional_war$team == team_abbr, , drop = FALSE]
  position_order <- c("C", "1B", "2B", "3B", "SS", "OF", "DH", "SP", "RP")
  team_position_rows <- team_position_rows[order(match(team_position_rows$position, position_order)), , drop = FALSE]
  team_position_cards <- if (nrow(team_position_rows)) vapply(seq_len(nrow(team_position_rows)), function(row_index) {
    positional_war_cell(team_position_rows[row_index, , drop = FALSE])
  }, character(1)) else character()
  strength_rows <- team_position_rows[team_position_rows$status == "strength", , drop = FALSE]
  strength_rows <- strength_rows[order(num(strength_rows$mlb_rank)), , drop = FALSE]
  need_rows <- team_position_rows[team_position_rows$status == "need", , drop = FALSE]
  need_rows <- need_rows[order(-num(need_rows$mlb_rank)), , drop = FALSE]
  position_summary <- if (nrow(team_position_rows)) paste0(
    '<div class="position-war-summary"><span><small>Best value pockets</small><strong>',
    html_escape(paste(utils::head(strength_rows$position_label, 2L), collapse = " / ")),
    '</strong></span><span><small>Clearest upgrade lanes</small><strong>',
    html_escape(paste(utils::head(need_rows$position_label, 2L), collapse = " / ")),
    '</strong></span></div>'
  ) else '<div class="method-callout">No FanGraphs positional WAR rows matched this club abbreviation.</div>'

  prior_index <- if (index == 1L) length(team_names) else index - 1L
  next_index <- if (index == length(team_names)) 1L else index + 1L
  prior_slug <- slugify(team_names[[prior_index]])
  next_slug <- slugify(team_names[[next_index]])
  report_nav <- paste0(
    '<nav class="team-report-nav" aria-label="Team report navigation">',
    '<a href="', prior_slug, '.html">&larr; ', html_escape(team_names[[prior_index]]), '</a>',
    '<a href="../team-reports.html">All teams</a>',
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
  ace_card <- if (nrow(team_ace) && nrow(team_ace_arsenal)) {
    team_ace_arsenal_card(team_ace, team_ace_arsenal, pitch_types)
  } else {
    ""
  }
  matchup_cards <- if (nrow(team_matchups)) {
    vapply(seq_len(min(3L, nrow(team_matchups))), function(row_index) matchup_edge_card(team_matchups[row_index, , drop = FALSE]), character(1))
  } else character()

  report_lines <- c(
    report_nav,
    '<section class="team-report-hero"><div><span class="eyebrow">SABRhood team report</span><h1>', html_escape(team), '</h1><p>', html_escape(team_row$team_story[[1L]]), '</p></div>',
    '<div class="team-report-hero__score"><small>League intelligence</small><strong>#', html_escape(fmt_int(team_row$team_index_rank[[1L]])), '</strong><span>', html_escape(fmt_score(team_row$team_index[[1L]])), ' index</span></div></section>',
    '<section class="team-fingerprint" aria-label="Team component ranks">',
    rank_meter("Run generation", team_row$offense_rank[[1L]], paste("Composite score", fmt_score(team_row$run_generation_score[[1L]]))),
    rank_meter("Pitching", team_row$pitching_rank[[1L]], paste("Composite score", fmt_score(team_row$pitching_score[[1L]]))),
    rank_meter("Recent play", team_row$form_rank[[1L]], paste(fmt_int(team_row$surging_signals[[1L]]), "recent risers")),
    rank_meter("Bullpen readiness", team_row$bullpen_rank[[1L]], paste("Current state", team_row$bullpen_health[[1L]])),
    '</section>',
    '<section class="section-heading"><span class="eyebrow">Roster construction</span><h2>Where this club creates, and loses, WAR</h2><p>Players are split by position and total WAR is calculated; the green border indicates a club is top-ten in positional WAR, the red border indicates a bottom-ten club.</p></section>',
    position_summary,
    if (length(team_position_cards)) paste0('<div class="position-war-board">', paste0(team_position_cards, collapse = ""), '</div>') else '',
    '<section class="broadcast-three"><div class="section-heading section-heading--tight"><span class="eyebrow">Broadcast three</span><h2>Water cooler notes</h2><p>Three concise, evidence-linked notes to start a broadcast conversation.</p></div><ol>', notes_html, '</ol></section>',
    '<section class="section-heading"><span class="eyebrow">Why it changed</span><h2>Player movement with league context attached</h2><p>The recent-window shift and the full-season league standing are separate, so a surge never floats free of the player\'s actual level.</p></section>',
    if (length(form_cards)) paste0('<div class="player-context-grid player-context-grid--report">', paste0(form_cards, collapse = ""), '</div>') else '<div class="method-callout">No players met both recent and baseline form thresholds.</div>',
    '<section class="section-heading"><span class="eyebrow">Team highs</span><h2>The category leaders inside this clubhouse</h2><p>Familiar production now sits beside tracked force: 100+ mph contact and pitches are shown as both totals and opportunity rates.</p></section>',
    '<div class="leaderboard-grid team-leaderboard-grid">',
    leaderboard_card(team_hitter_tracking, "100+ mph batted balls", "batted_balls_100_plus", fmt_int, "Tracked-contact season total", FALSE, 5L, "Club contact"),
    leaderboard_card(qualified_team_contact_tracking, "100+ mph contact rate", "batted_balls_100_plus_rate", fmt_rate, "Minimum 30 tracked batted balls", FALSE, 5L, "Club contact"),
    leaderboard_card(team_pitcher_tracking, "Pitches at 100+ mph", "pitches_100_plus", fmt_int, "Across every pitch type", FALSE, 5L, "Club velocity"),
    leaderboard_card(qualified_team_pitch_tracking, "100+ mph pitch rate", "pitches_100_plus_rate", fmt_rate, "Minimum 100 tracked pitches", FALSE, 5L, "Club velocity"),
    leaderboard_card(qualified_team_hitters, "Batting average", "batting_average", fmt_dec, "Best qualified contact results", FALSE, 5L, "Club offense"),
    leaderboard_card(team_hitters, "Home runs", "home_runs", fmt_int, "The club power board", FALSE, 5L, "Club offense"),
    leaderboard_card(qualified_team_hitters, "OPS", "ops", fmt_dec, "On-base and slugging production", FALSE, 5L, "Club offense"),
    leaderboard_card(team_pitchers, "Strikeouts", "strikeouts", fmt_int, "Total batters finished by strikeout", FALSE, 5L, "Club pitching"),
    leaderboard_card(qualified_team_pitchers, "Strikeout rate", "strikeout_rate", fmt_rate, "Strikeouts per batter faced", FALSE, 5L, "Club pitching"),
    leaderboard_card(qualified_team_pitchers, "Lowest OPS allowed", "ops", fmt_dec, "Qualified opponent production", TRUE, 5L, "Club pitching"),
    '</div>',
    '<section class="section-heading"><span class="eyebrow">Ace-in-the-Hole</span><h2>The staff ace&rsquo;s complete arsenal</h2><p>The highest-WAR qualified starter leads the card. Every tracked offering is shown with usage, pitch traits, outcomes, and same-pitch MLB percentiles.</p></section>',
    if (nzchar(ace_card)) ace_card else '<div class="method-callout">No qualified starter had a complete tracked arsenal for this report.</div>',
    '<section class="section-heading"><span class="eyebrow">Matchup intelligence</span><h2>The largest qualified handedness edges</h2></section>',
    if (length(matchup_cards)) paste0('<div class="signal-grid">', paste0(matchup_cards, collapse = ""), '</div>') else '<div class="method-callout">No players met the two-sided matchup threshold.</div>',
    '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Bullpen decision board</span><h2>Best available matchup for either side</h2><p>The board now shows only the highest-ranked active reliever against a left-handed hitter and a right-handed hitter. Workload availability, role, season performance, and handedness all remain visible.</p></div>',
    bullpen_decision_cards(team_bullpen),
    '</section>',
    '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Story leads</span><h2>Team Headlines</h2></div>',
    if (nrow(team_stories)) render_table(utils::head(team_stories, 6L), c("subject", "headline", "story_score"), c("Subject", "Reporting lead", "Score"), list(story_score = fmt_score), "data-table story-queue-table") else '<p>No current story candidates.</p>',
    '</section>',
    '<div class="method-callout"><strong>Research status:</strong> this automated report is a reporting foundation, not a finished scouting report. Active rosters are date-stamped; injuries, same-day transactions, probable pitchers, and opponent context still require pregame confirmation.</div>',
    report_nav
  )
  writeLines(enc2utf8(report_lines), file.path(team_include_dir, paste0(slug, ".html")), useBytes = TRUE)
  qmd_lines <- c(
    "---",
    paste0('title: "', team, ' Team report"'),
    paste0('description: "Automated SABRhood research report for the ', team, '."'),
    "---",
    "",
    paste0("{{< include ../includes/team-reports/", slug, ".html >}}")
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

qualified_run_game_pitchers <- run_game_pitchers[
  num(run_game_pitchers$eligible_pitches) >= 100,
  ,
  drop = FALSE
]
qualified_run_game_pitchers <- qualified_run_game_pitchers[
  order(num(qualified_run_game_pitchers$stealing_runs_saved), decreasing = TRUE),
  ,
  drop = FALSE
]
qualified_run_game_catchers <- run_game_catchers[
  num(run_game_catchers$eligible_pitches) >= 500,
  ,
  drop = FALSE
]
qualified_run_game_catchers <- qualified_run_game_catchers[
  order(num(qualified_run_game_catchers$stealing_runs_saved), decreasing = TRUE),
  ,
  drop = FALSE
]
qualified_run_game_runners <- run_game_runners[
  num(run_game_runners$attempts) >= 3,
  ,
  drop = FALSE
]
qualified_run_game_runners <- qualified_run_game_runners[
  order(num(qualified_run_game_runners$stealing_runs), decreasing = TRUE),
  ,
  drop = FALSE
]
qualified_framers <- catcher_framing[
  num(catcher_framing$called_pitches) >= 1000,
  ,
  drop = FALSE
]
qualified_framers <- qualified_framers[
  order(num(qualified_framers$framing_runs_estimate), decreasing = TRUE),
  ,
  drop = FALSE
]
qualified_abs <- abs_challenges[
  abs_challenges$challenge_type != "team-summary" &
    num(abs_challenges$challenge_opportunities) >= 100,
  ,
  drop = FALSE
]
qualified_abs <- qualified_abs[
  order(num(qualified_abs$runs_vs_expected), decreasing = TRUE),
  ,
  drop = FALSE
]
best_run_windows <- run_game_notes[
  run_game_notes$note_type == "best_count_to_run" &
    num(run_game_notes$reliability) >= 0.25,
  ,
  drop = FALSE
]
best_run_windows <- best_run_windows[
  order(num(best_run_windows$run_window_index), decreasing = TRUE),
  ,
  drop = FALSE
]
best_run_windows$target_label <- ifelse(
  num(best_run_windows$target_base) == 3,
  "Third",
  "Second"
)

write_fragment("run-game.html", c(
  '<section class="dashboard-block"><div class="section-heading section-heading--tight">',
  '<span class="eyebrow">Run Game Engine</span><h2>Every foot between the bases, broken down</h2>',
  '<p>Pitcher hold, catcher throwing, runner selection, pitch-count windows, called-pitch framing, and ABS challenge value are scored independently. Low samples are pulled toward league average.</p></div>',
  '<div class="run-game-section-stack">',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Pitcher control</span>',
  '<h2>Pitching Leaders in Baserunning Runs Saved</h2><p>Lower attempt and success-allowed indexes are better; the pitchers who intimidate runners the most.</p></div>',
  render_table(
    utils::head(qualified_run_game_pitchers, 15L),
    c("player_name", "team", "eligible_pitches", "attempt_index", "success_allowed_index", "stealing_runs_saved", "reliability"),
    c("Pitcher", "Team", "Eligible", "Attempt index", "Success allowed", "Runs saved", "Reliability"),
    list(
      eligible_pitches = fmt_int,
      attempt_index = function(value) fmt_dec(value, 0L),
      success_allowed_index = function(value) fmt_dec(value, 0L),
      stealing_runs_saved = function(value) fmt_signed(value, 2L),
      reliability = fmt_rate
    ),
    "data-table run-game-table"
  ),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">The best arms amd mitts</span>',
  '<h2>Catcher throwing and called-pitch framing</h2><p>The throwing board adjusts for pitchers and runners faced. The framing board estimates extra called strikes from location and context.</p></div>',
  '<div class="run-game-board-grid"><div><h3>Throwing value</h3>',
  render_table(
    utils::head(qualified_run_game_catchers, 12L),
    c("player_name", "team", "eligible_pitches", "attempt_index", "success_allowed_index", "stealing_runs_saved"),
    c("Catcher", "Team", "Eligible", "Attempt", "Success allowed", "Runs saved"),
    list(
      eligible_pitches = fmt_int,
      attempt_index = function(value) fmt_dec(value, 0L),
      success_allowed_index = function(value) fmt_dec(value, 0L),
      stealing_runs_saved = function(value) fmt_signed(value, 2L)
    ),
    "data-table run-game-table"
  ),
  '</div><div><h3>Framing development score</h3>',
  render_table(
    utils::head(qualified_framers, 12L),
    c("catcher_name", "team", "called_pitches", "extra_strikes", "framing_runs_estimate", "framing_score", "reliability"),
    c("Catcher", "Team", "Called pitches", "Extra strikes", "Runs*", "Score*", "Reliability"),
    list(
      called_pitches = fmt_int,
      extra_strikes = function(value) fmt_signed(value, 1L),
      framing_runs_estimate = function(value) fmt_signed(value, 2L),
      framing_score = function(value) fmt_dec(value, 1L),
      reliability = fmt_rate
    ),
    "data-table run-game-table"
  ),
  '</div></div></section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Run Theives</span>',
  '<h2>Best adjusted runners</h2><p>Aggression and success are estimated separately, then combined into stealing run value.</p></div>',
  render_table(
    utils::head(qualified_run_game_runners, 15L),
    c("player_name", "team", "attempts", "adjusted_attempt_rate", "adjusted_success_rate", "successes_above_expected", "stealing_runs", "reliability"),
    c("Runner", "Team", "Attempts", "Adj. attempt", "Adj. success", "Successes vs exp.", "Stealing runs", "Reliability"),
    list(
      attempts = fmt_int,
      adjusted_attempt_rate = fmt_rate,
      adjusted_success_rate = fmt_rate,
      successes_above_expected = function(value) fmt_signed(value, 2L),
      stealing_runs = function(value) fmt_signed(value, 2L),
      reliability = fmt_rate
    ),
    "data-table run-game-table"
  ),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">When to go</span>',
  '<h2>Pitcher-specific count windows</h2><p>Projected offspeed and breaking-ball mix, plate flight time, disengagements, and observed running behavior shape the index. This is a scouting prompt, not a green light by itself.</p></div>',
  render_table(
    utils::head(best_run_windows, 15L),
    c("pitcher_name", "fielding_team", "target_label", "count_key", "eligible_pitches", "run_window_index", "reliability", "note"),
    c("Pitcher", "Team", "Target", "Count", "Eligible", "Window index", "Reliability", "Research note"),
    list(
      eligible_pitches = fmt_int,
      run_window_index = function(value) fmt_dec(value, 0L),
      reliability = fmt_rate
    ),
    "data-table run-game-table"
  ),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">ABS challenge intelligence</span>',
  '<h2>Official challenge value leaderboard</h2><p>Batter, catcher, and pitcher roles are shown together. Runs vs. expected compares observed challenge value with the official opportunity model.</p></div>',
  render_table(
    utils::head(qualified_abs, 18L),
    c("player_name", "team", "challenge_type", "challenge_opportunities", "challenges", "overturn_rate", "overturns_vs_expected", "runs_vs_expected", "reliability"),
    c("Player", "Team", "Role", "Opportunities", "Challenges", "Overturn rate", "Overturns vs exp.", "Runs vs exp.", "Reliability"),
    list(
      challenge_opportunities = fmt_int,
      challenges = fmt_int,
      overturn_rate = fmt_rate,
      overturns_vs_expected = function(value) fmt_signed(value, 2L),
      runs_vs_expected = function(value) fmt_signed(value, 2L),
      reliability = fmt_rate
    ),
    "data-table run-game-table"
  ),
  '</section>',
  '</div></section>',
  '<div class="method-callout"><strong>Development status:</strong> catcher stints are reconstructed from official lineups and defensive substitutions; inferred assignments remain reliability-weighted. Framing runs and framing score are SABRhood development estimates, not official Statcast framing. ABS data comes from the official Baseball Savant leaderboard. Data through ',
  html_escape(run_game_model$source_through[[1L]]), '.</div>'
))

qualified_official_fielders <- official_fielding[
  num(official_fielding$innings) >= 150,
  ,
  drop = FALSE
]
qualified_official_fielders <- qualified_official_fielders[
  order(num(qualified_official_fielders$fielding_runs), decreasing = TRUE),
  ,
  drop = FALSE
]
qualified_advancement_fielders <- advancement_fielders[
  num(advancement_fielders$opportunities) >= 50,
  ,
  drop = FALSE
]
qualified_advancement_fielders <- qualified_advancement_fielders[
  order(num(qualified_advancement_fielders$advancement_prevention_score), decreasing = TRUE),
  ,
  drop = FALSE
]
fielding_team_board <- official_team_fielding[
  order(num(official_team_fielding$fielding_runs), decreasing = TRUE),
  ,
  drop = FALSE
]
latest_fielding_play <- fielding_play_day[
  order(as.Date(fielding_play_day$game_date), decreasing = TRUE),
  ,
  drop = FALSE
]
latest_fielding_play <- latest_fielding_play[1L, , drop = FALSE]
fielding_position_cards <- vapply(
  c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"),
  function(position_name) {
    rows <- qualified_official_fielders[
      qualified_official_fielders$primary_position == position_name,
      ,
      drop = FALSE
    ]
    rows <- rows[
      order(num(rows$fielding_runs), decreasing = TRUE),
      ,
      drop = FALSE
    ]
    rows <- utils::head(rows, 5L)
    table_columns <- if (position_name == "C") {
      c("player_name", "team", "innings", "fielding_runs", "range_runs", "catching_runs")
    } else {
      c("player_name", "team", "innings", "fielding_runs", "range_runs", "arm_runs")
    }
    table_labels <- if (position_name == "C") {
      c("Fielder", "Team", "Inn.", "FRV", "Range", "Catcher")
    } else {
      c("Fielder", "Team", "Inn.", "FRV", "Range", "Arm")
    }
    paste0(
      '<article class="fielding-position-card"><header><span>', position_name,
      '</span><h3>', switch(
        position_name,
        C = "Catcher", `1B` = "First base", `2B` = "Second base",
        `3B` = "Third base", SS = "Shortstop", LF = "Left field",
        CF = "Center field", RF = "Right field"
      ), '</h3></header>',
      render_table(
        rows,
        table_columns,
        table_labels,
        list(
          innings = fmt_int,
          fielding_runs = function(value) fmt_signed(value, 1L),
          range_runs = function(value) fmt_signed(value, 1L),
          arm_runs = function(value) fmt_signed(value, 1L),
          catching_runs = function(value) fmt_signed(value, 1L)
        ),
        "data-table fielding-table"
      ),
      '</article>'
    )
  },
  character(1)
)
qualified_pitcher_fielders <- fielding_players[
  fielding_players$position == "P" & num(fielding_players$opportunities) >= 9,
  ,
  drop = FALSE
]
advancement_teams <- advancement_teams[
  order(num(advancement_teams$advancement_prevention_score), decreasing = TRUE),
  ,
  drop = FALSE
]
advancement_teams$extra_base_rank <- seq_len(nrow(advancement_teams))
qualified_pitcher_fielders <- qualified_pitcher_fielders[
  order(num(qualified_pitcher_fielders$adjusted_range_runs), decreasing = TRUE),
  ,
  drop = FALSE
]
pitcher_fielding_card <- paste0(
  '<article class="fielding-position-card"><header><span>P</span><h3>Pitcher</h3></header>',
  render_table(
    utils::head(qualified_pitcher_fielders, 5L),
    c("player_name", "team", "opportunities", "adjusted_range_runs", "fielding_score"),
    c("Pitcher", "Team", "Chances", "Estimated runs", "Fielding Index"),
    list(
      opportunities = fmt_int,
      adjusted_range_runs = function(value) fmt_signed(value, 2L),
      fielding_score = function(value) fmt_dec(value, 1L)
    ),
    "data-table fielding-table"
  ),
  '</article>'
)
fielding_position_cards <- c(pitcher_fielding_card, fielding_position_cards)

write_fragment("fielding.html", c(
  '<section class="dashboard-block"><div class="section-heading section-heading--tight">',
  '<span class="eyebrow">Fielding Engine v1</span><h2>Defense, down to every play</h2>',
  '<p>Official Statcast Fielding Run Value anchors the public board. SABRhood development models add credited batted-ball opportunities and runner-advancement prevention without presenting those estimates as official tracking metrics.</p></div>',
  '<section class="dashboard-block fielding-play-feature"><div class="section-heading section-heading--tight">',
  '<span class="eyebrow">Play of the Day</span><h2>', html_escape(latest_fielding_play$fielder_name[[1L]]), '</h2>',
  '<p>', html_escape(latest_fielding_play$play_story[[1L]]), '</p><p class="fielding-play-description">',
  html_escape(latest_fielding_play$description[[1L]]), '</p></div>',
  '<div class="fielding-play-meta"><span>', html_escape(latest_fielding_play$game_date[[1L]]), '</span><span>',
  html_escape(latest_fielding_play$fielding_team[[1L]]), ' &middot; ', html_escape(latest_fielding_play$position[[1L]]),
  '</span><span>', html_escape(fmt_signed(latest_fielding_play$play_runs_saved[[1L]], 2L)), ' estimated runs</span><span>',
  html_escape(fmt_dec(latest_fielding_play$launch_speed[[1L]], 1L)), ' mph &middot; ',
  html_escape(fmt_int(latest_fielding_play$hit_distance[[1L]])), ' ft</span><span>',
  html_escape(fmt_rate(latest_fielding_play$estimated_out_probability[[1L]])), ' estimated out chance</span></div>',
  '<p class="fielding-disclaimer">', html_escape(latest_fielding_play$publication_status[[1L]]), '</p></section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Defensive alignment</span>',
  '<h2>The All-MLB Defense Team</h2><p>Official Fielding Run Value selects every covered position. Pitchers use a separately labeled SABRhood play-context estimate because the official source does not supply compatible pitcher FRV.</p></div>',
  '<figure class="feature-graphic feature-graphic--wide"><img src="images/graphics-feed/all-mlb-defense-team.png" alt=""><figcaption><strong>Eight gloves, one field.</strong> Selections use official Fielding Run Value with a playing-time adjustment.</figcaption></figure>',
  '<div class="section-action"><a class="btn btn-sabr-navy" href="races.html">Open the AL and NL Gold Glove rosters</a></div></section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Position rooms</span>',
  '<h2>The best defenders at every position</h2><p>Each position gets its own qualified leaderboard, so catcher value is not compared directly with center-field range or shortstop conversion.</p></div>',
  '<div class="fielding-position-grid">', fielding_position_cards, '</div>',
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Official value</span>',
  '<h2>Fielding Run Value leaders</h2><p>Range, arm, double-play, catcher throwing, blocking, and framing components remain visible instead of collapsing every skill into one unexplained number.</p></div>',
  render_table(
    utils::head(qualified_official_fielders, 20L),
    c("player_name", "team", "primary_position", "innings", "fielding_runs", "range_runs", "arm_runs", "throwing_runs", "framing_runs"),
    c("Fielder", "Team", "Pos.", "Innings", "FRV", "Range", "Arm", "Throwing", "Framing"),
    list(
      innings = fmt_int,
      fielding_runs = function(value) fmt_signed(value, 1L),
      range_runs = function(value) fmt_signed(value, 1L),
      arm_runs = function(value) fmt_signed(value, 1L),
      throwing_runs = function(value) fmt_signed(value, 1L),
      framing_runs = function(value) fmt_signed(value, 1L)
    ),
    "data-table fielding-table"
  ),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">The next 90 feet</span>',
  '<h2>Runner-advancement prevention</h2><p>Who keeps a runner from taking third on a single, scoring from second, or moving on an out after adjusting for runner, base state, contact, outs, inning, and score.</p></div>',
  '<figure class="feature-graphic feature-graphic--wide"><img src="images/graphics-feed/runner-advancement-defenders.png" alt="Scatter plot comparing advances stopped with official throwing run value"><figcaption>Advances stopped are shown on the horizontal axis and official throwing run value on the vertical axis; point size represents opportunities.</figcaption></figure>',
  render_table(
    utils::head(qualified_advancement_fielders, 20L),
    c("player_name", "team", "position", "opportunities", "adjusted_advancement_rate", "advancements_prevented", "advancement_runs_saved", "advancement_prevention_score", "reliability"),
    c("Fielder", "Team", "Pos.", "Chances", "Advance rate allowed", "Advances stopped", "Runs saved", "Score", "Sample confidence"),
    list(
      opportunities = fmt_int,
      adjusted_advancement_rate = fmt_rate,
      advancements_prevented = function(value) fmt_signed(value, 1L),
      advancement_runs_saved = function(value) fmt_signed(value, 2L),
      advancement_prevention_score = function(value) fmt_dec(value, 0L),
      reliability = fmt_rate
    ),
    "data-table fielding-table"
  ),
  '</section>',
  '<section class="dashboard-block"><div class="section-heading section-heading--tight"><span class="eyebrow">Club identity</span>',
  '<h2>Team Defensive DNA</h2><p>Official team value beside the separate SABRhood conversion and extra-base prevention lenses.</p></div>',
  '<div class="fielding-board-grid"><div><h3>Official Fielding Run Value</h3>',
  render_table(
    fielding_team_board,
    c("official_fielding_rank", "team", "fielding_runs", "range_runs", "arm_runs", "catching_runs"),
    c("Rank", "Team", "FRV", "Range", "Arm", "Catching"),
    list(
      official_fielding_rank = fmt_int,
      fielding_runs = function(value) fmt_signed(value, 1L),
      range_runs = function(value) fmt_signed(value, 1L),
      arm_runs = function(value) fmt_signed(value, 1L),
      catching_runs = function(value) fmt_signed(value, 1L)
    ),
    "data-table fielding-table"
  ),
  '</div><div><h3>Extra-base prevention</h3>',
  render_table(
    advancement_teams,
    c("extra_base_rank", "team", "opportunities", "adjusted_advancement_rate", "advancement_runs_saved", "advancement_prevention_score"),
    c("Rank", "Team", "Chances", "Advance rate allowed", "Runs saved", "Score"),
    list(
      extra_base_rank = fmt_int,
      opportunities = fmt_int,
      adjusted_advancement_rate = fmt_rate,
      advancement_runs_saved = function(value) fmt_signed(value, 2L),
      advancement_prevention_score = function(value) fmt_dec(value, 0L)
    ),
    "data-table fielding-table"
  ),
  '</div></div></section>',
  '<div class="method-callout"><strong>Development status:</strong> the SABRhood PBP expected-out model does not have official starting position, jump, route, wall, or opportunity-time tracking. Those estimates are labeled separately from official Fielding Run Value. Data through ',
  html_escape(fielding_model$source_through[[1L]]), '.</div>'
))

cat("Generated site fragments in", include_dir, "\n")
