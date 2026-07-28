args <- commandArgs(trailingOnly = TRUE)
check_rendered <- "--rendered" %in% args
allow_stale_data <- "--allow-stale-data" %in% args

site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
failures <- character()

fail <- function(message) {
  failures <<- c(failures, message)
}

canonical_file_md5 <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  canonical <- paste0(paste(lines, collapse = "\n"), "\n")
  temporary <- tempfile(fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  writeBin(charToRaw(enc2utf8(canonical)), connection)
  close(connection)
  unname(tools::md5sum(temporary))
}

read_html <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

archive_article_text <- function(html) {
  main <- regmatches(html, regexpr("(?s)<main[^>]*>.*?</main>", html, perl = TRUE))
  if (!length(main) || !nzchar(main)) return("")
  main <- gsub(
    '(?s)<header[^>]*class="[^"]*article-masthead[^"]*"[^>]*>.*?</header>',
    "",
    main,
    perl = TRUE
  )
  main <- gsub("(?s)<script[^>]*>.*?</script>", "", main, perl = TRUE)
  main <- gsub("(?s)<style[^>]*>.*?</style>", "", main, perl = TRUE)
  main <- gsub("<[^>]+>", " ", main, perl = TRUE)
  trimws(gsub("\\s+", " ", main, perl = TRUE))
}

required_data <- c(
  "data-contract-summary.csv",
  "historical-anniversary-notes.csv",
  "history-match-notes.csv",
  "daily-retrosheet-history.csv",
  "current-season-hitter-game-lines.csv",
  "current-season-pitcher-game-lines.csv",
  "historical-milestone-notes.csv",
  "historical-player-profiles.csv",
  "hitter-performance-summary.csv",
  "pitcher-performance-summary.csv",
  "pitch-type-summary.csv",
  "hitter-recent-form.csv",
  "pitcher-recent-form.csv",
  "bullpen-availability.csv",
  "run-expectancy-24.csv",
  "manager-data-summary.csv"
  ,"manager-hook-validation-metrics.csv"
  ,"manager-hook-calibration.csv"
  ,"active-milestone-watch.csv"
  ,"offensive-race-board.csv"
  ,"run-prevention-race-board.csv"
  ,"team-intelligence-summary.csv"
  ,"manager-hook-model.csv"
  ,"manager-hook-scenarios.csv"
  ,"bullpen-matchup-selector.csv"
  ,"hitter-matchup-edges.csv"
  ,"pitcher-matchup-edges.csv"
  ,"signature-pitch-board.csv"
  ,"daily-story-queue.csv"
  ,"team-broadcast-notes.csv"
  ,"hitter-change-profiles.csv"
  ,"pitcher-change-profiles.csv"
  ,"career-trajectory-projections.csv"
  ,"career-trajectory-comparables.csv"
  ,"career-three-season-forecasts.csv"
  ,"career-trajectory-model-card.csv"
  ,"career-trajectory-backtest-summary.csv"
  ,"career-trajectory-probability-calibration.csv"
  ,"career-trajectory-holdout-validation.csv"
  ,"career-trajectory-rate-validation.csv"
  ,"career-trajectory-weight-tuning.csv"
  ,"career-trajectory-tuning-evaluation.csv"
  ,"career-trajectory-weight-profiles.csv"
  ,"daily-projection-demo.csv"
  ,"daily-projection-margin.csv"
  ,"daily-projection-scorelines.csv"
  ,"daily-projection-drivers.csv"
  ,"daily-projections-live.csv"
  ,"daily-projection-margin-live.csv"
  ,"daily-projection-scorelines-live.csv"
  ,"daily-projection-drivers-live.csv"
  ,"daily-projection-components-live.csv"
  ,"projection-publication-readiness.csv"
  ,"daily-batter-simulation-summary.csv"
  ,"daily-starter-simulation-summary.csv"
  ,"daily-lineup-simulation-summary.csv"
  ,"daily-player-simulations.csv"
  ,"daily-player-simulation-skips.csv"
  ,"daily-player-simulation-model-card.csv"
  ,"game-projection-backtest.csv"
  ,"game-projection-backtest-metrics.csv"
  ,"game-projection-calibration.csv"
  ,"game-score-calibrator-coefficients.csv"
  ,"game-winner-model-coefficients.csv"
  ,"game-score-model-card.csv"
  ,"projection-ledger-status.csv"
  ,"projection-feedback-metrics.csv"
  ,"projection-feedback-calibration.csv"
  ,"player-projection-feedback-metrics.csv"
  ,"projection-feedback-ledger.csv"
  ,"projection-input-readiness.csv"
  ,"bullpen-chain-demo.csv"
  ,"projection-hook-path.csv"
  ,"pitch-usage-change-board.csv"
  ,"daily-game-inputs.csv"
  ,"daily-batting-orders.csv"
  ,"daily-probable-starters.csv"
  ,"active-rosters.csv"
  ,"active-roster-bullpens.csv"
  ,"active-roster-bullpen-selector.csv"
  ,"daily-park-weather.csv"
  ,"daily-series-context.csv"
  ,"daily-series-player-lines.csv"
  ,"daily-recent-game-lines.csv"
  ,"aaa-hitter-watch.csv"
  ,"aaa-pitcher-watch.csv"
  ,"fangraphs-season-hitters.csv"
  ,"fangraphs-season-pitchers.csv"
  ,"award-race-board.csv"
  ,"mvp-era-stat-profiles.csv"
  ,"mvp-modern-model-weights.csv"
  ,"aaa-call-up-radar.csv"
  ,"aaa-standings-current.csv"
  ,"aaa-standings-movement.csv"
  ,"aaa-team-rankings.csv"
  ,"hitter-tracking-totals.csv"
  ,"pitcher-tracking-totals.csv"
  ,"team-tracking-totals.csv"
  ,"mlb-standings-current.csv"
  ,"mlb-standings-movement.csv"
  ,"daily-newsletter-stories.csv"
  ,"daily-newsletter-edition.csv"
  ,"graphics-feed-manifest.csv"
  ,"daily-player-probabilities.csv"
  ,"daily-matchup-event-probabilities.csv"
  ,"daily-matchup-event-diagnostics.csv"
  ,"daily-matchup-event-model-card.csv"
  ,"daily-state-simulation-games.csv"
  ,"daily-state-simulation-hitters.csv"
  ,"daily-state-simulation-bullpen-inputs.csv"
  ,"daily-state-simulation-model-card.csv"
  ,"state-simulation-feedback-metrics.csv"
  ,"daily-state-simulation-reliever-inputs.csv"
  ,"daily-state-simulation-relievers.csv"
  ,"daily-state-simulation-events.csv"
  ,"baserunning-league-rates.csv"
  ,"baserunning-runner-profiles.csv"
  ,"baserunning-pitcher-hold-profiles.csv"
  ,"baserunning-park-factors.csv"
  ,"baserunning-model-card.csv"
  ,"run-game-pitcher-ratings.csv"
  ,"run-game-catcher-ratings.csv"
  ,"run-game-runner-ratings.csv"
  ,"run-game-battery-ratings.csv"
  ,"run-game-count-windows.csv"
  ,"run-game-notes.csv"
  ,"run-game-model-card.csv"
  ,"catcher-framing-ratings.csv"
  ,"catcher-framing-model-card.csv"
  ,"abs-challenge-leaderboard.csv"
  ,"abs-challenge-model-card.csv"
  ,"official-fielding-run-value.csv"
  ,"official-team-fielding-run-value.csv"
  ,"fielding-player-ratings.csv"
  ,"fielding-team-ratings.csv"
  ,"runner-advancement-fielding-ratings.csv"
  ,"runner-advancement-team-ratings.csv"
  ,"fielding-play-of-day.csv"
  ,"gold-glove-watch.csv"
  ,"fielding-model-card.csv"
  ,"state-simulation-calibration-status.csv"
  ,"state-reliever-feedback-metrics.csv"
  ,"matchup-event-feedback-metrics.csv"
  ,"matchup-event-feedback-by-event.csv"
  ,"rolling-league-pitch-usage.csv"
  ,"rolling-league-production.csv"
  ,"rolling-league-pitch-quality.csv"
  ,"rolling-league-batted-ball.csv"
  ,"rolling-league-workload.csv"
  ,"insane-baseball-awards.csv"
  ,"team-positional-war.csv"
  ,"player-market-groups.csv"
  ,"player-market-players.csv"
  ,"hitter-discipline-profiles.csv"
  ,"arsenal-spotlights.csv"
  ,"pull-rate-leader-batted-balls.csv"
  ,"award-race-history.csv"
  ,"award-race-display.csv"
  ,"award-race-events.csv"
  ,"award-race-current-leaders.csv"
  ,"refresh-health.csv"
  ,"daily-slate-status.csv"
)

required_fragments <- c(
  "home-snapshot.html",
  "today-dashboard.html",
  "newsletter-daily.html",
  "player-leaders.html",
  "pitch-lab.html",
  "team-pulse.html",
  "methodology-data.html"
  ,"projections-model.html"
  ,"article-listing.html"
  ,"home-research.html"
  ,"history-desk.html"
  ,"history-match-desk.html"
  ,"home-team-pulse.html"
  ,"league-races.html"
  ,"story-desk.html"
  ,"home-story-desk.html"
  ,"matchup-edges.html"
  ,"team-report-index.html"
  ,"home-player-change.html"
  ,"player-change-cards.html"
  ,"career-trajectories.html"
  ,"player-market.html"
  ,"home-career-trajectories.html"
  ,"daily-projections.html"
  ,"home-projections.html"
  ,"aaa-watch.html"
  ,"graphics-feed.html"
  ,"newsletter-graphics.html"
  ,"league-leaderboards.html"
  ,"standings-desk.html"
  ,"league-trends.html"
  ,"insane-awards.html"
  ,"run-game.html"
  ,"fielding.html"
)

allowed_empty_data <- c(
  "daily-batting-orders.csv",
  "daily-player-simulation-skips.csv",
  "daily-series-player-lines.csv",
  "daily-recent-game-lines.csv",
  "history-match-notes.csv"
)

for (name in required_data) {
  path <- file.path(site_root, "data", "derived", name)
  if (!file.exists(path)) {
    fail(paste("Missing derived data product:", name))
    next
  }
  product <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(error) error
  )
  if (inherits(product, "error")) {
    fail(paste("Unreadable derived data product:", name))
  } else if (nrow(product) < 1L && !name %in% allowed_empty_data) {
    fail(paste("Empty derived data product:", name))
  }
}

gold_glove_path <- file.path(
  site_root,
  "data",
  "derived",
  "gold-glove-watch.csv"
)
if (file.exists(gold_glove_path)) {
  gold_glove <- utils::read.csv(
    gold_glove_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required_gold_columns <- c(
    "league", "primary_position", "position_rank",
    "player_name", "team", "gold_glove_score"
  )
  if (!all(required_gold_columns %in% names(gold_glove))) {
    fail("Gold Glove Watch is missing league-position ranking fields")
  } else {
    leaders <- gold_glove[
      suppressWarnings(as.numeric(gold_glove$position_rank)) == 1,
      ,
      drop = FALSE
    ]
    leader_groups <- paste(leaders$league, leaders$primary_position)
    expected_groups <- as.vector(outer(
      c("AL", "NL"),
      c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"),
      paste
    ))
    if (!setequal(leader_groups, expected_groups)) {
      fail("Gold Glove Watch must contain one AL and one NL leader at every tracked position")
    }
  }
}

refresh_health_path <- file.path(site_root, "data", "derived", "refresh-health.csv")
if (file.exists(refresh_health_path)) {
  refresh_health <- utils::read.csv(refresh_health_path, stringsAsFactors = FALSE, check.names = FALSE)
  required_health_columns <- c(
    "product_group", "source_through", "expected_through", "lag_days",
    "max_lag_days", "cadence", "status", "checked_at_utc", "reference_date"
  )
  if (!all(required_health_columns %in% names(refresh_health))) {
    fail("Refresh-health report is missing required columns")
  } else if (any(refresh_health$status != "current") && !allow_stale_data) {
    stale_groups <- refresh_health$product_group[refresh_health$status != "current"]
    fail(paste("Stale public product groups:", paste(stale_groups, collapse = ", ")))
  }
}

manifest_path <- file.path(site_root, "data", "derived", "manifest.csv")
if (!file.exists(manifest_path)) {
  fail("Missing derived-data manifest")
} else {
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  manifest_required <- c("file", "rows", "columns", "bytes", "md5", "synced_at_utc")
  if (!all(manifest_required %in% names(manifest))) {
    fail("Derived-data manifest is missing required columns")
  } else {
    for (index in seq_len(nrow(manifest))) {
      product_path <- file.path(site_root, "data", "derived", manifest$file[[index]])
      if (!file.exists(product_path)) {
        fail(paste("Manifest product is missing:", manifest$file[[index]]))
      } else if (!identical(canonical_file_md5(product_path), manifest$md5[[index]])) {
        fail(paste("Manifest checksum does not match:", manifest$file[[index]]))
      }
    }
  }
}

published_files <- list.files(file.path(site_root, "data"), recursive = TRUE, full.names = FALSE)
raw_name_hits <- grep("(^|[/\\\\])(pbp|statcast)|mlb_pbp|enhanced_pbp", published_files, ignore.case = TRUE, value = TRUE)
if (length(raw_name_hits)) {
  fail(paste("Possible raw play-by-play file in public data:", paste(raw_name_hits, collapse = ", ")))
}

for (name in required_fragments) {
  path <- file.path(site_root, "includes", name)
  if (!file.exists(path) || file.info(path)$size < 50) {
    fail(paste("Missing or empty generated fragment:", name))
  }
}

required_defense_graphics <- c(
  "all-mlb-defense-team.png",
  "gold-glove-watch-rosters.png",
  "defensive-position-leaders.png",
  "runner-advancement-defenders.png"
)
for (name in required_defense_graphics) {
  path <- file.path(site_root, "images", "graphics-feed", name)
  if (!file.exists(path) || file.info(path)$size < 10000) {
    fail(paste("Missing or unexpectedly small defensive graphic:", name))
  }
}

source_files <- list.files(
  site_root,
  pattern = "\\.(qmd|yml|yaml|R|html)$",
  recursive = TRUE,
  full.names = TRUE
)
source_files <- source_files[!grepl("/docs/", gsub("\\\\", "/", source_files))]

for (path in source_files) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  joined <- paste(lines, collapse = "\n")
  relative <- substring(gsub("\\\\", "/", path), nchar(site_root) + 2L)
  if (grepl("[A-Za-z]:(?:[/\\\\]+)Users(?:[/\\\\]+)", joined, perl = TRUE)) {
    fail(paste("Local absolute path found in:", relative))
  }
  if (grepl("<U\\+[0-9A-Fa-f]{4,6}>", joined, perl = TRUE)) {
    fail(paste("Undecoded Unicode token found in:", relative))
  }
}

re24_path <- file.path(site_root, "data", "derived", "run-expectancy-24.csv")
if (file.exists(re24_path)) {
  re24 <- utils::read.csv(re24_path, stringsAsFactors = FALSE)
  if (nrow(re24) != 24L) fail("RE24 product must contain exactly 24 base-out states")
}

archive_source_root <- file.path(site_root, "legacy-assets", "posts")
archive_snapshot_paths <- if (dir.exists(archive_source_root)) {
  list.files(archive_source_root, pattern = "[.]html$", full.names = TRUE)
} else {
  character()
}
if (!length(archive_snapshot_paths)) {
  fail("Canonical archive article snapshots are missing from legacy-assets/posts")
} else {
  qmd_stems <- tools::file_path_sans_ext(basename(list.files(
    file.path(site_root, "posts"),
    pattern = "[.]qmd$"
  )))
  snapshot_stems <- tools::file_path_sans_ext(basename(archive_snapshot_paths))
  missing_snapshots <- setdiff(qmd_stems, snapshot_stems)
  if (length(missing_snapshots)) {
    fail(paste(
      "Article sources are missing canonical HTML snapshots:",
      paste(missing_snapshots, collapse = ", ")
    ))
  }
}

if (check_rendered) {
  required_pages <- c(
    "index.html", "today.html", "standings.html", "races.html", "insane-awards.html", "league-trends.html", "story-desk.html", "matchups.html", "players.html", "player-change-engine.html", "career-trajectories.html", "teams.html", "team-reports.html", "history.html", "history-match.html", "pitch-lab.html", "run-game.html", "fielding.html",
    "projections.html", "aaa.html", "newsletter.html", "graphics-feed.html", "leaderboards.html", "blog.html", "broadcast.html",
    "methodology.html", "glossary.html", "about.html", "404.html"
  )
  for (name in required_pages) {
    path <- file.path(site_root, "docs", name)
    if (!file.exists(path) || file.info(path)$size < 500) {
      fail(paste("Missing rendered page:", name))
    }
  }
  report_pages <- list.files(file.path(site_root, "docs", "team-reports"), pattern = "[.]html$", full.names = TRUE)
  if (length(report_pages) != 30L) {
    fail(paste("Rendered team report count must be 30; found", length(report_pages)))
  } else if (any(file.info(report_pages)$size < 1000)) {
    fail("One or more rendered team reports are unexpectedly small")
  }
  required_legacy_assets <- c(
    "site_libs/kePrint-0.0.1/kePrint.js",
    "site_libs/lightable-0.0.1/lightable.css"
  )
  for (name in required_legacy_assets) {
    if (!file.exists(file.path(site_root, "docs", name))) {
      fail(paste("Missing legacy article asset:", name))
    }
  }
  required_article_pages <- file.path("posts", basename(archive_snapshot_paths))
  for (name in required_article_pages) {
    path <- file.path(site_root, "docs", name)
    snapshot_path <- file.path(archive_source_root, basename(name))
    if (!file.exists(path) || file.info(path)$size < 1000) {
      fail(paste("Missing rendered research article:", name))
    } else {
      html <- read_html(path)
      if (!grepl("../styles.css", html, fixed = TRUE)) {
        fail(paste("Research article is missing the global stylesheet:", name))
      }
      if (!grepl("../includes/article-team-themes.css", html, fixed = TRUE)) {
        fail(paste("Research article is missing the team-theme stylesheet:", name))
      }
      if (!grepl("../site_libs/bootstrap/bootstrap.min.css", html, fixed = TRUE)) {
        fail(paste("Research article is missing the current Bootstrap stylesheet:", name))
      }
      if (!grepl('<body[^>]*class="[^"]*legacy-article-page[^"]*"[^>]*>', html, perl = TRUE)) {
        fail(paste("Research article was not normalized:", name))
      }
      if (!grepl('class="[^"]*article-masthead[^"]*"', html, perl = TRUE)) {
        fail(paste("Research article is missing its branded masthead:", name))
      }
      if (!grepl('<header id="quarto-header"', html, fixed = TRUE)) {
        fail(paste("Research article is missing the normal site navbar:", name))
      }
      if (!grepl(
        'class="nav-link active" href="../blog.html" aria-current="page"',
        html,
        fixed = TRUE
      )) {
        fail(paste("Research article navbar does not identify Research as active:", name))
      }
      if (!grepl('<footer class="footer"', html, fixed = TRUE)) {
        fail(paste("Research article is missing the normal site footer:", name))
      }
      if (!grepl("../images/thesabrhood2clean.png", html, fixed = TRUE)) {
        fail(paste("Research article navbar logo path is invalid:", name))
      }
      if (grepl('class="legacy-article-nav"', html, fixed = TRUE)) {
        fail(paste("Research article still contains retired archive navigation:", name))
      }
      if (!grepl('class="legacy-article-body"', html, fixed = TRUE)) {
        fail(paste("Research article is missing its responsive reading frame:", name))
      }
      if (grepl('(?:src|href)="[^"]*_files/libs/', html, perl = TRUE)) {
        fail(paste("Research article still references page-local library assets:", name))
      }
      snapshot_html <- read_html(snapshot_path)
      if (!identical(archive_article_text(snapshot_html), archive_article_text(html))) {
        fail(paste("Research article body content changed during publication:", name))
      }
    }
  }
  retired_outputs <- c(
    file.path(site_root, "docs", "team-dossiers"),
    file.path(site_root, "docs", "team-dossiers.html"),
    file.path(site_root, "docs", "team-dossiers_files")
  )
  if (any(file.exists(retired_outputs) | dir.exists(retired_outputs))) {
    fail("Retired team-dossier outputs are still present")
  }
}

if (length(failures)) {
  cat("Site validation failed:\n- ", paste(failures, collapse = "\n- "), "\n", sep = "")
  quit(status = 1L)
}

cat(
  "Site validation passed:",
  length(required_data), "data products,",
  length(required_fragments), "generated fragments",
  if (check_rendered) "and rendered pages" else "",
  "\n"
)
