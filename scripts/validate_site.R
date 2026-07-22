args <- commandArgs(trailingOnly = TRUE)
check_rendered <- "--rendered" %in% args

site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
failures <- character()

fail <- function(message) {
  failures <<- c(failures, message)
}

required_data <- c(
  "data-contract-summary.csv",
  "historical-anniversary-notes.csv",
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
  ,"aaa-hitter-watch.csv"
  ,"aaa-pitcher-watch.csv"
  ,"fangraphs-season-hitters.csv"
  ,"fangraphs-season-pitchers.csv"
  ,"award-race-board.csv"
  ,"graphics-feed-manifest.csv"
  ,"daily-player-probabilities.csv"
  ,"rolling-league-pitch-usage.csv"
  ,"rolling-league-production.csv"
  ,"insane-baseball-awards.csv"
  ,"team-positional-war.csv"
  ,"hitter-discipline-profiles.csv"
  ,"arsenal-spotlights.csv"
  ,"pull-rate-leader-batted-balls.csv"
  ,"award-race-history.csv"
  ,"award-race-display.csv"
  ,"award-race-events.csv"
  ,"award-race-current-leaders.csv"
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
  ,"home-team-pulse.html"
  ,"league-races.html"
  ,"story-desk.html"
  ,"home-story-desk.html"
  ,"matchup-edges.html"
  ,"team-dossier-index.html"
  ,"home-player-change.html"
  ,"player-change-cards.html"
  ,"daily-projections.html"
  ,"home-projections.html"
  ,"aaa-watch.html"
  ,"graphics-feed.html"
  ,"newsletter-graphics.html"
  ,"league-leaderboards.html"
  ,"league-trends.html"
  ,"insane-awards.html"
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
  } else if (nrow(product) < 1L && !name %in% c("daily-batting-orders.csv")) {
    fail(paste("Empty derived data product:", name))
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
      } else if (!identical(unname(tools::md5sum(product_path)), manifest$md5[[index]])) {
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
  if (grepl("[A-Za-z]:[/\\\\]Users[/\\\\]", joined, perl = TRUE)) {
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

if (check_rendered) {
  required_pages <- c(
    "index.html", "today.html", "races.html", "insane-awards.html", "league-trends.html", "story-desk.html", "matchups.html", "players.html", "player-change-engine.html", "teams.html", "team-dossiers.html", "history.html", "pitch-lab.html",
    "projections.html", "aaa.html", "newsletter.html", "graphics-feed.html", "leaderboards.html", "blog.html", "broadcast.html",
    "methodology.html", "glossary.html", "about.html", "404.html"
  )
  for (name in required_pages) {
    path <- file.path(site_root, "docs", name)
    if (!file.exists(path) || file.info(path)$size < 500) {
      fail(paste("Missing rendered page:", name))
    }
  }
  dossier_pages <- list.files(file.path(site_root, "docs", "team-dossiers"), pattern = "[.]html$", full.names = TRUE)
  if (length(dossier_pages) != 30L) {
    fail(paste("Rendered team dossier count must be 30; found", length(dossier_pages)))
  } else if (any(file.info(dossier_pages)$size < 1000)) {
    fail("One or more rendered team dossiers are unexpectedly small")
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
