args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || args[[1L]] %in% c("-h", "--help")) {
  cat(
    "Create a new SABRhood article draft.\n\n",
    "Usage:\n",
    '  Rscript scripts/new_article.R "Headline" ["Team"] ["Topic"]\n\n',
    "Example:\n",
    '  Rscript scripts/new_article.R "How the cutter changed Boston" "Boston Red Sox" "Pitching"\n',
    sep = ""
  )
  quit(status = if (length(args)) 0L else 1L)
}

site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
themes <- utils::read.csv(
  file.path(site_root, "config", "mlb-team-themes.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

headline <- trimws(args[[1L]])
team <- if (length(args) >= 2L && nzchar(trimws(args[[2L]]))) trimws(args[[2L]]) else "League"
topic <- if (length(args) >= 3L && nzchar(trimws(args[[3L]]))) trimws(args[[3L]]) else "Analysis"

team_index <- match(tolower(team), tolower(themes$team))
if (is.na(team_index)) {
  stop(
    "Unknown team '", team, "'. Use a team name from config/mlb-team-themes.csv or League.",
    call. = FALSE
  )
}
team <- themes$team[[team_index]]
theme_slug <- themes$slug[[team_index]]

slugify <- function(x) {
  ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  slug <- tolower(gsub("[^a-z0-9]+", "-", ascii))
  gsub("(^-+|-+$)", "", slug)
}

yaml_quote <- function(x) paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
article_date <- Sys.Date()
filename <- paste0(article_date, "-", slugify(headline), ".qmd")
output_path <- file.path(site_root, "articles", filename)

if (file.exists(output_path)) {
  stop("Article already exists: ", output_path, call. = FALSE)
}

lines <- c(
  "---",
  paste("title:", yaml_quote(headline)),
  'description: "One sentence that explains why the story matters."',
  paste("date:", article_date),
  'author: "Tyler Schultz"',
  paste("team:", yaml_quote(team)),
  paste("topic:", yaml_quote(topic)),
  'image: "../images/SABRHOODpng.png"',
  "draft: true",
  "format:",
  "  html:",
  "    toc: true",
  "    toc-depth: 2",
  "---",
  "",
  paste0(":::: {.article-shell .theme-", theme_slug, "}"),
  paste0("[", team, " · ", topic, "]{.article-kicker}"),
  "",
  '<p class="article-deck">',
  "Write a short deck that tells the reader what changed and why it matters.",
  "</p>",
  "",
  "::: {.article-callout}",
  "Put the strongest finding, quote, or broadcast-ready takeaway here.",
  ":::",
  "",
  "## The finding",
  "",
  "Write the story here. Add charts from `images/` with standard Quarto image syntax.",
  "",
  "## What it means",
  "",
  "Close by placing the result in team, player-development, or game context.",
  "::::"
)

writeLines(lines, output_path, useBytes = TRUE)
cat("Created article draft:\n", output_path, "\n", sep = "")
cat("Edit the article, replace the image, then change draft: true to draft: false.\n")
