site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
article_root <- file.path(site_root, "docs", "posts")

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

if (!dir.exists(article_root)) {
  cat("No rendered legacy article directory found; nothing to normalize.\n")
  quit(status = 0L)
}

article_themes <- c(
  "A.J Ewing Gets the Call" = "new-york-mets",
  "bello_article_final" = "boston-red-sox",
  "ceddannesnewgroove" = "boston-red-sox",
  "CleanPig" = "boston-red-sox",
  "durbin_article" = "boston-red-sox",
  "fla 2025 v 2026 article" = "league",
  "Series Recap Tigers Sox" = "boston-red-sox",
  "sorianopreseason26" = "los-angeles-angels"
)

add_body_classes <- function(html, classes) {
  body_match <- regexpr("<body[^>]*>", html, perl = TRUE)
  if (body_match[[1L]] < 0L) return(html)

  body_tag <- regmatches(html, body_match)
  # The legacy pages do not use body-level data attributes. Reconstructing this
  # tag is safer than trying to append into several generations of Quarto HTML.
  existing <- if (grepl("\\bnav-fixed\\b", body_tag, perl = TRUE)) "nav-fixed" else character()
  replacement <- paste0(
    '<body class="',
    paste(unique(c(existing, classes)), collapse = " "),
    '">'
  )

  regmatches(html, body_match) <- replacement
  html
}

ensure_stylesheet <- function(html, href) {
  if (grepl(href, html, fixed = TRUE)) return(html)
  link <- paste0('<link rel="stylesheet" href="', href, '">')
  sub("</head>", paste0(link, "\n</head>"), html, fixed = TRUE)
}

add_archive_nav <- function(html) {
  if (grepl('class="legacy-article-nav"', html, fixed = TRUE)) return(html)
  nav <- paste0(
    '<nav class="legacy-article-nav" aria-label="Research article navigation">',
    '<a class="legacy-article-nav__brand" href="../index.html">The SABRhood</a>',
    '<a class="legacy-article-nav__back" href="../blog.html">Research archive</a>',
    "</nav>"
  )

  if (grepl('<div id="quarto-content"', html, fixed = TRUE)) {
    return(sub('<div id="quarto-content"', paste0(nav, '\n<div id="quarto-content"'), html, fixed = TRUE))
  }
  sub("<body[^>]*>", paste0("\\0\n", nav), html, perl = TRUE)
}

repair_mojibake <- function(html) {
  replacements <- c(
    "â€™" = "’",
    "â€˜" = "‘",
    "â€œ" = "“",
    "â€" = "”",
    "â€”" = "—",
    "â€“" = "–",
    "Â·" = "·",
    "Ã±" = "ñ",
    "Ã©" = "é"
  )
  for (bad in names(replacements)) {
    html <- gsub(bad, replacements[[bad]], html, fixed = TRUE)
  }
  html
}

paths <- list.files(article_root, pattern = "[.]html$", full.names = TRUE)
updated <- 0L

for (path in paths) {
  stem <- tools::file_path_sans_ext(basename(path))
  theme <- unname(article_themes[[stem]] %||% "league")
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  original <- html

  html <- ensure_stylesheet(html, "../styles.css")
  html <- ensure_stylesheet(html, "../includes/article-team-themes.css")
  html <- add_body_classes(html, c("article-page", "legacy-article-page", paste0("theme-", theme)))
  html <- add_archive_nav(html)
  html <- repair_mojibake(html)

  if (!identical(html, original)) {
    writeLines(html, path, useBytes = TRUE)
    updated <- updated + 1L
  }
}

cat("Normalized", length(paths), "legacy research articles;", updated, "updated.\n")
