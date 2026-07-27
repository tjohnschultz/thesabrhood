site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
article_root <- Sys.getenv(
  "SABRHOOD_ARCHIVE_OUTPUT_ROOT",
  unset = file.path(site_root, "docs", "posts")
)

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

if (!dir.exists(article_root)) {
  cat("No rendered archive article directory found; nothing to normalize.\n")
  quit(status = 0L)
}

# Article copy lives in immutable HTML snapshots under legacy-assets/posts.
# This map controls presentation only; unknown future articles use the league
# palette until an explicit team theme is assigned.
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

add_classes_to_tag <- function(tag, classes) {
  class_match <- regexpr('class="[^"]*"', tag, perl = TRUE)
  if (class_match[[1L]] >= 0L) {
    class_attr <- regmatches(tag, class_match)
    existing <- strsplit(gsub('^class="|"$', "", class_attr), "\\s+")[[1L]]
    replacement <- paste0('class="', paste(unique(c(existing, classes)), collapse = " "), '"')
    regmatches(tag, class_match) <- replacement
    return(tag)
  }
  sub(
    ">$",
    paste0(' class="', paste(classes, collapse = " "), '">'),
    tag,
    perl = TRUE
  )
}

add_body_classes <- function(html, classes) {
  body_match <- regexpr("<body[^>]*>", html, perl = TRUE)
  if (body_match[[1L]] < 0L) return(html)
  body_tag <- regmatches(html, body_match)
  regmatches(html, body_match) <- add_classes_to_tag(body_tag, classes)
  html
}

ensure_stylesheet <- function(html, href) {
  if (grepl(href, html, fixed = TRUE)) return(html)
  link <- paste0('<link rel="stylesheet" href="', href, '">')
  sub("</head>", paste0(link, "\n</head>"), html, fixed = TRUE)
}

canonicalize_asset_paths <- function(html) {
  stylesheet_replacements <- c(
    'href="[^"]*(?:site_libs|libs)/bootstrap/bootstrap[^"]*[.]min[.]css"' =
      'href="../site_libs/bootstrap/bootstrap.min.css"',
    'href="[^"]*(?:site_libs|libs)/bootstrap/bootstrap-icons[.]css"' =
      'href="../site_libs/bootstrap/bootstrap-icons.css"',
    'href="[^"]*(?:site_libs|libs)/quarto-html/tippy[.]css"' =
      'href="../site_libs/quarto-html/tippy.css"',
    'href="[^"]*(?:site_libs|libs)/quarto-html/quarto-syntax-highlighting[^"]*[.]css"' =
      'href="../site_libs/quarto-html/quarto-syntax-highlighting.css"',
    'href="[^"]*(?:site_libs|libs)/lightable-0[.]0[.]1/lightable[.]css"' =
      'href="../site_libs/lightable-0.0.1/lightable.css"'
  )
  script_replacements <- c(
    'src="[^"]*(?:site_libs|libs)/clipboard/clipboard[.]min[.]js"' =
      'src="../site_libs/clipboard/clipboard.min.js"',
    'src="[^"]*(?:site_libs|libs)/quarto-html/quarto[.]js"' =
      'src="../site_libs/quarto-html/quarto.js"',
    'src="[^"]*(?:site_libs|libs)/quarto-html/popper[.]min[.]js"' =
      'src="../site_libs/quarto-html/popper.min.js"',
    'src="[^"]*(?:site_libs|libs)/quarto-html/tippy[.]umd[.]min[.]js"' =
      'src="../site_libs/quarto-html/tippy.umd.min.js"',
    'src="[^"]*(?:site_libs|libs)/quarto-html/anchor[.]min[.]js"' =
      'src="../site_libs/quarto-html/anchor.min.js"',
    'src="[^"]*(?:site_libs|libs)/bootstrap/bootstrap[.]min[.]js"' =
      'src="../site_libs/bootstrap/bootstrap.min.js"',
    'src="[^"]*(?:site_libs|libs)/kePrint-0[.]0[.]1/kePrint[.]js"' =
      'src="../site_libs/kePrint-0.0.1/kePrint.js"'
  )
  for (pattern in names(stylesheet_replacements)) {
    html <- gsub(pattern, stylesheet_replacements[[pattern]], html, perl = TRUE)
  }
  for (pattern in names(script_replacements)) {
    html <- gsub(pattern, script_replacements[[pattern]], html, perl = TRUE)
  }
  html
}

strip_legacy_page_styles <- function(html) {
  matches <- gregexpr("(?s)<style[^>]*>.*?</style>", html, perl = TRUE)
  blocks <- regmatches(html, matches)[[1L]]
  if (!length(blocks) || identical(blocks, character(0))) return(html)

  replacements <- vapply(blocks, function(block) {
    if (grepl(
      "body\\s*\\{|main[.]content\\s*\\{|h1[.]title|[.]quarto-title",
      block,
      perl = TRUE
    )) "" else block
  }, character(1))
  regmatches(html, matches) <- list(replacements)
  html
}

promote_article_masthead <- function(html) {
  header_matches <- gregexpr("<header[^>]*>", html, perl = TRUE)[[1L]]
  if (header_matches[[1L]] < 0L) return(html)
  header_tags <- regmatches(html, list(header_matches))[[1L]]
  target <- which(grepl('id="title-block-header"', header_tags, fixed = TRUE))[1L]
  if (is.na(target)) return(html)
  header_tags[[target]] <- add_classes_to_tag(header_tags[[target]], "article-masthead")
  regmatches(html, list(header_matches)) <- list(header_tags)
  html
}

extract_fragment <- function(html, pattern, label) {
  fragment <- regmatches(html, regexpr(pattern, html, perl = TRUE))
  if (!length(fragment) || !nzchar(fragment)) {
    stop("Current site output is missing its ", label, ".", call. = FALSE)
  }
  fragment
}

rewrite_site_chrome_for_article <- function(fragment) {
  fragment <- gsub(
    '((?:href|src)=")[.]/',
    "\\1../",
    fragment,
    perl = TRUE
  )
  fragment <- sub(
    'class="nav-link active" href="../index.html" aria-current="page"',
    'class="nav-link" href="../index.html"',
    fragment,
    fixed = TRUE
  )
  sub(
    'class="nav-link" href="../blog.html"',
    'class="nav-link active" href="../blog.html" aria-current="page"',
    fragment,
    fixed = TRUE
  )
}

load_current_site_chrome <- function() {
  reference_candidates <- unique(c(
    file.path(dirname(article_root), "index.html"),
    file.path(site_root, "docs", "index.html")
  ))
  reference_path <- reference_candidates[file.exists(reference_candidates)][1L]
  if (is.na(reference_path)) {
    stop("Rendered index.html is required to normalize archived articles.", call. = FALSE)
  }
  reference <- paste(
    readLines(reference_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  list(
    header = rewrite_site_chrome_for_article(extract_fragment(
      reference,
      '(?s)<header id="quarto-header"[^>]*>.*?</header>',
      "navbar"
    )),
    footer = rewrite_site_chrome_for_article(extract_fragment(
      reference,
      '(?s)<footer class="footer"[^>]*>.*?</footer>',
      "footer"
    ))
  )
}

upsert_current_site_chrome <- function(html, chrome) {
  legacy_nav_pattern <- '(?s)<nav class="legacy-article-nav"[^>]*>.*?</nav>'
  html <- gsub(legacy_nav_pattern, "", html, perl = TRUE)

  header_pattern <- '(?s)<header id="quarto-header"[^>]*>.*?</header>'
  if (grepl(header_pattern, html, perl = TRUE)) {
    html <- sub(header_pattern, chrome$header, html, perl = TRUE)
  } else {
    body_tag <- regmatches(html, regexpr("<body[^>]*>", html, perl = TRUE))
    if (!length(body_tag) || !nzchar(body_tag)) {
      stop("Archived article is missing its body tag.", call. = FALSE)
    }
    html <- sub(body_tag, paste0(body_tag, "\n", chrome$header), html, fixed = TRUE)
  }

  footer_pattern <- '(?s)<footer class="footer"[^>]*>.*?</footer>'
  if (grepl(footer_pattern, html, perl = TRUE)) {
    return(sub(footer_pattern, chrome$footer, html, perl = TRUE))
  }
  sub("</body>", paste0(chrome$footer, "\n</body>"), html, fixed = TRUE)
}

wrap_article_body <- function(html) {
  if (grepl(
    '<article[^>]*class="[^"]*legacy-article-body[^"]*"',
    html,
    perl = TRUE
  )) return(html)

  masthead_pattern <- paste0(
    '(?s)(<header[^>]*class="[^"]*article-masthead[^"]*"[^>]*>',
    ".*?</header>)"
  )
  if (!grepl(masthead_pattern, html, perl = TRUE)) return(html)

  html <- sub(
    masthead_pattern,
    '\\1\n<article class="legacy-article-body">',
    html,
    perl = TRUE
  )
  sub("</main>", "</article>\n</main>", html, fixed = TRUE)
}

paths <- list.files(article_root, pattern = "[.]html$", full.names = TRUE)
site_chrome <- load_current_site_chrome()
updated <- 0L

for (path in paths) {
  stem <- tools::file_path_sans_ext(basename(path))
  theme <- unname(article_themes[[stem]] %||% "league")
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  original <- html

  html <- canonicalize_asset_paths(html)
  html <- ensure_stylesheet(html, "../site_libs/bootstrap/bootstrap.min.css")
  html <- ensure_stylesheet(html, "../styles.css")
  html <- ensure_stylesheet(html, "../includes/article-team-themes.css")
  html <- strip_legacy_page_styles(html)
  html <- promote_article_masthead(html)
  html <- add_body_classes(
    html,
    c("nav-fixed", "article-page", "legacy-article-page", paste0("theme-", theme))
  )
  html <- upsert_current_site_chrome(html, site_chrome)
  html <- wrap_article_body(html)

  if (!identical(html, original)) {
    writeLines(html, path, useBytes = TRUE)
    updated <- updated + 1L
  }
}

cat("Normalized", length(paths), "archive research articles;", updated, "updated.\n")
