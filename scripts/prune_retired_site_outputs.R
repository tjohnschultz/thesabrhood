site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
docs_root <- normalizePath(file.path(site_root, "docs"), winslash = "/", mustWork = TRUE)

retired <- c(
  file.path(docs_root, "team-dossiers"),
  file.path(docs_root, "team-dossiers.html"),
  file.path(docs_root, "team-dossiers_files")
)

removed <- character()
for (path in retired) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!startsWith(normalized, paste0(docs_root, "/"))) {
    stop("Refusing to prune a path outside docs: ", normalized, call. = FALSE)
  }
  if (file.exists(normalized) || dir.exists(normalized)) {
    unlink(normalized, recursive = TRUE, force = TRUE)
    if (file.exists(normalized) || dir.exists(normalized)) {
      stop("Could not remove retired output: ", normalized, call. = FALSE)
    }
    removed <- c(removed, substring(normalized, nchar(docs_root) + 2L))
  }
}

cat(
  if (length(removed)) {
    paste("Removed retired site outputs:", paste(removed, collapse = ", "))
  } else {
    "No retired site outputs were present."
  },
  "\n"
)
