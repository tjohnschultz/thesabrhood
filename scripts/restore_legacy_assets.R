site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source_root <- file.path(site_root, "legacy-assets")
output_root <- file.path(site_root, "docs")

if (!dir.exists(source_root)) quit(status = 0L)

files <- list.files(source_root, recursive = TRUE, full.names = TRUE)
files <- files[!file.info(files)$isdir]
for (source in files) {
  relative <- substring(gsub("\\\\", "/", source), nchar(gsub("\\\\", "/", source_root)) + 2L)
  destination <- file.path(output_root, relative)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = TRUE, copy.mode = FALSE)) {
    stop("Could not restore legacy asset: ", relative, call. = FALSE)
  }
}

cat("Restored", length(files), "legacy article assets.\n")
