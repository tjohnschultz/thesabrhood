derived_dir <- file.path("data", "derived")
files <- sort(list.files(derived_dir, pattern = "\\.csv$", full.names = TRUE))
files <- files[basename(files) != "manifest.csv"]
if (!length(files)) stop("No derived CSV products are available.", call. = FALSE)
synced_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

canonical_file_signature <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  canonical <- paste0(paste(lines, collapse = "\n"), "\n")
  temporary <- tempfile(fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  writeBin(charToRaw(enc2utf8(canonical)), connection)
  close(connection)
  list(bytes = nchar(canonical, type = "bytes"), md5 = unname(tools::md5sum(temporary)))
}

rows <- lapply(files, function(path) {
  product <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  signature <- canonical_file_signature(path)
  data.frame(
    file = basename(path),
    rows = nrow(product),
    columns = ncol(product),
    bytes = signature$bytes,
    md5 = signature$md5,
    synced_at_utc = synced_at,
    stringsAsFactors = FALSE
  )
})
manifest <- do.call(rbind, rows)
utils::write.csv(manifest, file.path(derived_dir, "manifest.csv"), row.names = FALSE)
cat("Updated derived-data manifest for", nrow(manifest), "products.\n")
