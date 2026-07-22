derived_dir <- file.path("data", "derived")
files <- sort(list.files(derived_dir, pattern = "\\.csv$", full.names = TRUE))
files <- files[basename(files) != "manifest.csv"]
if (!length(files)) stop("No derived CSV products are available.", call. = FALSE)
synced_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
rows <- lapply(files, function(path) {
  product <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  info <- file.info(path)
  data.frame(
    file = basename(path),
    rows = nrow(product),
    columns = ncol(product),
    bytes = unname(info$size),
    md5 = unname(tools::md5sum(path)),
    synced_at_utc = synced_at,
    stringsAsFactors = FALSE
  )
})
manifest <- do.call(rbind, rows)
utils::write.csv(manifest, file.path(derived_dir, "manifest.csv"), row.names = FALSE)
cat("Updated derived-data manifest for", nrow(manifest), "products.\n")
