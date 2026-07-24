supabase_storage_config <- function(
    url = Sys.getenv("SABRHOOD_SUPABASE_URL"),
    secret_key = Sys.getenv("SABRHOOD_SUPABASE_SECRET_KEY"),
    bucket = Sys.getenv(
      "SABRHOOD_SUPABASE_BUCKET",
      unset = "pipeline-releases"
    )) {
  url <- sub("/+$", "", trimws(url))
  secret_key <- trimws(secret_key)
  bucket <- trimws(bucket)

  if (!grepl("^https://[A-Za-z0-9.-]+[.]supabase[.]co$", url)) {
    stop(
      "SABRHOOD_SUPABASE_URL must be the hosted project URL, such as ",
      "https://project-ref.supabase.co.",
      call. = FALSE
    )
  }
  if (!startsWith(secret_key, "sb_secret_")) {
    stop(
      "SABRHOOD_SUPABASE_SECRET_KEY must be a backend sb_secret_ key. ",
      "Publishable and legacy anonymous keys are not accepted.",
      call. = FALSE
    )
  }
  if (!grepl("^[a-z0-9][a-z0-9-]{1,62}$", bucket)) {
    stop("The Supabase bucket name is invalid.", call. = FALSE)
  }

  list(url = url, secret_key = secret_key, bucket = bucket)
}

supabase_encode_object_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  path <- sub("^/+", "", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (!length(parts) || any(!nzchar(parts)) || any(parts %in% c(".", ".."))) {
    stop("Object paths must contain safe, non-empty segments.", call. = FALSE)
  }
  paste(
    vapply(parts, utils::URLencode, character(1), reserved = TRUE),
    collapse = "/"
  )
}

supabase_storage_object_url <- function(config, object_path) {
  paste0(
    config$url,
    "/storage/v1/object/",
    utils::URLencode(config$bucket, reserved = TRUE),
    "/",
    supabase_encode_object_path(object_path)
  )
}

supabase_storage_upload_raw <- function(
    config,
    object_path,
    body,
    content_type = "application/octet-stream",
    upsert = FALSE,
    timeout_seconds = 180L) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The httr R package is required for Supabase uploads.", call. = FALSE)
  }
  if (!is.raw(body)) {
    stop("Upload body must be a raw vector.", call. = FALSE)
  }

  response <- httr::POST(
    supabase_storage_object_url(config, object_path),
    httr::add_headers(.headers = c(
      apikey = config$secret_key,
      `Content-Type` = content_type,
      `cache-control` = "no-store",
      `x-upsert` = if (isTRUE(upsert)) "true" else "false"
    )),
    httr::timeout(timeout_seconds),
    body = body,
    encode = "raw"
  )

  status <- httr::status_code(response)
  if (status < 200L || status >= 300L) {
    detail <- httr::content(response, as = "text", encoding = "UTF-8")
    if (nchar(detail) > 500L) {
      detail <- paste0(substr(detail, 1L, 500L), "...")
    }
    stop(
      "Supabase Storage rejected ",
      object_path,
      " with HTTP ",
      status,
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }

  invisible(response)
}

supabase_storage_probe <- function(
    config = supabase_storage_config(),
    upload = supabase_storage_upload_raw,
    now = Sys.time()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite R package is required for the storage probe.", call. = FALSE)
  }

  probe_id <- paste0(
    format(now, "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "-",
    Sys.getpid()
  )
  object_path <- paste0("_connection-tests/", probe_id, ".json")
  payload <- charToRaw(jsonlite::toJSON(
    list(
      probe_id = probe_id,
      purpose = "Verify private backend write access",
      created_at_utc = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  ))
  upload(
    config = config,
    object_path = object_path,
    body = payload,
    content_type = "application/json",
    upsert = FALSE
  )

  object_path
}

release_upload_plan <- function(release_root, chunk_bytes = 40 * 1024^2) {
  release_root <- normalizePath(release_root, mustWork = TRUE)
  if (!file.exists(file.path(release_root, "manifest.json"))) {
    stop("The staged release does not contain manifest.json.", call. = FALSE)
  }
  if (!is.numeric(chunk_bytes) || length(chunk_bytes) != 1L ||
      is.na(chunk_bytes) || chunk_bytes < 1024L) {
    stop("chunk_bytes must be at least 1024.", call. = FALSE)
  }

  files <- list.files(
    release_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[file.exists(files) & !dir.exists(files)]
  relative <- vapply(
    files,
    release_relative_path,
    character(1),
    root = release_root
  )
  bytes <- as.numeric(file.info(files)$size)
  data.frame(
    path = relative,
    source = files,
    bytes = bytes,
    parts = pmax(1L, as.integer(ceiling(bytes / chunk_bytes))),
    stringsAsFactors = FALSE
  )
}

upload_release_file <- function(
    config,
    release_key,
    file_path,
    relative_path,
    chunk_bytes = 40 * 1024^2,
    upload = supabase_storage_upload_raw) {
  file_bytes <- as.numeric(file.info(file_path)$size)
  file_sha256 <- release_sha256(file_path)
  object_base <- paste0(
    "releases/",
    release_key,
    "/objects/",
    release_normalize_path(relative_path)
  )

  connection <- file(file_path, open = "rb")
  on.exit(close(connection), add = TRUE)
  part_records <- list()
  part_number <- 0L
  repeat {
    body <- readBin(connection, what = "raw", n = chunk_bytes)
    if (!length(body) && part_number > 0L) {
      break
    }
    part_number <- part_number + 1L
    part_sha256 <- digest::digest(body, algo = "sha256", serialize = FALSE)

    if (file_bytes <= chunk_bytes) {
      object_path <- object_base
    } else {
      object_path <- paste0(
        object_base,
        ".chunks/part-",
        sprintf("%05d", part_number),
        ".bin"
      )
    }
    upload(
      config = config,
      object_path = object_path,
      body = body,
      content_type = "application/octet-stream",
      upsert = FALSE
    )
    part_records[[part_number]] <- list(
      object_path = object_path,
      bytes = length(body),
      sha256 = part_sha256
    )

    if (file_bytes <= chunk_bytes || length(body) < chunk_bytes) {
      break
    }
  }

  list(
    path = release_normalize_path(relative_path),
    bytes = file_bytes,
    sha256 = file_sha256,
    storage_mode = if (file_bytes <= chunk_bytes) "object" else "chunked",
    parts = part_records
  )
}

upload_staged_release <- function(
    release_root,
    config = supabase_storage_config(),
    chunk_bytes = 40 * 1024^2,
    upload = supabase_storage_upload_raw) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("jsonlite and digest are required for release uploads.", call. = FALSE)
  }

  release_root <- normalizePath(release_root, mustWork = TRUE)
  local_manifest <- jsonlite::read_json(
    file.path(release_root, "manifest.json"),
    simplifyVector = TRUE
  )
  release_key <- local_manifest$release_key
  validate_release_key(release_key)
  plan <- release_upload_plan(release_root, chunk_bytes = chunk_bytes)
  records <- vector("list", nrow(plan))

  for (index in seq_len(nrow(plan))) {
    records[[index]] <- upload_release_file(
      config = config,
      release_key = release_key,
      file_path = plan$source[[index]],
      relative_path = plan$path[[index]],
      chunk_bytes = chunk_bytes,
      upload = upload
    )
  }

  remote_manifest <- list(
    contract_version = 1L,
    release_key = release_key,
    uploaded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = "staged",
    files = records
  )
  remote_manifest_path <- paste0(
    "releases/",
    release_key,
    "/remote-manifest.json"
  )
  upload(
    config = config,
    object_path = remote_manifest_path,
    body = charToRaw(jsonlite::toJSON(
      remote_manifest,
      auto_unbox = TRUE,
      pretty = TRUE,
      na = "null"
    )),
    content_type = "application/json",
    upsert = FALSE
  )

  list(
    release_key = release_key,
    remote_manifest_path = remote_manifest_path,
    manifest = remote_manifest
  )
}

promote_supabase_release <- function(
    release_key,
    remote_manifest_path,
    config = supabase_storage_config(),
    upload = supabase_storage_upload_raw) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite R package is required for promotion.", call. = FALSE)
  }
  validate_release_key(release_key)
  expected_path <- paste0(
    "releases/",
    release_key,
    "/remote-manifest.json"
  )
  if (!identical(release_normalize_path(remote_manifest_path), expected_path)) {
    stop("Remote manifest path does not match the release key.", call. = FALSE)
  }

  pointer <- list(
    release_key = release_key,
    manifest = expected_path,
    promoted_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  upload(
    config = config,
    object_path = "current.json",
    body = charToRaw(jsonlite::toJSON(
      pointer,
      auto_unbox = TRUE,
      pretty = TRUE
    )),
    content_type = "application/json",
    upsert = TRUE
  )

  invisible(pointer)
}
