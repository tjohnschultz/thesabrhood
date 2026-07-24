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

supabase_storage_download_url <- function(config, object_path) {
  paste0(
    config$url,
    "/storage/v1/object/authenticated/",
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
    timeout_seconds = 600L) {
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

supabase_storage_download_raw <- function(
    config,
    object_path,
    timeout_seconds = 600L) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The httr R package is required for Supabase downloads.", call. = FALSE)
  }

  response <- httr::GET(
    supabase_storage_download_url(config, object_path),
    httr::add_headers(.headers = c(
      apikey = config$secret_key,
      `cache-control` = "no-store"
    )),
    httr::timeout(timeout_seconds)
  )
  status <- httr::status_code(response)
  if (status < 200L || status >= 300L) {
    detail <- httr::content(response, as = "text", encoding = "UTF-8")
    if (nchar(detail) > 500L) {
      detail <- paste0(substr(detail, 1L, 500L), "...")
    }
    stop(
      "Could not download ",
      object_path,
      " from Supabase Storage (HTTP ",
      status,
      ")",
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }

  httr::content(response, as = "raw")
}

supabase_storage_list_raw <- function(
    config,
    prefix,
    limit = 1000L,
    offset = 0L,
    request = NULL,
    timeout_seconds = 120L) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("httr and jsonlite are required for Supabase object listing.", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L ||
      is.na(prefix) || startsWith(prefix, "/") ||
      grepl("(^|/)[.][.]($|/)", prefix)) {
    stop("The Supabase list prefix is invalid.", call. = FALSE)
  }
  limit <- as.integer(limit)
  offset <- as.integer(offset)
  if (is.na(limit) || limit < 1L || limit > 1000L ||
      is.na(offset) || offset < 0L) {
    stop("List limit and offset are invalid.", call. = FALSE)
  }

  if (is.null(request)) {
    request <- function(url, headers, body, timeout_seconds) {
      httr::POST(
        url,
        httr::add_headers(.headers = headers),
        httr::timeout(timeout_seconds),
        body = charToRaw(jsonlite::toJSON(
          body,
          auto_unbox = TRUE,
          na = "null"
        )),
        encode = "raw"
      )
    }
  }
  response <- request(
    url = paste0(
      config$url,
      "/storage/v1/object/list/",
      utils::URLencode(config$bucket, reserved = TRUE)
    ),
    headers = c(
      apikey = config$secret_key,
      `Content-Type` = "application/json",
      `cache-control` = "no-store"
    ),
    body = list(
      prefix = release_normalize_path(prefix),
      limit = limit,
      offset = offset,
      sortBy = list(column = "name", order = "asc")
    ),
    timeout_seconds = timeout_seconds
  )

  if (is.list(response) && !inherits(response, "response")) {
    return(response)
  }
  status <- httr::status_code(response)
  if (status < 200L || status >= 300L) {
    detail <- httr::content(response, as = "text", encoding = "UTF-8")
    if (nchar(detail) > 500L) {
      detail <- paste0(substr(detail, 1L, 500L), "...")
    }
    stop(
      "Could not list Supabase Storage objects (HTTP ",
      status,
      ")",
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }

  jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

list_supabase_releases <- function(
    config = supabase_storage_config(),
    list_objects = supabase_storage_list_raw,
    download = supabase_storage_download_raw) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("jsonlite and digest are required for release inventory.", call. = FALSE)
  }
  release_entries <- list()
  offset <- 0L
  page_size <- 1000L
  repeat {
    page <- list_objects(
      config = config,
      prefix = "releases",
      limit = page_size,
      offset = offset
    )
    release_entries <- c(release_entries, page)
    if (length(page) < page_size) {
      break
    }
    offset <- offset + page_size
    if (offset >= 10000L) {
      stop("Release inventory exceeded the 10,000-entry safety limit.", call. = FALSE)
    }
  }

  keys <- unique(vapply(release_entries, function(entry) {
    if (is.null(entry$name)) "" else as.character(entry$name)
  }, character(1)))
  keys <- keys[grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", keys)]
  keys <- sort(keys)
  if (!length(keys)) {
    return(data.frame(
      release_key = character(),
      uploaded_at_utc = character(),
      files = integer(),
      objects = integer(),
      bytes = numeric(),
      status = character(),
      integrity = character(),
      private_files = integer(),
      public_files = integer(),
      site_files = integer(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(keys, function(release_key) {
    manifest_path <- paste0(
      "releases/",
      release_key,
      "/remote-manifest.json"
    )
    manifest <- tryCatch(
      jsonlite::fromJSON(
        rawToChar(download(config, manifest_path)),
        simplifyVector = FALSE
      ),
      error = function(error) NULL
    )
    if (is.null(manifest) ||
        !identical(manifest$release_key, release_key) ||
        !identical(as.integer(manifest$contract_version), 2L) ||
        is.null(manifest$uploaded_at_utc) ||
        is.null(manifest$status) ||
        is.null(manifest$files)) {
      return(data.frame(
        release_key = release_key,
        uploaded_at_utc = NA_character_,
        files = NA_integer_,
        objects = NA_integer_,
        bytes = NA_real_,
        status = "unreadable",
        integrity = "unreadable",
        private_files = NA_integer_,
        public_files = NA_integer_,
        site_files = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    remote_paths <- vapply(
      manifest$files,
      function(file_record) as.character(file_record$path),
      character(1)
    )
    local_record_index <- match("manifest.json", remote_paths)
    local_manifest <- if (is.na(local_record_index)) {
      NULL
    } else {
      tryCatch({
        local_record <- manifest$files[[local_record_index]]
        local_body <- do.call(c, lapply(local_record$parts, function(part) {
          part_body <- download(config, part$object_path)
          if (length(part_body) != as.numeric(part$bytes) ||
              !identical(
                digest::digest(
                  part_body,
                  algo = "sha256",
                  serialize = FALSE
                ),
                part$sha256
              )) {
            stop("Local manifest part failed verification.", call. = FALSE)
          }
          part_body
        }))
        if (length(local_body) != as.numeric(local_record$bytes) ||
            !identical(
              digest::digest(
                local_body,
                algo = "sha256",
                serialize = FALSE
              ),
              local_record$sha256
            )) {
          stop("Local manifest failed verification.", call. = FALSE)
        }
        jsonlite::fromJSON(rawToChar(local_body), simplifyVector = FALSE)
      }, error = function(error) NULL)
    }
    component_names <- c("private_state", "public_data", "site")
    component_counts <- setNames(rep(NA_integer_, 3L), component_names)
    valid_local_manifest <- !is.null(local_manifest) &&
      identical(local_manifest$release_key, release_key) &&
      identical(as.integer(local_manifest$contract_version), 2L) &&
      !is.null(local_manifest$components)
    if (valid_local_manifest) {
      component_counts <- vapply(component_names, function(component_name) {
        component <- local_manifest$components[[component_name]]
        if (is.null(component) || is.null(component$files)) {
          0L
        } else {
          as.integer(component$files)
        }
      }, integer(1))
    }
    integrity <- if (!valid_local_manifest ||
        any(is.na(component_counts))) {
      "unreadable"
    } else if (all(component_counts > 0L)) {
      "complete"
    } else {
      "incomplete"
    }
    object_count <- sum(vapply(
      manifest$files,
      function(file_record) length(file_record$parts),
      integer(1)
    )) + 1L
    data.frame(
      release_key = release_key,
      uploaded_at_utc = as.character(manifest$uploaded_at_utc),
      files = length(manifest$files),
      objects = object_count,
      bytes = sum(vapply(
        manifest$files,
        function(file_record) as.numeric(file_record$bytes),
        numeric(1)
      )),
      status = as.character(manifest$status),
      integrity = integrity,
      private_files = component_counts[["private_state"]],
      public_files = component_counts[["public_data"]],
      site_files = component_counts[["site"]],
      stringsAsFactors = FALSE
    )
  })
  inventory <- do.call(rbind, rows)
  order_value <- ifelse(
    is.na(inventory$uploaded_at_utc),
    "",
    inventory$uploaded_at_utc
  )
  inventory[order(order_value, decreasing = TRUE), , drop = FALSE]
}

plan_supabase_retention <- function(
    inventory,
    keep_complete = 2L,
    protect = character()) {
  required_columns <- c(
    "release_key",
    "uploaded_at_utc",
    "bytes",
    "status",
    "integrity"
  )
  if (!all(required_columns %in% names(inventory))) {
    stop("Release inventory does not satisfy the retention contract.", call. = FALSE)
  }
  keep_complete <- as.integer(keep_complete)
  if (is.na(keep_complete) || keep_complete < 1L || keep_complete > 100L) {
    stop("keep_complete must be between 1 and 100.", call. = FALSE)
  }
  protect <- unique(protect[nzchar(protect)])
  if (length(protect)) {
    invisible(lapply(protect, validate_release_key))
  }

  newest_complete <- head(
    inventory$release_key[inventory$integrity == "complete"],
    keep_complete
  )
  protected <- unique(c(protect, newest_complete))
  action <- rep("delete-candidate", nrow(inventory))
  reason <- rep("older unprotected staged release", nrow(inventory))

  explicitly_protected <- inventory$release_key %in% protect
  newest_protected <- inventory$release_key %in% newest_complete
  unreadable <- inventory$integrity == "unreadable"
  not_staged <- inventory$status != "staged"
  action[explicitly_protected | newest_protected | unreadable | not_staged] <- "retain"
  reason[explicitly_protected] <- "explicitly protected"
  reason[newest_protected] <- "newest complete release"
  reason[unreadable] <- "unreadable releases require investigation"
  reason[not_staged] <- "non-staged status"

  data.frame(
    release_key = inventory$release_key,
    uploaded_at_utc = inventory$uploaded_at_utc,
    integrity = inventory$integrity,
    mebibytes = round(inventory$bytes / 1024^2, 2L),
    action = action,
    reason = reason,
    stringsAsFactors = FALSE
  )
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
  manifest_path <- file.path(release_root, "manifest.json")
  if (!file.exists(manifest_path)) {
    stop("The staged release does not contain manifest.json.", call. = FALSE)
  }
  if (!is.numeric(chunk_bytes) || length(chunk_bytes) != 1L ||
      is.na(chunk_bytes) || chunk_bytes < 1024L) {
    stop("chunk_bytes must be at least 1024.", call. = FALSE)
  }

  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  if (is.null(manifest$contract_version) || manifest$contract_version < 2L ||
      is.null(manifest$packages)) {
    stop(
      "The staged release predates the packaged upload contract. Restage it ",
      "with the current backend release tool.",
      call. = FALSE
    )
  }
  package_paths <- if (is.data.frame(manifest$packages)) {
    manifest$packages$path
  } else {
    vapply(manifest$packages, function(package) package$path, character(1))
  }
  files <- c(manifest_path, file.path(release_root, package_paths))
  if (any(!file.exists(files)) || any(dir.exists(files))) {
    stop("One or more packaged release artifacts are missing.", call. = FALSE)
  }
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
    upload = supabase_storage_upload_raw,
    progress = NULL) {
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
      upsert = TRUE
    )
    if (is.function(progress)) {
      progress(
        object_path = object_path,
        bytes = length(body),
        part_number = part_number
      )
    }
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
    upload = supabase_storage_upload_raw,
    progress = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("jsonlite and digest are required for release uploads.", call. = FALSE)
  }

  release_root <- normalizePath(release_root, mustWork = TRUE)
  local_manifest <- jsonlite::read_json(
    file.path(release_root, "manifest.json"),
    simplifyVector = TRUE
  )
  if (local_manifest$contract_version < 2L) {
    stop("Release must use packaged storage contract version 2.", call. = FALSE)
  }
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
      upload = upload,
      progress = progress
    )
  }

  remote_manifest <- list(
    contract_version = 2L,
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
    upsert = TRUE
  )
  if (is.function(progress)) {
    progress(
      object_path = remote_manifest_path,
      bytes = length(charToRaw(jsonlite::toJSON(
        remote_manifest,
        auto_unbox = TRUE,
        pretty = TRUE,
        na = "null"
      ))),
      part_number = 1L
    )
  }

  list(
    release_key = release_key,
    remote_manifest_path = remote_manifest_path,
    manifest = remote_manifest
  )
}

download_verified_supabase_release <- function(
    release_key,
    output_root,
    config = supabase_storage_config(),
    local_release_root = NULL,
    download = supabase_storage_download_raw,
    progress = NULL,
    include_paths = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("jsonlite and digest are required for release verification.", call. = FALSE)
  }
  validate_release_key(release_key)
  remote_manifest_path <- paste0(
    "releases/",
    release_key,
    "/remote-manifest.json"
  )
  manifest_body <- download(config, remote_manifest_path)
  manifest <- jsonlite::fromJSON(
    rawToChar(manifest_body),
    simplifyVector = FALSE
  )
  if (!identical(manifest$release_key, release_key) ||
      !identical(as.integer(manifest$contract_version), 2L) ||
      !identical(manifest$status, "staged")) {
    stop("Remote release manifest failed its identity checks.", call. = FALSE)
  }

  if (!is.null(local_release_root)) {
    local_release_root <- normalizePath(local_release_root, mustWork = TRUE)
  }
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  output_root <- normalizePath(output_root, mustWork = TRUE)
  if (length(list.files(output_root, all.files = TRUE, no.. = TRUE))) {
    stop("Verified-download output directory must be empty.", call. = FALSE)
  }
  verified_bytes <- 0
  verified_objects <- 1L
  file_records <- manifest$files
  if (!is.null(include_paths)) {
    include_paths <- unique(release_normalize_path(include_paths))
    manifest_paths <- vapply(
      file_records,
      function(record) release_normalize_path(record$path),
      character(1)
    )
    missing_paths <- setdiff(include_paths, manifest_paths)
    if (length(missing_paths)) {
      stop(
        "Remote release does not contain requested files: ",
        paste(missing_paths, collapse = ", "),
        call. = FALSE
      )
    }
    file_records <- file_records[manifest_paths %in% include_paths]
  }

  for (file_record in file_records) {
    relative_path <- release_normalize_path(file_record$path)
    if (grepl("(^|/)[.][.]($|/)", relative_path) ||
        startsWith(relative_path, "/")) {
      stop("Remote manifest contains an unsafe file path.", call. = FALSE)
    }
    restored_path <- file.path(output_root, relative_path)
    dir.create(dirname(restored_path), recursive = TRUE, showWarnings = FALSE)
    output <- file(restored_path, open = "wb")
    tryCatch(
      {
        for (part in file_record$parts) {
          body <- download(config, part$object_path)
          observed_part_sha <- digest::digest(
            body,
            algo = "sha256",
            serialize = FALSE
          )
          if (length(body) != as.numeric(part$bytes) ||
              !identical(observed_part_sha, part$sha256)) {
            stop(
              "Remote part failed checksum verification: ",
              part$object_path,
              call. = FALSE
            )
          }
          writeBin(body, output)
          verified_bytes <- verified_bytes + length(body)
          verified_objects <- verified_objects + 1L
          if (is.function(progress)) {
            progress(
              object_path = part$object_path,
              bytes = length(body),
              status = "verified"
            )
          }
        }
      },
      finally = {
        close(output)
      }
    )

    observed_file_bytes <- as.numeric(file.info(restored_path)$size)
    observed_file_sha <- release_sha256(restored_path)
    if (observed_file_bytes != as.numeric(file_record$bytes) ||
        !identical(observed_file_sha, file_record$sha256)) {
      stop(
        "Reconstructed file failed checksum verification: ",
        relative_path,
        call. = FALSE
      )
    }

    if (!is.null(local_release_root)) {
      local_path <- file.path(local_release_root, relative_path)
      if (!file.exists(local_path) ||
          !identical(release_sha256(local_path), observed_file_sha)) {
        stop(
          "Remote file does not match the local staged release: ",
          relative_path,
          call. = FALSE
        )
      }
    }
  }

  list(
    release_key = release_key,
    manifest_path = remote_manifest_path,
    files = length(file_records),
    objects = verified_objects,
    bytes = verified_bytes,
    status = "verified",
    manifest = manifest,
    output_root = output_root
  )
}

verify_supabase_release <- function(
    release_key,
    config = supabase_storage_config(),
    local_release_root = NULL,
    download = supabase_storage_download_raw,
    progress = NULL) {
  verification_root <- tempfile(paste0("sabrhood-verify-", release_key, "-"))
  dir.create(verification_root, recursive = TRUE)
  on.exit(unlink(verification_root, recursive = TRUE, force = TRUE), add = TRUE)

  download_verified_supabase_release(
    release_key = release_key,
    output_root = verification_root,
    config = config,
    local_release_root = local_release_root,
    download = download,
    progress = progress
  )
}

restore_supabase_release <- function(
    release_key,
    target_root,
    components = "private_state",
    config = supabase_storage_config(),
    download = supabase_storage_download_raw,
    progress = NULL) {
  allowed_components <- names(release_store_contract()$components)
  components <- unique(components)
  invalid_components <- setdiff(components, allowed_components)
  if (!length(components) || length(invalid_components)) {
    stop(
      "Restore components must be selected from: ",
      paste(allowed_components, collapse = ", "),
      call. = FALSE
    )
  }

  target_root <- path.expand(target_root)
  target_parent <- dirname(target_root)
  dir.create(target_parent, recursive = TRUE, showWarnings = FALSE)
  target_parent <- normalizePath(target_parent, mustWork = TRUE)
  target_root <- file.path(target_parent, basename(target_root))
  if (dir.exists(target_root) || file.exists(target_root)) {
    stop("Restore target already exists: ", target_root, call. = FALSE)
  }

  download_root <- tempfile(paste0("sabrhood-download-", release_key, "-"))
  dir.create(download_root, recursive = TRUE)
  on.exit(unlink(download_root, recursive = TRUE, force = TRUE), add = TRUE)
  downloaded <- download_verified_supabase_release(
    release_key = release_key,
    output_root = download_root,
    config = config,
    download = download,
    progress = progress,
    include_paths = c(
      "manifest.json",
      paste0("packages/", components, ".tar.gz")
    )
  )
  local_manifest <- jsonlite::read_json(
    file.path(download_root, "manifest.json"),
    simplifyVector = FALSE
  )

  staging_target <- file.path(
    target_parent,
    paste0(".restore-staging-", release_key, "-", Sys.getpid())
  )
  if (dir.exists(staging_target) || file.exists(staging_target)) {
    stop("Restore staging path already exists.", call. = FALSE)
  }
  dir.create(staging_target, recursive = TRUE)
  restored_files <- 0L
  restored_bytes <- 0
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(staging_target)) {
      unlink(staging_target, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  for (component in components) {
    package_path <- file.path(
      download_root,
      "packages",
      paste0(component, ".tar.gz")
    )
    if (!file.exists(package_path)) {
      stop("Release is missing package for component: ", component, call. = FALSE)
    }
    archive_entries <- release_normalize_path(utils::untar(
      package_path,
      list = TRUE,
      tar = "internal"
    ))
    component_prefix <- paste0("components/", component, "/")
    permitted_parents <- c("components", paste0("components/", component))
    unsafe_entries <- archive_entries[
      grepl("(^|/)[.][.]($|/)", archive_entries) |
        startsWith(archive_entries, "/") |
        !(archive_entries %in% permitted_parents |
            startsWith(archive_entries, component_prefix))
    ]
    if (length(unsafe_entries)) {
      stop("Package contains paths outside its component root.", call. = FALSE)
    }

    unpack_root <- tempfile(paste0("sabrhood-unpack-", component, "-"))
    dir.create(unpack_root)
    on.exit(unlink(unpack_root, recursive = TRUE, force = TRUE), add = TRUE)
    utils::untar(
      package_path,
      exdir = unpack_root,
      tar = "internal"
    )
    source_root <- file.path(unpack_root, "components", component)
    expected_entries <- local_manifest$components[[component]]$entries
    for (entry in expected_entries) {
      relative_path <- release_normalize_path(entry$path)
      if (grepl("(^|/)[.][.]($|/)", relative_path) ||
          startsWith(relative_path, "/")) {
        stop("Local manifest contains an unsafe restore path.", call. = FALSE)
      }
      source_path <- file.path(source_root, relative_path)
      destination <- file.path(staging_target, relative_path)
      if (!file.exists(source_path) ||
          as.numeric(file.info(source_path)$size) != as.numeric(entry$bytes) ||
          !identical(release_sha256(source_path), entry$sha256)) {
        stop(
          "Extracted component failed file verification: ",
          relative_path,
          call. = FALSE
        )
      }
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(source_path, destination, overwrite = FALSE, copy.date = TRUE)) {
        stop("Could not write restored file: ", relative_path, call. = FALSE)
      }
      restored_files <- restored_files + 1L
      restored_bytes <- restored_bytes + as.numeric(entry$bytes)
    }
  }

  if (!file.rename(staging_target, target_root)) {
    stop("Could not finalize the isolated restore directory.", call. = FALSE)
  }
  completed <- TRUE
  list(
    release_key = release_key,
    components = components,
    files = restored_files,
    bytes = restored_bytes,
    target_root = target_root,
    verified_objects = downloaded$objects,
    status = "restored"
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
