release_store_contract <- function() {
  list(
    version = 1L,
    components = list(
      private_state = c(
        ".private-data/pbp",
        ".private-data/sources",
        ".private-data/projection-ledger",
        ".private-data/models"
      ),
      public_data = c(
        "data/derived",
        "images/graphics-feed"
      ),
      site = "docs"
    ),
    required_files = c(
      "data/derived/manifest.csv",
      "data/derived/refresh-health.csv",
      "docs/index.html"
    )
  )
}

release_normalize_path <- function(path) {
  gsub("\\\\", "/", path)
}

release_relative_path <- function(path, root) {
  normalized_path <- release_normalize_path(normalizePath(path, mustWork = TRUE))
  normalized_root <- release_normalize_path(normalizePath(root, mustWork = TRUE))
  prefix <- paste0(normalized_root, "/")

  if (!startsWith(normalized_path, prefix)) {
    stop("Release input is outside the repository: ", path, call. = FALSE)
  }

  substring(normalized_path, nchar(prefix) + 1L)
}

release_component_files <- function(repository_root, component_roots) {
  paths <- character()

  for (relative_root in component_roots) {
    absolute_root <- file.path(repository_root, relative_root)
    if (!dir.exists(absolute_root) && !file.exists(absolute_root)) {
      next
    }

    if (dir.exists(absolute_root)) {
      paths <- c(
        paths,
        list.files(
          absolute_root,
          recursive = TRUE,
          full.names = TRUE,
          all.files = TRUE,
          no.. = TRUE
        )
      )
    } else {
      paths <- c(paths, absolute_root)
    }
  }

  paths <- unique(paths[file.exists(paths) & !dir.exists(paths)])
  paths[order(release_normalize_path(paths))]
}

release_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest R package is required to build a release.", call. = FALSE)
  }
  digest::digest(path, file = TRUE, algo = "sha256", serialize = FALSE)
}

release_inventory <- function(repository_root, include_hashes = FALSE) {
  repository_root <- normalizePath(repository_root, mustWork = TRUE)
  contract <- release_store_contract()
  components <- list()

  for (component_name in names(contract$components)) {
    paths <- release_component_files(
      repository_root,
      contract$components[[component_name]]
    )
    relative_paths <- if (length(paths)) {
      vapply(
        paths,
        release_relative_path,
        character(1),
        root = repository_root
      )
    } else {
      character()
    }
    sizes <- if (length(paths)) {
      as.numeric(file.info(paths)$size)
    } else {
      numeric()
    }
    hashes <- if (isTRUE(include_hashes) && length(paths)) {
      vapply(paths, release_sha256, character(1))
    } else {
      rep(NA_character_, length(paths))
    }

    components[[component_name]] <- data.frame(
      path = relative_paths,
      bytes = sizes,
      sha256 = hashes,
      stringsAsFactors = FALSE
    )
  }

  components
}

validate_release_inputs <- function(repository_root) {
  contract <- release_store_contract()
  missing <- contract$required_files[
    !file.exists(file.path(repository_root, contract$required_files))
  ]
  if (length(missing)) {
    stop(
      "Release inputs are incomplete. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  health_path <- file.path(repository_root, "data", "derived", "refresh-health.csv")
  health <- utils::read.csv(health_path, stringsAsFactors = FALSE)
  if (!all(c("product_group", "status") %in% names(health))) {
    stop("refresh-health.csv does not contain the expected columns.", call. = FALSE)
  }
  failing <- health$product_group[is.na(health$status) | health$status != "current"]
  if (length(failing)) {
    stop(
      "Release health gate failed for: ",
      paste(unique(failing), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

release_key_default <- function(now = Sys.time()) {
  paste0(format(now, "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", Sys.getpid())
}

validate_release_key <- function(release_key) {
  if (!is.character(release_key) || length(release_key) != 1L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", release_key)) {
    stop(
      "release_key must use only letters, numbers, periods, underscores, and hyphens.",
      call. = FALSE
    )
  }
  invisible(release_key)
}

copy_release_component <- function(repository_root, destination_root, inventory) {
  if (!nrow(inventory)) {
    return(invisible(NULL))
  }

  for (index in seq_len(nrow(inventory))) {
    source <- file.path(repository_root, inventory$path[[index]])
    destination <- file.path(destination_root, inventory$path[[index]])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = FALSE, copy.date = TRUE)) {
      stop("Could not copy release input: ", inventory$path[[index]], call. = FALSE)
    }
  }

  invisible(NULL)
}

build_local_release <- function(
    repository_root,
    store_root = file.path(repository_root, ".backend", "release-store"),
    release_key = release_key_default()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite R package is required to build a release.", call. = FALSE)
  }

  repository_root <- normalizePath(repository_root, mustWork = TRUE)
  validate_release_key(release_key)
  validate_release_inputs(repository_root)

  dir.create(store_root, recursive = TRUE, showWarnings = FALSE)
  store_root <- normalizePath(store_root, mustWork = TRUE)
  releases_root <- file.path(store_root, "releases")
  dir.create(releases_root, recursive = TRUE, showWarnings = FALSE)
  final_root <- file.path(releases_root, release_key)
  if (dir.exists(final_root) || file.exists(final_root)) {
    stop("Release already exists: ", release_key, call. = FALSE)
  }

  staging_root <- file.path(
    store_root,
    paste0(".staging-", release_key, "-", Sys.getpid())
  )
  if (dir.exists(staging_root)) {
    unlink(staging_root, recursive = TRUE, force = TRUE)
  }
  dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(staging_root)) {
      unlink(staging_root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  inventory <- release_inventory(repository_root, include_hashes = TRUE)
  for (component_name in names(inventory)) {
    component_root <- file.path(staging_root, "components", component_name)
    copy_release_component(
      repository_root,
      component_root,
      inventory[[component_name]]
    )
  }

  component_summary <- lapply(inventory, function(component) {
    list(
      files = nrow(component),
      bytes = sum(component$bytes),
      entries = component
    )
  })
  manifest <- list(
    contract_version = release_store_contract()$version,
    release_key = release_key,
    created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = "staged",
    components = component_summary
  )
  jsonlite::write_json(
    manifest,
    file.path(staging_root, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  if (!file.rename(staging_root, final_root)) {
    stop("Could not finalize the staged release directory.", call. = FALSE)
  }
  completed <- TRUE

  list(
    release_key = release_key,
    path = final_root,
    manifest = manifest
  )
}

promote_local_release <- function(store_root, release_key) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite R package is required to promote a release.", call. = FALSE)
  }

  validate_release_key(release_key)
  store_root <- normalizePath(store_root, mustWork = TRUE)
  release_root <- file.path(store_root, "releases", release_key)
  manifest_path <- file.path(release_root, "manifest.json")
  if (!file.exists(manifest_path)) {
    stop("Cannot promote a release without a manifest: ", release_key, call. = FALSE)
  }

  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  if (!identical(manifest$release_key, release_key)) {
    stop("Release manifest key does not match its directory.", call. = FALSE)
  }

  pointer <- list(
    release_key = release_key,
    promoted_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    manifest = release_normalize_path(
      file.path("releases", release_key, "manifest.json")
    )
  )
  pointer_path <- file.path(store_root, "current.json")
  temporary_pointer <- paste0(pointer_path, ".tmp-", Sys.getpid())
  jsonlite::write_json(
    pointer,
    temporary_pointer,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  previous_pointer <- file.path(store_root, "previous.json")
  if (file.exists(pointer_path) &&
      !file.copy(pointer_path, previous_pointer, overwrite = TRUE)) {
    unlink(temporary_pointer, force = TRUE)
    stop("Could not preserve the prior release pointer.", call. = FALSE)
  }
  if (file.exists(pointer_path)) {
    unlink(pointer_path, force = TRUE)
  }
  if (!file.rename(temporary_pointer, pointer_path)) {
    if (file.exists(previous_pointer)) {
      file.copy(previous_pointer, pointer_path, overwrite = TRUE)
    }
    stop("Could not promote the release pointer.", call. = FALSE)
  }

  invisible(pointer)
}
