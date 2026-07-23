retrosheet_attribution <- paste(
  "The information used here was obtained free of charge from and is",
  "copyrighted by Retrosheet. Interested parties may contact Retrosheet at",
  "20 Sunset Rd., Newark, DE 19711."
)

retrosheet_season_url <- function(season) {
  season <- as.integer(season)
  if (is.na(season) || season < 1910L || season > as.integer(format(Sys.Date(), "%Y"))) {
    stop("Retrosheet season must be between 1910 and the current year.", call. = FALSE)
  }
  sprintf("https://www.retrosheet.org/events/%deve.zip", season)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

sha256_text <- function(value) {
  digest::digest(value, algo = "sha256", serialize = FALSE)
}

read_retrosheet_record <- function(line) {
  parsed <- utils::read.csv(
    text = line,
    header = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character",
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    na.strings = character()
  )
  as.character(parsed[1L, , drop = TRUE])
}

retrosheet_date <- function(value) {
  parsed <- as.Date(value, format = "%m/%d/%Y")
  if (is.na(parsed)) parsed <- as.Date(value, format = "%Y/%m/%d")
  parsed
}

parse_retrosheet_event_files <- function(paths, source_uri, source_sha256, season) {
  games <- list()
  events <- list()
  raw_records <- list()
  game_index <- 0L
  event_index <- 0L
  raw_index <- 0L

  for (path in sort(paths)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    current_game <- NULL
    current_info <- list()
    current_event_sequence <- 0L

    flush_game <- function() {
      if (is.null(current_game)) return(invisible(NULL))
      played_on <- retrosheet_date(current_info$date %||% NA_character_)
      if (is.na(played_on)) {
        stop("Game ", current_game, " has no valid info,date record.", call. = FALSE)
      }
      game_index <<- game_index + 1L
      games[[game_index]] <<- data.frame(
        source_game_id = current_game,
        played_on = as.character(played_on),
        game_number = suppressWarnings(as.integer(current_info$number %||% "0")),
        home_team_code = current_info$hometeam %||% NA_character_,
        away_team_code = current_info$visteam %||% NA_character_,
        site_code = current_info$site %||% NA_character_,
        day_night = current_info$daynight %||% NA_character_,
        metadata_json = jsonlite::toJSON(
          current_info,
          auto_unbox = TRUE,
          null = "null"
        ),
        stringsAsFactors = FALSE
      )
      invisible(NULL)
    }

    for (line_number in seq_along(lines)) {
      line <- lines[[line_number]]
      if (!nzchar(line)) next
      fields <- read_retrosheet_record(line)
      record_type <- fields[[1L]]

      raw_index <- raw_index + 1L
      raw_id <- sprintf("%s:%d", basename(path), line_number)
      raw_records[[raw_index]] <- data.frame(
        record_type = record_type,
        provider_record_id = raw_id,
        payload_json = jsonlite::toJSON(
          list(file = basename(path), line = line_number, record = fields),
          auto_unbox = TRUE,
          null = "null"
        ),
        payload_sha256 = sha256_text(line),
        stringsAsFactors = FALSE
      )

      if (identical(record_type, "id")) {
        flush_game()
        current_game <- fields[[2L]]
        current_info <- list()
        current_event_sequence <- 0L
      } else if (identical(record_type, "info") && !is.null(current_game)) {
        key <- fields[[2L]]
        current_info[[key]] <- fields[[3L]]
      } else if (identical(record_type, "play") && !is.null(current_game)) {
        current_event_sequence <- current_event_sequence + 1L
        event_index <- event_index + 1L
        events[[event_index]] <- data.frame(
          source_event_id = sprintf("%s:%06d", current_game, current_event_sequence),
          source_game_id = current_game,
          event_sequence = current_event_sequence,
          inning = suppressWarnings(as.integer(fields[[2L]])),
          batting_side = suppressWarnings(as.integer(fields[[3L]])),
          batter_source_id = fields[[4L]],
          count_state = fields[[5L]],
          pitch_sequence = fields[[6L]],
          event_text = fields[[7L]],
          metadata_json = jsonlite::toJSON(
            list(file = basename(path), line = line_number),
            auto_unbox = TRUE
          ),
          stringsAsFactors = FALSE
        )
      }
    }
    flush_game()
  }

  bind_rows_base <- function(items, columns) {
    if (!length(items)) {
      empty <- as.data.frame(
        setNames(replicate(length(columns), character(), simplify = FALSE), columns),
        stringsAsFactors = FALSE
      )
      return(empty)
    }
    do.call(rbind, items)
  }

  games_table <- bind_rows_base(games, required_adapter_columns$games)
  events_table <- bind_rows_base(events, required_adapter_columns$events)
  raw_records_table <- bind_rows_base(raw_records, required_adapter_columns$raw_records)
  played_dates <- as.Date(games_table$played_on)

  result <- list(
    provider_key = "retrosheet",
    source_uri = source_uri,
    source_version = as.character(season),
    source_sha256 = source_sha256,
    effective_from = as.character(min(played_dates)),
    effective_through = as.character(max(played_dates)),
    games = games_table,
    events = events_table,
    raw_records = raw_records_table
  )
  validate_adapter_result(result)
  result
}

`%||%` <- function(left, right) {
  if (is.null(left) || !length(left) || is.na(left[[1L]]) || !nzchar(left[[1L]])) {
    right
  } else {
    left[[1L]]
  }
}

retrosheet_adapter <- function(season, archive_path = NULL, cache_dir = NULL) {
  season <- as.integer(season)
  source_uri <- retrosheet_season_url(season)
  owned_archive <- is.null(archive_path)

  if (owned_archive) {
    if (is.null(cache_dir)) cache_dir <- file.path(tempdir(), "sabrhood-retrosheet")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    archive_path <- file.path(cache_dir, basename(source_uri))
    if (!file.exists(archive_path)) {
      utils::download.file(source_uri, archive_path, mode = "wb", quiet = FALSE)
    }
  }
  archive_path <- normalizePath(archive_path, mustWork = TRUE)
  source_sha256 <- sha256_file(archive_path)

  extraction_dir <- tempfile(sprintf("retrosheet-%d-", season))
  dir.create(extraction_dir, recursive = TRUE)
  on.exit(unlink(extraction_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(archive_path, exdir = extraction_dir)
  event_files <- list.files(
    extraction_dir,
    pattern = "\\.EV[ANF]$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  if (!length(event_files)) {
    stop("Retrosheet archive contains no regular-season event files.", call. = FALSE)
  }

  parse_retrosheet_event_files(
    paths = event_files,
    source_uri = if (owned_archive) source_uri else paste0("file://", archive_path),
    source_sha256 = source_sha256,
    season = season
  )
}
