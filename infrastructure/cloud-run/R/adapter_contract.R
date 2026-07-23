empty_character <- function(size) rep(NA_character_, size)

required_adapter_columns <- list(
  games = c(
    "source_game_id", "played_on", "game_number", "home_team_code",
    "away_team_code", "site_code", "day_night", "metadata_json"
  ),
  events = c(
    "source_event_id", "source_game_id", "event_sequence", "inning",
    "batting_side", "batter_source_id", "count_state", "pitch_sequence",
    "event_text", "metadata_json"
  ),
  raw_records = c(
    "record_type", "provider_record_id", "payload_json", "payload_sha256"
  )
)

validate_adapter_result <- function(result) {
  required_fields <- c(
    "provider_key", "source_uri", "source_version", "source_sha256",
    "effective_from", "effective_through", "games", "events", "raw_records"
  )
  missing_fields <- setdiff(required_fields, names(result))
  if (length(missing_fields)) {
    stop(
      "Adapter result is missing fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(result$provider_key) || length(result$provider_key) != 1L ||
      !nzchar(result$provider_key)) {
    stop("provider_key must be one non-empty string.", call. = FALSE)
  }
  if (!grepl("^[0-9a-f]{64}$", result$source_sha256)) {
    stop("source_sha256 must be a lowercase SHA-256 digest.", call. = FALSE)
  }

  for (table_name in names(required_adapter_columns)) {
    value <- result[[table_name]]
    if (!is.data.frame(value)) {
      stop(table_name, " must be a data frame.", call. = FALSE)
    }
    missing_columns <- setdiff(required_adapter_columns[[table_name]], names(value))
    if (length(missing_columns)) {
      stop(
        table_name,
        " is missing columns: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (nrow(result$games)) {
    duplicate_games <- duplicated(result$games$source_game_id)
    if (any(duplicate_games)) {
      stop("Adapter emitted duplicate source_game_id values.", call. = FALSE)
    }
    if (any(is.na(as.Date(result$games$played_on)))) {
      stop("Adapter emitted an invalid played_on date.", call. = FALSE)
    }
    if (any(result$games$home_team_code == result$games$away_team_code)) {
      stop("Adapter emitted a game with identical home and away teams.", call. = FALSE)
    }
  }

  if (nrow(result$events)) {
    if (anyDuplicated(result$events$source_event_id)) {
      stop("Adapter emitted duplicate source_event_id values.", call. = FALSE)
    }
    missing_games <- setdiff(
      unique(result$events$source_game_id),
      result$games$source_game_id
    )
    if (length(missing_games)) {
      stop(
        "Events reference unknown games: ",
        paste(utils::head(missing_games, 5L), collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (nrow(result$raw_records) &&
      (anyDuplicated(result$raw_records$provider_record_id) ||
       any(!grepl("^[0-9a-f]{64}$", result$raw_records$payload_sha256)))) {
    stop("Raw records require unique IDs and lowercase SHA-256 digests.", call. = FALSE)
  }

  invisible(result)
}

