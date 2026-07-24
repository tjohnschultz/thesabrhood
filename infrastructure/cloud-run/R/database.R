database_connect <- function(database_url = Sys.getenv("DATABASE_URL", unset = "")) {
  if (!nzchar(database_url)) {
    stop("DATABASE_URL is required for a database write.", call. = FALSE)
  }
  DBI::dbConnect(RPostgres::Postgres(), dbname = database_url)
}

sql_quote <- function(connection, value) {
  if (length(value) != 1L) stop("SQL parameters must be scalar.", call. = FALSE)
  if (inherits(value, c("Date", "POSIXct", "POSIXlt"))) value <- as.character(value)
  if (is.na(value)) return("NULL")
  as.character(DBI::dbQuoteString(connection, value))
}

start_pipeline_run <- function(connection, pipeline_key, trigger_kind, metadata = list()) {
  sql <- sprintf(
    paste(
      "insert into observability.pipeline_runs",
      "(pipeline_key, trigger_kind, git_sha, execution_ref, metadata)",
      "values (%s, %s, %s, %s, %s::jsonb)",
      "returning id"
    ),
    sql_quote(connection, pipeline_key),
    sql_quote(connection, trigger_kind),
    sql_quote(connection, Sys.getenv("GIT_SHA", unset = NA_character_)),
    sql_quote(connection, Sys.getenv("EXECUTION_REF", unset = NA_character_)),
    sql_quote(connection, jsonlite::toJSON(metadata, auto_unbox = TRUE))
  )
  DBI::dbGetQuery(connection, sql)$id[[1L]]
}

finish_pipeline_run <- function(
    connection,
    run_id,
    status,
    rows_read = 0,
    rows_written = 0,
    source_through = NA_character_,
    error = NULL) {
  error_class <- if (is.null(error)) NA_character_ else class(error)[[1L]]
  error_message <- if (is.null(error)) NA_character_ else conditionMessage(error)
  sql <- sprintf(
    paste(
      "update observability.pipeline_runs",
      "set status = %s, finished_at = now(), rows_read = %d, rows_written = %d,",
      "source_through = %s::date, error_class = %s, error_message = %s",
      "where id = %s::uuid"
    ),
    sql_quote(connection, status),
    as.integer(rows_read),
    as.integer(rows_written),
    sql_quote(connection, source_through),
    sql_quote(connection, error_class),
    sql_quote(connection, error_message),
    sql_quote(connection, run_id)
  )
  DBI::dbExecute(connection, sql)
}

write_retrosheet_result <- function(connection, result, run_id, publish = FALSE) {
  validate_adapter_result(result)
  release_key <- sprintf(
    "%s-%s-%s",
    result$provider_key,
    result$source_version,
    substr(result$source_sha256, 1L, 12L)
  )
  existing <- DBI::dbGetQuery(
    connection,
    sprintf(
      paste(
        "select id, status from ingest.batches",
        "where provider_key = %s and source_sha256 = %s"
      ),
      sql_quote(connection, result$provider_key),
      sql_quote(connection, result$source_sha256)
    )
  )
  if (nrow(existing) && identical(existing$status[[1L]], "accepted")) {
    existing_release <- DBI::dbGetQuery(
      connection,
      sprintf(
        "select id, status from publishing.releases where release_key = %s",
        sql_quote(connection, release_key)
      )
    )
    if (!nrow(existing_release)) {
      stop("Accepted batch has no corresponding staged release.", call. = FALSE)
    }
    if (isTRUE(publish) && identical(existing_release$status[[1L]], "staged")) {
      DBI::dbExecute(
        connection,
        sprintf(
          "select publishing.publish_release(%s::uuid)",
          sql_quote(connection, existing_release$id[[1L]])
        )
      )
    }
    return(list(
      batch_id = existing$id[[1L]],
      release_id = existing_release$id[[1L]],
      already_present = TRUE
    ))
  }

  DBI::dbWithTransaction(connection, {
    batch_id <- if (nrow(existing)) {
      existing$id[[1L]]
    } else {
      DBI::dbGetQuery(
        connection,
        sprintf(
          paste(
            "insert into ingest.batches",
            "(provider_key, pipeline_run_id, source_uri, source_version,",
            "source_sha256, effective_from, effective_through, row_count)",
            "values (%s, %s::uuid, %s, %s, %s, %s::date, %s::date, %d)",
            "returning id"
          ),
          sql_quote(connection, result$provider_key),
          sql_quote(connection, run_id),
          sql_quote(connection, result$source_uri),
          sql_quote(connection, result$source_version),
          sql_quote(connection, result$source_sha256),
          sql_quote(connection, result$effective_from),
          sql_quote(connection, result$effective_through),
          nrow(result$raw_records)
        )
      )$id[[1L]]
    }

    raw_stage <- result$raw_records
    DBI::dbWriteTable(
      connection, "stage_raw_records", raw_stage,
      temporary = TRUE, overwrite = TRUE, row.names = FALSE
    )
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into ingest.raw_records",
          "(batch_id, record_type, provider_record_id, payload, payload_sha256)",
          "select %s::uuid, record_type, provider_record_id,",
          "payload_json::jsonb, payload_sha256 from stage_raw_records",
          "on conflict do nothing"
        ),
        sql_quote(connection, batch_id)
      )
    )

    team_codes <- sort(unique(c(
      result$games$home_team_code,
      result$games$away_team_code
    )))
    team_codes <- team_codes[!is.na(team_codes) & nzchar(team_codes)]
    team_stage <- data.frame(
      team_code = team_codes,
      name = team_codes,
      valid_from = rep(result$effective_from, length(team_codes)),
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(
      connection, "stage_teams", team_stage,
      temporary = TRUE, overwrite = TRUE, row.names = FALSE
    )
    DBI::dbExecute(
      connection,
      paste(
        "insert into canonical.teams (team_code, name, valid_from)",
        "select team_code, name, valid_from::date from stage_teams",
        "on conflict (team_code, valid_from) do update set name = excluded.name"
      )
    )

    DBI::dbWriteTable(
      connection, "stage_games", result$games,
      temporary = TRUE, overwrite = TRUE, row.names = FALSE
    )
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into canonical.games",
          "(source_provider_key, source_game_id, played_on, game_number,",
          "home_team_id, away_team_id, home_team_code, away_team_code,",
          "site_code, day_night, source_batch_id, metadata)",
          "select %s, g.source_game_id, g.played_on::date,",
          "coalesce(g.game_number, 0)::smallint, ht.id, at.id,",
          "g.home_team_code, g.away_team_code, g.site_code, g.day_night,",
          "%s::uuid, g.metadata_json::jsonb",
          "from stage_games g",
          "left join canonical.teams ht on ht.team_code = g.home_team_code",
          "and ht.valid_from = %s::date",
          "left join canonical.teams at on at.team_code = g.away_team_code",
          "and at.valid_from = %s::date",
          "on conflict (source_provider_key, source_game_id) do update set",
          "played_on = excluded.played_on, game_number = excluded.game_number,",
          "home_team_id = excluded.home_team_id, away_team_id = excluded.away_team_id,",
          "home_team_code = excluded.home_team_code, away_team_code = excluded.away_team_code,",
          "site_code = excluded.site_code, day_night = excluded.day_night,",
          "source_batch_id = excluded.source_batch_id, metadata = excluded.metadata"
        ),
        sql_quote(connection, result$provider_key),
        sql_quote(connection, batch_id),
        sql_quote(connection, result$effective_from),
        sql_quote(connection, result$effective_from)
      )
    )

    DBI::dbWriteTable(
      connection, "stage_events", result$events,
      temporary = TRUE, overwrite = TRUE, row.names = FALSE
    )
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into canonical.game_events",
          "(game_id, source_provider_key, source_event_id, event_sequence,",
          "inning, batting_side, batter_source_id, count_state, pitch_sequence,",
          "event_text, source_batch_id, metadata)",
          "select g.id, %s, e.source_event_id, e.event_sequence, e.inning,",
          "e.batting_side, e.batter_source_id, e.count_state, e.pitch_sequence,",
          "e.event_text, %s::uuid, e.metadata_json::jsonb",
          "from stage_events e",
          "join canonical.games g on g.source_provider_key = %s",
          "and g.source_game_id = e.source_game_id",
          "on conflict (source_provider_key, source_event_id) do update set",
          "game_id = excluded.game_id, event_sequence = excluded.event_sequence,",
          "inning = excluded.inning, batting_side = excluded.batting_side,",
          "batter_source_id = excluded.batter_source_id, count_state = excluded.count_state,",
          "pitch_sequence = excluded.pitch_sequence, event_text = excluded.event_text,",
          "source_batch_id = excluded.source_batch_id, metadata = excluded.metadata"
        ),
        sql_quote(connection, result$provider_key),
        sql_quote(connection, batch_id),
        sql_quote(connection, result$provider_key)
      )
    )

    DBI::dbExecute(
      connection,
      sprintf(
        "update ingest.batches set status = 'accepted' where id = %s::uuid",
        sql_quote(connection, batch_id)
      )
    )

    release_id <- DBI::dbGetQuery(
      connection,
      sprintf(
        paste(
          "insert into publishing.releases (release_key, pipeline_run_id, notes)",
          "values (%s, %s::uuid, %s)",
          "on conflict (release_key) do update set notes = excluded.notes",
          "returning id"
        ),
        sql_quote(connection, release_key),
        sql_quote(connection, run_id),
        sql_quote(connection, "Retrosheet historical release")
      )
    )$id[[1L]]

    provider_batches <- DBI::dbGetQuery(
      connection,
      sprintf(
        paste(
          "select source_sha256, effective_through",
          "from ingest.batches",
          "where provider_key = %s and status = 'accepted'",
          "order by effective_from, source_sha256"
        ),
        sql_quote(connection, result$provider_key)
      )
    )
    provider_checksum <- digest::digest(
      paste(provider_batches$source_sha256, collapse = ":"),
      algo = "sha256",
      serialize = FALSE
    )
    provider_source_through <- max(as.Date(provider_batches$effective_through))
    provider_row_count <- DBI::dbGetQuery(
      connection,
      sprintf(
        "select count(*)::bigint as n from canonical.game_events where source_provider_key = %s",
        sql_quote(connection, result$provider_key)
      )
    )$n[[1L]]
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into publishing.release_sources",
          "(release_id, provider_key, source_through, row_count, checksum_sha256)",
          "values (%s::uuid, %s, %s::date, %s, %s)",
          "on conflict (release_id, provider_key) do update set",
          "source_through = excluded.source_through, row_count = excluded.row_count,",
          "checksum_sha256 = excluded.checksum_sha256"
        ),
        sql_quote(connection, release_id),
        sql_quote(connection, result$provider_key),
        sql_quote(connection, provider_source_through),
        format(as.numeric(provider_row_count), scientific = FALSE, trim = TRUE),
        sql_quote(connection, provider_checksum)
      )
    )

    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into publishing.release_games (release_id, game_id, audience)",
          "select %s::uuid, id, 'public' from canonical.games",
          "where source_provider_key = %s",
          "on conflict do nothing"
        ),
        sql_quote(connection, release_id),
        sql_quote(connection, result$provider_key)
      )
    )
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into publishing.release_game_events",
          "(release_id, game_event_id, audience)",
          "select %s::uuid, id, 'public' from canonical.game_events",
          "where source_provider_key = %s",
          "on conflict do nothing"
        ),
        sql_quote(connection, release_id),
        sql_quote(connection, result$provider_key)
      )
    )

    contract_status <- if (
      nrow(result$games) > 0L &&
      nrow(result$events) > 0L &&
      !any(is.na(result$games$played_on))
    ) "pass" else "fail"
    DBI::dbExecute(
      connection,
      sprintf(
        paste(
          "insert into observability.release_checks",
          "(release_id, check_key, status, observed_value, expected_value)",
          "values (%s::uuid, 'minimum_release_contract', %s, %s, %s)",
          "on conflict (release_id, check_key) do update set",
          "status = excluded.status, observed_value = excluded.observed_value,",
          "expected_value = excluded.expected_value, checked_at = now()"
        ),
        sql_quote(connection, release_id),
        sql_quote(connection, contract_status),
        sql_quote(
          connection,
          sprintf("%d games / %d events", nrow(result$games), nrow(result$events))
        ),
        sql_quote(connection, "at least one valid game and event")
      )
    )

    if (isTRUE(publish)) {
      DBI::dbExecute(
        connection,
        sprintf(
          "select publishing.publish_release(%s::uuid)",
          sql_quote(connection, release_id)
        )
      )
    }

    list(
      batch_id = batch_id,
      release_id = release_id,
      already_present = FALSE
    )
  })
}
