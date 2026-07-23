run_retrosheet_pipeline <- function(
    season,
    archive_path = NULL,
    dry_run = FALSE,
    publish = FALSE,
    trigger_kind = Sys.getenv("TRIGGER_KIND", unset = "local")) {
  message("Parsing Retrosheet season ", season, "...")
  result <- retrosheet_adapter(season, archive_path = archive_path)
  message(
    "Adapter contract passed: ",
    format(nrow(result$games), big.mark = ","),
    " games, ",
    format(nrow(result$events), big.mark = ","),
    " events, ",
    format(nrow(result$raw_records), big.mark = ","),
    " raw records."
  )

  if (isTRUE(dry_run)) {
    message("Dry run complete; no database connection was made.")
    return(invisible(result))
  }

  connection <- database_connect()
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  run_id <- start_pipeline_run(
    connection,
    pipeline_key = "retrosheet_historical",
    trigger_kind = trigger_kind,
    metadata = list(season = as.integer(season), publish_requested = isTRUE(publish))
  )

  tryCatch(
    {
      write_result <- write_retrosheet_result(
        connection,
        result,
        run_id,
        publish = publish
      )
      finish_pipeline_run(
        connection,
        run_id,
        status = "succeeded",
        rows_read = nrow(result$raw_records),
        rows_written = nrow(result$games) + nrow(result$events),
        source_through = max(as.Date(result$games$played_on))
      )
      message(
        "Database write complete. Batch ",
        write_result$batch_id,
        if (!is.na(write_result$release_id)) paste0("; release ", write_result$release_id) else "",
        if (publish) " is now published." else " is staged but not published."
      )
      invisible(write_result)
    },
    error = function(error) {
      try(
        finish_pipeline_run(
          connection,
          run_id,
          status = "failed",
          rows_read = nrow(result$raw_records),
          error = error
        ),
        silent = TRUE
      )
      stop(error)
    }
  )
}
