script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]
  dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
}

tests_root <- script_path()
cloud_run_root <- dirname(tests_root)
source(file.path(cloud_run_root, "R", "adapter_contract.R"))
source(file.path(cloud_run_root, "R", "retrosheet_adapter.R"))

fixture_path <- file.path(tests_root, "fixtures", "2025TST.EVA")
result <- parse_retrosheet_event_files(
  paths = fixture_path,
  source_uri = "fixture://2025TST.EVA",
  source_sha256 = paste(rep("a", 64L), collapse = ""),
  season = 2025L
)

stopifnot(
  nrow(result$games) == 1L,
  nrow(result$events) == 3L,
  result$games$source_game_id[[1L]] == "TST202504010",
  result$games$home_team_code[[1L]] == "BBB",
  result$games$away_team_code[[1L]] == "AAA",
  result$events$event_sequence[[3L]] == 3L,
  result$events$event_text[[3L]] == "HR/F",
  grepl("copyrighted by Retrosheet", retrosheet_attribution, fixed = TRUE)
)

cat("Retrosheet adapter contract test passed.\n")
