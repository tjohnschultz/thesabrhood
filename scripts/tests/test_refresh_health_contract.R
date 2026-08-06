source(file.path("scripts", "refresh_health_contract.R"), local = FALSE)

groups <- c(
  "completed_game_pbp",
  "daily_slate",
  "fangraphs_season",
  "award_race_history"
)
rows <- data.frame(
  product_group = groups,
  blocks_publication = publication_blocks_for(groups),
  stringsAsFactors = FALSE
)

stopifnot(
  identical(
    publication_role_for(groups),
    c(
      "core_publication",
      "core_publication",
      "legacy_reference",
      "legacy_reference"
    )
  ),
  identical(
    resolve_publication_gate_groups(rows),
    c("completed_game_pbp", "daily_slate")
  ),
  identical(
    resolve_publication_gate_groups(
      rows,
      "daily_slate, fangraphs_season"
    ),
    c("daily_slate", "fangraphs_season")
  )
)

unknown_error <- tryCatch(
  {
    resolve_publication_gate_groups(rows, "not_a_product")
    ""
  },
  error = function(error) conditionMessage(error)
)
stopifnot(grepl("not_a_product", unknown_error, fixed = TRUE))

cat("Refresh-health publication contract tests passed.\n")
