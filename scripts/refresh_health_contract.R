legacy_reference_groups <- function() {
  c("fangraphs_season", "award_race_history")
}

publication_role_for <- function(product_group) {
  ifelse(
    as.character(product_group) %in% legacy_reference_groups(),
    "legacy_reference",
    "core_publication"
  )
}

publication_blocks_for <- function(product_group) {
  publication_role_for(product_group) == "core_publication"
}

resolve_publication_gate_groups <- function(rows, configured_gate = "") {
  required <- c("product_group", "blocks_publication")
  missing <- setdiff(required, names(rows))
  if (!is.data.frame(rows) || !nrow(rows) || length(missing)) {
    stop(
      "Refresh-health rows are empty or missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (nzchar(trimws(configured_gate))) {
    groups <- trimws(strsplit(configured_gate, ",", fixed = TRUE)[[1L]])
  } else {
    groups <- as.character(rows$product_group[as.logical(rows$blocks_publication)])
  }

  unknown <- setdiff(groups, as.character(rows$product_group))
  if (length(unknown)) {
    stop(
      "Unknown freshness gate group(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  unique(groups)
}
