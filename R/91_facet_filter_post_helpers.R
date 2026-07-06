# =============================================================================
# 18_facet_filter_post_helpers.R
# Visual Revolution: EDA facet filter -> post graphic WITHOUT rebuilding the plot
# =============================================================================
#
# Purpose:
#   You make a big faceted EDA plot first using the normal Visual Revolution
#   function. Then this script filters the original plot object down to one or
#   a few selected facets/stats while preserving the original:
#
#     - geoms
#     - geom_text labels
#     - percentile labels
#     - team colors
#     - fill/color scales
#     - theme
#     - titles/subtitles/captions unless you choose to override them
#     - facet structure
#     - original plot identity
#
# Main idea:
#   DO NOT rebuild the chart from plot_data.
#   Filter the data inside the original ggplot object instead.
#
# Main functions:
#   vr_available_facets()
#   vr_keep_facets()
#   vr_filter_eda_facets()
#   vr_save_filtered_post()
#
# Example:
#   eda <- plot_team_hitting_stat_blocks(
#     views = views,
#     team = "BOS",
#     stat_block = "damage",
#     use_team_theme = TRUE,
#     team_abbr = "BOS",
#     print_plot = FALSE
#   )
#
#   vr_available_facets(eda)
#
#   post <- vr_keep_facets(
#     eda,
#     keep_stats = "OPS",
#     title = "Boston's Damage Stands Out",
#     subtitle = "Red Sox OPS compared to league average"
#   )
#
#   post$plot
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(rlang)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# -----------------------------------------------------------------------------
# Basic object helpers
# -----------------------------------------------------------------------------

vr_is_ggplot <- function(x) {
  inherits(x, "ggplot")
}

vr_is_visual_revolution_output <- function(x) {
  is.list(x) &&
    !vr_is_ggplot(x) &&
    "plot" %in% names(x) &&
    vr_is_ggplot(x$plot)
}

vr_get_plot <- function(x) {
  if (vr_is_ggplot(x)) {
    return(x)
  }

  if (vr_is_visual_revolution_output(x)) {
    return(x$plot)
  }

  stop(
    "Input must be a ggplot object or a Visual Revolution output with $plot.",
    call. = FALSE
  )
}

vr_get_plot_data <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (vr_is_visual_revolution_output(x) && "plot_data" %in% names(x)) {
    return(x$plot_data)
  }

  if (vr_is_ggplot(x)) {
    return(x$data)
  }

  if (vr_is_visual_revolution_output(x)) {
    return(x$plot$data)
  }

  stop(
    "Could not find plot data. Pass a data frame, ggplot, or Visual Revolution output.",
    call. = FALSE
  )
}

vr_droplevels_df <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  df %>%
    mutate(across(where(is.factor), droplevels))
}

# -----------------------------------------------------------------------------
# Facet/stat column detection
# -----------------------------------------------------------------------------

vr_default_stat_cols <- function() {
  c("stat", "metric", "measure", "variable", "facet")
}

vr_default_stat_label_cols <- function() {
  c("stat_label", "metric_label", "measure_label", "variable_label", "facet_label")
}

vr_find_cols <- function(df, candidates) {
  if (!is.data.frame(df)) {
    return(character())
  }

  intersect(candidates, names(df))
}

vr_stat_cols_in_data <- function(df,
                                 stat_cols = vr_default_stat_cols(),
                                 stat_label_cols = vr_default_stat_label_cols()) {
  c(
    vr_find_cols(df, stat_cols),
    vr_find_cols(df, stat_label_cols)
  ) %>%
    unique()
}

# -----------------------------------------------------------------------------
# Inspect available facets/stats from an EDA object
# -----------------------------------------------------------------------------

vr_available_facets <- function(x,
                                stat_cols = vr_default_stat_cols(),
                                stat_label_cols = vr_default_stat_label_cols()) {
  df <- vr_get_plot_data(x)

  stat_col <- vr_find_cols(df, stat_cols)
  label_col <- vr_find_cols(df, stat_label_cols)

  if (length(stat_col) == 0 && length(label_col) == 0) {
    stop(
      "No stat/facet columns found in plot_data. Looked for: ",
      paste(c(stat_cols, stat_label_cols), collapse = ", "),
      call. = FALSE
    )
  }

  stat_col <- stat_col[[1]] %||% NA_character_
  label_col <- label_col[[1]] %||% NA_character_

  out <- df

  if (!is.na(stat_col) && !is.na(label_col)) {
    out <- out %>%
      distinct(
        stat = .data[[stat_col]],
        stat_label = .data[[label_col]]
      )
  } else if (!is.na(stat_col)) {
    out <- out %>%
      distinct(stat = .data[[stat_col]]) %>%
      mutate(stat_label = stat)
  } else {
    out <- out %>%
      distinct(stat_label = .data[[label_col]]) %>%
      mutate(stat = stat_label)
  }

  out %>%
    arrange(stat_label)
}

# -----------------------------------------------------------------------------
# Row filtering logic
# -----------------------------------------------------------------------------

vr_filter_rows_by_facets <- function(df,
                                     keep_stats = NULL,
                                     keep_stat_labels = NULL,
                                     keep_regex = NULL,
                                     stat_cols = vr_default_stat_cols(),
                                     stat_label_cols = vr_default_stat_label_cols(),
                                     extra_filter = NULL,
                                     drop_levels = TRUE) {
  if (!is.data.frame(df)) {
    return(df)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  out <- df

  available_stat_cols <- vr_find_cols(out, stat_cols)
  available_label_cols <- vr_find_cols(out, stat_label_cols)
  available_filter_cols <- unique(c(available_stat_cols, available_label_cols))

  # Filter by raw stat key, such as "OPS", "Whiff_pct", "HardHit_pct".
  if (!is.null(keep_stats)) {
    if (length(available_stat_cols) > 0) {
      keep_flag <- rep(FALSE, nrow(out))

      for (col in available_stat_cols) {
        keep_flag <- keep_flag | as.character(out[[col]]) %in% keep_stats
      }

      out <- out[keep_flag, , drop = FALSE]
    }
  }

  # Filter by displayed facet label, such as "OPS", "Whiff%", "Hard-Hit%".
  if (!is.null(keep_stat_labels)) {
    if (length(available_label_cols) > 0) {
      keep_flag <- rep(FALSE, nrow(out))

      for (col in available_label_cols) {
        keep_flag <- keep_flag | as.character(out[[col]]) %in% keep_stat_labels
      }

      out <- out[keep_flag, , drop = FALSE]
    }
  }

  # Regex filter across any stat-like columns.
  if (!is.null(keep_regex)) {
    if (length(available_filter_cols) > 0) {
      keep_flag <- rep(FALSE, nrow(out))

      for (col in available_filter_cols) {
        keep_flag <- keep_flag | grepl(keep_regex, as.character(out[[col]]), ignore.case = TRUE)
      }

      out <- out[keep_flag, , drop = FALSE]
    }
  }

  # Optional user-supplied function for extra filtering.
  # Example:
  #   extra_filter = function(df) dplyr::filter(df, pitch_type %in% c("4-Seam", "Slider"))
  if (!is.null(extra_filter)) {
    if (!is.function(extra_filter)) {
      stop("extra_filter must be NULL or a function that takes a data frame.", call. = FALSE)
    }

    out <- extra_filter(out)
  }

  if (drop_levels) {
    out <- vr_droplevels_df(out)
  }

  out
}

# -----------------------------------------------------------------------------
# Filter layer data while preserving the original layer/geoms/mapping
# -----------------------------------------------------------------------------

vr_filter_layer_data <- function(layer,
                                 keep_stats = NULL,
                                 keep_stat_labels = NULL,
                                 keep_regex = NULL,
                                 stat_cols = vr_default_stat_cols(),
                                 stat_label_cols = vr_default_stat_label_cols(),
                                 extra_filter = NULL,
                                 drop_levels = TRUE) {
  # Most ggplot layers use layer$data = waiver() and inherit plot$data.
  # Those layers should be left alone; filtering plot$data is enough.
  if (is.data.frame(layer$data)) {
    layer$data <- vr_filter_rows_by_facets(
      layer$data,
      keep_stats = keep_stats,
      keep_stat_labels = keep_stat_labels,
      keep_regex = keep_regex,
      stat_cols = stat_cols,
      stat_label_cols = stat_label_cols,
      extra_filter = extra_filter,
      drop_levels = drop_levels
    )
  }

  layer
}

# -----------------------------------------------------------------------------
# Main function: preserve original EDA plot, only filter selected facets/stats
# -----------------------------------------------------------------------------

vr_filter_eda_facets <- function(x,
                                 keep_stats = NULL,
                                 keep_stat_labels = NULL,
                                 keep_regex = NULL,
                                 stat_cols = vr_default_stat_cols(),
                                 stat_label_cols = vr_default_stat_label_cols(),
                                 extra_filter = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 tag = NULL,
                                 facet_ncol = NULL,
                                 facet_nrow = NULL,
                                 drop_levels = TRUE,
                                 return_same_type = TRUE) {
  if (is.null(keep_stats) && is.null(keep_stat_labels) && is.null(keep_regex) && is.null(extra_filter)) {
    stop(
      "Provide at least one filter: keep_stats, keep_stat_labels, keep_regex, or extra_filter.",
      call. = FALSE
    )
  }

  original_is_vr_output <- vr_is_visual_revolution_output(x)
  original_is_ggplot <- vr_is_ggplot(x)

  p <- vr_get_plot(x)
  p2 <- p

  # Filter plot-level data.
  if (is.data.frame(p2$data)) {
    p2$data <- vr_filter_rows_by_facets(
      p2$data,
      keep_stats = keep_stats,
      keep_stat_labels = keep_stat_labels,
      keep_regex = keep_regex,
      stat_cols = stat_cols,
      stat_label_cols = stat_label_cols,
      extra_filter = extra_filter,
      drop_levels = drop_levels
    )
  }

  # Filter layer-specific data, if a layer has its own data.
  if (length(p2$layers) > 0) {
    p2$layers <- lapply(
      p2$layers,
      vr_filter_layer_data,
      keep_stats = keep_stats,
      keep_stat_labels = keep_stat_labels,
      keep_regex = keep_regex,
      stat_cols = stat_cols,
      stat_label_cols = stat_label_cols,
      extra_filter = extra_filter,
      drop_levels = drop_levels
    )
  }

  # Best-effort facet layout tweaks.
  # This keeps the same facet variable and scales, but lets you make one-facet
  # posts less cramped if desired.
  if (!is.null(facet_ncol)) {
    if (!is.null(p2$facet$params)) {
      p2$facet$params$ncol <- facet_ncol
    }
  }

  if (!is.null(facet_nrow)) {
    if (!is.null(p2$facet$params)) {
      p2$facet$params$nrow <- facet_nrow
    }
  }

  # Optional label override only. If left NULL, original title/subtitle/caption remain.
  labs_args <- list()

  if (!is.null(title)) labs_args$title <- title
  if (!is.null(subtitle)) labs_args$subtitle <- subtitle
  if (!is.null(caption)) labs_args$caption <- caption
  if (!is.null(tag)) labs_args$tag <- tag

  if (length(labs_args) > 0) {
    p2 <- p2 + do.call(ggplot2::labs, labs_args)
  }

  # If input was a plain ggplot, return a plain ggplot by default.
  if (original_is_ggplot && !original_is_vr_output && return_same_type) {
    return(p2)
  }

  # If input was a Visual Revolution output, update its plot and plot_data while
  # preserving every other object: summary_df, player_df, league_df, metadata, etc.
  if (original_is_vr_output) {
    out <- x
    out$plot <- p2

    if ("plot_data" %in% names(out) && is.data.frame(out$plot_data)) {
      out$plot_data <- vr_filter_rows_by_facets(
        out$plot_data,
        keep_stats = keep_stats,
        keep_stat_labels = keep_stat_labels,
        keep_regex = keep_regex,
        stat_cols = stat_cols,
        stat_label_cols = stat_label_cols,
        extra_filter = extra_filter,
        drop_levels = drop_levels
      )
    } else if (is.data.frame(p2$data)) {
      out$plot_data <- p2$data
    }

    out$facet_filter <- list(
      keep_stats = keep_stats,
      keep_stat_labels = keep_stat_labels,
      keep_regex = keep_regex,
      filtered_at = Sys.time()
    )

    return(out)
  }

  p2
}

# Friendly alias: this is the function name you should actually use.
vr_keep_facets <- vr_filter_eda_facets

# Another alias if you think in terms of post graphics.
vr_make_post_from_eda <- vr_filter_eda_facets

# -----------------------------------------------------------------------------
# Filter by stat and an extra dimension, while preserving the same plot
# -----------------------------------------------------------------------------
# Useful after EDA when you want:
#   - one stat
#   - only a few pitch types
#   - only selected comparisons/splits
#
# Example:
#   post <- vr_keep_facets_and_values(
#     eda,
#     keep_stats = "SLG",
#     keep_values = list(pitch_type = c("4-Seam", "Slider"))
#   )
#

vr_keep_facets_and_values <- function(x,
                                      keep_stats = NULL,
                                      keep_stat_labels = NULL,
                                      keep_regex = NULL,
                                      keep_values = list(),
                                      title = NULL,
                                      subtitle = NULL,
                                      caption = NULL,
                                      tag = NULL,
                                      facet_ncol = NULL,
                                      facet_nrow = NULL,
                                      drop_levels = TRUE,
                                      return_same_type = TRUE) {
  if (!is.list(keep_values)) {
    stop("keep_values must be a named list, e.g. list(pitch_type = c('Slider', '4-Seam')).", call. = FALSE)
  }

  extra_filter <- function(df) {
    out <- df

    for (nm in names(keep_values)) {
      if (nm %in% names(out)) {
        out <- out %>%
          filter(.data[[nm]] %in% keep_values[[nm]])
      }
    }

    out
  }

  vr_filter_eda_facets(
    x = x,
    keep_stats = keep_stats,
    keep_stat_labels = keep_stat_labels,
    keep_regex = keep_regex,
    extra_filter = extra_filter,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    facet_ncol = facet_ncol,
    facet_nrow = facet_nrow,
    drop_levels = drop_levels,
    return_same_type = return_same_type
  )
}

# -----------------------------------------------------------------------------
# Diagnostics: see where data lives inside a ggplot
# -----------------------------------------------------------------------------

vr_plot_data_sources <- function(x) {
  p <- vr_get_plot(x)

  plot_rows <- if (is.data.frame(p$data)) nrow(p$data) else NA_integer_
  plot_cols <- if (is.data.frame(p$data)) paste(names(p$data), collapse = ", ") else NA_character_

  layer_info <- lapply(seq_along(p$layers), function(i) {
    layer <- p$layers[[i]]

    has_own_data <- is.data.frame(layer$data)

    data.frame(
      layer = i,
      geom = class(layer$geom)[[1]],
      stat = class(layer$stat)[[1]],
      has_own_data = has_own_data,
      rows = if (has_own_data) nrow(layer$data) else NA_integer_,
      cols = if (has_own_data) paste(names(layer$data), collapse = ", ") else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(
    data.frame(
      layer = 0,
      geom = "plot$data",
      stat = NA_character_,
      has_own_data = is.data.frame(p$data),
      rows = plot_rows,
      cols = plot_cols,
      stringsAsFactors = FALSE
    ),
    bind_rows(layer_info)
  )
}

# -----------------------------------------------------------------------------
# Save filtered post graphics without changing the plot object
# -----------------------------------------------------------------------------

vr_save_filtered_post <- function(x,
                                  filename,
                                  width = 8,
                                  height = 8,
                                  dpi = 300,
                                  bg = "white") {
  p <- vr_get_plot(x)

  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )

  invisible(filename)
}

vr_save_social_post <- function(x,
                                filename,
                                size = c("square", "portrait", "wide"),
                                dpi = 300,
                                bg = "white") {
  size <- match.arg(size)

  dims <- switch(
    size,
    square = c(width = 8, height = 8),
    portrait = c(width = 8, height = 10),
    wide = c(width = 12, height = 6.75)
  )

  vr_save_filtered_post(
    x = x,
    filename = filename,
    width = dims[["width"]],
    height = dims[["height"]],
    dpi = dpi,
    bg = bg
  )
}

# -----------------------------------------------------------------------------
# Optional: save every facet as its own post graphic
# -----------------------------------------------------------------------------
# This is useful when browsing a large EDA plot and turning each facet into a
# separate carousel candidate.
#
# Example:
#   vr_save_each_facet(
#     eda,
#     folder = "graphics/bos_damage_facets",
#     prefix = "bos_damage",
#     team_abbr = "BOS"
#   )
#

vr_clean_filename_piece <- function(x) {
  x %>%
    as.character() %>%
    tolower() %>%
    gsub("%", "pct", ., fixed = TRUE) %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("^_+|_+$", "", .)
}

vr_save_each_facet <- function(x,
                               folder,
                               prefix = "facet",
                               title_template = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               size = c("square", "portrait", "wide"),
                               dpi = 300,
                               bg = "white") {
  size <- match.arg(size)

  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  facets <- vr_available_facets(x)

  files <- vector("character", nrow(facets))

  for (i in seq_len(nrow(facets))) {
    stat_i <- facets$stat[[i]]
    label_i <- facets$stat_label[[i]]

    title_i <- if (!is.null(title_template)) {
      gsub("\u007bstat_label\u007d", label_i, title_template, fixed = TRUE)
    } else {
      NULL
    }

    filtered <- vr_keep_facets(
      x,
      keep_stats = stat_i,
      title = title_i,
      subtitle = subtitle,
      caption = caption,
      facet_ncol = 1
    )

    file_i <- file.path(
      folder,
      paste0(prefix, "_", vr_clean_filename_piece(stat_i), ".png")
    )

    vr_save_social_post(
      filtered,
      filename = file_i,
      size = size,
      dpi = dpi,
      bg = bg
    )

    files[[i]] <- file_i
  }

  tibble::tibble(
    stat = facets$stat,
    stat_label = facets$stat_label,
    file = files
  )
}

# =============================================================================
# End of 18_facet_filter_post_helpers.R
# =============================================================================
