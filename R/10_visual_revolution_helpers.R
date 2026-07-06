# -----------------------------------------------------------------------------
# R/11_visual_revolution_helpers.R
# Visual Revolution helper layer
#
# Purpose:
#   Shared helpers for fast visual EDA, stat blocks, formatting, source loops,
#   time grouping, league context, and plot export.
#
# Source after your existing helpers/summary files and after 10_pitchtype_viz_pack.R.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Project source loop
# -----------------------------------------------------------------------------

source_project_r <- function(path = "R",
                             pattern = "\\.R$",
                             echo = FALSE,
                             recursive = FALSE) {
  files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive)
  files <- sort(files)

  if (length(files) == 0) {
    warning("No R files found in: ", path, call. = FALSE)
    return(invisible(character(0)))
  }

  for (file in files) {
    if (isTRUE(echo)) message("Sourcing: ", file)
    source(file, local = FALSE)
  }

  invisible(files)
}

# -----------------------------------------------------------------------------
# Small safety helpers
# -----------------------------------------------------------------------------

vr_clean_group_by <- function(group.by = NULL) {
  if (is.null(group.by)) return(NULL)
  if (length(group.by) == 1 && is.na(group.by)) return(NULL)
  group.by[!is.na(group.by)]
}

vr_has_col <- function(df, col) {
  !is.null(df) && is.data.frame(df) && col %in% names(df)
}

vr_require_cols <- function(df, cols, fn = "function") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(fn, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

vr_pick_col <- function(df, candidates, required = FALSE, fn = "function") {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])

  if (isTRUE(required)) {
    stop(
      fn, " could not find any of these columns: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }

  NA_character_
}

vr_safe_rate <- function(num, den) {
  ifelse(!is.na(den) & den > 0, num / den, NA_real_)
}

vr_weighted_mean_safe <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

vr_pct_rank_value <- function(value, distribution) {
  value <- suppressWarnings(as.numeric(value))
  distribution <- suppressWarnings(as.numeric(distribution))
  distribution <- distribution[!is.na(distribution)]

  if (length(distribution) == 0 || is.na(value)) return(NA_real_)
  100 * mean(distribution <= value, na.rm = TRUE)
}

vr_first_non_missing <- function(x) {
  x <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(x) == 0) return(NA)
  x[1]
}

# -----------------------------------------------------------------------------
# Stat labels and stat blocks
# -----------------------------------------------------------------------------

vr_default_stat_labels <- function() {
  c(
    Usage_pct = "Usage %",
    PA = "PA",
    AB = "AB",
    H = "Hits",
    SO = "Strikeouts",
    BB = "Walks",
    HR = "Home runs",
    TB = "Total bases",
    AVG = "AVG",
    OBP = "OBP",
    SLG = "SLG",
    OPS = "OPS",
    ISO = "ISO",
    K_pct = "K %",
    BB_pct = "BB %",
    HR_pct = "HR %",
    XBH_pct = "XBH %",
    Swing_pct = "Swing %",
    Whiff_pct = "Whiff %",
    Chase_pct = "Chase %",
    Zone_pct = "Zone %",
    Contact_pct = "Contact %",
    OContact_pct = "O-contact %",
    ZContact_pct = "Z-contact %",
    ZSwing_pct = "Z-swing %",
    Heart_pct = "Heart %",
    Shadow_pct = "Shadow %",
    HardHit_pct = "Hard-hit %",
    barrel_pct = "Barrel %",
    Barrel_pct = "Barrel %",
    GB_pct = "Ground-ball %",
    FB_pct = "Fly-ball %",
    LD_pct = "Line-drive %",
    PU_pct = "Popup %",
    Pull_pct = "Pull %",
    Middle_pct = "Middle %",
    Cent_pct = "Center %",
    Oppo_pct = "Oppo %",
    pull_fbpct = "Pull FB %",
    Pull_FB_pct = "Pull FB %",
    HRFB = "HR/FB %",
    EV = "Exit velocity",
    maxEV = "Max EV",
    LA = "Launch angle",
    velo = "Velocity",
    spin = "Spin rate",
    IVB = "IVB",
    HB = "HB",
    HAA = "HAA",
    VAA = "VAA"
  )
}

vr_percent_stats <- function() {
  c(
    "Usage_pct", "K_pct", "BB_pct", "HR_pct", "XBH_pct",
    "Swing_pct", "Whiff_pct", "Chase_pct", "Zone_pct", "Contact_pct",
    "OContact_pct", "ZContact_pct", "ZSwing_pct", "Heart_pct", "Shadow_pct",
    "HardHit_pct", "barrel_pct", "Barrel_pct", "GB_pct", "FB_pct", "LD_pct",
    "PU_pct", "Pull_pct", "Middle_pct", "Cent_pct", "Oppo_pct",
    "pull_fbpct", "Pull_FB_pct", "HRFB"
  )
}

vr_label_stats <- function(stats, stat_labels = NULL, percent_stats = NULL) {
  labels <- vr_default_stat_labels()

  if (!is.null(stat_labels)) {
    if (is.null(names(stat_labels))) {
      stop("`stat_labels` must be a named character vector.", call. = FALSE)
    }
    labels[names(stat_labels)] <- stat_labels
  }

  out <- ifelse(stats %in% names(labels), unname(labels[stats]), stats)

  if (!is.null(percent_stats)) {
    out <- ifelse(
      stats %in% percent_stats & !grepl("%", out, fixed = TRUE),
      paste0(out, " (%)"),
      out
    )
  }

  out
}

vr_format_value <- function(value,
                            stat,
                            percent_stats = vr_percent_stats(),
                            scale_percents = TRUE,
                            digits = NULL) {
  if (length(value) == 0 || is.na(value)) return(NA_character_)

  if (stat %in% percent_stats) {
    val <- if (isTRUE(scale_percents)) value * 100 else value
    return(paste0(round(val, 1), "%"))
  }

  if (stat %in% c("AVG", "OBP", "SLG", "OPS", "ISO")) {
    return(sprintf("%.3f", round(value, 3)))
  }

  if (stat %in% c("EV", "maxEV", "LA", "velo", "IVB", "HB", "HAA", "VAA")) {
    return(as.character(round(value, 1)))
  }

  if (stat == "spin") return(as.character(round(value, 0)))

  if (!is.null(digits)) return(as.character(round(value, digits)))
  as.character(round(value, 2))
}

pitcher_stat_blocks <- function() {
  list(
    usage = c("Usage_pct"),
    command = c("Zone_pct", "Heart_pct", "Shadow_pct"),
    approach = c("Swing_pct", "Chase_pct", "ZSwing_pct"),
    miss_bat = c("Whiff_pct", "Contact_pct", "OContact_pct", "ZContact_pct"),
    contact_allowed = c("HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB"),
    shape = c("velo", "spin", "IVB", "HB", "HAA", "VAA"),
    results = c("K_pct", "BB_pct", "SLG"),
    full = c(
      "Usage_pct", "Zone_pct", "Heart_pct", "Chase_pct", "Whiff_pct", "Contact_pct",
      "velo", "spin", "IVB", "HB", "HardHit_pct", "barrel_pct", "GB_pct", "EV", "LA", "HRFB"
    )
  )
}

hitter_stat_blocks <- function() {
  list(
    plate_discipline = c("BB_pct", "K_pct", "Swing_pct", "Chase_pct", "Zone_pct", "ZSwing_pct"),
    contact = c("Contact_pct", "Whiff_pct", "ZContact_pct", "OContact_pct"),
    contact_quality = c("EV", "maxEV", "LA", "HardHit_pct", "barrel_pct"),
    batted_ball = c("GB_pct", "FB_pct", "LD_pct", "PU_pct"),
    spray = c("Pull_pct", "Middle_pct", "Oppo_pct", "pull_fbpct"),
    damage = c("AVG", "OBP", "SLG", "OPS", "ISO", "HR_pct", "XBH_pct"),
    full = c(
      "BB_pct", "K_pct", "Chase_pct", "Whiff_pct", "Contact_pct",
      "EV", "HardHit_pct", "barrel_pct", "GB_pct", "Pull_pct", "SLG", "OPS", "ISO"
    )
  )
}

vr_resolve_stat_block <- function(stat_block = NULL,
                                  stats = NULL,
                                  blocks,
                                  always_include = NULL,
                                  available_stats = NULL) {
  if (!is.null(stats)) {
    out <- unique(stats)
  } else {
    if (is.null(stat_block)) stat_block <- "full"
    if (!stat_block %in% names(blocks)) {
      stop(
        "Unknown stat_block: ", stat_block,
        ". Available blocks: ", paste(names(blocks), collapse = ", "),
        call. = FALSE
      )
    }
    out <- blocks[[stat_block]]
  }

  out <- unique(c(always_include, out))

  if (!is.null(available_stats)) {
    out <- out[out %in% available_stats]
  }

  out
}

# -----------------------------------------------------------------------------
# Weighting and league context
# -----------------------------------------------------------------------------

vr_weight_col_for_stat <- function(stat, available_cols) {
  preferred <- switch(
    stat,
    Usage_pct = "Pitches",
    Swing_pct = "Pitches",
    Zone_pct = "Pitches",
    Heart_pct = "Pitches",
    Shadow_pct = "Pitches",
    Whiff_pct = "Swings",
    Contact_pct = "Swings",
    Chase_pct = "OZoneP",
    ZSwing_pct = "ZoneP",
    OContact_pct = "Chase",
    ZContact_pct = "ZSwings",
    HardHit_pct = "BBE",
    barrel_pct = "BBE",
    Barrel_pct = "BBE",
    GB_pct = "BBE",
    FB_pct = "BBE",
    LD_pct = "BBE",
    PU_pct = "BBE",
    Pull_pct = "BBE",
    Middle_pct = "BBE",
    Oppo_pct = "BBE",
    pull_fbpct = "BBE",
    Pull_FB_pct = "BBE",
    EV = "BBE",
    maxEV = "BBE",
    LA = "BBE",
    HRFB = "FB",
    AVG = "AB",
    OBP = "PA",
    SLG = "AB",
    OPS = "PA",
    ISO = "AB",
    K_pct = "PA",
    BB_pct = "PA",
    HR_pct = "PA",
    XBH_pct = "PA",
    velo = "Shape_Pitches",
    spin = "Shape_Pitches",
    IVB = "Shape_Pitches",
    HB = "Shape_Pitches",
    HAA = "Shape_Pitches",
    VAA = "Shape_Pitches",
    "Pitches"
  )

  fallbacks <- unique(c(preferred, "PA", "Pitches", "Swings", "BBE", "AB", "Shape_Pitches"))
  hit <- fallbacks[fallbacks %in% available_cols]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

vr_league_values_by_group <- function(summary_df,
                                      stats,
                                      group_cols = NULL,
                                      method = c("weighted_mean", "median", "mean"),
                                      min_weight = 0) {
  library(dplyr)
  library(tidyr)

  method <- match.arg(method)
  group_cols <- vr_clean_group_by(group_cols)
  stats <- stats[stats %in% names(summary_df)]

  if (length(stats) == 0) {
    stop("No selected stats are present in `summary_df`.", call. = FALSE)
  }

  base_df <- summary_df %>% dplyr::mutate(.vr_row_id = dplyr::row_number())

  long <- base_df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stats), names_to = "stat", values_to = "value") %>%
    dplyr::mutate(
      .weight_col = vapply(stat, vr_weight_col_for_stat, character(1), available_cols = names(base_df)),
      .weight = vapply(
        seq_len(dplyr::n()),
        function(i) {
          col <- .weight_col[i]
          row_id <- .vr_row_id[i]
          if (is.na(col) || !col %in% names(base_df)) return(NA_real_)
          suppressWarnings(as.numeric(base_df[[col]][row_id]))
        },
        numeric(1)
      )
    ) %>%
    dplyr::filter(!is.na(value))

  if (!is.null(group_cols) && length(group_cols) > 0) {
    group_expr <- rlang::syms(c(group_cols, "stat"))
  } else {
    group_expr <- rlang::syms("stat")
  }

  long %>%
    dplyr::group_by(!!!group_expr) %>%
    dplyr::summarise(
      league_value = dplyr::case_when(
        method == "weighted_mean" ~ vr_weighted_mean_safe(value, .weight),
        method == "median" ~ median(value, na.rm = TRUE),
        method == "mean" ~ mean(value, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      league_n = dplyr::n(),
      league_weight = sum(.weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.na(min_weight) | league_weight >= min_weight | league_n > 0)
}

vr_percentile_table <- function(summary_df,
                                player_df,
                                stats,
                                group_cols = NULL,
                                player_id_col = NULL) {
  library(dplyr)
  library(tidyr)

  group_cols <- vr_clean_group_by(group_cols)
  stats <- stats[stats %in% names(summary_df)]

  if (length(stats) == 0) {
    stop("No selected stats are present for percentile calculation.", call. = FALSE)
  }

  league_long <- summary_df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stats), names_to = "stat", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  player_long <- player_df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stats), names_to = "stat", values_to = "player_value") %>%
    dplyr::filter(!is.na(player_value))

  join_cols <- c(group_cols, "stat")

  player_long %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      percentile = {
        cur_stat <- stat
        cur_row <- dplyr::cur_data()
        tmp <- league_long %>% dplyr::filter(.data$stat == cur_stat)
        if (length(group_cols) > 0) {
          for (gc in group_cols) {
            cur_val <- cur_row[[gc]][1]
            tmp <- tmp %>% dplyr::filter(.data[[gc]] == cur_val)
          }
        }
        vr_pct_rank_value(player_value, tmp$value)
      }
    ) %>%
    dplyr::ungroup()
}

# -----------------------------------------------------------------------------
# Time grouping helpers
# -----------------------------------------------------------------------------

vr_add_time_unit <- function(df,
                             time_unit = c("year", "month", "date"),
                             date_col = "game_date",
                             year_col = "year",
                             out_col = ".time_group") {
  library(dplyr)
  library(lubridate)

  time_unit <- match.arg(time_unit)

  if (!date_col %in% names(df) && !year_col %in% names(df)) {
    stop("Could not find `", date_col, "` or `", year_col, "` for time grouping.", call. = FALSE)
  }

  df <- df %>%
    dplyr::mutate(
      .vr_date = if (date_col %in% names(.)) lubridate::ymd(.data[[date_col]], quiet = TRUE) else as.Date(NA)
    )

  if (time_unit == "year") {
    if (year_col %in% names(df)) {
      df[[out_col]] <- suppressWarnings(as.integer(df[[year_col]]))
    } else {
      df[[out_col]] <- lubridate::year(df$.vr_date)
    }
  }

  if (time_unit == "month") {
    df[[out_col]] <- lubridate::floor_date(df$.vr_date, unit = "month")
  }

  if (time_unit == "date") {
    df[[out_col]] <- df$.vr_date
  }

  df %>% dplyr::select(-.vr_date)
}

vr_add_time_unit_to_views <- function(views,
                                      time_unit = c("year", "month", "date"),
                                      date_col = "game_date",
                                      year_col = "year",
                                      out_col = ".time_group") {
  time_unit <- match.arg(time_unit)

  if (is.null(views) || !is.list(views)) {
    stop("`views` must be a list of data frames.", call. = FALSE)
  }

  out <- views
  for (nm in intersect(names(out), c("pbp", "pitch", "pa", "bbe"))) {
    if (is.data.frame(out[[nm]])) {
      out[[nm]] <- vr_add_time_unit(out[[nm]], time_unit = time_unit, date_col = date_col, year_col = year_col, out_col = out_col)
    }
  }
  out
}

# -----------------------------------------------------------------------------
# Export helpers for quick visual browsing
# -----------------------------------------------------------------------------

save_vr_plot <- function(plot_obj,
                         filename,
                         folder = "eda_exports",
                         width = 10,
                         height = 6,
                         dpi = 150) {
  library(ggplot2)

  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  p <- if (inherits(plot_obj, "ggplot")) plot_obj else plot_obj$plot

  if (!inherits(p, "ggplot")) {
    stop("`plot_obj` must be a ggplot or a list containing `$plot`.", call. = FALSE)
  }

  path <- file.path(folder, filename)
  if (!grepl("\\.png$", path, ignore.case = TRUE)) path <- paste0(path, ".png")

  ggplot2::ggsave(path, p, width = width, height = height, dpi = dpi)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

save_vr_plot_set <- function(plot_list,
                             folder = "eda_exports",
                             width = 10,
                             height = 6,
                             dpi = 150) {
  if (is.null(names(plot_list)) || any(names(plot_list) == "")) {
    names(plot_list) <- paste0("plot_", seq_along(plot_list))
  }

  paths <- character(length(plot_list))

  for (i in seq_along(plot_list)) {
    nm <- names(plot_list)[i]
    safe_nm <- gsub("[^A-Za-z0-9_ -]", "", nm)
    safe_nm <- gsub("[ ]+", "_", safe_nm)
    paths[i] <- save_vr_plot(plot_list[[i]], paste0(sprintf("%02d_", i), safe_nm, ".png"), folder, width, height, dpi)
  }

  names(paths) <- names(plot_list)
  paths
}

# -----------------------------------------------------------------------------
# Multi-season data helpers
# -----------------------------------------------------------------------------

bind_pbp_seasons <- function(...,
                             .list = NULL,
                             season_col = "year",
                             date_col = "game_date") {
  library(dplyr)
  library(lubridate)
  library(purrr)

  pieces <- if (!is.null(.list)) .list else list(...)

  if (length(pieces) == 0) {
    stop("Provide season data frames through `...` or `.list`.", call. = FALSE)
  }

  nm <- names(pieces)

  purrr::imap_dfr(pieces, function(df, name) {
    if (!is.data.frame(df)) stop("Every season object must be a data frame.", call. = FALSE)

    out <- df
    if (date_col %in% names(out)) {
      out[[date_col]] <- lubridate::ymd(out[[date_col]], quiet = TRUE)
    }

    if (!season_col %in% names(out)) {
      guessed_year <- suppressWarnings(as.integer(name))
      if (!is.na(guessed_year)) {
        out[[season_col]] <- guessed_year
      } else if (date_col %in% names(out)) {
        out[[season_col]] <- lubridate::year(out[[date_col]])
      }
    }

    out
  })
}

prepare_visual_revolution_views <- function(pbp,
                                            enhanced = FALSE,
                                            date_col = "game_date",
                                            season_col = "year") {
  library(dplyr)
  library(lubridate)

  if (!date_col %in% names(pbp)) {
    stop("`pbp` must include a date column named `", date_col, "`.", call. = FALSE)
  }

  pbp <- pbp %>%
    mutate(
      "{date_col}" := lubridate::ymd(.data[[date_col]], quiet = TRUE),
      "{season_col}" := if (season_col %in% names(.)) .data[[season_col]] else lubridate::year(.data[[date_col]]),
      year_month = lubridate::floor_date(.data[[date_col]], unit = "month")
    )

  prepare_pbp_views(pbp, enhanced = enhanced)
}
