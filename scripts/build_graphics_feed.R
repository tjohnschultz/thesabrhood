workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(ggplot2))

data_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path(workspace, "data", "derived"))
graphics_dir <- Sys.getenv("SABRHOOD_GRAPHICS_DIR", unset = file.path(workspace, "images", "graphics-feed"))
dir.create(graphics_dir, recursive = TRUE, showWarnings = FALSE)

read_product <- function(name) {
  data <- utils::read.csv(file.path(data_dir, name), stringsAsFactors = FALSE, check.names = FALSE)
  token_codes <- c("00E1", "00E9", "00ED", "00F1", "00F3", "00FA")
  token_map <- stats::setNames(c("a", "e", "i", "n", "o", "u"), paste0("<", "U+", token_codes, ">"))
  character_columns <- names(data)[vapply(data, is.character, logical(1))]
  for (column in character_columns) {
    for (token in names(token_map)) data[[column]] <- gsub(token, token_map[[token]], data[[column]], fixed = TRUE)
  }
  data
}

hitters <- read_product("hitter-change-profiles.csv")
pitchers <- read_product("pitcher-change-profiles.csv")
pitch_mix <- read_product("pitch-usage-change-board.csv")
teams <- read_product("team-intelligence-summary.csv")
league_pitch_usage <- read_product("rolling-league-pitch-usage.csv")
league_production <- read_product("rolling-league-production.csv")
award_races <- read_product("award-race-board.csv")
award_history <- read_product("award-race-display.csv")
positional_war <- read_product("team-positional-war.csv")
discipline <- read_product("hitter-discipline-profiles.csv")
arsenal_spotlights <- read_product("arsenal-spotlights.csv")
pull_spray <- read_product("pull-rate-leader-batted-balls.csv")

brand_navy <- "#0C2340"
brand_red <- "#BD3039"
brand_steel <- "#617487"
brand_cream <- "#F5F1E8"
brand_blue <- "#4E7696"

sabr_theme <- function() {
  theme_minimal(base_family = "Arial", base_size = 16) +
    theme(
      plot.background = element_rect(fill = brand_cream, color = NA),
      panel.background = element_rect(fill = brand_cream, color = NA),
      plot.title = element_text(color = brand_navy, face = "bold", size = 29, margin = margin(b = 8)),
      plot.subtitle = element_text(color = brand_steel, size = 14, margin = margin(b = 22)),
      axis.title = element_text(color = brand_steel, size = 12),
      axis.text = element_text(color = brand_navy, size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#D9D3C7", linewidth = 0.45),
      plot.caption = element_text(color = brand_steel, size = 9.5, hjust = 0, margin = margin(t = 22)),
      plot.tag = element_text(color = brand_red, face = "bold", size = 10, hjust = 1),
      plot.tag.position = c(0.98, 0.985),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(34, 48, 28, 42)
    )
}

brand_plot <- function(plot) {
  plot + labs(tag = "THE SABRHOOD  /  VISUAL DESK")
}

save_plot <- function(plot, file_name, width = 16, height = 10) {
  path <- file.path(graphics_dir, file_name)
  ggsave(path, brand_plot(plot), width = width, height = height, units = "in", dpi = 150, bg = brand_cream)
  path
}

movement_plot <- function(data, perspective, file_name) {
  data <- data[is.finite(data$dominant_change_abs_z) & data$recent_pa >= 15, , drop = FALSE]
  data <- data[order(-data$change_signal_score, -data$dominant_change_abs_z), , drop = FALSE]
  data <- utils::head(data, 12L)
  data$signed_z <- ifelse(data$dominant_change_direction == "improving", data$dominant_change_abs_z, -data$dominant_change_abs_z)
  data$label <- paste0(data$player_name, "  |  ", data$team, "  |  ", data$dominant_change_label)
  data$label <- factor(data$label, levels = rev(data$label))
  data$direction <- ifelse(data$signed_z >= 0, "Improving", "Declining")
  title <- if (perspective == "Hitter") "The Hitters Changing Fastest" else "The Pitchers Changing Fastest"
  subtitle <- "Largest recent-versus-prior movement, standardized against the MLB change distribution"
  plot <- ggplot(data, aes(x = signed_z, y = label, fill = direction)) +
    geom_vline(xintercept = 0, color = brand_steel, linewidth = 0.65) +
    geom_col(width = 0.66) +
    geom_text(aes(label = sprintf("%+.1f z", signed_z)),
      hjust = ifelse(data$signed_z >= 0, -0.12, 1.12), color = brand_navy, fontface = "bold", size = 3.8) +
    scale_fill_manual(values = c(Improving = brand_red, Declining = brand_navy)) +
    scale_x_continuous(expand = expansion(mult = c(0.14, 0.14))) +
    labs(title = title, subtitle = subtitle, x = "Standard deviations from the typical MLB change", y = NULL,
      fill = NULL, caption = "THE SABRHOOD  |  Direction reflects player-performance context  |  Minimum 15 recent PA/BF") +
    sabr_theme() +
    theme(panel.grid.major.y = element_blank(), legend.position = "top", axis.text.y = element_text(face = "bold", size = 10.5))
  save_plot(plot, file_name)
}

fingerprint_plot <- function(data, perspective, file_name) {
  percentile_cols <- c("season_ops_percentile", "season_woba_estimate_percentile", "season_strikeout_rate_percentile",
    "season_walk_rate_percentile", "season_hard_hit_rate_percentile", "season_run_value_per_pa_percentile")
  labels <- c("OPS", "wOBA estimate", "Strikeout rate", "Walk rate", "Hard-hit rate", "Run value / PA")
  data <- data[data$season_pa >= 100, , drop = FALSE]
  separation <- apply(data[, percentile_cols, drop = FALSE], 1, function(x) max(abs(as.numeric(x) - 50), na.rm = TRUE))
  data <- data[order(-separation, -data$change_signal_score), , drop = FALSE]
  data <- utils::head(data, 8L)
  long <- do.call(rbind, lapply(seq_len(nrow(data)), function(index) {
    data.frame(player = paste0(data$player_name[[index]], " | ", data$team[[index]]), metric = labels,
      percentile = as.numeric(data[index, percentile_cols]), stringsAsFactors = FALSE)
  }))
  long$player <- factor(long$player, levels = rev(unique(long$player)))
  long$metric <- factor(long$metric, levels = labels)
  title <- paste(perspective, "Profiles Furthest From the League Middle")
  subtitle <- "Season percentiles show where each player separates from the MLB distribution"
  plot <- ggplot(long, aes(x = metric, y = player, fill = percentile)) +
    geom_tile(color = brand_cream, linewidth = 2) +
    geom_text(aes(label = sprintf("%d", round(percentile))), color = "white", fontface = "bold", size = 4) +
    scale_fill_gradient2(low = brand_blue, mid = brand_steel, high = brand_red, midpoint = 50, limits = c(0, 100)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = "MLB percentile",
      caption = "THE SABRHOOD  |  Percentile direction describes the statistic; it is not always a good-to-bad scale") +
    sabr_theme() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold", angle = 18, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 11), legend.position = "top")
  save_plot(plot, file_name)
}

pitch_mix_plot <- function() {
  data <- pitch_mix[is.finite(pitch_mix$usage_delta_pp) & pitch_mix$recent_pitches >= 40, , drop = FALSE]
  data <- data[order(-abs(data$usage_delta_pp), -data$change_signal_score), , drop = FALSE]
  data <- utils::head(data, 14L)
  data$label <- paste0(data$pitcher_name, "  |  ", data$pitch_name, "  |  ", data$team)
  data$label <- factor(data$label, levels = rev(data$label))
  data$direction_label <- ifelse(data$usage_delta_pp >= 0, "Used more", "Used less")
  plot <- ggplot(data, aes(x = usage_delta_pp, y = label, fill = direction_label)) +
    geom_vline(xintercept = 0, color = brand_steel, linewidth = 0.65) +
    geom_col(width = 0.66) +
    geom_text(aes(label = sprintf("%+.1f pp", usage_delta_pp)),
      hjust = ifelse(data$usage_delta_pp >= 0, -0.1, 1.1), color = brand_navy, fontface = "bold", size = 3.7) +
    scale_fill_manual(values = c("Used more" = brand_red, "Used less" = brand_navy)) +
    scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
    labs(title = "The Pitches Taking On New Roles", subtitle = "Largest usage changes over the most recent five-game window",
      x = "Change in pitch usage (percentage points)", y = NULL, fill = NULL,
      caption = "THE SABRHOOD  |  Recent five games versus prior-season window  |  Minimum 40 recent pitches") +
    sabr_theme() +
    theme(panel.grid.major.y = element_blank(), legend.position = "top", axis.text.y = element_text(face = "bold", size = 10.2))
  save_plot(plot, "pitch-usage-movers.png")
}

team_quadrant_plot <- function() {
  teams$label_score <- abs(scale(teams$offense_woba)) + abs(scale(teams$opponent_woba))
  label_teams <- teams$team[order(-teams$label_score)][1:12]
  plot <- ggplot(teams, aes(x = offense_woba, y = opponent_woba)) +
    geom_vline(xintercept = mean(teams$offense_woba, na.rm = TRUE), color = brand_steel, linewidth = 0.7) +
    geom_hline(yintercept = mean(teams$opponent_woba, na.rm = TRUE), color = brand_steel, linewidth = 0.7) +
    geom_point(aes(size = team_index, color = team_index), alpha = 0.9) +
    geom_text(data = teams[teams$team %in% label_teams, , drop = FALSE], aes(label = team),
      nudge_y = 0.0035, check_overlap = TRUE, color = brand_navy, fontface = "bold", size = 3.4) +
    scale_y_reverse() +
    scale_color_gradient(low = brand_blue, high = brand_red) +
    scale_size_continuous(range = c(3, 8)) +
    labs(title = "Which Teams Have Separated From the Pack?",
      subtitle = "Offensive production and run prevention on shared season scales",
      x = "Team estimated wOBA (more offense to the right)", y = "Opponent estimated wOBA (better prevention toward the top)",
      color = "Team index", size = "Team index",
      caption = "THE SABRHOOD  |  Crosshairs represent MLB means  |  Labels highlight the largest two-axis outliers") +
    sabr_theme() +
    theme(legend.position = "top")
  save_plot(plot, "team-identity-quadrant.png")
}

league_pitch_usage_plot <- function() {
  data <- league_pitch_usage
  data$date <- as.Date(data$date)
  final <- data[data$date == max(data$date), , drop = FALSE]
  top_types <- utils::head(final$pitch_type[order(-final$season_usage_rate)], 8L)
  data <- data[data$pitch_type %in% top_types & data$date >= min(data$date) + 13, , drop = FALSE]
  ordering <- final$pitch_name[match(top_types, final$pitch_type)]
  data$pitch_name <- factor(data$pitch_name, levels = ordering)
  plot <- ggplot(data, aes(x = date, y = 100 * usage_rate_rolling, group = pitch_type)) +
    geom_line(color = brand_navy, linewidth = 1.15) +
    geom_point(data = data[!duplicated(data$pitch_type, fromLast = TRUE), , drop = FALSE], color = brand_red, size = 2.6) +
    facet_wrap(~ pitch_name, ncol = 2, scales = "free_y") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "How the League Is Choosing to Pitch", subtitle = "Fourteen-day rolling usage for MLB's eight most common pitch types",
      x = NULL, y = "Pitch usage (%)",
      caption = "THE SABRHOOD  |  Calendar-day rolling windows  |  Latest point highlighted in red") +
    sabr_theme() +
    theme(strip.text = element_text(color = brand_navy, face = "bold", size = 12), panel.grid.minor = element_blank())
  save_plot(plot, "league-pitch-usage-trends.png", height = 12)
}

league_production_plot <- function() {
  data <- league_production
  data$date <- as.Date(data$date)
  data <- data[data$date >= min(data$date) + 13, , drop = FALSE]
  metric_columns <- c("slugging_percentage", "home_run_rate", "strikeout_rate", "walk_rate", "hard_hit_rate")
  metric_labels <- c("Slugging", "Home-run rate", "Strikeout rate", "Walk rate", "Hard-hit rate")
  long <- do.call(rbind, lapply(seq_along(metric_columns), function(index) {
    values <- as.numeric(data[[metric_columns[[index]]]])
    data.frame(date = data$date, metric = metric_labels[[index]], index = 100 * values / mean(values, na.rm = TRUE), stringsAsFactors = FALSE)
  }))
  long$metric <- factor(long$metric, levels = metric_labels)
  last_rows <- long[!duplicated(long$metric, fromLast = TRUE), , drop = FALSE]
  plot <- ggplot(long, aes(x = date, y = index, group = metric)) +
    geom_hline(yintercept = 100, color = brand_steel, linewidth = 0.55) +
    geom_line(color = brand_navy, linewidth = 1.15) +
    geom_point(data = last_rows, color = brand_red, size = 2.6) +
    facet_wrap(~ metric, ncol = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "The Shape of MLB Offense Is Moving", subtitle = "Fourteen-day league rates indexed to each metric's season average (100)",
      x = NULL, y = "Index (season average = 100)",
      caption = "THE SABRHOOD  |  Values above 100 are above the season rolling average for that statistic") +
    sabr_theme() +
    theme(strip.text = element_text(color = brand_navy, face = "bold", size = 12), panel.grid.minor = element_blank())
  save_plot(plot, "league-production-trends.png", height = 13)
}

award_race_plot <- function(award, league, file_name) {
  data <- award_races[award_races$award == award & award_races$league == league, , drop = FALSE]
  if (!nrow(data)) stop("No award-race rows for ", league, " ", award, call. = FALSE)
  data <- data[order(as.numeric(data$rank)), , drop = FALSE]
  data <- utils::head(data, 8L)
  data$label <- paste0(data$player_name, "  |  ", data$team, "  |  ", data$role)
  data$label <- factor(data$label, levels = rev(data$label))
  data$standing <- ifelse(data$rank == 1, "Leader", "Chasing")
  award_label <- if (award == "ROTY watch") "ROTY" else award
  subtitle <- if (award == "ROTY watch") {
    "Role-balanced rookie score: hitters and pitchers are graded within their own performance models"
  } else {
    "Transparent performance index built from FanGraphs season totals and rates"
  }
  plot <- ggplot(data, aes(x = as.numeric(award_score), y = label, fill = standing)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = sprintf("%.1f", as.numeric(award_score))), hjust = -0.14,
      color = brand_navy, fontface = "bold", size = 4) +
    scale_fill_manual(values = c(Leader = brand_red, Chasing = brand_navy)) +
    scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), expand = expansion(mult = c(0, 0))) +
    labs(title = paste(league, award_label, "Performance Ladder"), subtitle = subtitle,
      x = "Award performance score", y = NULL, fill = NULL,
      caption = "THE SABRHOOD  |  Performance index, not a ballot forecast  |  Current FanGraphs season snapshot") +
    sabr_theme() +
    theme(panel.grid.major.y = element_blank(), legend.position = "none",
      axis.text.y = element_text(face = "bold", size = 10.5))
  save_plot(plot, file_name)
}

award_history_plot <- function(league, file_name) {
  data <- award_history[award_history$award == "MVP" & award_history$league == league, , drop = FALSE]
  if (!nrow(data)) stop("No award-history rows for ", league, " MVP", call. = FALSE)
  data$checkpoint_date <- as.Date(data$checkpoint_date)
  data$race_rating <- as.numeric(data$race_rating)
  data$race_rank <- as.numeric(data$race_rank)
  data$is_current_leader <- as.logical(data$is_current_leader)
  endpoint_date <- max(data$checkpoint_date)
  endpoints <- data[data$checkpoint_date == endpoint_date, , drop = FALSE]
  endpoints <- endpoints[order(endpoints$race_rank), , drop = FALSE]
  endpoints$label_y <- endpoints$race_rating
  if (nrow(endpoints) > 1L) {
    for (index in 2:nrow(endpoints)) {
      endpoints$label_y[[index]] <- min(endpoints$label_y[[index]], endpoints$label_y[[index - 1L]] - 1.15)
    }
  }
  players <- endpoints$player_name
  palette <- c(brand_red, brand_navy, brand_blue, "#D39A2C", "#287B6A", "#76558B", "#C46A3B", "#6D7782")
  colors <- stats::setNames(palette[seq_along(players)], players)
  data$player_name <- factor(data$player_name, levels = players)
  data$leader_weight <- ifelse(data$is_current_leader, "Current leader", "Challenger")
  leader_changes <- data[data$race_rank == 1 & !duplicated(data$checkpoint_date), , drop = FALSE]
  plot <- ggplot(data, aes(x = checkpoint_date, y = race_rating, group = player_name, color = player_name)) +
    geom_line(aes(linewidth = leader_weight), alpha = 0.94) +
    geom_point(data = endpoints, size = 3.2) +
    geom_point(data = leader_changes, shape = 21, fill = brand_cream, stroke = 1.15, size = 3.4) +
    geom_segment(data = endpoints, aes(x = endpoint_date, xend = endpoint_date + 2.5, y = race_rating, yend = label_y),
      linewidth = 0.55, show.legend = FALSE) +
    geom_text(data = endpoints, aes(x = endpoint_date + 3.2, y = label_y, label = paste0(player_name, "  ", sprintf("%.1f", race_rating))),
      hjust = 0, fontface = "bold", size = 3.45, show.legend = FALSE) +
    scale_color_manual(values = colors, drop = FALSE) +
    scale_linewidth_manual(values = c("Current leader" = 1.8, "Challenger" = 0.9), guide = "none") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(mult = c(0.02, 0.19))) +
    scale_y_continuous(limits = c(max(0, floor(min(endpoints$label_y, data$race_rating, na.rm = TRUE) - 1)), 100),
      breaks = scales::breaks_pretty(n = 7), expand = expansion(mult = c(0.01, 0.03))) +
    coord_cartesian(clip = "off") +
    labs(
      title = paste(league, "MVP Race, Week by Week"),
      subtitle = "The current top eight traced through cumulative, date-bounded FanGraphs checkpoints",
      x = NULL, y = "Season-to-date Race Rating", color = NULL,
      caption = "THE SABRHOOD  |  Weekly cumulative checkpoints  |  Open circles mark the leader at each checkpoint  |  No future statistics enter an earlier rating"
    ) +
    sabr_theme() +
    theme(legend.position = "none", panel.grid.minor = element_blank(), plot.margin = margin(34, 180, 28, 42))
  save_plot(plot, file_name, width = 16, height = 10)
}

positional_war_plot <- function() {
  data <- positional_war
  data$position_label <- factor(data$position_label,
    levels = c("Catcher", "First base", "Second base", "Third base", "Shortstop", "Outfield", "Designated hitter", "Starting pitching", "Relief pitching"))
  team_order <- stats::aggregate(data$percentile, list(team = data$team), mean, na.rm = TRUE)
  team_order <- team_order$team[order(-team_order$x)]
  data$team <- factor(data$team, levels = rev(team_order))
  plot <- ggplot(data, aes(x = position_label, y = team, fill = percentile)) +
    geom_tile(color = brand_cream, linewidth = 1.25) +
    geom_text(aes(label = mlb_rank), color = "white", fontface = "bold", size = 3.2) +
    scale_fill_gradient2(low = brand_red, mid = brand_steel, high = brand_navy, midpoint = 50, limits = c(0, 100)) +
    labs(title = "Where Every MLB Roster Creates Its Value", subtitle = "Team WAR rank by primary position; 1 is the strongest positional group in MLB",
      x = NULL, y = NULL, fill = "Position percentile",
      caption = "THE SABRHOOD  |  FanGraphs WAR assigned to each player's primary listed position  |  SP and RP use season start share") +
    sabr_theme() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold", angle = 32, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 9), legend.position = "top")
  save_plot(plot, "mlb-positional-war-map.png", width = 16, height = 14)
}

discipline_frontier_plot <- function() {
  data <- discipline[is.finite(discipline$chase_rate) & is.finite(discipline$zone_contact_rate), , drop = FALSE]
  data$frontier <- (1 - data$chase_rate) + data$zone_contact_rate
  labels <- utils::head(data$player_name[order(-data$discipline_score)], 14L)
  label_data <- data[data$player_name %in% labels, , drop = FALSE]
  plot <- ggplot(data, aes(x = 100 * chase_rate, y = 100 * zone_contact_rate)) +
    geom_vline(xintercept = 100 * mean(data$chase_rate), color = brand_steel, linewidth = 0.6) +
    geom_hline(yintercept = 100 * mean(data$zone_contact_rate), color = brand_steel, linewidth = 0.6) +
    geom_point(aes(size = pitches, color = discipline_score), alpha = 0.55) +
    geom_point(data = label_data, color = brand_red, size = 3.4) +
    geom_text(data = label_data, aes(label = paste0(player_name, " | ", team)), nudge_y = 0.55,
      check_overlap = TRUE, color = brand_navy, fontface = "bold", size = 3.2) +
    scale_color_gradient(low = brand_blue, high = brand_red) +
    scale_size_continuous(range = c(1.5, 6)) +
    scale_x_reverse() +
    labs(title = "The Plate-Discipline Frontier", subtitle = "Chase less to move right; make more contact in the zone to move up",
      x = "Chase rate (lower is better, right)", y = "Zone-contact rate (%)", color = "Discipline score", size = "Pitches",
      caption = "THE SABRHOOD  |  Minimum 300 pitches seen  |  Crosshairs are MLB means") +
    sabr_theme() + theme(legend.position = "top")
  save_plot(plot, "hitter-discipline-frontier.png")
}

arsenal_spotlight_plot <- function() {
  data <- arsenal_spotlights
  data$is_featured <- data$pitch_type == data$featured_pitch_type
  data$player_label <- paste0(data$player_name, " | ", data$team, "\n",
    data$featured_pitch_name, " +", sprintf("%.1f", data$featured_usage_delta_pp), " pp usage")
  data$player_label <- factor(data$player_label, levels = unique(data$player_label))
  plot <- ggplot(data, aes(x = average_horizontal_break, y = average_induced_vertical_break)) +
    geom_hline(yintercept = 0, color = brand_steel, linewidth = 0.45) +
    geom_vline(xintercept = 0, color = brand_steel, linewidth = 0.45) +
    geom_point(aes(size = usage_rate, fill = is_featured), shape = 21, color = brand_navy, stroke = 1.1, alpha = 0.9) +
    geom_text(aes(label = pitch_name), nudge_y = 0.65, check_overlap = TRUE, color = brand_navy, fontface = "bold", size = 3.2) +
    facet_wrap(~ player_label, ncol = 2, scales = "free") +
    scale_fill_manual(
      values = c(`TRUE` = brand_red, `FALSE` = brand_cream),
      labels = c(`TRUE` = "Rising pitch", `FALSE` = "Other pitch")
    ) +
    scale_size_continuous(range = c(3, 10)) +
    labs(title = "The Pitches Taking Over Their Arsenals", subtitle = "A pitch earned more usage and more whiffs recently; the full arsenal shows what it plays beside",
      x = "Horizontal break", y = "Induced vertical break", size = "Season usage", fill = "Featured pitch",
      caption = "THE SABRHOOD  |  Featured pitch: usage and whiff rate both increased over the last five games") +
    sabr_theme() +
    theme(strip.text = element_text(color = brand_navy, face = "bold", size = 11), legend.position = "top")
  save_plot(plot, "arsenal-takeover-spotlights.png", height = 12)
}

pull_spray_plot <- function() {
  data <- pull_spray
  data$x <- as.numeric(data$hit_coord_x) - 125
  data$y <- 250 - as.numeric(data$hit_coord_y)
  data <- data[is.finite(data$x) & is.finite(data$y), , drop = FALSE]
  data$result_group <- ifelse(data$event_key == "home_run", "Home run",
    ifelse(data$event_key %in% c("single", "double", "triple"), "Other hit", "Out"))
  leader <- data$batter_name[[1L]]
  team <- data$batting_team[[1L]]
  pull_rate <- 100 * as.numeric(data$leader_pull_rate[[1L]])
  plot <- ggplot(data, aes(x = x, y = y, color = spray_direction)) +
    annotate("segment", x = 0, xend = -120, y = 0, yend = 190, color = brand_navy, linewidth = 1) +
    annotate("segment", x = 0, xend = 120, y = 0, yend = 190, color = brand_navy, linewidth = 1) +
    annotate("curve", x = -120, xend = 120, y = 190, yend = 190, curvature = -0.32, color = brand_navy, linewidth = 1) +
    geom_point(aes(size = launch_speed, shape = result_group), alpha = 0.75, stroke = 0.7) +
    scale_color_manual(values = c(pull = brand_red, middle = brand_navy, opposite = brand_blue), na.value = brand_steel) +
    scale_size_continuous(range = c(2, 7)) +
    coord_equal(xlim = c(-135, 135), ylim = c(-5, 210), clip = "off") +
    labs(title = paste0(leader, " Owns the Pull Side"), subtitle = paste0(team, " hitter leads the qualified field with a ", sprintf("%.1f", pull_rate), "% pull rate"),
      x = NULL, y = NULL, color = "Spray direction", size = "Exit velocity", shape = "Result",
      caption = "THE SABRHOOD  |  MLBAM batted-ball coordinates  |  Point size reflects exit velocity") +
    guides(
      size = "none",
      color = guide_legend(order = 1L, nrow = 1L),
      shape = guide_legend(order = 2L, nrow = 1L)
    ) +
    sabr_theme() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
  save_plot(plot, "pull-rate-leader-spray.png", width = 13, height = 12)
}

movement_plot(hitters, "Hitter", "hitter-change-leaders.png")
movement_plot(pitchers, "Pitcher", "pitcher-change-leaders.png")
fingerprint_plot(hitters, "Hitter", "hitter-league-separation.png")
fingerprint_plot(pitchers, "Pitcher", "pitcher-league-separation.png")
pitch_mix_plot()
team_quadrant_plot()
league_pitch_usage_plot()
league_production_plot()
award_history_plot("AL", "al-mvp-race.png")
award_history_plot("NL", "nl-mvp-race.png")
award_race_plot("Cy Young", "AL", "al-cy-young-race.png")
award_race_plot("Cy Young", "NL", "nl-cy-young-race.png")
award_race_plot("ROTY watch", "AL", "al-roty-watch.png")
award_race_plot("ROTY watch", "NL", "nl-roty-watch.png")
award_race_plot("Reliever of the Year", "AL", "al-reliever-race.png")
award_race_plot("Reliever of the Year", "NL", "nl-reliever-race.png")
positional_war_plot()
discipline_frontier_plot()
arsenal_spotlight_plot()
pull_spray_plot()

manifest <- data.frame(
  graphic_id = c("league-pitch-usage-trends", "league-production-trends", "mlb-positional-war-map", "hitter-discipline-frontier", "arsenal-takeover-spotlights", "pull-rate-leader-spray", "hitter-change-leaders", "pitcher-change-leaders", "pitch-usage-movers", "hitter-league-separation", "pitcher-league-separation", "team-identity-quadrant"),
  page_group = c("League trends", "League trends", "Team identity", "Player profiles", "Pitch changes", "Player profiles", "Player movement", "Player movement", "Pitch changes", "League separation", "League separation", "Team identity"),
  title = c("League Pitch Usage Trends", "League Production Trends", "MLB Positional WAR Map", "Plate-Discipline Frontier", "Arsenal Takeover Spotlights", "Pull-Rate Leader Spray Chart", "Hitters Changing Fastest", "Pitchers Changing Fastest", "Pitches Taking On New Roles", "Hitter League Separation", "Pitcher League Separation", "Team Identity Quadrant"),
  subtitle = c(
    "Fourteen-day rolling pitch-type usage reveals how MLB's collective arsenal is changing.",
    "League production rates show when the run environment, contact, and plate discipline are moving together.",
    "Every team and primary position ranked by FanGraphs WAR to expose roster strengths and needs.",
    "The league's best combinations of chase avoidance and in-zone contact.",
    "Full arsenal movement maps for pitchers whose rising pitch gained usage and whiffs together.",
    "A field-level view of the qualified hitter who produces the league's highest pull rate.",
    "The strongest recent hitter shifts after comparing each player with his earlier-season baseline and the league change distribution.",
    "The strongest recent pitcher shifts after comparing each arm with his earlier-season baseline and the league change distribution.",
    "The pitch types with the largest recent usage changes, paired with pitcher and team context.",
    "Multi-stat season fingerprints for hitters furthest from the league middle.",
    "Multi-stat season fingerprints for pitchers furthest from the league middle.",
    "All 30 clubs positioned by offensive production and run prevention."
  ),
  image_path = file.path("images", "graphics-feed", paste0(c("league-pitch-usage-trends", "league-production-trends", "mlb-positional-war-map", "hitter-discipline-frontier", "arsenal-takeover-spotlights", "pull-rate-leader-spray", "hitter-change-leaders", "pitcher-change-leaders", "pitch-usage-movers", "hitter-league-separation", "pitcher-league-separation", "team-identity-quadrant"), ".png")),
  file_name = paste0(c("league-pitch-usage-trends", "league-production-trends", "mlb-positional-war-map", "hitter-discipline-frontier", "arsenal-takeover-spotlights", "pull-rate-leader-spray", "hitter-change-leaders", "pitcher-change-leaders", "pitch-usage-movers", "hitter-league-separation", "pitcher-league-separation", "team-identity-quadrant"), ".png"),
  alt_text = c(
    "Small-multiple line charts showing rolling MLB pitch-type usage.",
    "Small-multiple line charts showing indexed rolling MLB production rates.",
    "Heatmap ranking every MLB team by FanGraphs WAR at each primary position.",
    "Scatterplot comparing hitter chase rate with zone contact rate.",
    "Faceted movement maps showing four pitchers' full arsenals and a highlighted rising pitch.",
    "Spray chart showing batted balls for MLB's qualified pull-rate leader.",
    "Horizontal bars ranking hitters by standardized recent performance change.",
    "Horizontal bars ranking pitchers by standardized recent performance change.",
    "Horizontal bars showing the largest recent changes in pitch usage.",
    "Heatmap of season percentiles for hitters with the largest league separation.",
    "Heatmap of season percentiles for pitchers with the largest league separation.",
    "Scatterplot positioning MLB teams by offense and run prevention."
  ),
  coverage_note = c(
    "Fourteen-day rolling league pitch usage through the current PBP date.",
    "Fourteen-day rolling league production through the current PBP date.",
    "Current FanGraphs season WAR aggregated by primary listed position.",
    "Pitch-level discipline profiles with at least 300 pitches seen.",
    "Recent five-game pitch changes placed inside each pitcher's season arsenal.",
    "Full-season batted balls for the qualified pull-rate leader.",
    "Recent versus prior performance; movement standardized against MLB.",
    "Recent versus prior performance allowed; movement standardized against MLB.",
    "Recent five-game pitch mix versus the prior-season window.",
    "Full-season pitch-by-pitch summary percentiles.",
    "Full-season pitch-by-pitch summary percentiles.",
    "Full-season team summaries through the current data date."
  ),
  orientation = "landscape",
  featured = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
  display_order = seq_len(12L),
  source_acquired_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  publication_status = "prototype",
  stringsAsFactors = FALSE
)

utils::write.csv(manifest, file.path(data_dir, "graphics-feed-manifest.csv"), row.names = FALSE, na = "")
cat("Built", nrow(manifest), "branded graphics feed assets.\n")
