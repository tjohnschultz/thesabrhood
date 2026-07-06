mlb_team_colors <- tribble(
  ~team_abbr, ~team_name, ~primary_color, ~secondary_color,
  "ARI", "Arizona Diamondbacks", "#A71930", "#000000",
  "ATL", "Atlanta Braves", "#CE1141", "#13274F",
  "BAL", "Baltimore Orioles", "#DF4601", "#000000",
  "BOS", "Boston Red Sox", "#BD3039", "#0C2340",
  "CHC", "Chicago Cubs", "#0E3386", "#CC3433",
  "CWS", "Chicago White Sox", "#27251F", "#C4CED4",
  "CIN", "Cincinnati Reds", "#C6011F", "#000000",
  "CLE", "Cleveland Guardians", "#E31937", "#0C2340",
  "COL", "Colorado Rockies", "#33006F", "#C4CED4",
  "DET", "Detroit Tigers", "#0C2340", "#FA4616",
  "HOU", "Houston Astros", "#002D62", "#EB6E1F",
  "KC",  "Kansas City Royals", "#004687", "#BD9B60",
  "LAA", "Los Angeles Angels", "#BA0021", "#003263",
  "LAD", "Los Angeles Dodgers", "#005A9C", "#EF3E42",
  "MIA", "Miami Marlins", "#00A3E0", "#EF3340",
  "MIL", "Milwaukee Brewers", "#12284B", "#FFC52F",
  "MIN", "Minnesota Twins", "#002B5C", "#D31145",
  "NYM", "New York Mets", "#002D72", "#FF5910",
  "NYY", "New York Yankees", "#0C2340", "#C4CED4",
  "ATH", "Athletics", "#003831", "#EFB21E",
  "PHI", "Philadelphia Phillies", "#E81828", "#002D72",
  "PIT", "Pittsburgh Pirates", "#FDB827", "#27251F",
  "SD",  "San Diego Padres", "#2F241D", "#FFC425",
  "SF",  "San Francisco Giants", "#FD5A1E", "#27251F",
  "SEA", "Seattle Mariners", "#0C2C56", "#005C5C",
  "STL", "St. Louis Cardinals", "#C41E3A", "#0C2340",
  "TB",  "Tampa Bay Rays", "#092C5C", "#8FBCE6",
  "TEX", "Texas Rangers", "#003278", "#C0111F",
  "TOR", "Toronto Blue Jays", "#134A8E", "#E8291C",
  "WSH", "Washington Nationals", "#AB0003", "#14225A"
)

get_team_colors <- function(team_abbr) {
  mlb_team_colors %>%
    filter(team_abbr == toupper(team_abbr)) %>%
    slice(1)
}

theme_mlb_team <- function(team_abbr) {
  
  colors <- get_team_colors(team_abbr)
  
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(
        color = colors$primary_color,
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        color = colors$secondary_color,
        size = 12
      ),
      axis.title = element_text(
        color = colors$primary_color,
        face = "bold"
      ),
      axis.text = element_text(
        color = "#222222"
      ),
      panel.grid.major = element_line(
        color = scales::alpha(colors$secondary_color, 0.25)
      ),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(
        fill = colors$primary_color,
        color = NA
      ),
      strip.text = element_text(
        color = "white",
        face = "bold"
      ),
      legend.title = element_text(
        color = colors$primary_color,
        face = "bold"
      )
    )
}