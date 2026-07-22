% Generated manually for the development package.
\name{daily-inputs-and-trends}
\alias{standardize_baseballr_rosters}
\alias{filter_active_roster_bullpen}
\alias{assemble_baseballr_game_inputs}
\alias{build_open_meteo_url}
\alias{build_pitch_usage_change_board}
\alias{build_aaa_performance_watch}
\title{Build daily BaseballR inputs and compact reporting watches}
\usage{
standardize_baseballr_rosters(rosters, as_of_date = Sys.Date())

filter_active_roster_bullpen(availability, active_rosters,
  probable_starter_ids = character())

assemble_baseballr_game_inputs(schedule, probables = NULL,
  batting_orders = NULL)

build_open_meteo_url(latitude, longitude, start_date,
  end_date = start_date, timezone = "auto")

build_pitch_usage_change_board(pitches, recent_games = 5L,
  minimum_recent_pitches = 15L, minimum_baseline_pitches = 40L)

build_aaa_performance_watch(hitting, pitching, minimum_pa = 100L,
  minimum_ip = 25, prospect_age = 24L)
}
\arguments{
\item{rosters}{One or more results returned by \code{baseballr::mlb_rosters()}.}
\item{as_of_date}{Date the roster was acquired.}
\item{availability}{A bullpen workload and availability table.}
\item{active_rosters}{Standardized active-roster rows.}
\item{probable_starter_ids}{MLBAM identifiers excluded from the bullpen pool.}
\item{schedule}{A BaseballR schedule result, usually for one date.}
\item{probables}{Bound results from \code{baseballr::mlb_probables()}.}
\item{batting_orders}{Bound results from \code{baseballr::mlb_batting_orders()}.}
\item{latitude,longitude}{Ballpark coordinates.}
\item{start_date,end_date}{Inclusive forecast dates.}
\item{timezone}{IANA timezone or \code{auto}.}
\item{pitches}{Canonical pitch rows from \code{build_pitch_view()}.}
\item{recent_games}{Number of latest pitcher appearances in the recent window.}
\item{minimum_recent_pitches}{Minimum recent pitch-type sample.}
\item{minimum_baseline_pitches}{Minimum earlier pitch-type sample.}
\item{hitting,pitching}{Triple-A season tables from \code{baseballr::mlb_stats()}.}
\item{minimum_pa}{Minimum Triple-A hitter plate appearances.}
\item{minimum_ip}{Minimum Triple-A pitcher innings.}
\item{prospect_age}{Maximum age for the young-player reporting lens.}
}
\value{
Stable tibbles or lists of tibbles for daily game inputs, active-roster bullpen
eligibility, ballpark forecast acquisition, pitch-mix changes, and Triple-A
performance watches.
}
\description{
These interfaces turn changing upstream BaseballR responses into small,
versioned reporting contracts while preserving MLBAM identifiers, sample
sizes, and interpretation boundaries.
}
