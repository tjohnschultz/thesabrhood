\name{build_team_positional_war}
\alias{build_team_positional_war}
\title{Build team positional WAR and need boards}
\usage{
build_team_positional_war(hitters, pitchers, minimum_pa = 1L, minimum_outs = 3L)
}
\arguments{
\item{hitters}{Compact hitter table from \code{standardize_fangraphs_season()}.}
\item{pitchers}{Compact pitcher table from \code{standardize_fangraphs_season()}.}
\item{minimum_pa}{Minimum hitter plate appearances retained.}
\item{minimum_outs}{Minimum pitcher outs retained.}
}
\value{One row per team and position group with MLB rank and need label.}
\description{Aggregates FanGraphs player WAR into primary position groups and identifies team strengths and needs.}
