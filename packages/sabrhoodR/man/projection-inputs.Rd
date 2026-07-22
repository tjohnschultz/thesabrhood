% Generated manually for the development package.
\name{validate_projection_game_inputs}
\alias{validate_projection_game_inputs}
\alias{plan_bullpen_chain}
\title{Validate projection inputs and plan a bullpen chain}
\usage{
validate_projection_game_inputs(games, allow_projected_lineups = TRUE)

plan_bullpen_chain(
  candidates,
  batter_pockets,
  max_pitches_per_reliever = 30L,
  pitches_per_batter = 4
)
}
\arguments{
\item{games}{One row per scheduled game with starter, lineup, park, weather, and roster fields.}
\item{allow_projected_lineups}{Whether projected lineups satisfy the conditional-run gate.}
\item{candidates}{Bullpen candidates accepted by \code{rank_reliever_options()}.}
\item{batter_pockets}{Ordered handedness and leverage pockets.}
\item{max_pitches_per_reliever}{Scenario workload ceiling.}
\item{pitches_per_batter}{Planning estimate for updating workload.}
}
\value{
\code{validate_projection_game_inputs()} returns the supplied games with
readiness fields. \code{plan_bullpen_chain()} returns one selected reliever row
per matchup pocket with workload and alternative options.
}
\description{
Creates explicit publication gates for daily game simulations and connects the
existing reliever selector to an ordered, workload-aware bullpen scenario.
}
