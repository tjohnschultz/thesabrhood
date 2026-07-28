\name{simulate_game_state_phase5}
\alias{simulate_game_state_phase5}
\title{Simulate a Phase 5 game with a learned manager hook}
\usage{
simulate_game_state_phase5(...)
}
\arguments{
\item{...}{Arguments passed to \code{\link[=simulate_game_state_phase3]{simulate_game_state_phase3()}}, including
\code{manager_hook_coefficients}, starter pitch limits, and
\code{run_environment_multiplier}.}
}
\value{
The detailed simulation result from
\code{\link[=simulate_game_state_phase3]{simulate_game_state_phase3()}}.
}
\description{
Phase 5 keeps the runner, park, and named-reliever layers from Phase 4 and
adds a time-validated pitcher-removal model plus starter-specific pregame
workload limits and a park/weather run environment.
}
