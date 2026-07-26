\name{simulate_game_state_phase4}
\alias{simulate_game_state_phase4}
\title{Simulate a Phase 4 game with empirical baserunning profiles}
\usage{
simulate_game_state_phase4(...)
}
\arguments{
\item{...}{Arguments passed to \code{\link[=simulate_game_state_phase3]{simulate_game_state_phase3()}}.}
}
\value{
The detailed simulation result from
\code{\link[=simulate_game_state_phase3]{simulate_game_state_phase3()}}.
}
\description{
A Phase 4 entry point around
\code{\link[=simulate_game_state_phase3]{simulate_game_state_phase3()}} that
activates runner-level advancement, steal tendency, steal success, empirical
park, and pitcher-hold inputs when supplied.
}
