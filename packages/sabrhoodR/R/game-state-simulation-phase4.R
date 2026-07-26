#' Simulate a Phase 4 game with empirical baserunning profiles
#'
#' A Phase 4 entry point around [simulate_game_state_phase3()] that activates
#' runner-level advancement, steal tendency, steal success, empirical park,
#' and pitcher-hold inputs when supplied.
#'
#' @param ... Arguments passed to [simulate_game_state_phase3()].
#'
#' @return The detailed simulation result from [simulate_game_state_phase3()].
#' @export
simulate_game_state_phase4 <- function(...) {
  simulate_game_state_phase3(...)
}
