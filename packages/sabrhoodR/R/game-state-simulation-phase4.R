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

#' Simulate a Phase 5 game with a learned manager hook
#'
#' Phase 5 keeps the runner, park, and named-reliever layers from Phase 4 and
#' adds a time-validated pitcher-removal model plus starter-specific pregame
#' workload limits and a park/weather run environment.
#'
#' @param ... Arguments passed to [simulate_game_state_phase3()], including
#'   `manager_hook_coefficients`, starter pitch limits, and
#'   `run_environment_multiplier`.
#'
#' @return The detailed simulation result from [simulate_game_state_phase3()].
#' @export
simulate_game_state_phase5 <- function(...) {
  simulate_game_state_phase3(...)
}
