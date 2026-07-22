#' Score candidate stories for editorial products
#'
#' @param candidates A data frame containing zero-to-one component scores.
#' @param weights Named numeric weights for rarity, magnitude, timeliness,
#'   relevance, confidence, and novelty.
#'
#' @return The candidates with a zero-to-100 `story_score` and `score_method`.
#' @export
score_story_candidates <- function(
    candidates,
    weights = c(
      rarity = 1.25,
      magnitude = 1.00,
      timeliness = 1.15,
      relevance = 1.10,
      confidence = 1.35,
      novelty = 0.85
    )) {
  if (!is.data.frame(candidates)) stop("`candidates` must be a data frame.", call. = FALSE)
  components <- names(weights)
  if (is.null(components) || any(components == "")) stop("`weights` must be named.", call. = FALSE)
  missing <- setdiff(components, names(candidates))
  if (length(missing) > 0L) stop("Story candidates are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (any(!is.finite(weights)) || any(weights <= 0)) stop("Story weights must be positive and finite.", call. = FALSE)

  values <- as.matrix(candidates[components])
  storage.mode(values) <- "double"
  if (any(!is.finite(values))) stop("Story components must be finite.", call. = FALSE)
  if (any(values < 0 | values > 1)) stop("Story components must be between zero and one.", call. = FALSE)

  weighted_log <- sweep(log(pmax(values, 0.01)), 2L, weights, `*`)
  candidates$story_score <- round(100 * exp(rowSums(weighted_log) / sum(weights)), 1)
  candidates$score_method <- "sabrhood_story_score_v1"
  dplyr::arrange(candidates, dplyr::desc(.data$story_score))
}
