#' @param uncertainty Method to produce confidence intervals of the predictions (parameter uncertainty). Either "none" (default) or "boots".
#' @param level Required confidence level, if \code{uncertainty="boots"}.
#' @param num.boots Number of bootstrap repetitions, if \code{uncertainty="boots"}. A low number may not produce intervals for all customers if they are not sampled.
