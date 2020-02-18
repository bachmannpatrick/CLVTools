#' @name clv.model.expectation
#' @title Expectation as predicted by the model
#' @param obj Estimated CLV model
#' @param dates.periods Vector of dates for which to calculate the expectation
#' @template template_param_verbose
#' @description Calculates the number of repeat transactions of a random customer in each period
#' @details
#' \code{dates.periods} Vector of dates at which to calculate the expected transactions.
#' The first date is t=0 and every subsequent date is treated as the next period. Therefore, the dates should be the next time unit.
#' @return The number of repeat transactions for all customers in each period
#' @keywords internal
