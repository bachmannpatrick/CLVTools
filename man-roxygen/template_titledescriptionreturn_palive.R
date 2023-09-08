#'
#' @title <%=name_model_full %>: Probability of Being Alive
#'
#'
#' @description
#' Calculates the probability of a customer being alive at the end of the calibration period,
#' based on a customer's past transaction behavior and the <%=name_model_full %> model parameters.
#'
#' \describe{
#' \item{\code{<%=name_model_short%>_nocov_PAlive}}{ P(alive) for the <%=name_model_full %> model without covariates}
#' \item{\code{<%=name_model_short%>_staticcov_PAlive}}{ P(alive) for the <%=name_model_full %> model with static covariates}
#' }
#'
#'
#' @return Returns a vector with the PAlive for each customer.
#'

