#'
#' @title <%=name_model_full %>: Conditional Expected Transactions
#'
#' @description
#' Calculates the expected number of transactions in a given time period based
#' on a customer's past transaction behavior and the <%=name_model_full %> model parameters.
#'
#' \itemize{
#' \item{\code{<%=name_model_short%>_nocov_CET}}{ Conditional Expected Transactions without covariates}
#' \item{\code{<%=name_model_short%>_staticcov_CET}}{ Conditional Expected Transactions with static covariates}
#' }
#'
#' @return
#' Returns a vector containing the conditional expected transactions for the existing
#' customers in the <%=name_model_full %> model.
