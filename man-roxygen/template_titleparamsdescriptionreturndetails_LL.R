#'
#' @title <%=name_model_full %>: Log-Likelihood functions
#'
#' @param vLogparams vector with the <%=name_model_full %> model parameters at log scale. See Details.
#' @param vParams vector with the parameters for the <%=name_model_full %> model at loc scale and the static covariates at origina scale. See Details.
#'
#' @description
#' Calculates the Log-Likelihood values for the <%=name_model_full %> model with and without covariates.
#'
#' The function \code{<%=name_model_short%>_nocov_LL_ind} calculates the individual LogLikelihood
#' values for each customer for the given parameters.
#'
#' The function \code{<%=name_model_short%>_nocov_LL_sum} calculates the LogLikelihood value summed
#' across customers for the given parameters.
#'
#' The function \code{<%=name_model_short%>_staticcov_LL_ind} calculates the individual LogLikelihood
#' values for each customer for the given parameters and covariates.
#'
#' The function \code{<%=name_model_short%>_staticcov_LL_sum} calculates the individual LogLikelihood values summed
#' across customers.
#'
#' @details \code{vLogparams} is a vector with model parameters \code{<%=model_params_ordered%>} at log-scale, in this order.
#'
#' @details \code{vParams} is vector with the <%=name_model_full %> model parameters at log scale,
#' followed by the parameters for the lifetime covariates at original scale and then
#' followed by the parameters for the transaction covariates at original scale
#'
#' @return
#'  Returns the respective Log-Likelihood value(s) for the <%=name_model_full %> model
#'  with or without covariates.
