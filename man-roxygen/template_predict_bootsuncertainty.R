#'
#' @section Uncertainty Estimates:
#' Bootstrapping is used to provide confidence intervals of all predicted metrics.
#' These provide an estimate of parameter uncertainty.
#' To create bootstrapped data, customer ids are sampled with replacement until reaching original
#' length and all transactions of the sampled customers are used to create a new \code{clv.data} object.
#' A new model is fit on the bootstrapped data with the same specification as \code{object}
#' (incl. start parameters and `optimx.args`) and it is then used to predict on this data.
#' All prediction parameters are forwarded to the prediction on the bootstrapped data.
#'
#' Per customer, confidence intervals of each predicted metric are created using
#' a "reversed quantile" approach.
#'
#' See \link{clv.bootstrapped.apply} to create a custom bootstrapping procedure.
#'
