#'
#'
#'
#' @section Uncertainty Estimates:
#' Bootstrapping is used to provide confidence intervals of all predicted metrics.
#' These provide an estimate of parameter uncertainty.
#' To create bootstrapped data, customer ids are sampled with replacement until reaching original
#' length and all transactions of the sampled customers are used to create a new \code{clv.data} object.
#' A new model is fit on the bootstrapped data with the exact same specification as used when
#' fitting \code{object} (incl. start parameters and `optimx.args`) and it is then used to predict on this data.
#'
#' It is highly recommended to fit the original model (\code{object}) with a robust optimization
#' method, such as Nelder-Mead (\code{optimx.args=list(method='Nelder-Mead')}).
#' This ensures that the model can also be fit on the bootstrapped data.
#'
#' All prediction parameters, incl \code{prediction.end} and \code{continuous.discount.factor}, are forwarded
#' to the prediction on the bootstrapped data.
#' Per customer, the boundaries of the confidence intervals of each predicted metric are the
#' sample quantiles (\code{quantile(x, probs=c((1-level)/2, 1-(1-level)/2)}).
#'
#' See \link{clv.bootstrapped.apply} to create a custom bootstrapping procedure.
#'
#'
#'
