#' @param clv.data The data object on which the model is fitted.
#' @param start.params.model Named start parameters containing the optimization start parameters for the model without covariates.
#' @param optimx.args Additional arguments to control the optimization which are forwarded to \code{\link[optimx:optimx]{optimx::optimx}}.
#' If multiple optimization methods are specified, only the result of the last method is further processed.
