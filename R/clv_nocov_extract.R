#' @include class_clv_fitted.R
#' @importFrom stats logLik
#' @export
logLik.clv.fitted <- function(object, ...){
  last.row.optimx <- tail(object@optimx.estimation.output, n = 1)
  return(structure( (-1)*(last.row.optimx[["value"]]),
                    nall  = nobs(object),
                    nobs  = nobs(object),
                    df    = length(coef(last.row.optimx)),
                    class ="logLik"))
}


#' @include class_clv_fitted.R
#' @importFrom stats nobs
#' @export
nobs.clv.fitted   <- function(object, ...){
  # Observations are number of customers
  return(nrow(object@cbs))
}

