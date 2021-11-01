#' @name pmf
#'
#' @title Probability Mass Function
#' @param object The fitted transaction model.
#' @param x Vector of positive integer numbers (>=1) indicating the number of transactions x for
#' which the PMF should be calculated.
#'
#' @description
#' Calculate P(X(t)=x), the probability to make exactly x repeat transactions in
#' the interval (0, T] (the estimation period). Note that T is T.cal, the observation period which
#' differs by customer.
#'
#'
#' @returns
#' Returns a \code{data.table} with ids and depending on \code{x}, multiple columns of PMF values, each column
#' for one value in \code{x}.
#' \item{Id}{customer identification}
#' \item{pmf.x.Y}{PMF values for Y number of transactions}
#'
#' @seealso The model fitting functions \code{\link[CLVTools:pnbd]{pnbd},
#' \link[CLVTools:bgnbd]{bgnbd}, \link[CLVTools:ggomnbd]{ggomnbd}}.
#'
#' @examples
#' \donttest{
#' data("cdnow")
#'
#' # Fit the ParetoNBD model on the CDnow data
#' pnbd.cdnow <- pnbd(clvdata(cdnow, time.unit="w",
#'                            estimation.split=37,
#'                            date.format="ymd"))
#'
#' # Calculate the PMF for 0 to 10 transactions
#' #  in the estimation period
#' pmf(pnbd.cdnow, x=0:10)
#'
#' # Compare vs. actuals (CBS in estimation period):
#' x    mean(pmf)       actual percentage of x
#' 0    0.6165143       1432/2357= 0.607552
#' 1    0.163130227     439/2357 = 0.18625371
#' 2    0.079697124     214/2357 = 0.09079338
#' 3    0.046111155     100/2357 = 0.04242681
#' 4    0.028749294     62/2357  = 0.02630462
#' 5    0.018591868     38/2357  = 0.01612219
#' 6    0.012213620     29/2357  = 0.01230378
#' 7    0.008029474     23/2357  = 0.00975817
#' 8    0.005206442     7/2357   = 0.00296988
#' 9    0.003266914     5/2357   = 0.00212134
#' 10   0.001919501     5/2357   = 0.00212134
#' }
#'
NULL

#' @exportMethod pmf
setGeneric(name = "pmf", def = function(object, x=0:5)
  standardGeneric("pmf"))



#' @include class_clv_fitted_transactions.R
#' @rdname pmf
setMethod(f = "pmf", signature = signature(object="clv.fitted.transactions"), definition = function(object, x=0:5){
  check_err_msg(check_user_data_pmfx(x=x))
  x <- sort(unique(x))
  return(clv.template.controlflow.pmf(clv.fitted=object, x=x, plot=plot))
})
