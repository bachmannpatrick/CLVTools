#' @name pmf
#'
#' @title Probability Mass Function
#' @param object The fitted transaction model.
#' @param x Vector of positive integer numbers (>=0) indicating the number of repeat transactions x for
#' which the PMF should be calculated.
#'
#' @description
#' Calculate P(X(t)=x), the probability to make exactly \code{x} repeat transactions in
#' the interval (0, t]. This interval is in the estimation period and excludes values of \code{t=0}.
#' Here, \code{t} is defined as the observation period \code{T.cal} which differs by customer.
#' Note that the PMF for the Pareto/NBD with dynamic covariates is still experimental and very slow.
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
#' @seealso \code{\link[CLVTools:plot.clv.fitted.transactions]{plot}} to visually compare
#' the PMF values against actuals.
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
#' # x    mean(pmf)    actual percentage of x
#' # 0    0.616514     1432/2357= 0.6075519
#' # 1    0.168309     436/2357 = 0.1849809
#' # 2    0.080971     208/2357 = 0.0882478
#' # 3    0.046190     100/2357 = 0.0424268
#' # 4    0.028566     60/2357  = 0.0254561
#' # 5    0.018506     36/2357  = 0.0152737
#' # 6    0.012351     27/2357  = 0.0114552
#' # 7    0.008415     21/2357  = 0.0089096
#' # 8    0.005822     5/2357   = 0.0021213
#' # 9    0.004074     4/2357   = 0.0016971
#' # 10   0.002877     7/2357   = 0.0029699
#' }
#'
NULL

#' @exportMethod pmf
setGeneric(name = "pmf", def = function(object, x=0:5)
  standardGeneric("pmf"))



#' @include class_clv_fitted_transactions.R
#' @rdname pmf
setMethod(f = "pmf", signature = signature(object="clv.fitted.transactions"), definition = function(object, x=0:5){
  check_err_msg(check_user_data_integer_vector_greater0(vec=x, var.name="x"))
  return(clv.template.controlflow.pmf(clv.fitted=object, x=x))
})
