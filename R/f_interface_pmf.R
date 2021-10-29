#' @name pmf
#'
#' @title Probability Mass Function
#' @param object Fitted transaction model xxxx
#' @param x Integer vector
#'
#'
#' @description
#' Calculate P(X(t)=x), the probability to make exactly x repeat transactions in the interval (0, t].
#'
NULL

#' @exportMethod pmf
setGeneric(name = "pmf", def = function(object, x=0:5)
  standardGeneric("pmf"))

# Results vs. CBS:
#
# x    pmf             actual percentage of x
# 1    0.163130227     439/2357 = 0.18625371
# 2    0.079697124     214/2357 = 0.09079338
# 3    0.046111155     100/2357 = 0.04242681
# 4    0.028749294     62/2357  = 0.02630462
# 5    0.018591868     38/2357  = 0.01612219
# 6    0.012213620     29/2357  = 0.01230378
# 7    0.008029474     23/2357  = 0.00975817
# 8    0.005206442     7/2357   = 0.00296988
# 9    0.003266914     5/2357   = 0.00212134
# 10   0.001919501     5/2357   = 0.00212134
#
NULL


#' @include class_clv_fitted_transactions.R
#' @rdname pmf
setMethod(f = "pmf", signature = signature(object="clv.fitted.transactions"), definition = function(object, x=0:5){
  return(clv.template.controlflow.pmf(clv.fitted=object, x=x, plot=plot))
})
