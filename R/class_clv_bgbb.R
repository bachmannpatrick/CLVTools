#' @importFrom methods setClass
#' @keywords internal
#' @include class_clv_model_bgbb_nocov.R class_clv_fitted.R
setClass(Class = "clv.bgbb", contains = "clv.fitted",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))



# Convenience constructor to encapsulate all steps for object creation
clv.bgbb <- function(cl, clv.data){

  dt.cbs.bgbb <- bgbb_cbs(clv.data = clv.data)
  clv.model <- new("clv.model.bgbb.no.cov")

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.bgbb",
             clv.fitted(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.bgnbd))
}


bgbb_cbs <- function(clv.data){
  Date <- Price <- x <- date.first.actual.trans <- date.last.transaction <- NULL
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     t.x:      Time between first actual and last transaction
  #     T.cal:    Time between first actual transaction and end of calibration period
  #     n:        Number of opportunities to buy
  #     Spending: Average (mean) spending per transaction (of all transactions, not only repeat)
  #
  #     All time is expressed in time units
  stop("CBS for BG/BB to be defined")
}
