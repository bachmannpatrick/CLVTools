#' @templateVar name_model_full Gamma/Gamma
#' @templateVar name_class_clvmodel clv.model.gg
#' @template template_class_clvfittedmodels
#'
#' @template template_slot_ggcbs
#'
#' @seealso \link{clv.fitted-class}, \link{clv.model.gg-class}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_gg.R class_clv_data.R class_clv_fitted.R
setClass(Class = "clv.gg", contains = "clv.fitted",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


clv.gg <- function(cl, clv.data){

  dt.cbs.gg <- gg_cbs(clv.data = clv.data)
  clv.model    <- clv.model.gg()

  return(new("clv.gg",
             clv.fitted(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.gg))
}


gg_cbs <- function(clv.data){
  Date <- Price <- x <- NULL
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     Spending: Average (mean) spending per transaction (of all transactions, not only repeat)
  #
  #     All time is expressed in time units
  trans.dt <- clv.data@data.transactions[Date <= clv.data@clv.time@timepoint.estimation.end]

  #Initial cbs, for every Id a row
  cbs <- trans.dt[, list(x = .N, Spending = mean(Price, na.rm=TRUE)), by="Id"]

  # Only repeat transactions -> Number of transactions - 1
  cbs[, x := x - 1]

  setcolorder(cbs, c("Id","x","Spending"))

  return(cbs)
}
