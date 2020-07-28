#' @templateVar name_model_full Pareto/NBD
#' @templateVar name_class_clvmodel clv.model.pnbd.no.cov
#' @template template_class_clvfittedtransactionmodels
#'
#' @template template_slot_pnbdcbs
#'
#' @seealso \linkS4class{clv.fitted.transactions}, \linkS4class{clv.model.pnbd.no.cov}, \linkS4class{clv.pnbd.static.cov}, \linkS4class{clv.pnbd.dynamic.cov}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_pnbd.R class_clv_data.R class_clv_fitted_transactions.R
setClass(Class = "clv.pnbd", contains = "clv.fitted.transactions",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))




#' @importFrom methods new
clv.pnbd <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_cbs(clv.data = clv.data)
  clv.model   <- clv.model.pnbd.no.cov()

  return(new("clv.pnbd",
             clv.fitted.transactions(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}


pnbd_cbs <- function(clv.data){
  Date <- Price <- x <- date.first.actual.trans <- date.last.transaction <- NULL
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     t.x:      Time between first actual and last transaction
  #     T.cal:    Time between first actual transaction and end of calibration period
  #
  #     All time is expressed in time units

  trans.dt <- clv.data.get.transactions.in.estimation.period(clv.data = clv.data)

  #Initial cbs, for every Id a row
  cbs <- trans.dt[ , list(x                        =.N,
                          date.first.actual.trans  = min(Date),
                          date.last.transaction    = max(Date)),
                   by="Id"]


  # Only repeat transactions -> Number of transactions - 1
  cbs[, x := x - 1]

  # t.x, T.cal
  cbs[, ':='(t.x      = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = date.last.transaction)),
             T.cal    = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = clv.data@clv.time@timepoint.estimation.end)))]
  cbs[, date.last.transaction := NULL]

  setkeyv(cbs, c("Id", "date.first.actual.trans"))
  setcolorder(cbs, c("Id","x","t.x","T.cal", "date.first.actual.trans"))

  return(cbs)
}
