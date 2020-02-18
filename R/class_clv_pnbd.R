#' @importFrom methods setClass
#' @keywords internal
#' @include class_clv_model_pnbd_nocov.R class_clv_data_no_covariates.R class_clv_fitted.R
setClass(Class = "clv.pnbd", contains = "clv.fitted",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))



# Convenience constructor to encapsulate all steps for object creation
#' @include class_clv_data_no_covariates.R
clv.pnbd <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_cbs(clv.data = clv.data)
  clv.model <- new("clv.model.pnbd.no.cov")

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.pnbd",
             clv.fitted(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}


pnbd_cbs <- function(clv.data){
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     t.x:      Time between first actual and last transaction
  #     T.cal:    Time between first actual transaction and end of calibration period
  #     Spending: Average (mean) spending per transaction (of all transactions, not only repeat)
  #
  #     All time is expressed in time units

  trans.dt <- clv.data@data.transactions[Date <= clv.data@clv.time@timepoint.estimation.end]

  #Initial cbs, for every Id a row
  if(clv.data@has.spending){
    cbs <- trans.dt[ , list(x                        =.N,
                            date.first.actual.trans  = min(Date),
                            date.last.transaction    = max(Date),
                            Spending                 = mean(Price, na.rm=TRUE)),
                     by="Id"]
  }else{
    cbs <- trans.dt[ , list(x                        =.N,
                            date.first.actual.trans  = min(Date),
                            date.last.transaction    = max(Date)),
                     by="Id"]
  }

  # Only repeat transactions -> Number of transactions - 1
  cbs[, x := x - 1]

  # t.x, T.cal
  cbs[, ':='(t.x      = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = date.last.transaction)),
             T.cal    = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = clv.data@clv.time@timepoint.estimation.end)))]

  setkeyv(cbs, c("Id", "date.first.actual.trans"))
  if(clv.data@has.spending)
    setcolorder(cbs, c("Id","x","t.x","T.cal","Spending","date.first.actual.trans", "date.last.transaction"))
  else
    setcolorder(cbs, c("Id","x","t.x","T.cal", "date.first.actual.trans", "date.last.transaction"))

  # add date of first repeat transaction to cbs as often needed
  # ??** TODO: Zero repeaters will be NA..?!
  # cbs[obj@data.repeat.trans, date.first.repeat.trans := Date, mult="first", on="Id"]
  return(cbs)
}
