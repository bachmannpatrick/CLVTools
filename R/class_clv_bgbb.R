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
             cbs = dt.cbs.bgbb))
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
  #     n.cal:    Number of opportunities to buy
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

  if(clv.data@has.opportunities){
    cbs <- merge(cbs, clv.data@data.opportunities, key = "Id")
  } else if(clv.data@has.opportunities.n_cal){
    cbs <- cbs[, n.cal := clv.data@data.opportunities.n_cal]
  }

  # Only repeat transactions -> Number of transactions - 1
  cbs[, x := x - 1]

  # t.x, T.cal
  cbs[, ':='(t.x      = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = date.last.transaction)))]

  setkeyv(cbs, c("Id", "date.first.actual.trans"))
  if(clv.data@has.spending)
    setcolorder(cbs, c("Id","x","t.x","n.cal","Spending","date.first.actual.trans", "date.last.transaction"))
  else
    setcolorder(cbs, c("Id","x","t.x","n.cal", "date.first.actual.trans", "date.last.transaction"))

  return(cbs)
}
