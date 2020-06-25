setClass(Class = "clv.gg", contains = "clv.fitted.spending",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.gg <- function(cl, clv.data){

  dt.cbs.gg   <- gg_cbs(clv.data = clv.data)
  clv.model   <- clv.model.gg()

  return(new("clv.gg",
             clv.fitted.spending(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.gg))
}

gg_cbs <- function(clv.data){
  Date <- Price <- x <- date.first.actual.trans <- date.last.transaction <- NULL
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     Spending: Average (mean) spending per transaction (of all transactions, not only repeat)

  trans.dt <- clv.data.get.transactions.in.estimation.period(clv.data = clv.data)

  cbs <- trans.dt[ , list(x         =.N,
                          Spending  = mean(Price, na.rm=TRUE)),
                   by="Id"]

  setcolorder(cbs, c("Id","x","Spending"))

  return(cbs)
}
