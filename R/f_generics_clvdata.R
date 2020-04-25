#' @include all_generics.R class_clv_data.R
setMethod(f="clv.controlflow.plot.get.data", signature = signature(obj="clv.data"), definition = function(obj, dt.expectation.seq, cumulative, verbose){
  # Add the number of repeat transactions to dt.date.seq

  num.repeat.trans <- i.num.repeat.trans <- Date <- period.until <- NULL

  # Add period at every repeat transaction (and therefore copy)
  dt.repeat.trans  <- copy(obj@data.repeat.trans)

  # join (roll: -Inf=NOCF) period number onto all repeat transaction by dates
  #   ie assign each repeat transaction the next period number to which it belongs
  dt.repeat.trans <- dt.expectation.seq[dt.repeat.trans, on = c("period.until"="Date"), roll=-Inf, rollends=c(FALSE, FALSE)]
  # !period.until now is missleading, as it stands for the repeat transaction date!

  # Count num rep trans in every time unit
  dt.repeat.trans <- dt.repeat.trans[, list(num.repeat.trans = .N), by="period.num"]
  setorderv(dt.repeat.trans, order = 1L, cols = "period.num") # sort in ascending order

  # make double to avoid coercion warning in melt
  dt.expectation.seq[dt.repeat.trans, num.repeat.trans := as.numeric(i.num.repeat.trans), on = "period.num"]

  # set 0 where there are no transactions
  #   for when there are transactions again later on
  dt.expectation.seq[is.na(num.repeat.trans), num.repeat.trans := 0]

  # After last transaction, there are no more transactions.
  #   dt.expectation can however be longer. Set these intentionally to NA so that
  #   nothing is plotted (setting 0 plots a line at the bottom)
  date.last.repeat.transaction <- obj@data.repeat.trans[, max(Date)]
  dt.expectation.seq[period.until > date.last.repeat.transaction, num.repeat.trans := NA_real_]

  if(cumulative)
    dt.expectation.seq[, num.repeat.trans := cumsum(num.repeat.trans)]

  return(dt.expectation.seq)
})

