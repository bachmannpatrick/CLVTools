# params_i:        parameters and first transaction for each customer. passed on to fct.expectation.
# dates.periods:   at which dates to calculate expectation
# fct.expectation: function to calculate expectation value at point t
DoExpectation <- function(dt.expectation.seq, params_i, fct.expectation, clv.time){

  period.num <- date.first.actual.trans <- expectation <- expectation_i <- date.period <- number.of.tu <- t_i <- period.until.trans <- NULL

  # Table with expectation periods
  #   period.until: Here we stand, only customers already alive here

  # For every period in table, calculate expectation
  for(p.no in dt.expectation.seq$period.num){
    period.until <- dt.expectation.seq[period.num == p.no, period.until]

    # Remove who is not alive yet
    alive.params_i <- params_i[date.first.actual.trans <= period.until]

    # calculate model prediction for this customers at point t
    #   t_i: Individual time alive until the beginning of this period (ie first trans to period.until)
    alive.params_i[, t_i := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                           interv = interval(start = date.first.actual.trans,
                                                                             end   = period.until))]
    # Calculate expectation for each alive customer
    alive.params_i[, expectation_i := fct.expectation(.SD)]

    # expectation is sum of all single customer's expectation
    dt.expectation.seq[period.num == p.no , expectation := alive.params_i[, sum(expectation_i, na.rm=TRUE)]]
  }

  # Transform cumulative back to incremental ------------------------------------------
  #   First entry is already correct because for k=1: incremental == cumulative
  #   All afterwards are the incremental differences
  #   -> Subtract previous element, for first subtract 0
  #   Same as: [first, diff(expectation)]
  dt.expectation.seq[order(period.until, decreasing = FALSE), expectation := expectation - c(0, expectation[seq(from=1, to=.N-1)])]


  return(dt.expectation.seq)
}
