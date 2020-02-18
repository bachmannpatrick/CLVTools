clv.time.expectation.periods <- function(clv.time, user.tp.end){

  # Table with each row representing a period (with period number, start and end dates)
  #   required when executing plot() to calculate the unconditional expectation and to
  #     roll-join repeat transactions on
  #
  #   First expectation period
  #     Start: estimation.start
  #     End: end of time.unit
  #       => This period is often/mostly only partial
  #
  #   All other expectation periods:
  #     Start: Beginning of time.unit
  #     End: End of time.unit
  #
  #   Period number: How many periods from min(Start) until end of the period in this row

  # First period
  tp.first.period.start  <- clv.time@timepoint.estimation.start

  tp.second.period.start <- clv.time.floor.date(clv.time = clv.time, timepoint = tp.first.period.start) +
    clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                            user.number.periods = 1L)


  if(!is.null(user.tp.end)){
    if(is.numeric(user.tp.end)){

      # Make periods integer because of lubridate::period limitations
      #   This is the same behavior as in clv.time.set.sample.periods
      if(user.tp.end %% 1 != 0){
        warning("The parameter prediction.end may not indicate partial periods. Digits after the decimal point are cut off.",
                call. = FALSE)
        user.tp.end <- as.integer(user.tp.end)
      }

      # holdout.start + periods - 1L
      # or: estimation.end + periods
      # NOT including tp of next period, because expectation is done including tp.expectation.end
      tp.expectation.end <- clv.time@timepoint.holdout.start +
        clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                                user.number.periods = user.tp.end) - 1L
    }else{
      # datey
      tp.expectation.end <- clv.time.convert.user.input.to.timepoint(clv.time = clv.time,
                                                                     user.timepoint = user.tp.end)
    }
  }else{
    # Is null
    tp.expectation.end <- clv.time@timepoint.holdout.end
  }

  # Check that input is valid for expectation sequence ------------------------------------------------

  # if(tp.expectation.end <= clv.time@timepoint.estimation.end)
  #   check_err_msg("The prediction end cannot be before the end of the estimation!")

  if(tp.expectation.end <= tp.first.period.start)
    check_err_msg("The end cannot be before the start of the expectation period!")


  if(clv.time.interval.in.number.tu(clv.time = clv.time,
                                    interv = interval(start = tp.first.period.start,
                                                      end = tp.expectation.end)) <= 3){
    check_err_msg("The expectation needs to be calculated across a minimum of 3 periods!")
  }


  # Table:
  #   The implementation of the dyncov expectation function requires each timepoint
  #     to be the start of a covariate time unit
  #   Because the expectation refers to the period covered by [0, date_i] and only calculates until beginning
  #     of the period (ie backwards and not until end of the period marked by date_i), the
  #     expectation.end has to be included by the last timepoint (ie last timepoint is after expectation.end)
  #   All timepoints in the table need to be exactly 1 time unit apart because the
  #     incremental values are derived from the cumulative expectation function (ie "what is gain from 1 period difference").
  #     Except the first to second period which may be apart less

  # First:  expectation.start
  #           expectation is 0 but always include because transaction data is counted
  # Second: ceiling_tu(expectation.start)
  #   If first and second are same (ie expectation.start falls on period start), only use one
  # All after: +1 TU of previous
  # Last: minimum required date
  #     minimum required date: max(expectation.end, ceiling_tu(expectation.end))
  #
  # The time/time.units/period math to actually calculate this has too many edgecases
  #   Therefore use a naive, but correct approach:
  #   Add more periods from the start of first full period (from which there are only full time.units)
  #     until the expectation end is covered.
  #   (also there is no seq(from,to,by="tu") currently implemented in clv.time)

  # First period to start with:
  #   Either expectation.start if on time unit or expectation.start+ceiling_tu(expectation.start)
  vec.tp.expectation.date.i <- clv.time.ceiling.date(clv.time=clv.time,
                                                     timepoint = tp.first.period.start)
  if(vec.tp.expectation.date.i != tp.first.period.start)
    vec.tp.expectation.date.i <- c(tp.first.period.start, vec.tp.expectation.date.i)


  # Add time units until expectation.end is covered
  repeat{
    # last currently in vec
    tp.current.end <- max(vec.tp.expectation.date.i)

    # Add +1 TU at the RHS of the vec
    vec.tp.expectation.date.i <-
      c(vec.tp.expectation.date.i,
        tp.current.end + clv.time.number.timeunits.to.timeperiod(clv.time=clv.time,
                                                                 user.number.periods = 1L))
    # Is already covered?
    if(max(vec.tp.expectation.date.i) >= tp.expectation.end){
      break
    }
  }

  # Make data.table, sort, add period.num (used throughout)
  dt.expectation <- data.table(period.first = vec.tp.expectation.date.i)
  setkeyv(dt.expectation, cols = "period.first")
  dt.expectation[, period.num := seq.int(from=1, to=.N)]

  return(dt.expectation)
}
