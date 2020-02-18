clv.time.sequence.of.covariate.timepoints <- function(clv.time, tp.start, tp.end){
  # Marks all timepoints for which covariates are required if dyncov models should work between start and end.
  # First covariate is required at floor_timeunit(tp.start), last covariate is required at
  # floor_timeunit(tp.end), because the covariate always is supposed to influence the upcoming period.

  tp.cov.start <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.start)
  tp.cov.end   <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.end)

  # create with the sequence from tp.cov.start until and including tp.cov.end
  #   period.offset marks the offset, ie number of periods to add
  num.offsets <- clv.time.interval.in.number.tu(clv.time = clv.time,
                                                interv = interval(start = tp.cov.start, end = tp.cov.end))

  # If the num.offsets falls inbetween, but this should not happen...
  num.offsets <- ceiling(num.offsets)

  if(num.offsets <= 1) # Offset of 1 is 2 periods only
    stop("Cannot create covariate date sequence for 2 or less periods!")

  dt.cov.seq <- data.table(period.offset = seq.int(from=0, to=num.offsets, by = 1))

  # by period.offset because lubridate::period() only accepts length 1 inputs
  dt.cov.seq[, Cov.Date := tp.cov.start +
               clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                                       user.number.periods = period.offset),
             by = "period.offset"]

  dt.cov.seq[, period.offset := NULL]

  # Key and order by Dates
  setkeyv(dt.cov.seq, "Cov.Date")

  return(dt.cov.seq)
}
