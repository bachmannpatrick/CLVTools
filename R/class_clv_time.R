#' Time Unit defining conceptual periods
#'
#' Represents a time unit and offers methods for time unit related functionality.
#' It stores all information related to timepoints (i.e. time and dates) of the
#' estimation and holdout sample periods and offers methods to parse user input
#' and to do 'time-unit math'.
#'
#' This encapsulation of all time unit functionality in one class allows for custom type of
#' time units such as Bi-weekly or irregularly spaced time units.
#'
#' \code{clv.time} is a virtual class and sub-classes implement the actual parsing and calculations.
#'
#' \linkS4class{clv.time.date} uses data type \code{Date} for time units equal or
#' greater than a single day that do not require a time of day.
#'
#' \linkS4class{clv.time.datetime} uses data type \code{POSIXct} for
#' time units smaller than a single day.
#'
#'
#' @slot timepoint.estimation.start Single \code{Date} or \code{POSIXct} that stores the start of the estimation period.
#' @slot timepoint.estimation.end Single \code{Date} or \code{POSIXct} that stores the end of the estimation period.
#' @slot timepoint.holdout.start Single \code{Date} or \code{POSIXct} that stores the start of the holdout period.
#' @slot timepoint.holdout.end Single \code{Date} or \code{POSIXct} that stores the end of the holdout period.
#' @slot time.format Single character vector with the format that is used to parse dates and times given as characters.
#' @slot estimation.period.in.tu Single numeric indicating the length of the estimation period in number of time units.
#' @slot holdout.period.in.tu Single numeric indicating the length of the holdout period in number of time units.
#' @slot name.time.unit Single character vector storing the human-readable name of the time unit for output.
#'
#' @seealso \link{summary.clv.time} for a summary about an object of class \code{clv.time}
#' @seealso \linkS4class{clv.time.days} for an implementation of time unit 'Days'
#' @seealso \linkS4class{clv.time.weeks} for an implementation of time unit 'Weeks'
#' @seealso \linkS4class{clv.time.years} for an implementation of time unit 'Years'
#'
#'
#' @include all_generics.R
#' @keywords internal
setClass("clv.time", contains = "VIRTUAL",
         slots = list(
           timepoint.estimation.start = "ANY",
           timepoint.estimation.end   = "ANY",
           timepoint.holdout.start    = "ANY",
           timepoint.holdout.end      = "ANY",

           estimation.period.in.tu = "numeric",
           holdout.period.in.tu    = "numeric",

           time.format    = "character",
           name.time.unit = "character"),

         prototype = list(
           estimation.period.in.tu = numeric(0),
           holdout.period.in.tu    = numeric(0),

           time.format             = character(0),
           name.time.unit          = character(0)))


# No constructor function, as should not be instanciated


# **FUTURE: future usage:
# custom time interval appraoch:
# cut(hour(now()), breaks=c(0,6,12,18,24), include.lowest = TRUE, labels = FALSE)



clv.time.possible.time.units <- function(){
  return(c("hours","days", "weeks", "years"))
}

clv.time.has.holdout <- function(clv.time){
  return(clv.time@holdout.period.in.tu > 0)
}


# set.sample.periods ------------------------------------------------------------------------
#' @importFrom lubridate period
clv.time.set.sample.periods <- function(clv.time, tp.first.transaction, tp.last.transaction, user.estimation.end){

  tp.estimation.start <- tp.first.transaction

  if(!is.null(user.estimation.end)){
    # specific end

    if(is.numeric(user.estimation.end)){

      # Have to cut of because lubridate does not allow non-integer period() (to add to tp.first.transaction)
      #   This is the same behavior as in clv.time.get.prediction.table
      if(user.estimation.end %%1 != 0)
        warning("The parameter estimation.split may not indicate partial periods. Digits after the decimal point are cut off.")

      user.estimation.end <- as.integer(user.estimation.end)

      tp.estimation.end <- tp.estimation.start +
        clv.time.number.timeunits.to.timeperiod(clv.time=clv.time, user.number.periods=user.estimation.end)
    }else{
      tp.estimation.end <- clv.time.convert.user.input.to.timepoint(clv.time=clv.time,
                                                                    user.timepoint=user.estimation.end)
    }

    # Need to be 2 periods because otherwise for days, holdout can be not on estimation.end but still be of length zero
    #   ie 2 periods to still have 1 as holdout
    if(tp.estimation.end > tp.last.transaction-clv.time.number.timeunits.to.timeperiod(clv.time, 2L))
      stop("Parameter estimation.split needs to indicate a point at least 2 periods before the last transaction!", call. = FALSE)

    # + 1 day is the same for all because most fine-grained change that Date can do
    tp.holdout.start   <- tp.estimation.end + clv.time.epsilon(clv.time=clv.time)
    tp.holdout.end     <- tp.last.transaction
    holdout.period.in.tu <- clv.time.interval.in.number.tu(clv.time,
                                                           interv=interval(start = tp.holdout.start,
                                                                           end   = tp.holdout.end))
  }else{
    # NULL: no specific end - until end of data (last transaction)
    #   **TODO: last transaction or full period where last transaction is in?

    # tp.holdout.start and .end HAVE to be end of estimation period as this is used elsewhere!
    #   ie to ensure prediction.end (with clv.time.get.prediction.table) finds correct end if user gives NULL
    tp.estimation.end  <- tp.last.transaction
    tp.holdout.start   <- tp.estimation.end
    tp.holdout.end     <- tp.estimation.end
    holdout.period.in.tu <- 0
  }

  estimation.period.in.tu <- clv.time.interval.in.number.tu(clv.time,
                                                            interv=interval(start = tp.estimation.start,
                                                                            end   = tp.estimation.end))

  if(estimation.period.in.tu < 1)
    stop("Parameter estimation.split needs to be at least 1 time.unit after the start!", call. = FALSE)

  clv.time@timepoint.estimation.start <- tp.estimation.start
  clv.time@timepoint.estimation.end   <- tp.estimation.end
  clv.time@timepoint.holdout.start    <- tp.holdout.start
  clv.time@timepoint.holdout.end      <- tp.holdout.end

  clv.time@holdout.period.in.tu       <- holdout.period.in.tu
  clv.time@estimation.period.in.tu    <- estimation.period.in.tu

  return(clv.time)
}



clv.time.expectation.periods <- function(clv.time, user.tp.end){

  period.num <- NULL

  # Table with each row representing a period (with period number, start and end dates)
  #   required when executing plot() to calculate the unconditional expectation and to
  #     roll-join repeat transactions on


  #   First expectation period
  #     Start: estimation.start
  #     End:   End of time.unit
  #       => This period is often/mostly only partial
  #
  #   All other expectation periods:
  #     Start: Beginning of time.unit
  #     End:   End of time.unit
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

      # holdout.start + periods - epsilon
      # or: estimation.end + periods
      # NOT including tp of next period, because expectation is done including tp.expectation.end
      tp.expectation.end <- clv.time@timepoint.holdout.start +
        clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                                user.number.periods = user.tp.end) - clv.time.epsilon(clv.time=clv.time)
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
    check_err_msg("The end cannot be before the start of the estimation period!")


  if(clv.time.interval.in.number.tu(clv.time = clv.time,
                                    interv = interval(start = tp.first.period.start,
                                                      end = tp.expectation.end)) <= 3){
    check_err_msg("The expectation needs to be calculated across a minimum of 3 periods!")
  }


  # Table
  #   The implementation of the dyncov expectation function requires each timepoint
  #     to be the start of a covariate time unit
  #   Because the expectation refers to the period covered by [0, date_i] and only calculates until the beginning
  #     of the period (ie backwards and not until end of the period marked by date_i), the
  #     expectation.end has to be included by the last timepoint (ie last timepoint is after expectation.end)
  #   All timepoints in the table need to be exactly 1 time unit apart because the
  #     incremental values are derived from the cumulative expectation function (ie "what is gain from 1 period difference").
  #     Except the first to second period which may be apart less
  #
  # Periods
  # First:  expectation.start
  #           expectation is 0 but always include because transaction data is counted
  #
  # Second: ceiling_tu(expectation.start)
  #   If first and second are same (ie expectation.start falls on period start), only use one
  #
  # All afterwards: +1 TU of previous
  #
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
  dt.expectation <- data.table(period.until = vec.tp.expectation.date.i)
  setkeyv(dt.expectation, cols = "period.until")
  dt.expectation[, period.num := seq.int(from=1, to=.N)]

  return(dt.expectation)
}


# Prediction table
#   period.first: First tp belonging to prediction period. First possible point after estimation.end.
#   period.last: Last tp belonging to prediction period.
#   period.length: Total length of prediction period, including period.first and period.last
#
#   Table for length 0 prediction period:
#     period.length = 0
#     For this to hold, period.first = period.last.
#     Because no prediction period exists, period.first has to be on estimation.end. If it was on +epsilon,
#       the prediction period was already > 0 length
#     Therefore, period.first = period.last = estimation.end
#
#' @include class_clv_time.R
#' @importFrom lubridate interval
clv.time.get.prediction.table <- function(clv.time, user.prediction.end){

  # Explanation of "period"
  #   Transactions can happen on estimation end
  #     Start of 1 period forward hence only is 1 timepoint after estimation.end
  #     End of 1 period forward should, including the start and end itself, represent a single period
  #     => This is + 1 lubridate::period to estimation.end
  #         Result is the end of 1 period forward, if end is counted towards the period
  #         Example:
  #           Estimation end: Sun 2019-10-06
  #           1 week forward
  #             First timepoint of 1 period: Mon 2019-10-07
  #             Last timepoint of full 1 period: Sun 2019-10-13
  #               In lubridate:: Have to use estimation.end
  #               ymd("2019-10-06")+lubridate::period(1, "weeks") = ymd("2019-10-13")
  #             First timepoint of 1st period: Mon 2019-10-07
  #             Last timepoint of full 2nd period: Sun 2019-10-20 (14d = 2weeks)
  #               ymd("2019-10-06")+lubridate::period(2, "weeks") = ymd("2019-10-20")
  #
  #   If the timepoint on which to end is given, it is counted towards / included in the prediction period.
  #     The length/number of periods has to account for this and the fact that the period only starts after
  #       the estimation end.
  #     Example:
  #       Estimation end: Wed 2019-06-12
  #       Prediction end: Wed 2019-06-19
  #         -> Prediction period is (Wed 2019-06-12 - Wed 2019-06-19] = [Thu 2019-06-13 - Wed 2019-06-19]
  #             Length = 7d = 1 week
  #             In lubridate: have to use estimation.end
  #                as.numeric(as.period(interval(start = ymd("2019-06-12"), end = ymd("2019-06-19"))), "week") = 1
  #       Estimation end: Wed 2019-06-12
  #       Prediction end: Fr 2019-06-28 (16d)
  #         -> Prediction period is (Wed 2019-06-12 - Wed 2019-06-28] = [Thu 2019-06-13 - Wed 2019-06-28]
  #             Length = 16d = 2+2/7 week
  #                as.numeric(as.period(interval(start = ymd("2019-06-12"), end = ymd("2019-06-28"))), "week") = 2.285
  #

  fct.make.prediction.table <- function(period.last, period.length){
    # If there is no prediction period:
    #   - period.last is estimation.end
    #   - there is no period.first (ie no first timepoint of the prediction period)
    if(period.length == 0){
      period.first <- period.last <- clv.time@timepoint.estimation.end
    }else{
      period.first <- clv.time@timepoint.estimation.end + clv.time.epsilon(clv.time = clv.time)
    }

    if(period.length < 0 | period.last < period.first){
      stop("The prediction period need to end after the estimation period!")
    }

    return(data.table(period.first  = period.first,
                      period.last   = period.last,
                      period.length = period.length))
  }

  # Prediction end given:
  #   Timepoint: Until and including this point. Length can be inferred from this.
  #   Numeric: This many periods. Due to limitations in lubridate's periods (and the Date class which
  #     represents only full days), only whole periods can be added up (because what is 0.2345 weeks even?)

  if(!is.numeric(user.prediction.end)){

    if(!is.null(user.prediction.end)){
      # Calcuate number of periods bases on given date
      #  parse date, char, posixt. then count number of timeunits
      prediction.end.date <- clv.time.convert.user.input.to.timepoint(clv.time=clv.time,
                                                                      user.timepoint=user.prediction.end)
    }else{
      # Whether there is a holdout sample is checked in the predict inputchecks
      prediction.end.date <- clv.time@timepoint.holdout.end
    }



    # As explained above, estimation.end has to be used as the start of the interval
    #   to correctly count the number of periods which are [estimation.end+1TP, prediction.end.date]
    number.of.time.units <- clv.time.interval.in.number.tu(clv.time=clv.time,
                                                           interv=interval(start = clv.time@timepoint.estimation.end,
                                                                           end   = prediction.end.date))

    return(fct.make.prediction.table(period.last = prediction.end.date,
                                     period.length = number.of.time.units))

  }else{

    # prediction.end is numeric

    # Make periods integer because of lubridate::period limitations
    #   This is the same behavior as in clv.time.set.sample.periods
    if(user.prediction.end %% 1 != 0)
      warning("The parameter prediction.end may not indicate partial periods. Digits after the decimal point are cut off.",
              call. = FALSE)

    number.of.time.units <- as.integer(user.prediction.end)

    # Alternatives to cutting off digits / partial
    #   - Strictly allow only full periods (ie stop in inputchecks)
    #   - Use lubridate::duration and round to closest full timepoint (day/second)
    #       and recalculate exact length from this TP

    # Timepoint that marks the end of this many periods
    #   As explained above, the periods have to be added to estimation.end
    prediction.end.date <- clv.time@timepoint.estimation.end +
      clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                              user.number.periods = number.of.time.units)

    return(fct.make.prediction.table(period.last = prediction.end.date,
                                     period.length = number.of.time.units))
  }
}



clv.time.sequence.of.covariate.timepoints <- function(clv.time, tp.start, tp.end){

  Cov.Date <- period.offset <- NULL
  # Marks all timepoints for which covariates are required if dyncov models should work between start and end.
  # First covariate is required at floor_timeunit(tp.start), last covariate is required at
  # floor_timeunit(tp.end), because the covariate always is supposed to influence the upcoming period.

  tp.cov.start <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.start)
  tp.cov.end   <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.end)

  # create with the sequence from tp.cov.start until and including tp.cov.end
  #   period.offset marks the offset, ie number of periods to add
  num.offsets <- clv.time.interval.in.number.tu(clv.time = clv.time,
                                                interv = interval(start = tp.cov.start, end = tp.cov.end))

  # If the num.offsets falls inbetween, but this should not happen
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


# Stubs to catch un-inplemented methods ---------------------------------------------------------
#   ie "abstract" methods

setMethod("clv.time.epsilon", signature = "clv.time", function(clv.time){
  stop("This method needs to be implemented by a subclass.")
})

# convert user given date/datetimes
setMethod("clv.time.convert.user.input.to.timepoint", signature = "clv.time", function(clv.time, user.timepoint){
  stop("This method needs to be implemented by a subclass.")
})

setMethod("clv.time.interval.in.number.tu", signature = "clv.time", def = function(clv.time, interv){
  stop("This method needs to be implemented by a subclass.")
})

setMethod("clv.time.number.timeunits.to.timeperiod", signature = "clv.time", function(clv.time, user.number.periods){
  stop("This method needs to be implemented by a subclass.")
})

setMethod("clv.time.tu.to.ly", signature = "clv.time", function(clv.time){
  stop("This method needs to be implemented by a subclass.")
})

setMethod("clv.time.floor.date", signature = "clv.time", function(clv.time, timepoint){
  stop("This method needs to be implemented by a subclass.")
})

# only for pnbd dyncov createwalks
setMethod("clv.time.ceiling.date", signature = "clv.time", function(clv.time, timepoint){
  stop("This method needs to be implemented by a subclass.")
})

setMethod("clv.time.format.timepoint", signature = "clv.time", function(clv.time, timepoint){
  stop("This method needs to be implemented by a subclass.")
})
