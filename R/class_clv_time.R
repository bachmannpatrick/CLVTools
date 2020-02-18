#' Time Unit defining conceptual periods
#'
#' Represents a time unit and offers methods for time unit related functionality.
#' It stores all information related to timepoints (ie time and dates) of the
#' estimation and holdout sample periods and offers methods to parse user input
#' and to do 'time-unit math'.
#'
#' This encapsulation of all time unit functionality in one class allows for custom type of
#' time units such as Bi-weekly or irregularly spaced time units.
#'
#' \code{clv.time} is a virtual class and sub-classes implement the actual parsing and calculations.
#'
#' \code{\link[CLVTools:clv.time.date]{clv.time.date}} uses data type \code{Date} for time units equal or
#' greater than a single day that do not require a time of day.
#'
#' \code{\link[CLVTools:clv.time.datetime]{clv.time.datetime}} uses data type \code{POSIXct} for
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
#' @seealso \code{\link[CLVTools:summary.clv.time]{summary.clv.time}} for a summary about an object of class \code{clv.time}
#' @seealso \code{\link[CLVTools:clv.time.days]{clv.time.days}} for an implementation of time unit 'Days'
#' @seealso \code{\link[CLVTools:clv.time.weeks]{clv.time.weeks}} for an implementation of time unit 'Weeks'
#' @seealso \code{\link[CLVTools:clv.time.years]{clv.time.years}} for an implementation of time unit 'Years'
#'
#'
#' @include all_generics.R
#' @keywords internal
setClass("clv.time",
         slots = list(
           timepoint.estimation.start = "ANY",
           timepoint.estimation.end   = "ANY",
           timepoint.holdout.start    = "ANY",
           timepoint.holdout.end      = "ANY",

           estimation.period.in.tu = "numeric",
           holdout.period.in.tu    = "numeric",

           time.format = "character",

           name.time.unit = "character"
         ),
         contains = "VIRTUAL")


# custom time interval appraoch:
# cut(hour(now()), breaks=c(0,6,12,18,24), include.lowest = TRUE, labels = FALSE)

clv.time.possible.time.units <- function(){
  return(c("hours","days", "weeks", "years")) #, "months", "14 days"))"quarters"))
}

# set.sample.periods ------------------------------------------------------------------------


#' @importFrom lubridate period
setMethod("clv.time.set.sample.periods", signature = signature(clv.time="clv.time"), definition =function(clv.time, tp.first.transaction, tp.last.transaction, user.estimation.end){

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
    tp.holdout.start   <- tp.estimation.end + 1L # For dates: +1 = 1 full day. For POSIXct: +1 = 1 second
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

  validObject(clv.time)

  return(clv.time)
})

