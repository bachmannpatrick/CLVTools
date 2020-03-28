#' POSIXct based time-units
#'
#' Virtual base class for time units that define periods smaller than a single day.
#' This class processes time and dates at the single seconds level and
#' stores all timepoints using data type \code{POSIXct}.
#'
#' @slot timepoint.estimation.start Single \code{POSIXct} that stores the start of the estimation period.
#' @slot timepoint.estimation.end Single \code{POSIXct} that stores the end of the estimation period.
#' @slot timepoint.holdout.start Single \code{POSIXct} that stores the start of the holdout period.
#' @slot timepoint.holdout.end Single \code{POSIXct} that stores the end of the holdout period.
#' @slot timezone Single character vector indicating the enforced timezone when parsing inputs to timepoints.
#' Defaults to UTC, but may be overwritten by a subclass to enforce a different timezone than UTC.
#' @seealso
#' For time unit implementations based on this class:
#' \code{\link[CLVTools:clv.time.hours-class]{clv.time.hours}}
#'
#'
#' @include class_clv_time.R all_generics.R
#' @keywords internal
setClass("clv.time.datetime", contains = c("clv.time", "VIRTUAL"),
         slots = c(
           # Use ct because dates in transaction data have to be ct
           timepoint.estimation.start = "POSIXct",
           timepoint.estimation.end   = "POSIXct",
           timepoint.holdout.start    = "POSIXct",
           timepoint.holdout.end      = "POSIXct",

           timezone = "character"))

setMethod("initialize", signature = signature(.Object="clv.time.datetime"),
          definition = function(.Object, time.format, name.time.unit,timezone,...){

            # Define customer initialize because otherwise when creating a new
            #   object with new() for a subclass: The validObject() is called
            #   that finds a subclass with Date/posixct to have the wrong class for the
            #   timepoints.* slots.
            # Reason is that clv.time has slots of "ANY" (S4) while clv.time.datetime expects "POSIXct".
            # Therefore define own initializer but do not call parent constructor/initializer.
            # To keep the copy/assign functionality, assign the passed args
            # for initialization (time.format+name) directly
            # callNextMethod()

            .Object@time.format                <- time.format
            .Object@name.time.unit             <- name.time.unit
            .Object@timezone                   <- timezone #"UTC" #timezone
            .Object@timepoint.estimation.start <- as.POSIXct(character(0))
            .Object@timepoint.estimation.end   <- as.POSIXct(character(0))
            .Object@timepoint.holdout.start    <- as.POSIXct(character(0))
            .Object@timepoint.holdout.end      <- as.POSIXct(character(0))
            return(.Object)
          })



# Parsing methods ------------------------------------------------------------------------
# Only allow one timezone set in clv.time@timezone: UTC, because it does not switch DST
#   Any diverging timezone is blocked when converting. Also, otherwise it would
#   be difficult to determine at runtime from userinput which timezone to enforce
#     (ie first, or transaction dates)
# Can be changed by a subclass which overwrites the slot timezone

#' @importFrom lubridate force_tz
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.datetime",
                                                                            user.timepoint="Date"),
definition = function(clv.time, user.timepoint){

  # Treat Date as if at midnight
  #   enforce by cuting off any time with floor_date

  # Convert Date to POSIXct:
  # - as.POSIXct adds time and converts to local timezone. Setting timezone with as.POSIXct
  #     does not seem to have any effect but always uses local/Sys timezone.
  # - as_datetime(x, tz) does tz conversion but can lead to unacceptable date switches
  #     (because Date as treated as UTC).
  # - as.POSIXlt always converts Dates to midnight UTC
  # - force_tz does not work on Dates (obviously)
  # => convert to Midnight UTC with as.lt + set tz to the desired one + convert to .ct of same tz
  #     Should not result in another timezone than UTC because Midtnight / beginning of day always exists, even on DST switch

  # Manual setting is slightly faster than force_tz. But it really is the internal base as.POSIXct that is slow
  tp.lt.midnight <- as.POSIXlt.Date(user.timepoint)
  attr(tp.lt.midnight, "tzone") <- clv.time@timezone
  return(as.POSIXct.POSIXlt(tp.lt.midnight, tz = clv.time@timezone))
})


setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.datetime",
                                                                            user.timepoint="POSIXlt"),
definition = function(clv.time, user.timepoint){

  if(tz(user.timepoint) != clv.time@timezone)
    stop(paste0("Only POSIXlt objects with timezone ", clv.time@timezone, " are accepted!"))

  return(as.POSIXct.POSIXlt(x = user.timepoint, tz = clv.time@timezone))
})


setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.datetime",
                                                                            user.timepoint="POSIXct"),
definition = function(clv.time, user.timepoint){

  if(tz(user.timepoint) != clv.time@timezone)
    stop(paste0("Only POSIXct objects with timezone ", clv.time@timezone, " are accepted!"))

  # Correct timezone, can use directly
  return(user.timepoint)
})


#' @importFrom lubridate parse_date_time
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.datetime",
                                                                            user.timepoint="character"),
definition = function(clv.time, user.timepoint){

  timepoints <- parse_date_time(x = user.timepoint,
                                orders = clv.time@time.format,
                                tz = clv.time@timezone,
                                quiet = TRUE)
  if(anyNA(timepoints))
    stop("The provided date and time failed to parse with the previously set date.format!", call. = FALSE)

  return(timepoints)
})

setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.datetime",
                                                                            user.timepoint="ANY"),
definition = function(clv.time, user.timepoint){
  # None of these cases
  stop("The provided data is in an unknown format! Only Date, POSIXct/lt, and character are accepted!", call. = FALSE)
})
