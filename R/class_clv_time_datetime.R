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
#' @seealso For time unit implementations based on this class: \linkS4class{clv.time.hours}
#'
#'
#' @include class_clv_time.R all_generics.R
#' @keywords internal
setClass("clv.time.datetime", contains = c("clv.time", "VIRTUAL"),
         slots = list(
           # Use ct because dates in transaction data have to be ct
           timepoint.estimation.start = "POSIXct",
           timepoint.estimation.end   = "POSIXct",
           timepoint.holdout.start    = "POSIXct",
           timepoint.holdout.end      = "POSIXct",

           timezone = "character"),
         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           timezone = character(0),

           timepoint.estimation.start = as.POSIXct(character(0)),
           timepoint.estimation.end   = as.POSIXct(character(0)),
           timepoint.holdout.start    = as.POSIXct(character(0)),
           timepoint.holdout.end      = as.POSIXct(character(0))))


# Because this class is VIRTUAL, no instance can be created and the
#   usual approach of using a constructor function where an instance is created
#   does not work.
# In case validity methods are added, the "initialize" method needs to be
#   defined and omit calling the parent class initialize (see PR linked to issue #47)


#' @importFrom lubridate seconds
setMethod("clv.time.epsilon", signature = signature(clv.time="clv.time.datetime"), function(clv.time){
  # Alternative: return 1L
  return(seconds(x = 1L))
})


setMethod("clv.time.format.timepoint", signature = signature(clv.time="clv.time.datetime"), definition = function(clv.time, timepoint){
  return(format.POSIXct(timepoint, "%Y-%m-%d %H:%M:%S"))
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
