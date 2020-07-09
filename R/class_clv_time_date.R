#' Date based time-units
#'
#' Virtual base class for time units that need only whole day granularity.
#' This class processes time and dates at day level ignoring the time of day
#' and stores all timepoints using data type \code{Date}.
#'
#' @slot timepoint.estimation.start Single \code{Date} that stores the start of the estimation period.
#' @slot timepoint.estimation.end Single \code{Date} that stores the end of the estimation period.
#' @slot timepoint.holdout.start Single \code{Date} that stores the start of the holdout period.
#' @slot timepoint.holdout.end Single \code{Date} that stores the end of the holdout period.
#'
#'
#' @seealso
#' For time unit implementations based on this class: \linkS4class{clv.time.days}, \linkS4class{clv.time.weeks}, \linkS4class{clv.time.years}
#'
#' @include class_clv_time.R all_generics.R
#' @keywords internal
setClass("clv.time.date", contains = c("clv.time", "VIRTUAL"),
         slots = c(
           timepoint.estimation.start = "Date",
           timepoint.estimation.end   = "Date",
           timepoint.holdout.start    = "Date",
           timepoint.holdout.end      = "Date"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           timepoint.estimation.start = as.Date(character(0)),
           timepoint.estimation.end   = as.Date(character(0)),
           timepoint.holdout.start    = as.Date(character(0)),
           timepoint.holdout.end      = as.Date(character(0))))


# Because this class is VIRTUAL, no instance can be created and the
#   usual approach of using a constructor function where an instance is created
#   does not work.
# In case validity methods are added, the "initialize" method needs to be
#   defined and omit calling the parent class initialize (see PR linked to issue #47)



setMethod("clv.time.format.timepoint", signature = signature(clv.time="clv.time.date"), definition = function(clv.time, timepoint){
  return(format.Date(timepoint, "%Y-%m-%d"))
})


#' @importFrom lubridate days
setMethod("clv.time.epsilon", signature =  signature(clv.time="clv.time.date"), function(clv.time){
  # Alternative: return 1L
  return(days(x = 1L))
})



# Parsing methods ------------------------------------------------------------------------
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.date",
                                                                            user.timepoint="Date"), definition = function(clv.time, user.timepoint){
  # Date is Date, nothing to do
  return(user.timepoint)
})

#' @importFrom lubridate floor_date
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.date",
                                                                            user.timepoint="POSIXlt"), definition = function(clv.time, user.timepoint){

  message("The time of day stored in the provided POSIXlt object is ignored (cut off).")

  return(as.Date.POSIXlt(user.timepoint))
})

#' @importFrom lubridate tz
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.date",
                                                                            user.timepoint="POSIXct"), definition = function(clv.time, user.timepoint){
  message("The time of day stored in the provided data (of type POSIXct) is ignored (cut off).")

  return(as.Date.POSIXct(x=user.timepoint, tz = tz(user.timepoint)))
})


#' @importFrom lubridate parse_date_time
setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.date",
                                                                            user.timepoint="character"), definition = function(clv.time, user.timepoint){

  dates <- as.Date.POSIXct(parse_date_time(x=user.timepoint,
                                           orders = clv.time@time.format,
                                           tz="UTC",
                                           quiet = TRUE),
                           tz = "UTC")
  if(anyNA(dates))
    stop("The provided date failed to parse with the previously set date.format!", call. = FALSE)

  return(dates)
})

setMethod("clv.time.convert.user.input.to.timepoint", signature = signature(clv.time="clv.time.date",
                                                                            user.timepoint="ANY"), definition = function(clv.time, user.timepoint){
  # None of these cases
  stop("The provided data is in an unknown format! Only Date, POSIXct/lt, and character are accepted!", call. = FALSE)
})



