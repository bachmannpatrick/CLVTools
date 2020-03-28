#' Time unit representing a single hour
#'
#' @include class_clv_time.R class_clv_time_datetime.R all_generics.R
#' @keywords internal
setClass("clv.time.hours", contains = "clv.time.datetime")


# Constructor
#   Cannot set estimation/holdout start/end here because for this it needs transaction dates, which first
#     need to be converted to dates and then returned to the transaction data table
#' @importFrom methods new
clv.time.hours <- function(time.format){
  return(new("clv.time.hours",
             time.format                = time.format,
             timezone                   = "UTC",
             name.time.unit             = "Hours"))
}


#' @importFrom lubridate period
setMethod("clv.time.number.timeunits.to.timeperiod", signature = signature(clv.time="clv.time.hours"), function(clv.time, user.number.periods){
  return(period(num=as.integer(user.number.periods), units="hours"))
})

#' @importFrom lubridate time_length
setMethod("clv.time.interval.in.number.tu", signature = signature(clv.time="clv.time.hours"), function(clv.time, interv){
  return(time_length(interv, unit = "hours"))
})

setMethod("clv.time.tu.to.ly", signature = signature(clv.time="clv.time.hours"), function(clv.time){
  return("Hourly")
})

#' @importFrom lubridate floor_date
setMethod("clv.time.floor.date",  signature = signature(clv.time="clv.time.hours"), function(clv.time,timepoint){
  return(floor_date(x=timepoint, unit="hours"))
})

#' @importFrom lubridate ceiling_date
setMethod("clv.time.ceiling.date",  signature = signature(clv.time="clv.time.hours"), function(clv.time,timepoint){
  return(ceiling_date(x=timepoint, unit="hours", change_on_boundary = FALSE))
})

