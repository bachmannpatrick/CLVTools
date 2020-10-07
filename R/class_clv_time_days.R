#' Time unit representing a single Day
#'
#' @template template_clvdate_description
#'
#' @seealso  \linkS4class{clv.time}
#' @seealso  \linkS4class{clv.time.date}
#'
#' @include class_clv_time.R class_clv_time_date.R all_generics.R
#' @keywords internal
setClass("clv.time.days", contains = "clv.time.date")


# Constructor
#   Cannot set estimation/holdout start/end here because for this it needs transaction dates, which first
#     need to be converted to dates and then returned to the transaction data table
#' @importFrom methods new
clv.time.days <- function(time.format){
  return(new("clv.time.days",
             time.format                = time.format,
             name.time.unit             = "Days"))
}

#' @importFrom lubridate period
setMethod("clv.time.number.timeunits.to.timeperiod", signature = signature(clv.time="clv.time.days"), function(clv.time, user.number.periods){
  # with periods not integers, in case its added to posix
  return(period(num=as.integer(user.number.periods), units="days"))
})

#' @importFrom lubridate time_length
setMethod("clv.time.interval.in.number.tu", signature = signature(clv.time="clv.time.days"), function(clv.time, interv){
  return(time_length(interv, unit = "days"))
})


setMethod("clv.time.tu.to.ly", signature = signature(clv.time="clv.time.days"), function(clv.time){
  return("Daily")
})


#' @importFrom lubridate floor_date
setMethod("clv.time.floor.date",  signature = signature(clv.time="clv.time.days"), function(clv.time,timepoint){
  return(floor_date(x=timepoint, unit="days"))
})

#' @importFrom lubridate ceiling_date
setMethod("clv.time.ceiling.date",  signature = signature(clv.time="clv.time.days"), function(clv.time,timepoint){
  return(ceiling_date(x=timepoint, unit="days", change_on_boundary = FALSE))
})




