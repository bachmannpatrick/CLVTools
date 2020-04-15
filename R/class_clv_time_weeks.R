#' Time unit representing a single Week
#'
#' @template template_clvdate_description
#'
#' @include class_clv_time.R class_clv_time_date.R all_generics.R
#' @keywords internal
setClass("clv.time.weeks", contains = "clv.time.date")


# Constructor
#   Cannot set estimation/holdout start/end here because for this it needs transaction dates, which first
#     need to be converted to dates and then returned to the transaction data table
#' @importFrom methods new
clv.time.weeks <- function(time.format){
  return(new("clv.time.weeks",
             time.format                = time.format,
             name.time.unit             = "Weeks"))
}


#' @importFrom lubridate period
setMethod("clv.time.number.timeunits.to.timeperiod", signature = signature(clv.time="clv.time.weeks"), function(clv.time, user.number.periods){
  # with periods not integers, in case its added to posix
  return(period(num=as.integer(user.number.periods), units="weeks"))
})



#' @importFrom lubridate time_length
setMethod("clv.time.interval.in.number.tu", signature = signature(clv.time="clv.time.weeks"), function(clv.time, interv){
  return(time_length(interv, unit = "weeks"))
})

setMethod("clv.time.tu.to.ly", signature = signature(clv.time="clv.time.weeks"), function(clv.time){
  return("Weekly")
})

#' @importFrom lubridate floor_date
setMethod("clv.time.floor.date",  signature = signature(clv.time="clv.time.weeks"), function(clv.time,timepoint){
  # getOption is default, but be explicit and might change in the future
  return(floor_date(x=timepoint, unit="weeks", week_start = getOption("lubridate.week.start", 7)))
})

#' @importFrom lubridate ceiling_date
setMethod("clv.time.ceiling.date",  signature = signature(clv.time="clv.time.weeks"), function(clv.time,timepoint){
  # getOption is default, but be explicit and might change in the future
  return(ceiling_date(x=timepoint, unit="weeks", week_start = getOption("lubridate.week.start", 7),
                      change_on_boundary = FALSE))
})



