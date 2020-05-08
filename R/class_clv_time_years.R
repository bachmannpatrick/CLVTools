#' Time unit representing a single Year
#'
#' @template template_clvdate_description
#'
#' @include class_clv_time.R class_clv_time_date.R all_generics.R
#' @keywords internal
setClass("clv.time.years", contains = "clv.time.date")


# Constructor
#   Cannot set estimation/holdout start/end here because for this it needs transaction dates, which first
#     need to be converted to dates and then returned to the transaction data table
#' @importFrom methods new
clv.time.years <- function(time.format){
  return(new("clv.time.years",
             time.format                = time.format,
             name.time.unit             = "Years"))
}


#' @importFrom lubridate period
setMethod("clv.time.number.timeunits.to.timeperiod", signature = signature(clv.time="clv.time.years"), function(clv.time, user.number.periods){
  # with periods, years have different length
  return(period(num=as.integer(user.number.periods), units="years"))
})


#' @importFrom lubridate time_length
setMethod("clv.time.interval.in.number.tu", signature = signature(clv.time="clv.time.years"), function(clv.time, interv){
  return(time_length(interv, unit = "years"))
})

setMethod("clv.time.tu.to.ly", signature = signature(clv.time="clv.time.years"), function(clv.time){
  return("Yearly")
})


#' @importFrom lubridate floor_date
setMethod("clv.time.floor.date",  signature = signature(clv.time="clv.time.years"), function(clv.time,timepoint){
  return(floor_date(x=timepoint, unit="years"))
})

#' @importFrom lubridate ceiling_date
setMethod("clv.time.ceiling.date",  signature = signature(clv.time="clv.time.years"), function(clv.time,timepoint){
  return(ceiling_date(x=timepoint, unit="years", change_on_boundary = FALSE))
})


