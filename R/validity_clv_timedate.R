#' @importFrom lubridate is.Date
#' @include class_clv_time_date.R
setValidity(Class = "clv.time.date", method = function(object){

  # Do not callNext()

  err.msg <- c()

  # Timepoints ------------------------------------------------------------------
  # may only be "Date"
  if(!is.Date(object@timepoint.estimation.end))
    err.msg <- c(err.msg, "Estimation end needs to be a Date")

  if(!is.Date(object@timepoint.estimation.start))
    err.msg <- c(err.msg, "Estimation start needs to be a Date")

  if(!is.Date(object@timepoint.holdout.start))
    err.msg <- c(err.msg, "Holdout start needs to be a Date")

  if(!is.Date(object@timepoint.holdout.end))
    err.msg <- c(err.msg, "Holdout end needs to be a Date")

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})
