#' @importFrom lubridate is.POSIXct
#' @include class_clv_time_datetime.R
setValidity(Class = "clv.time.datetime", method = function(object){

  # Do not callNext()

  err.msg <- c()

  # Timepoints ------------------------------------------------------------------
  # may only be "POSIXct"
  if(!is.POSIXct(object@timepoint.estimation.end))
    err.msg <- c(err.msg, "Estimation end needs to be POSIXct")

  if(!is.POSIXct(object@timepoint.estimation.start))
    err.msg <- c(err.msg, "Estimation start needs to be POSIXct")

  if(!is.POSIXct(object@timepoint.holdout.start))
    err.msg <- c(err.msg, "Holdout start needs to be POSIXct")

  if(!is.POSIXct(object@timepoint.holdout.end))
    err.msg <- c(err.msg, "Holdout end needs to be POSIXct")

  # timezone ------------------------------------------------------------------
  if(length(object@timezone) == 0)
    err.msg <- c(err.msg, "Timezone has to be set for all POSIX based time classes")

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})

