#' @include class_clv_time.R
setValidity(Class = "clv.time", method = function(object){

  # Do not callNext()

  err.msg <- c()

  # Timepoints ------------------------------------------------------------------
  if(anyNA(c(object@timepoint.holdout.start, object@timepoint.holdout.end,
             object@timepoint.estimation.start, object@timepoint.estimation.end)))
    err.msg <- c(err.msg, "No timepoints may be NA")

  if(!is.na(object@timepoint.estimation.start)){
    if(object@timepoint.estimation.start >= object@timepoint.estimation.end)
      err.msg <- c(err.msg, "Estimation start cannot be after or same as end")
  }

  if(object@timepoint.holdout.start > object@timepoint.holdout.end)
    err.msg <- c(err.msg, "Holdout start cannot be after end")


  # check class / type on clv.time.date and clv.time.datetime level


  # Periods ----------------------------------------------------------------
  if(object@estimation.period.in.tu <= 0)
    err.msg <- c(err.msg, "Estimation period needs to be > 0")

  # ** estimation.period and holdout.period needs to be integer for days

  # time.format ------------------------------------------------------------
  if(length(object@time.format) == 0)
    err.msg <- c(err.msg, "Time format needs to be set")

  # time.unit, name.time.unit ----------------------------------------------
  if(!(object@name.time.unit %in% c("Weeks", "Days", "Years", "Hours")))
    err.msg <- c(err.msg, "Illegal time unit name")

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})


