#' @include class_clv_fitted.R
setValidity(Class = "clv.fitted", method = function(object){
# **TODO: INCLUDE CHECKS FOR CORRELATION

  # Do not callNext()

  # estimation not finished, yet (ie after new())
  if(NROW(object@optimx.estimation.output) == 0)
    return(TRUE)

  err.msg <- c()


  # # CBS ------------------------------------------------------------------------------------------
  # ***TODO: Add back. Or in validity_helpers for static cov and other models
  # if(!all(c("Id", "x", "t.x", "T.cal", "date.first.actual.trans", "date.last.transaction") %in% colnames(object@cbs)))
  #   err.msg <- c(err.msg, "cbs needs to contain the following columns: Id, x, t.x, T.cal, date.first.actual.trans, date.last.transaction")
  #
  # if(object@cbs[, uniqueN(Id)] != nobs(object@clv.data))
  #   err.msg <- c(err.msg, "Every customer needs to be in cbs")
  #
  # if(object@cbs[, uniqueN(Id)] != nrow(object@cbs))
  #   err.msg <- c(err.msg, "There needs to be exactly one entry for every customer in cbs")
  #
  # if(!object@cbs[, is.character(Id)])
  #   err.msg <- c(err.msg, "Column Id in cbs needs to be character")
  #
  # if(!object@cbs[, is.numeric(x)])
  #   err.msg <- c(err.msg, "Column x in cbs needs to be character")
  #
  # if(!object@cbs[, is.numeric(t.x)])
  #   err.msg <- c(err.msg, "Column t.x in cbs needs to be character")
  #
  # if(!object@cbs[, is.numeric(T.cal)])
  #   err.msg <- c(err.msg, "Column T.cal in cbs needs to be character")
  #
  # if(object@cbs[, !is.POSIXct(date.first.actual.trans) & !is.Date(date.first.actual.trans) ])
  #   err.msg <- c(err.msg, "Column date.first.actual.trans in cbs needs to be either of class POSIXct or Date")
  #
  # if(object@cbs[, !is.POSIXct(date.last.transaction) & !is.Date(date.last.transaction)])
  #   err.msg <- c(err.msg, "Column date.last.transaction in cbs needs to be either of class POSIXct or Date")
  #
  # if( (object@cbs[, is.POSIXct(date.first.actual.trans)] & !is.POSIXct(object@clv.data@clv.time@timepoint.estimation.start)) |
  #     (object@cbs[, is.Date(date.first.actual.trans)] & !is.Date(object@clv.data@clv.time@timepoint.estimation.start)))
  #   err.msg <- c(err.msg, "Column date.first.actual.trans in cbs needs to be the same type as timepoint in clv.time")
  #
  # if( (object@cbs[, is.POSIXct(date.last.transaction)] & !is.POSIXct(object@clv.data@clv.time@timepoint.estimation.start)) |
  #     (object@cbs[, is.Date(date.last.transaction)] & !is.Date(object@clv.data@clv.time@timepoint.estimation.start)))
  #   err.msg <- c(err.msg, "Column date.last.transaction in cbs needs to be the same type as timepoint in clv.time")
  #
  # if(!"Id" %in% key(object@cbs))
  #   err.msg <- c(err.msg, "cbs needs to have a key on column Id")


  # estimation.used.correlation -----------------------------------------------------------------------
  if(length(object@estimation.used.correlation) > 1)
    err.msg <- c(err.msg, "estimation.used.correlation can only be of length 1")

  # if(length(object@estimation.used.correlation)>0)
  #   if(object@estimation.used.correlation){
  #     estimated.cor <- coef()
  #     if(anyNA(object@estimated.param.cor))
  #       err.msg <- c(err.msg, "estimated.cor is NA although  used correlation")
  #
  #     if(object@estimated.param.cor > 1 | object@estimated.param.cor < -1)
  #       err.msg <- c(err.msg, "estimated correlation cor out of range")
  #   }

  # if(object@estimation.used.correlation){
  #   if(!object@name.prefixed.param.m %in% colnames(coef(object@optimx.estimation.output)))
  #     err.msg <- c(err.msg, "name.prefixed.param.m not in optimized names but using correlation")
  # }else{
  #   if(object@name.prefixed.param.m %in% colnames(coef(object@optimx.estimation.output)))
  #     err.msg <- c(err.msg, "name.prefixed.param.m in optimized names but but using correlation")
  # }

  #'   # name.original.param.m -----------------------------------------------------------------------------
  #'   **TODO: CHECK FOR Correlation
  #'
  #'   if(length(object@name.original.param.m) > 1)
  #'     err.msg <- c(err.msg, "name.original.param.m can only be of length 1")
  #'
  #'   if(length(object@name.prefixed.param.m) > 1)
  #'     err.msg <- c(err.msg, "name.prefixed.param.m can only be of length 1")
  #'
  #'   if(length(object@name.correlation.cor) > 1)
  #'     err.msg <- c(err.msg, "name.correlation.cor can only be of length 1")
  #'
  #'
  #'   if(length(object@estimation.used.correlation)>0)
  #'     if(object@estimation.used.correlation){
  #'       if(length(object@name.prefixed.param.m)==0)
  #'         err.msg <- c(err.msg, "name.prefixed.param.m not set")
  #'
  #'         if(!object@name.prefixed.param.m %in% colnames(coef(object@optimx.estimation.output)))
  #'           err.msg <- c(err.msg, "name.prefixed.param.m not in optimized optimx coefficients")
  #'
  #'         if(!object@name.correlation.cor %in% names(coef(object)))
  #'           err.msg <- c(err.msg, "name.correlation.cor not in coef()")
  #'     }
  #'

  # optimx.estimation.output ---------------------------------------------------------------
  if(nrow(object@optimx.estimation.output) > 1)
    err.msg <- c(err.msg, "There may only one estimation method be used at a time.")

  if(!all(object@clv.model@names.prefixed.params.model %in% colnames(coef(object@optimx.estimation.output))))
    err.msg <- c(err.msg, "Not all model parameters were optimized")


  # optimx.hessian --------------------------------------------------------------------------
  if(nrow(object@optimx.hessian) > 1){
    # Could extract hessian
    if(!all(colnames(object@optimx.hessian) == rownames(object@optimx.hessian)))
      err.msg <- c(err.msg, "Hessian has diverging col/rownames")

    if(!all(object@clv.model@names.prefixed.params.model %in% colnames(object@optimx.hessian)))
      err.msg <- c(err.msg, "Hessian names need to contain prefixed model names")
  }

  # prediction.params.model ----------------------------------------------------------------------
  if(length(object@prediction.params.model)>0){
    if(!all(object@clv.model@names.original.params.model %in% names(object@prediction.params.model)))
      err.msg <- c(err.msg, "Prediction.params.model all need to be named after original-scale/display names of model parameters")
  }

  # Functional stuff ------------------------------------------------------------------------
  # Could be checked in tests only, but this is too relevant to no enforce it

  # Cannot do because obj gets converted to actual no.cov but still contains covariates model
  #   what makes vcov() fail (ie behave like cov model but do not have slots)
  # Same names + length of coef/vcov
  # if(!all(rownames(vcov(object)) == names(coef(object))) |
  #    !all(colnames(vcov(object)) == names(coef(object))))
  #   err.msg <- c(err.msg, "Vcov needs to have exactly the same names as coef")
  #
  # # coef named after untransformed model params
  # if(!all(names(coef(object)) %in% object@clv.model@names.original.params.model))
  #   err.msg <- c(err.msg, "Coef need to be named in original-scale/display")
  #
  # if(length(coef(object)) != length(coef(object@optimx.estimation.output)))
  #   err.msg <- c(err.msg, "All estimated parameters need to be reported in coef()")

  # Also check model and data
  validObject(object@clv.model)
  validObject(object@clv.data)

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})
