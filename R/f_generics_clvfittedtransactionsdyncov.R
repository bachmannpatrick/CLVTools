# . clv.controlflow.plot.check.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted.transactions.dynamic.cov"), function (obj, prediction.end, cumulative, plot, label.line, verbose) {
  period.until <- Cov.Date <- NULL

  # No nocov /staticcov checks (no need to call super method)
  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  dt.expectation <- clv.time.expectation.periods(clv.time = obj@clv.data@clv.time, user.tp.end = prediction.end)

  # only need to check one cov data, guaranteed that both are of same length
  if(dt.expectation[, max(period.until)] > obj@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given prediction.end!")

  check_err_msg(err.msg)
})


# . clv.controlflow.check.newdata ------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, user.newdata, prediction.end, ...){
  # Do static cov (and hence also nocov) inputchecks first for newdata
  callNextMethod()

  period.last <- Cov.Date <- NULL

  # prediction.end needs to be ok to work with it
  check_err_msg(check_user_data_predictionend(clv.fitted=clv.fitted, prediction.end=prediction.end))

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  #   newdata will replace existing data therefore check its cov
  #     Also clv.time in newdata has to be used for conversion of prediction.end
  dt.predictions <- clv.time.get.prediction.table(clv.time = user.newdata@clv.time,
                                                 user.prediction.end = prediction.end)

  tp.last.required.cov.period <- clv.time.floor.date(clv.time = user.newdata@clv.time,
                                                     timepoint = dt.predictions[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > user.newdata@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in parameter newdata are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})


# . clv.controlflow.predict.check.inputs ------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), function(clv.fitted, verbose, prediction.end, continuous.discount.factor, predict.spending, ...){
  # Do static cov (and hence also nocov) inputchecks first
  #   After this, newdata is basically ok
  callNextMethod()

  period.last <- Cov.Date <- NULL

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  dt.predictions <- clv.time.get.prediction.table(clv.time = clv.fitted@clv.data@clv.time,
                                                 user.prediction.end = prediction.end)
  tp.last.required.cov.period <- clv.time.floor.date(clv.time = clv.fitted@clv.data@clv.time,
                                                     timepoint = dt.predictions[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > clv.fitted@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})
