

setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted.dynamic.cov"), function (obj, prediction.end, cumulative, plot, label.line, verbose) {
  # No nocov /staticcov checks
  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  dt.expectation <- clv.time.expectation.periods(clv.time = obj@clv.data@clv.time, user.tp.end = prediction.end)

  # only need to check one cov data, guaranteed that both are of same length
  if(dt.expectation[, max(period.first)] > obj@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given prediction.end!")


  check_err_msg(err.msg)
})

