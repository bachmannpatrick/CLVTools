
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(obj="clv.fitted.dynamic.cov"), function(obj, prediction.end, continuous.discount.factor, predict.spending, verbose){
  # Do static cov (and hence also nocov) inputchecks first
  #   After this, newdata is basically ok
  callNextMethod()

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this

  dt.prediction <- clv.time.get.prediction.table(clv.time = obj@clv.data@clv.time,
                                                 user.prediction.end = prediction.end)
  tp.last.required.cov.period <- clv.time.floor.date(clv.time = obj@clv.data@clv.time,
                                                     timepoint = dt.prediction[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > obj@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})
