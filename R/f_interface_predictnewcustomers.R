setGeneric("clv.predict.new.customer", def = function(clv.fitted, clv.newcustomer){
  standardGeneric("clv.predict.new.customer")
})



#' @include class_clv_fitted_transactions.R
setMethod("clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, clv.newcustomer){

  if(!is(clv.newcustomer, "clv.newcustomer.no.cov") | is(clv.newcustomer, "clv.newcustomer.static.cov")){
    check_err_msg("Parameter newdata has to be output from calling `newcustomer()`!")
  }

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    clv.newcustomer=clv.newcustomer,
    t=clv.newcustomer@num.periods)))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, clv.newcustomer){
  # TODO[test]: Test that works with 1, 2, 3 covariates
  # TODO[test]: Test that for all models, the order of covariate columns does not change results
  # TODO[test]: Test that works with different life and trans covariates

  check_err_msg(check_user_data_predict_newcustomer_staticcov(clv.fitted=clv.fitted, clv.newcustomer=clv.newcustomer))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    clv.newcustomer=clv.newcustomer,
    t=clv.newcustomer@num.periods)))
})


#' @include class_clv_fitted_transactions_dynamiccov.R
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, clv.newcustomer){
  Cov.Date <- NULL

  check_err_msg(check_user_data_predict_newcustomer_dyncov(clv.fitted=clv.fitted, clv.newcustomer=clv.newcustomer))

  # TODO: Check predicting at least >2 (or min 3?) periods
  # if(clv.newcustomer@num.periods <= 2){
  #   stop("Have to plot at least 3 periods!", call. = FALSE)
  # }


  # Convert time point data and verify it correctness -------------------------------------------------------------------------------
  # This can only be done here once the clv.time object is known

  # readability
  clv.time <- clv.fitted@clv.data@clv.time


  # will be changed (by ref), therefore deep copy
  dt.cov.life <- copy(clv.newcustomer@data.cov.life)
  dt.cov.trans <- copy(clv.newcustomer@data.cov.trans)

  # Convert Cov.Date to timepoint
  dt.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]
  dt.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]

  setkeyv(dt.cov.life, cols = "Cov.Date")
  setkeyv(dt.cov.trans, cols = "Cov.Date")

  tp.first.transaction <- clv.time.convert.user.input.to.timepoint(clv.time = clv.fitted@clv.data@clv.time, user.timepoint = clv.newcustomer@first.transaction)
  tp.prediction.end <- tp.first.transaction + clv.time.number.timeunits.to.timeperiod(clv.time=clv.time, user.number.periods=clv.newcustomer@num.periods)

  check_err_msg(check_user_data_newcustomer_dyncovspecific(clv.time=clv.time, dt.cov.life=dt.cov.life, dt.cov.trans=dt.cov.trans, tp.first.transaction=tp.first.transaction, tp.prediction.end=tp.prediction.end))


  return(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=clv.newcustomer@num.periods,
    # create new object to leave original clv.newcustomer unchanged
    clv.newcustomer=clv.newcustomer.dynamic.cov(
      num.periods=clv.newcustomer@num.periods,
      data.cov.life=dt.cov.life,
      data.cov.trans=dt.cov.trans,
      first.transaction=tp.first.transaction)))
})


