setGeneric("clv.predict.new.customer", def = function(clv.fitted, newdata){
  # different generic per model to have different interfaces (with and w/o covariates)
  standardGeneric("clv.predict.new.customer")
})



#' @include class_clv_fitted_transactions.R
setMethod("clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, newdata){

  if(!is(newdata, "clv.newcustomer.no.cov") | is(newdata, "clv.newcustomer.static.cov")){
    check_err_msg("Parameter newdata has to be output from calling `newcustomer()`!")
  }

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    clv.newcustomer=newdata,
    t=newdata@num.periods)))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, newdata){
  # TODO[test]: Test that works with 1, 2, 3 covariates
  # TODO[test]: Test that for all models, the order of covariate columns does not change results
  # TODO[test]: Test that works with different life and trans covariates

  check_err_msg(check_user_data_predict_newcustomer_staticcov(clv.fitted=clv.fitted, newdata=newdata))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    clv.newcustomer=newdata,
    t=newdata@num.periods)))
})


#' @include class_clv_fitted_transactions_dynamiccov.R
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, newdata){
  Cov.Date <- NULL

  check_err_msg(check_user_data_predict_newcustomer_dynccov(clv.fitted=clv.fitted, newdata=newdata))

  # TODO: Check predicting at least >2 (or min 3?) periods
  # if(newdata@num.periods <= 2){
  #   stop("Have to plot at least 3 periods!", call. = FALSE)
  # }


  # Convert time point data and verify it correctness -------------------------------------------------------------------------------
  # This can only be done here once the clv.time object is known

  # readability
  clv.time <- clv.fitted@clv.data@clv.time


  # will be changed (by ref), therefore deep copy
  dt.cov.life <- copy(newdata@data.cov.life)
  dt.cov.trans <- copy(newdata@data.cov.trans)

  # Convert Cov.Date to timepoint
  dt.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]
  dt.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]

  setkeyv(dt.cov.life, cols = "Cov.Date")
  setkeyv(dt.cov.trans, cols = "Cov.Date")

  tp.first.transaction <- clv.time.convert.user.input.to.timepoint(clv.time = clv.fitted@clv.data@clv.time, user.timepoint = newdata@first.transaction)
  tp.prediction.end <- tp.first.transaction + clv.time.number.timeunits.to.timeperiod(clv.time=clv.time, user.number.periods=newdata@num.periods)

  check_err_msg(check_user_data_newcustomer_dyncovspecific(clv.time=clv.time, dt.cov.life=dt.cov.life, dt.cov.trans=dt.cov.trans, tp.first.transaction=tp.first.transaction, tp.prediction.end=tp.prediction.end))


  return(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=newdata@num.periods,
    # create new object to leave original newdata unchanged
    clv.newcustomer=clv.newcustomer.dynamic.cov(
      num.periods=newdata@num.periods,
      data.cov.life=dt.cov.life,
      data.cov.trans=dt.cov.trans,
      first.transaction=tp.first.transaction)))
})


