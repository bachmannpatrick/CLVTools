
# different generic per model to have different interfaces (with and w/o covariates)

#' @exportMethod predict.new.customers
setGeneric("predict.new.customers", def = function(clv.fitted, t, ...){
  standardGeneric("predict.new.customers")
})


#' @include class_clv_fitted_transactions.R
setMethod("predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, t, ...){
  check_err_msg(check_user_data_emptyellipsis(...))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=t)))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, t, data.cov.life, data.cov.trans, ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  # # TODO: Check that cov data contains no column 'Id'
  data.cov.life <- copy(as.data.table(data.cov.life))
  data.cov.trans <- copy(as.data.table(data.cov.trans))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    data.cov.life=data.cov.life,
    data.cov.trans=data.cov.trans,
    t=t)))
})

#' @include class_clv_fitted_transactions_dynamiccov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, t, data.cov.life, data.cov.trans, first.transaction, ...){

  check_err_msg(check_user_data_emptyellipsis(...))


  # TODO: Check predicting at least >2 (or min 3?) periods
  # if(t <= 2){
  #   stop("Have to plot at least 3 periods!", call. = FALSE)
  # }

  first.transaction <- CLVTools:::clv.time.convert.user.input.to.timepoint(clv.time = clv.fitted@clv.data@clv.time, user.timepoint = first.transaction)

  data.cov.life <- copy(as.data.table(data.cov.life))
  data.cov.trans <- copy(as.data.table(data.cov.trans))

  # # TODO: Check that both cov DTs have same min(Cov.Date)
  # # TODO: Check that first.transaction is within covariates
  # # TODO: Check that prediction end date is within covariates
  # # TODO: Check that cov data contains no column 'Id'
  # # TODO: Check that cov data contains column 'Cov.Date'
  # # TODO: Check that cov data contains correct columns (as in clv.fitted)
  # # TODO: Convert factors/strings to numerics as in SetXCov
  # if(first.transaction < data.cov.life[, min(Cov.Date)]){
  #
  # }

  return(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=t,
    data.cov.life=data.cov.life,
    data.cov.trans=data.cov.trans,
    first.transaction=first.transaction))
})


