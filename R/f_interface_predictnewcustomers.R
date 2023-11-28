
# different generic per model to have different interfaces (with and w/o covariates)

#' @exportMethod predict.new.customers
setGeneric("predict.new.customers", def = function(clv.fitted, ids, t_i, ...){
  standardGeneric("predict.new.customers")
})


#' @include class_clv_fitted_transactions.R
setMethod("predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, ids, t_i, ...){
  check_err_msg(check_user_data_emptyellipsis(...))

  return(clv.predict.new.customers(
    clv.fitted = clv.fitted,
    clv.data.new.customers = clv.data.create.new.customer.prediction.data(clv.data=clv.fitted@clv.data, ids=ids),
    t_i=t_i))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, ids, t_i, data.cov.life, data.cov.trans, name.id, ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  return(clv.predict.new.customers(
    clv.fitted = clv.fitted,
    clv.data.new.customers = clv.data.create.new.customer.prediction.data(clv.data=clv.fitted@clv.data, ids=ids, data.cov.life=data.cov.life, data.cov.trans=data.cov.trans, name.id=name.id),
    t_i=t_i))
})


clv.predict.new.customers <- function(clv.fitted, clv.data.new.customers, t_i){

  # Replace data in model with newdata
  #   Deep copy to not change user input
  clv.fitted@clv.data <- copy(clv.data.new.customers)

  # Do model dependent steps of adding newdata
  clv.fitted <- CLVTools:::clv.model.process.newdata(clv.model = clv.fitted@clv.model, clv.fitted=clv.fitted, verbose=FALSE)

  return(clv.model.predict.new.customer.unconditional.expectation(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted, t_i=t_i))
}


