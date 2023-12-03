
# different generic per model to have different interfaces (with and w/o covariates)

#' @exportMethod predict.new.customers
setGeneric("predict.new.customers", def = function(clv.fitted, t, ...){
  standardGeneric("predict.new.customers")
})


#' @include class_clv_fitted_transactions.R
setMethod("predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, t, ...){
  check_err_msg(check_user_data_emptyellipsis(...))

  return(clv.predict.new.customers(
    clv.fitted = clv.fitted,
    t=t))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, t, data.cov.life, data.cov.trans, ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  return(clv.predict.new.customers(
    clv.fitted = clv.fitted,
    data.cov.life=data.cov.life,
    data.cov.trans=data.cov.trans,
    t=t))
})


clv.predict.new.customers <- function(clv.fitted, t, ...){

  # # Replace data in model with newdata
  # #   Deep copy to not change user input
  # clv.fitted@clv.data <- copy(clv.data.new.customers)
  #
  # # Do model dependent steps of adding newdata
  # clv.fitted <- clv.model.process.newdata(clv.model = clv.fitted@clv.model, clv.fitted=clv.fitted, verbose=FALSE)

  return(drop(clv.model.predict.new.customer.unconditional.expectation(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted, t=t, ...)))
}


