
# different generic per model to have different interfaces (with and w/o covariates)

#' @exportMethod predict.new.customers
setGeneric("predict.new.customers", def = function(clv.fitted, t, ...){
  standardGeneric("predict.new.customers")
})


#' @include class_clv_fitted_transactions.R
setMethod("predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, t, ...){
  check_err_msg(check_user_data_newcustomer_t(t))
  check_err_msg(check_user_data_emptyellipsis(...))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=t)))
})


#' @include class_clv_fitted_transactions_staticcov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, t, data.cov.life, data.cov.trans, ...){

  # Basic inputchecks ---------------------------------------------------------------------
  check_err_msg(check_user_data_newcustomer_t(t))
  check_err_msg(check_user_data_emptyellipsis(...))
  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.life, names.cov=names(clv.fitted@prediction.params.life), name.of.covariate='Lifetime'))
  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.trans, names.cov=names(clv.fitted@prediction.params.trans), name.of.covariate='Transaction'))


  # Convert cov data ----------------------------------------------------------------------
  # Covariate data in SetStaticCovariates() can be given as factor/character which are then
  # turned into numeric dummies. This was considered here as well but dismissed
  # because it requires more than a single observation/level (ie need to
  # know 'm' and 'f' to create dummies but only one can be given here).
  # If the factors have more than 1 level (or contrasts) set, this would work
  # but was also dismissed as rather niche. It also simplifies input checks to not
  # convert the data

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    dt.cov.life=as.data.table(data.cov.life),
    dt.cov.trans=as.data.table(data.cov.trans),
    t=t)))
})

#' @include class_clv_fitted_transactions_dynamiccov.R
setMethod(f = "predict.new.customers", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, t, data.cov.life, data.cov.trans, first.transaction, ...){



  # TODO: Check predicting at least >2 (or min 3?) periods
  # if(t <= 2){
  #   stop("Have to plot at least 3 periods!", call. = FALSE)
  # }

  # readability
  clv.time <- clv.fitted@clv.data@clv.time

  check_err_msg(check_user_data_newcustomer_t(t))
  check_err_msg(check_user_data_newcustomer_firsttransaction(first.transaction))
  check_err_msg(check_user_data_emptyellipsis(...))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.life, names.col = c(names(clv.fitted@prediction.params.life), "Cov.Date"), name.of.covariate = "Lifetime"))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.trans, names.col = c(names(clv.fitted@prediction.params.trans), "Cov.Date"), name.of.covariate = "Transaction"))
  # TODO: Check first.transaction is valid date-type input


  dt.cov.life <- as.data.table(data.cov.life)
  dt.cov.trans <- as.data.table(data.cov.trans)

  # Check Cov.Date is allowed type
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.life,  name.date = 'Cov.Date', name.var="Lifetime covariate"))
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.trans, name.date = 'Cov.Date', name.var="Transaction covariate"))

  # Convert Cov.Date to timepoint
  dt.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]
  dt.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]

  setkeyv(dt.cov.life, cols = "Cov.Date")
  setkeyv(dt.cov.trans, cols = "Cov.Date")

  tp.first.transaction <- clv.time.convert.user.input.to.timepoint(clv.time = clv.fitted@clv.data@clv.time, user.timepoint = first.transaction)
  tp.prediction.end <- tp.first.transaction + clv.time.number.timeunits.to.timeperiod(clv.time=clv.time, user.number.periods=t)

  check_err_msg(check_user_data_newcustomer_dyncovspecific(clv.time=clv.time, dt.cov.life=dt.cov.life, dt.cov.trans=dt.cov.trans, tp.first.transaction=tp.first.transaction, tp.prediction.end=tp.prediction.end))


  return(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=t,
    dt.cov.life=dt.cov.life,
    dt.cov.trans=dt.cov.trans,
    tp.first.transaction=tp.first.transaction))
})


