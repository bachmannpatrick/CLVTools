#' @name clv.predict.new.customer
#' @title Expected number of transactions of an average new customers
#'
#' @description
#' Predict the number of transactions a single, average customer is expected to make in
#' the \code{t} periods since making the first transaction ("coming alive").
#' For covariate models, the prediction is for an average customer with the given covariates.
#'
#' The individual-level unconditional expectation that is also used for the
#' \link[CLVTools:plot.clv.fitted.transactions]{tracking plot} is used to make this prediction.
#' For models without covariates, the prediction hence is the same for all customers
#' and independent of when a customer comes alive.
#' For models with covariates, the prediction is the same for all customers
#' with the same covariates.
#' For models with dynamic covariates, the time point of the first purchase (\code{first.transaction}) is
#' additionally required because the exact covariates that are active during the prediction period have
#' to be known.
#'
#' @param clv.fitted A fitted transaction model for which prediction is desired. The data on which the model was fit and which is stored in it is NOT used for this prediction.
#' @param t A positive, numeric scalar indicating the number of periods to predict.
#' @param data.cov.life Numeric-only covariate data for the lifetime process for a single customer, \code{data.table} or \code{data.frame}. See details.
#' @param data.cov.trans Numeric-only covariate data for the transaction process for a single customer, \code{data.table} or \code{data.frame}. See details.
#' @param first.transaction For dynamic covariate models only: The time point of the first transaction of the customer for which a prediction is made.
#' Has to be within the time range of the covariate data.
#' @template template_param_dots
#'
#' @details
#' The covariate data has to contain one column for every covariate parameter in the fitted model. Only numeric values are allowed, no factors or characters.
#' No customer Id is required because the data on which the model was fit is not used for this prediction.
#'
#' For static covariates: One column for every covariate parameter in the estimated model.
#' No column \code{Id}. Exactly 1 row of numeric covariate data. \cr
#' For example: \code{data.frame(Gender=1, Age=30, Channel=0)}.
#'
#' For dynamic covariates: One column for every covariate parameter in the estimated model.
#' No column \code{Id}. A column \code{Cov.Date} with time points that mark the start of the period defined by \code{time.unit}.
#' For every \code{Cov.Date}, exactly 1 row of numeric covariate data. \cr
#' For example for weekly covariates: \code{data.frame(Cov.Date=c("2000-01-03", "2000-01-10"), Gender=c(1,1), High.Season=c(0, 1), Marketing=c(-0.5,1.12))} \cr
#' If \code{Cov.Date} is of type character, the \code{date.format} given when creating the the \code{clv.data} object is used to parse it.
#' The data has to cover the time from the customer's first transaction \code{first.transaction}
#' to the end of the prediction period given by \code{t}. It does not have to cover the same time range as when fitting the model.
#' See examples.
#'
#'
#' @return A numeric scalar indicating the expected number of transactions until \code{t}.
#'
#'
#' @examples
#' \donttest{
#' data("apparelTrans")
#' data("apparelStaticCov")
#' data("apparelDynCov")
#'
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 40)
#' clv.data.static.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelStaticCov,
#'                      names.cov.life = "Gender",
#'                      data.cov.trans = apparelStaticCov,
#'                      names.cov.trans = c("Gender", "Channel"))
#' clv.data.dyn.cov <-
#'   SetDynamicCovariates(clv.data = clv.data.apparel,
#'                        data.cov.life = apparelDynCov,
#'                        data.cov.trans = apparelDynCov,
#'                        names.cov.life = c("Marketing", "Gender"),
#'                        names.cov.trans = c("Marketing", "Gender"),
#'                        name.date = "Cov.Date")
#'
#'
#'
#' # No covariate model
#' p.apparel <- pnbd(clv.data.apparel)
#'
#' # Predict the number of transactions an average new
#' # customer is expected to make in the first 3.68 weeks
#' predict.new.customer(
#'   p.apparel,
#'   t=3.68
#' )
#'
#'
#'
#' # Static covariate model
#' p.apparel.static <- pnbd(clv.data.static.cov)
#'
#' # Predict the number of transactions an average new
#' # customer who is female (Gender=1) and who was acquired
#' # online (Channel=1) is expected to make in the first 3.68 weeks
#' predict.new.customer(
#'   p.apparel.static,
#'   t=3.68,
#'   # For the lifetime process, only Gender was used when fitting
#'   data.cov.life=data.frame(Gender=1),
#'   data.cov.trans=data.frame(Gender=1, Channel=0)
#' )
#'
#'
#' \dontrun{
#' # Dynamic covariate model
#'
#' p.apparel.dyn <- pnbd(clv.data.dyn.cov)
#'
#' # Predict the number of transactions an average new
#' # customer who is male (Gender=0), who was contacted
#' # 4, 0, and 7 times with direct marketing, and who was
#' # acquired on "2005-02-16" (first.transaction) is expected
#' # to make in the first 2.12 weeks.
#' # Note that the time range is very different from the one used
#' # when fitting the model. Cov.Date still has to match the
#' # beginning of the week.
#' predict.new.customer(
#'   p.apparel.dyn,
#'   t=2.12,
#'   data.cov.life=data.frame(
#'     Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
#'     Gender=c(0, 0, 0),
#'     Marketing=c(4, 0, 7)),
#'   data.cov.trans=data.frame(
#'     Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
#'     Gender=c(0, 0, 0),
#'     Marketing=c(4, 0, 7)),
#'   first.transaction = "2051-02-16"
#' )
#'
#' }
#' }
#'
NULL


#' @exportMethod clv.predict.new.customer
setGeneric("clv.predict.new.customer", def = function(clv.fitted, newdata, t){
  # different generic per model to have different interfaces (with and w/o covariates)
  standardGeneric("clv.predict.new.customer")
})



#' @include class_clv_fitted_transactions.R
#' @rdname clv.predict.new.customer
setMethod("clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, newdata, t){

  if(!is(newdata, "clv.newcustomer.no.cov") | is(newdata, "clv.newcustomer.static.cov")){
    stop("Parameter newdata has to be output from calling `newcustomer()`")
  }

  check_err_msg(check_user_data_newcustomer_t(t))

  return(drop(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=t)))
})


#' @include class_clv_fitted_transactions_staticcov.R
#' @rdname clv.predict.new.customer
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, newdata, t){

  # Basic inputchecks ---------------------------------------------------------------------
  if(!is(newdata, "clv.newcustomer.static.cov") | is(newdata, "clv.newcustomer.dynamic.cov")){
    stop("Parameter newdata has to be output from calling `newcustomer.static()`")
  }

  check_err_msg(check_user_data_newcustomer_t(t))

  # only need to check if right columns are here
  # if(!identical(sort(colnames(data.cov)), sort(names(clv.fitted@prediction.params.life)))){
  #   return(paste0("The Lifetime covariate data has to contain exactly the following columns: ", paste(names.cov, collapse = ", "), "!"))
  # }


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
    dt.cov.life=as.data.table(newdata@data.cov.life),
    dt.cov.trans=as.data.table(newdata@data.cov.trans),
    t=t)))
})


#' @include class_clv_fitted_transactions_dynamiccov.R
#' @rdname clv.predict.new.customer
setMethod(f = "clv.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, newdata, t){
  Cov.Date <- NULL


  # TODO: Check predicting at least >2 (or min 3?) periods
  # if(t <= 2){
  #   stop("Have to plot at least 3 periods!", call. = FALSE)
  # }

  # readability
  clv.time <- clv.fitted@clv.data@clv.time

  if(!is(newdata, "clv.newcustomer.dynamic.cov")){
    stop("Parameter newdata has to be output from calling `newcustomer.dynamic()`")
  }

  check_err_msg(check_user_data_newcustomer_t(t))
  check_err_msg(check_user_data_newcustomer_firsttransaction(newdata@first.transaction))


  # names.col = c(names(clv.fitted@prediction.params.life), "Cov.Date")
  # names.col = c(names(clv.fitted@prediction.params.trans), "Cov.Date")
  # if(!identical(sort(colnames(data.cov)), sort(names.col))){
  #   return(paste0("The ", name.of.covariate, " covariate data has to contain exactly the following columns: ", paste(names.col, collapse = ", "), "!"))
  # }


  # TODO: Check first.transaction is valid date-type input

  dt.cov.life <- as.data.table(newdata@data.cov.life)
  dt.cov.trans <- as.data.table(newdata@data.cov.trans)

  # Check Cov.Date is allowed type
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.life,  name.date = 'Cov.Date', name.var="Lifetime covariate"))
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.trans, name.date = 'Cov.Date', name.var="Transaction covariate"))

  # Convert Cov.Date to timepoint
  dt.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]
  dt.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]

  setkeyv(dt.cov.life, cols = "Cov.Date")
  setkeyv(dt.cov.trans, cols = "Cov.Date")

  tp.first.transaction <- clv.time.convert.user.input.to.timepoint(clv.time = clv.fitted@clv.data@clv.time, user.timepoint = newdata@first.transaction)
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


