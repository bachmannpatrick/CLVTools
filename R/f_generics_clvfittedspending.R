# . clv.controlflow.predict.check.inputs -----------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, verbose, ...){
  err.msg <- c()

  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))

  check_err_msg(err.msg)
})


# . clv.controlflow.check.newdata ------------------------------------------------------------------------
#' @importFrom methods is
setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, user.newdata, ...){
  err.msg <- c()

  # Check newdata
  if(!is(object = user.newdata, class2 = "clv.data")){
    # This also catches NULL, NA, empty vecs, and so on
    #   but allows all cov data subclasses
    err.msg <- c(err.msg, paste0("The parameter newdata needs to be a clv data object of class clv.data or a subclass thereof."))

  }else{
    # Is actually a clv.data object.
    #   Also check if it has spending
    if(!clv.data.has.spending(user.newdata))
      err.msg <- c(err.msg, paste0("The newdata object needs to contain spending data in order to predict spending with it."))
  }

  check_err_msg(err.msg)
})



setMethod("clv.controlflow.predict.build.result.table", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, verbose, ...){
  dt.predictions <- copy(clv.fitted@cbs[, "Id"])
  return(dt.predictions)
})

setMethod("clv.controlflow.predict.get.has.actuals", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, dt.predictions){
  return(clv.data.has.holdout(clv.fitted@clv.data))
})

setMethod("clv.controlflow.predict.add.actuals", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, dt.predictions, has.actuals, verbose, ...){
  actual.spending <- i.actual.spending <- Price <- NULL
  # Only if:
  #   - there is a holdout
  #   - the prediction is not beyond holdout
  #
  # Data until prediction end
  #   actual.spending:  $

  if(!has.actuals){
    return(dt.predictions)
  }else{
    # only what is in prediction period!
    dt.actual.spending <- clv.data.get.transactions.in.holdout.period(clv.fitted@clv.data)
    dt.actual.spending <- dt.actual.spending[, list(actual.spending = sum(Price)), by="Id"]


    setkeyv(dt.actual.spending, "Id")
    dt.predictions[dt.actual.spending,     actual.spending := i.actual.spending, on="Id"]
    dt.predictions[is.na(actual.spending), actual.spending := 0]
    return(dt.predictions)
  }
})

# . clv.controlflow.predict.post.process.prediction.table ------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.post.process.prediction.table", signature = signature(clv.fitted="clv.fitted.spending"), function(clv.fitted, dt.predictions, has.actuals, verbose, ...){

  # Present cols in desired order ------------------------------------------------------------
  if(has.actuals){
    cols <- c("Id", "predicted.Spending", "actual.spending")
  }else{
    cols <- c("Id", "predicted.Spending")
  }

  setcolorder(dt.predictions, cols)

  return(dt.predictions)
})
