# . clv.controlflow.estimate.put.inputs -----------------------------------------------------------------
setMethod("clv.controlflow.estimate.put.inputs", signature = signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, verbose, remove.first.transaction, ...){
  clv.fitted <- callNextMethod()

  clv.fitted@estimation.removed.first.transaction <- remove.first.transaction
  return(clv.fitted)
})



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
    # Is actually a clv.data object
    #   Also check if it has spending
    if(!clv.data.has.spending(user.newdata))
      err.msg <- c(err.msg, paste0("The newdata object needs to contain spending data in order to predict spending with it."))
  }

  check_err_msg(err.msg)
})


# . clv.controlflow.predict.build.result.table -----------------------------------------------------------------
setMethod("clv.controlflow.predict.build.result.table", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, verbose, ...){
  dt.predictions <- copy(clv.fitted@cbs[, "Id"])
  return(dt.predictions)
})

# . clv.controlflow.predict.get.has.actuals -----------------------------------------------------------------
setMethod("clv.controlflow.predict.get.has.actuals", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, dt.predictions){
  return(clv.data.has.holdout(clv.fitted@clv.data))
})


# . clv.controlflow.predict.add.actuals ----------------------------------------------------------------------
setMethod("clv.controlflow.predict.add.actuals", signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, dt.predictions, has.actuals, verbose, ...){
  actual.mean.spending <- i.actual.mean.spending <- Price <- NULL

  # Spending models have no prediction.end
  #   Therefore all data in holdout period
  #   actual.mean.spending: mean spending per transaction

  if(!has.actuals){
    return(dt.predictions)
  }else{
    # only what is in prediction period!
    dt.actual.spending <- clv.data.get.transactions.in.holdout.period(clv.fitted@clv.data)
    dt.actual.spending <- dt.actual.spending[, list(actual.mean.spending = mean(Price)), keyby="Id"]

    # Add to prediction table. Customers with no actual spending (not in table) are set to 0
    dt.predictions[dt.actual.spending,          actual.mean.spending := i.actual.mean.spending, on="Id"]
    dt.predictions[is.na(actual.mean.spending), actual.mean.spending := 0]
    return(dt.predictions)
  }
})


# . clv.controlflow.predict.post.process.prediction.table ------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.post.process.prediction.table", signature = signature(clv.fitted="clv.fitted.spending"), function(clv.fitted, dt.predictions, has.actuals, verbose, ...){

  # Present cols in desired order ------------------------------------------------------------
  if(has.actuals){
    cols <- c("Id", "actual.mean.spending", "predicted.mean.spending")
  }else{
    cols <- c("Id", "predicted.mean.spending")
  }

  setcolorder(dt.predictions, cols)

  return(dt.predictions)
})
