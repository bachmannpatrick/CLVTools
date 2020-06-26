# . clv.controlflow.predict.check.inputs ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, verbose, prediction.end, continuous.discount.factor, predict.spending, ...){
  err.msg <- c()

  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))

  err.msg <- c(err.msg, check_user_data_predictionend(clv.fitted=clv.fitted, prediction.end=prediction.end))

  # Cannot predict if no prediction.end (=null) and no holdout
  if(is.null(prediction.end) & clv.data.has.holdout(clv.fitted@clv.data) == FALSE)
    err.msg <- c(err.msg, "Cannot predict without prediction.end if there is no holdout!")

  err.msg <- c(err.msg, check_user_data_continuousdiscountfactor(continuous.discount.factor=continuous.discount.factor))

  err.msg <- c(err.msg, .check_user_data_single_boolean(b = predict.spending,
                                                        var.name = "predict.spending"))

  # predict.spending has to be single logical already
  check_err_msg(err.msg)

  # Check the data in the fitted model if it has spending
  if(predict.spending == TRUE & clv.data.has.spending(clv.fitted@clv.data) == FALSE)
    err.msg <- c(err.msg, "Cannot predict spending if there is no spending data!")

  check_err_msg(err.msg)
  # nothing to return
})


# . clv.controlflow.check.newdata ------------------------------------------------------------------------
#' @importFrom methods is extends
setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, user.newdata, prediction.end, ...){
  err.msg <- c()

  # Check newdata
  if(!is(object = user.newdata, class2 = "clv.data")){
    # This also catches NULL, NA, empty vecs, and so on
    #   but allows all cov data subclasses

    err.msg <- c(err.msg, paste0("The parameter newdata needs to be a clv data object of class ",
                                 class(clv.fitted@clv.data)))

  }else{
    # Is actually a clv.data object Also check if it is the right type
    # Check if the provided newdata is of the exact same class as the currently
    #   stored data object.
    # Cannot use is() because subclasses are recognized as well
    #   (ie clv.data.static.cov is recognized as clv.data)
    # Use extends with fullInfo=TRUE. If it is the exact same class, no distance object is
    #   returned but only TRUE. Use isTRUE to check for equality because can also return non boolean
    if(!isTRUE(extends(class1 = class(clv.fitted@clv.data), class2 = class(user.newdata), fullInfo = TRUE)))
      err.msg <- c(err.msg, paste0("An object of class ", class(clv.fitted@clv.data),
                                   " needs to be supplied for parameter newdata."))
  }

  check_err_msg(err.msg)
})


# . clv.controlflow.predict.build.result.table ---------------------------------------------------------------------
setMethod("clv.controlflow.predict.build.result.table", signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, verbose, prediction.end, ...){

  period.last <- period.first <- period.length <- NULL

  dt.predictions <- copy(clv.fitted@cbs[, "Id"])

  # Add information about range of prediction period
  #   tp.prediction.start: Start of prediction, including this timepoint
  #   tp.prediction.end: End of prediction period which includes this timepoint
  #   prediction.length: Length of period for which predictions should be made, in number of periods

  # Whether the prediction.end is valid after conversion is done in
  #   clv.time.get.prediction.table(). Cannot be done before because
  #   the end of the prediction period cannot be determined until after newdata is set
  dt.prediction.time.table <- clv.time.get.prediction.table(clv.time = clv.fitted@clv.data@clv.time,
                                                            user.prediction.end = prediction.end)

  # Add information to prediction table
  dt.predictions <- cbind(dt.predictions, dt.prediction.time.table)

  timepoint.prediction.first <- dt.predictions[1, period.first]
  timepoint.prediction.last  <- dt.predictions[1, period.last]
  prediction.period.length   <- dt.predictions[1, period.length]

  if(verbose)
    message("Predicting from ", timepoint.prediction.first, " until (incl.) ",
            timepoint.prediction.last, " (", format(prediction.period.length, digits = 4, nsmall=2)," ",
            clv.fitted@clv.data@clv.time@name.time.unit,").")

  return(dt.predictions)
})


# clv.controlflow.predict.get.has.actuals ---------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.get.has.actuals", signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, dt.predictions){
  period.last <- NULL
  timepoint.prediction.last  <- dt.predictions[1, period.last]
  return(clv.data.has.holdout(clv.fitted@clv.data) & (timepoint.prediction.last <= clv.fitted@clv.data@clv.time@timepoint.holdout.end))
})


# .clv.controlflow.predict.add.actuals ---------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.add.actuals", signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, dt.predictions, has.actuals, verbose, ...){

  Date <- period.first <- period.last <- actual.x <- i.actual.x <- NULL

  # Only if:
  #   - there is a holdout
  #   - the prediction is not beyond holdout
  #
  # Data until prediction end
  #   actual.x:         number of transactions
  #   actual.spending:  $

  timepoint.prediction.first <- dt.predictions[1, period.first]
  timepoint.prediction.last  <- dt.predictions[1, period.last]

  if(!has.actuals){
    return(dt.predictions)
  }else{
    # only what is in prediction period!
    dt.actual.transcations <- clv.fitted@clv.data@data.transactions[between(x = Date,
                                                                            lower = timepoint.prediction.first,
                                                                            upper = timepoint.prediction.last,
                                                                            incbounds = TRUE),
                                                                    list(actual.x = .N),
                                                                    by="Id"]

    setkeyv(dt.actual.transcations, "Id")
    dt.predictions[dt.actual.transcations, actual.x  := i.actual.x,  on="Id"]
    dt.predictions[is.na(actual.x),        actual.x  := 0]
    return(dt.predictions)
  }
})


# . clv.controlflow.predict.post.process.prediction.table ------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.post.process.prediction.table", signature = signature(clv.fitted="clv.fitted.transactions"), function(clv.fitted, dt.predictions, has.actuals, verbose, predict.spending, ...){

  predicted.Spending <- i.predicted.Spending <- actual.spending <- i.actual.spending <- NULL
  predicted.CLV <- DECT <- DERT <- NULL
  # Predict spending ---------------------------------------------------------------------------
  #   Estimate a GG model for this
  #   CLV: DERT * Spending
  #  Input checks already checked whether there is spending data in clv.data
  if(predict.spending){

    if(verbose)
      message("Estimating Gamma-Gamma model to predict spending...")

    fitted.gg <- gg(clv.fitted@clv.data, verbose = verbose)

    if(anyNA(coef(fitted.gg))){
      warning("The Gamma-Gamma spending model could not be fit. All spending is set to 0.", immediate. = TRUE, call. = FALSE)
      dt.predictions[, predicted.Spending := 0]
    }else{
      dt.spending <- predict(fitted.gg)
      dt.predictions[dt.spending, predicted.Spending := i.predicted.Spending, on = "Id"]

      if("actual.spending" %in% colnames(dt.spending)){
        dt.predictions[dt.spending, actual.spending := i.actual.spending, on = "Id"]
      }
    }

    # Calculate CLV
    if("DERT" %in% colnames(dt.predictions))
      dt.predictions[, predicted.CLV := DERT * predicted.Spending]
    if("DECT" %in% colnames(dt.predictions))
      dt.predictions[, predicted.CLV := DECT * predicted.Spending]
  }

  # Present cols in desired order ------------------------------------------------------------
  cols <- c("Id", "period.first", "period.last", "period.length")

  if(has.actuals)
    cols <- c(cols, "actual.x")

  # cannot determine otherwise alone from predict.spending
  if("actual.spending" %in% colnames(dt.predictions))
    cols <- c(cols, "actual.spending")

  if("DERT" %in% colnames(dt.predictions))
    cols <- c(cols, "PAlive", "CET", "DERT")
  if("DECT" %in% colnames(dt.predictions))
    cols <- c(cols, "PAlive", "CET", "DECT")

  if(predict.spending){
    cols <- c(cols, c("predicted.Spending", "predicted.CLV"))
  }

  setcolorder(dt.predictions, cols)

  return(dt.predictions)
})
