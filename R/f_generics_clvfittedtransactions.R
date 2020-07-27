# . clv.controlflow.predict.check.inputs ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, verbose, prediction.end, continuous.discount.factor, predict.spending, ...){
  err.msg <- c()

  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))

  err.msg <- c(err.msg, check_user_data_predictionend(clv.fitted=clv.fitted, prediction.end=prediction.end))

  # Cannot predict if no prediction.end (=null) and no holdout
  if(is.null(prediction.end) & clv.data.has.holdout(clv.fitted@clv.data) == FALSE)
    err.msg <- c(err.msg, "Cannot predict without prediction.end if there is no holdout!")

  err.msg <- c(err.msg, check_user_data_continuousdiscountfactor(continuous.discount.factor=continuous.discount.factor))

  # predict.spending
  # Spending can be predicted using either a function (ie gg), a logical (ie FALSE), or an
  #   already fitted spending model
  if(is(object = predict.spending, class2 = "clv.fitted.spending")){
    # Check if usable for prediction
    if(anyNA(coef(predict.spending)))
      err.msg <- c(err.msg, "The provided spending model in parameter 'predict.spending' cannot be used because its estimated coefficents contain NA!")

  }else{
    if(is.function(predict.spending)){
      # has to be a CLVTools spending model
      if(!isTRUE(all.equal(predict.spending, CLVTools::gg)))
        err.msg <- c(err.msg, "The method to predict spending has to be a spending model from CLVTools (ie gg)!")

    }else{
      # None of the above: Then it has to be a logical
      #   cannot use .check_user_data_single_boolean() because it will return message "has to be logical" what is not true
      #     for predict.spending
      if(!is.logical(predict.spending)){
        err.msg <- c(err.msg, "The parameter predict.spending has to be either an already fitted spending model, a method from CLVTools to fit a spending model (ie gg) or a logical (True/False)!")

        # Cannot continue if not a logical
      }else{
        # Is logical
        if(length(predict.spending)>1)
          err.msg <- c(err.msg, paste0("The parameter predict.spending can only contain a single element!"))
        if(anyNA(predict.spending))
          err.msg <- c(err.msg, paste0("The parameter predict.spending cannot be NA!"))
      }
    }
  }

  # predict.spending has to be valid already
  check_err_msg(err.msg)

  # There has to be spending data if it should be predicted from it
  if(is.logical(predict.spending)){
    if(predict.spending == TRUE & clv.data.has.spending(clv.fitted@clv.data) == FALSE)
      err.msg <- c(err.msg, "Cannot predict spending if there is no spending data!")

  }else{
    # function or fitted spending model but both required spending data
    if(clv.data.has.spending(clv.fitted@clv.data) == FALSE)
      err.msg <- c(err.msg, "Cannot predict spending if there is no spending data!")
  }

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

  # Only if:
  #   - there is a holdout
  #   - the prediction is not beyond holdout

  timepoint.prediction.last  <- dt.predictions[1, period.last]
  return(clv.data.has.holdout(clv.fitted@clv.data) & (timepoint.prediction.last <= clv.fitted@clv.data@clv.time@timepoint.holdout.end))
})


# .clv.controlflow.predict.add.actuals ---------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.add.actuals", signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, dt.predictions, has.actuals, verbose, ...){

  actual.total.spending <- i.actual.total.spending <- Price <- Date <- period.first <- period.last <- actual.x <- i.actual.x <- NULL

  # Only if:
  #   - there is a holdout
  #   - the prediction is not beyond holdout
  #
  # Data until prediction end
  #   actual.x:              number of transactions
  #   actual.total.spending: total spending

  timepoint.prediction.first <- dt.predictions[1, period.first]
  timepoint.prediction.last  <- dt.predictions[1, period.last]

  if(!has.actuals){
    return(dt.predictions)
  }else{
    # only what is in prediction period!
    dt.holdout.transactions <- clv.data.get.transactions.in.holdout.period(clv.fitted@clv.data)
    dt.actuals.transactions <- dt.holdout.transactions[between(x = Date,
                                                               lower = timepoint.prediction.first,
                                                               upper = timepoint.prediction.last,
                                                               incbounds = TRUE)]

    if(clv.data.has.spending(clv.fitted@clv.data)){
      dt.actuals  <- dt.actuals.transactions[, list(actual.x              = .N,
                                                    actual.total.spending = sum(Price)),
                                             keyby="Id"]
    }else{
      dt.actuals  <- dt.actuals.transactions[, list(actual.x              = .N), keyby="Id"]
    }

    dt.predictions[dt.actuals,       actual.x  := i.actual.x,  on="Id"]
    dt.predictions[is.na(actual.x),  actual.x  := 0]

    if(clv.data.has.spending(clv.fitted@clv.data)){
      dt.predictions[dt.actuals,                   actual.total.spending  := i.actual.total.spending,  on="Id"]
      dt.predictions[is.na(actual.total.spending), actual.total.spending  := 0]
    }
    return(dt.predictions)
  }
})


# . clv.controlflow.predict.post.process.prediction.table ------------------------------------------------------------------------------
setMethod("clv.controlflow.predict.post.process.prediction.table", signature = signature(clv.fitted="clv.fitted.transactions"), function(clv.fitted, dt.predictions, has.actuals, verbose, predict.spending, ...){
  predicted.mean.spending <- i.predicted.mean.spending <- actual.total.spending <- i.actual.total.spending <- NULL
  predicted.CLV <- DECT <- DERT <- NULL

  # Predict spending ---------------------------------------------------------------------------------------
  # depends on content of predict.spending:
  #   Predict spending
  #     logical TRUE:   fit gg     on data and predict
  #     function:       fit method on data and predict
  #     fitted model:   predict
  #
  #   Add predicted spending to prediction table
  # Input checks already checked whether there is spending data in clv.data


  # Add spending data and actuals to prediction table
  fct.add.spending.data <- function(dt.spending, dt.predictions){
    dt.predictions[dt.spending, predicted.mean.spending := i.predicted.mean.spending, on = "Id"]

    # The actual.mean.spending from dt.spending is not added anymore
    #   actual.total.spending is already in prediction table

    return(dt.predictions)
  }


  # Fit, predict, and add to prediction table
  fct.fit.and.predict.spending.model <- function(spending.method, verbose, dt.predictions){
    name.model <- as.character(spending.method@generic)
    if(verbose){
      message(paste0("Estimating ",name.model," model to predict spending..."))
    }

    # Fit spending method onto data
    fitted.spending <- do.call(what = spending.method, args = list(clv.data = clv.fitted@clv.data,
                                                                   verbose = verbose))

    if(anyNA(coef(fitted.spending))){

      warning(paste0("The ",name.model," spending model could not be fit. All spending is set to 0."), immediate. = TRUE, call. = FALSE)
      dt.predictions[, predicted.mean.spending := 0]

    }else{
      # Did fit fine
      res.sum <- summary(fitted.spending)
      if(res.sum$kkt1 == FALSE | res.sum$kkt2 == FALSE){
        warning(paste0("The KKT optimality conditions are not both met for the fitted ",name.model," spending model."), immediate. = TRUE, call. = FALSE)
      }

      dt.spending    <- predict(fitted.spending)
      dt.predictions <- fct.add.spending.data(dt.spending = dt.spending, dt.predictions = dt.predictions)
    }

    return(dt.predictions)
  }


  # Predict spending in any case except if predict.spending == FALSE
  #   use all.equal because can also be function or fitted spending model
  do.predict.spending <- !isTRUE(all.equal(predict.spending, FALSE))
  if(do.predict.spending){

    if(is.logical(predict.spending)){
      # Is TRUE because FALSE would not be here
      dt.predictions <- fct.fit.and.predict.spending.model(spending.method = gg, verbose = verbose,
                                                           dt.predictions = dt.predictions)
    }else{
      if(is.function(predict.spending)){
        # Use the method provided by the user
        dt.predictions <- fct.fit.and.predict.spending.model(spending.method = predict.spending, verbose = verbose,
                                                             dt.predictions = dt.predictions)
      }else{
        # is already fitted model
        # no further checks, the user is (hopefully) happy with how it fitted (coef not NA is checking in inputchecks)
        # newdata: Predict for data in transaction model, dont predict spending for data on which the spending model was fit
        dt.spending    <- predict(object = predict.spending, newdata = clv.fitted@clv.data, verbose = verbose)
        dt.predictions <- fct.add.spending.data(dt.spending = dt.spending, dt.predictions = dt.predictions)
      }
    }

    # If spending is predicted, CLV is also calculated
    # CLV: DERT/DECT * Spending
    if("DERT" %in% colnames(dt.predictions))
      dt.predictions[, predicted.CLV := DERT * predicted.mean.spending]
    if("DECT" %in% colnames(dt.predictions))
      dt.predictions[, predicted.CLV := DECT * predicted.mean.spending]
  }

  # Present cols in desired order ------------------------------------------------------------
  cols <- c("Id", "period.first", "period.last", "period.length")

  if(has.actuals){
    cols <- c(cols, "actual.x")

    # cannot determine otherwise alone from has.actuals
    if("actual.total.spending" %in% colnames(dt.predictions))
      cols <- c(cols, "actual.total.spending")
  }

  if("DERT" %in% colnames(dt.predictions))
    cols <- c(cols, "PAlive", "CET", "DERT")
  if("DECT" %in% colnames(dt.predictions))
    cols <- c(cols, "PAlive", "CET", "DECT")

  if(do.predict.spending)
    cols <- c(cols, "predicted.mean.spending")

  if("predicted.CLV" %in% colnames(dt.predictions))
    cols <- c(cols, "predicted.CLV")

  setcolorder(dt.predictions, cols)

  return(dt.predictions)
})
