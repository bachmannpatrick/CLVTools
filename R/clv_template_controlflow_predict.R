clv.template.controlflow.predict <- function(clv.fitted, verbose, user.newdata, uncertainty, num.boots, level, ...){

  # Check if can predict -----------------------------------------------------------------------------------------
  # Cannot predict if there are any NAs in any of the prediction.params
  clv.controlflow.check.prediction.params(clv.fitted = clv.fitted)

  # Process Newdata -------------------------------------------------------------------------------------
  # Because many of the following steps refer to the data stored in the fitted model,
  #   it first is replaced with newdata before any other steps are done
  if(!is.null(user.newdata)){
    # check newdata
    clv.controlflow.check.newdata(clv.fitted = clv.fitted, user.newdata = user.newdata, ...)

    # Replace data in model with newdata
    #   Deep copy to not change user input
    clv.fitted@clv.data <- copy(user.newdata)

    # Do model dependent steps of adding newdata
    clv.fitted <- clv.model.process.newdata(clv.model = clv.fitted@clv.model, clv.fitted=clv.fitted, verbose=verbose)
  }


  # Input checks ----------------------------------------------------------------------------------------
  #   Only after newdata replaced clv.data stored in clv.fitted because inputchecks use clv.fitted@clv.data
  clv.controlflow.predict.check.inputs(clv.fitted=clv.fitted, verbose=verbose, uncertainty=uncertainty, num.boots=num.boots, level=level, ...)

  # Prediction result table -----------------------------------------------------------------------------
  dt.predictions <- clv.controlflow.predict.build.result.table(clv.fitted=clv.fitted, verbose=verbose, ...)

  # Model prediction ------------------------------------------------------------------------------------
  dt.predictions <- clv.model.predict(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted,
                                      dt.predictions = dt.predictions, verbose = verbose, ...)
  setkeyv(dt.predictions, "Id")

  # Actuals ---------------------------------------------------------------------------------------------
  has.actuals    <- clv.controlflow.predict.get.has.actuals(clv.fitted, dt.predictions = dt.predictions)
  dt.predictions <- clv.controlflow.predict.add.actuals(clv.fitted = clv.fitted, dt.predictions = dt.predictions,
                                                        has.actuals = has.actuals, verbose = verbose, ...)


  # post.process / add any additional steps -------------------------------------------------------------
  # set col order etc
  dt.predictions <- clv.controlflow.predict.post.process.prediction.table(clv.fitted = clv.fitted,
                                                                          has.actuals = has.actuals,
                                                                          dt.predictions = dt.predictions,
                                                                          verbose = verbose, ...)

  if(tolower(uncertainty) == "boots"){
    if(verbose & num.boots < 1000){
      warning("It is recommended to run 1000 or more bootstrap repetitions.", immediate. = TRUE, call. = FALSE)
    }
    dt.predictions <- clv.controlflow.predict.add.uncertainty.estimates(clv.fitted=clv.fitted, dt.predictions = dt.predictions, num.boots=num.boots, level=level, verbose=verbose, ...)
    # TODO: Column order is correct also with CI columns
    # TODO[test]: All customers are kept also if not all are kept
  }


  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.predictions[]

  return(dt.predictions)
}

#' @importFrom stats quantile
clv.controlflow.predict.add.uncertainty.estimates <- function(clv.fitted, dt.predictions, num.boots, level, verbose, ...){
  Id <- i.value <- value.diff <- value <- value.star <- value.diff <- ci.name <- variable <- NULL

  dt.boots <- clv.fitted.bootstrap.predictions(clv.fitted = clv.fitted, num.boots = num.boots, verbose = verbose, ...)

  if(verbose){
    message("Calculating confidence intervals...")
  }

  # Customers that are sampled multiple times are added to the boostrapping data with suffix "_BOOTSID_<i?"
  # Remove this suffix again to get the original Id and calculate the quantiles across a single customers multiple draws
  # regex: "ends with _BOOTSTRAP_ID_<one or more digits>"
  dt.boots[, Id := sub("_BOOTSTRAP_ID_[0-9]+$", "", Id)]

  # quantiles for each predicted quantity
  # select only the existing ones
  cols.predictions <- c("PAlive", "CET", "DERT", "DECT", "predicted.mean.spending", "predicted.CLV")
  cols.predictions <- cols.predictions[cols.predictions %in% colnames(dt.boots)]

  # Long-format for easier handling of different prediction columns
  dt.boots <- melt(dt.boots, id.vars="Id", measure.vars=cols.predictions, variable.name="variable", value.name="value")
  dt.predictions.long <- melt(dt.predictions, id.vars="Id", measure.vars=cols.predictions, variable.name="variable", value.name="value")

  # Calculate quantiles for each customer and prediction column
  #
  # Reversed quantiles
  #   [theta_star - q_upper(diff), theta_star - q_lower(diff)]
  #   where diff = theta_boot - theta_star
  # Note that q_upper is used for the lower boundary and q_lower for the upper boundary while subtracting in both cases.
  # Therefore quantile(probs=) is reversed.

  # Calculate difference between bootstrapped and regular predictions
  dt.boots[dt.predictions.long, value.star := i.value, on=c("Id", "variable")]
  dt.boots[, value.diff := value - value.star]

  levels <- c((1-level)/2, 1-(1-level)/2)
  name.levels <- paste0(".CI.", levels*100) # outside table to avoid doing it for each customer

  dt.CI <- dt.boots[, list(
    ci.name=name.levels,
    # Have to use value.star[1] because there are >1 row if sampled more than once.
    # names=FALSE is considerably faster.
    ci.value = value.star[1] - quantile(value.diff, probs = rev(levels), names = FALSE)),
    keyby=c("Id", "variable")]

  # Presentable names
  dt.CI[, ci.name := paste0(variable, ci.name)]

  # To wide-format
  dt.CI <- dcast(dt.CI, Id ~ ci.name, value.var="ci.value")

  # Add intervals to predictions, keeping all predictions also if for some customers there are no ids
  # because they have never been sampled (fill with NA)
  return(merge(x=dt.predictions, y=dt.CI, by="Id", all=TRUE))
}



setGeneric("clv.fitted.bootstrap.predictions", function(clv.fitted, num.boots, verbose, ...){
  standardGeneric("clv.fitted.bootstrap.predictions")
})

setMethod(f = "clv.fitted.bootstrap.predictions", signature = signature(clv.fitted="clv.fitted.transactions"), definition = function(clv.fitted, num.boots, verbose, prediction.end, predict.spending, continuous.discount.factor){

    # have to explicitly give prediction.end because bootstrapping data has no holdout
    if(is.null(prediction.end)){
      boots.prediction.end <- clv.fitted@clv.data@clv.time@timepoint.holdout.end
    }else{
      boots.prediction.end <- prediction.end
    }

    if(verbose){
      # Print message before progress bar is created
      message("Bootstrapping ",num.boots," times for uncertainty estimates...")

      progress.bar <- txtProgressBar(max = num.boots, style = 3)
      update.pb    <- function(n){setTxtProgressBar(pb=progress.bar, value = n)}
    }else{
      # has to be also defined if verbose=F because used in boots.predict
      update.pb <- function(n){}
    }
    pb.i <- 0

    boots.predict <- function(clv.boot){
      pb.i <<- pb.i + 1
      update.pb(n = pb.i)
      return(predict(
        object = clv.boot,
        prediction.end = boots.prediction.end,
        verbose = FALSE,
        predict.spending = predict.spending,
        continuous.discount.factor = continuous.discount.factor,
        uncertainty = "none"))
    }

    l.boots <- clv.bootstrapped.apply(
      object = clv.fitted,
      num.boot = num.boots,
      fn.boot.apply = boots.predict,
      fn.sample = NULL,
      verbose = FALSE,
      start.params.model = clv.fitted@prediction.params.model
    )

    return(rbindlist(l.boots))
})


setMethod(f = "clv.fitted.bootstrap.predictions",signature = signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted, num.boots, verbose){

  # Largely the same as for clv.fitted.transactions but with different arguments to predict()


    if(verbose){
      # Print message before progress bar is created
      message("Bootstrapping ",num.boots," times for uncertainty estimates...")

      progress.bar <- txtProgressBar(max = num.boots, style = 3)
      update.pb    <- function(n){setTxtProgressBar(pb=progress.bar, value = n)}
    }else{
      # has to be also defined if verbose=F because used in boots.predict
      update.pb <- function(n){}
    }
    pb.i <- 0

    boots.predict <- function(clv.boot){
      pb.i <<- pb.i + 1
      update.pb(n = pb.i)
      return(predict(
        object = clv.boot,
        verbose = FALSE,
        uncertainty = "none"))
    }

    l.boots <- clv.bootstrapped.apply(
      object = clv.fitted,
      num.boot = num.boots,
      fn.boot.apply = boots.predict,
      fn.sample = NULL,
      verbose = FALSE,
      start.params.model = clv.fitted@prediction.params.model
    )
    return(rbindlist(l.boots))
  })
