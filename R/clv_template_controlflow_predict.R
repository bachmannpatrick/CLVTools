clv.template.controlflow.predict <- function(clv.fitted, verbose, user.newdata, uncertainty, ...){

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
  clv.controlflow.predict.check.inputs(clv.fitted=clv.fitted, verbose=verbose, ...)

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

  if(!is.null(uncertainty)){
    dt.ci <- clv.controlflow.make.uncertainty.estimates(clv.fitted=clv.fitted, num.boots=10, verbose=verbose, ...)
    dt.predictions <- dt.predictions[dt.ci, on="Id"]
  }


  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.predictions[]


  return(dt.predictions)
}


setGeneric("clv.controlflow.make.uncertainty.estimates", function(clv.fitted, num.boots, verbose, ...){
  standardGeneric("clv.controlflow.make.uncertainty.estimates")
})

setMethod(
  f = "clv.controlflow.make.uncertainty.estimates",
  signature = signature(clv.fitted="clv.fitted.transactions"),
  definition = function(clv.fitted, num.boots, verbose, prediction.end, predict.spending, continuous.discount.factor){

    # have to explicitly give prediction.end because bootstrapping data has no holdout
    if(is.null(prediction.end)){
      boots.prediction.end <- clv.fitted@clv.data@clv.time@timepoint.holdout.end
    }else{
      boots.prediction.end <- prediction.end
    }

    if(verbose){
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
        uncertainty = NULL))
    }

    if(verbose){
      message("Bootstrapping ",num.boots," times for uncertainty estimates...")
    }

    l.boots <- bootstrapped.apply(
      object = clv.fitted,
      num.boot = num.boots,
      fn.boot.apply = boots.predict,
      fn.sample = NULL,
      verbose = FALSE,
      start.params.model = clv.fitted@prediction.params.model
    )

    return(clv.fitted.confint.from.bootstrapped.predictions(rbindlist(l.boots)))
})

clv.fitted.confint.from.bootstrapped.predictions <- function(dt.boots){
  # quantiles for each predicted quantity
  cols.predictions <- c("PAlive", "CET", "DERT", "DECT", "predicted.mean.spending", "predicted.CLV")
  cols.predictions <- cols.predictions[cols.predictions %in% colnames(dt.boots)]

  dt.cis <- rbindlist(lapply(cols.predictions, function(p){
    rbindlist(lapply(c(0.1, 0.9), function(level){
      name.level <- paste0(p, ".CI.", level*100)
      return(dt.boots[, .(variable=name.level, value=quantile(get(p), probs=level)), by="Id"])
    }))
  }))

  return(dcast(dt.cis, formula = Id ~ variable, value.var = "value"))
}

setMethod(
  f = "clv.controlflow.make.uncertainty.estimates",
  signature = signature(clv.fitted="clv.fitted.spending"),
  definition = function(clv.fitted, num.boots, verbose){

    if(verbose){
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
        uncertainty = NULL))
    }

    if(verbose){
      message("Bootstrapping ",num.boots," times for uncertainty estimates...")
    }

    l.boots <- bootstrapped.apply(
      object = clv.fitted,
      num.boot = num.boots,
      fn.boot.apply = boots.predict,
      fn.sample = NULL,
      verbose = FALSE,
      start.params.model = clv.fitted@prediction.params.model
    )
    return(clv.fitted.confint.from.bootstrapped.predictions(rbindlist(l.boots)))
  })
