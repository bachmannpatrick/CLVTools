# . clv.controlflow.plot.check.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted"), function(obj, prediction.end, cumulative, plot, label.line, verbose){
  # Empty fallback method.
})

# . clv.controlflow.check.prediction.params -----------------------------------------------------------------
setMethod("clv.controlflow.check.prediction.params", signature = signature(clv.fitted = "clv.fitted"), function(clv.fitted){
  # Do not check coef() because correlation coef may be NA and can still predict
  if(anyNA(clv.fitted@prediction.params.model)){
    check_err_msg("Cannot proceed because there are NAs in the estimated model coefficients!")
  }
})

# . clv.controlflow.predict.set.prediction.params ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted){
  clv.fitted@prediction.params.model <- coef(clv.fitted)[clv.fitted@clv.model@names.original.params.model]
  return(clv.fitted)
})



# . clv.fitted.estimate.same.specification.on.new.data ------------------------------------------------------------------------
setMethod("clv.fitted.estimate.same.specification.on.new.data", signature = "clv.fitted", def = function(clv.fitted, newdata, ...){
  cl <- match.call(expand.dots = TRUE)

  # args to model function are the original specification args + `newdata` as
  # actual data arg
  args <- c(clv.fitted@model.specification.args, list(clv.data=newdata))

  # overwrite call args with what was passed
  args <- modifyList(args, val = list(...), keep.null = TRUE)

  new.fitted <- do.call(what = clv.fitted@clv.model@fn.model.generic, args=args)

  new.fitted@call <- cl
  return(new.fitted)
})

