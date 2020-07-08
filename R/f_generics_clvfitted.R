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


