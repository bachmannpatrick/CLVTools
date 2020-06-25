# . clv.controlflow.plot.check.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted"), function(obj, prediction.end, cumulative, plot, label.line, verbose){
  # Empty fallback method.
})


# . clv.controlflow.predict.set.prediction.params ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted){
  clv.fitted@prediction.params.model <- coef(clv.fitted)[clv.fitted@clv.model@names.original.params.model]
  return(clv.fitted)
})


