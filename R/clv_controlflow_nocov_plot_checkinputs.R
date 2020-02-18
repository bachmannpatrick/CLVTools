setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted"), function(obj, prediction.end, cumulative, plot, label.line, verbose){
  # Empty fallback method.
  #   clv.controlflow.plot.check.inputs is needed for fitted.dyncov models only to check dyncov length
})
