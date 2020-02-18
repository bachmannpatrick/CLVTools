setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(obj="clv.fitted"), definition = function(obj){
  obj@prediction.params.model <- coef(obj)[obj@clv.model@names.original.params.model]
  return(obj)
})
