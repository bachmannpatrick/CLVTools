setMethod("clv.controlflow.estimate.put.inputs", signature =  signature(obj="clv.fitted"), definition = function(obj, cl, use.cor, ...){

  obj@call <- cl

  # Should correlation be calculated? -----------------------------------------------------------------
  if(use.cor){
    # Using correlation
    obj@estimation.used.correlation <- TRUE
  }else{
    # No correlation
    obj@estimation.used.correlation <- FALSE
  }

  return(obj)
})
