setMethod("clv.controlflow.estimate.generate.start.params", signature = signature(obj="clv.fitted"), definition = function(obj, start.params.model,start.param.cor,verbose,...){

  # Model params
  if(is.null(start.params.model))
    untransformed.start.params.model <- setNames(obj@clv.model@start.params.model, obj@clv.model@names.original.params.model)
  else
    untransformed.start.params.model <- start.params.model[obj@clv.model@names.original.params.model] # ensure order

  transformed.start.params.model <- clv.model.transform.start.params.model(clv.model = obj@clv.model,
                                                                           original.start.params.model = untransformed.start.params.model)
  names(transformed.start.params.model) <- obj@clv.model@names.prefixed.params.model


  start.params <- transformed.start.params.model

  # Correlation param m
  if(obj@estimation.used.correlation){

    # Transform correlation to param m
    #   do model-specific transformation with the generated and transformed model parameters
    if(is.null(start.param.cor)){
      # Use cor=0 if none given
      start.param.cor.param.m <- clv.model.cor.to.m(clv.model=obj@clv.model, prefixed.params.model=transformed.start.params.model,
                                                    param.cor = 0)
    }else{
      start.param.cor.param.m <- clv.model.cor.to.m(clv.model=obj@clv.model, prefixed.params.model=transformed.start.params.model,
                                                    param.cor = start.param.cor)
    }

    # Name and add to all start params
    names(start.param.cor.param.m) <- obj@name.prefixed.cor.param.m
    start.params <- c(start.params, start.param.cor.param.m)
  }

  return(start.params)
})
