setMethod("clv.controlflow.estimate.generate.start.params", signature = signature(obj="clv.fitted.static.cov"),
                                # original signature: obj, start.params.model,start.param.cor,
          definition = function(obj,
                                start.params.model,
                                start.param.cor,
                                start.params.life,
                                start.params.trans,
                                start.params.constr,
                                verbose,
                                ...){

  # Call clv.fitted start params generation
  transformed.start.params.model <- callNextMethod()

  # Covariates part
  start.params.free.life  <- c()
  start.params.free.trans <- c()
  start.params.constr     <- c()

  if(length(obj@names.original.params.free.life) > 0){ # are there any free params?
    if(is.null(start.params.life)){  # start params given?
      start.params.free.life <- rep(obj@clv.model@start.param.cov, length(obj@names.original.params.free.life))
      names(start.params.free.life) <- obj@names.original.params.free.life
    }else{
      start.params.free.life <- start.params.life[obj@names.original.params.free.life] #ensure order
    }
    start.params.free.life        <- clv.model.transform.start.params.cov(obj@clv.model, start.params.free.life)
    names(start.params.free.life) <- obj@names.prefixed.params.free.life
  }

  if(length(obj@names.original.params.free.trans) > 0){ # are there any free params?
    if(is.null(start.params.trans)){
      start.params.free.trans <- rep(obj@clv.model@start.param.cov, length(obj@names.original.params.free.trans))
      names(start.params.free.trans) <- obj@names.original.params.free.trans
    }else{
      start.params.free.trans <- start.params.trans[obj@names.original.params.free.trans] #ensure order
    }
    start.params.free.trans        <- clv.model.transform.start.params.cov(obj@clv.model, start.params.free.trans)
    names(start.params.free.trans) <- obj@names.prefixed.params.free.trans
  }

  if(length(obj@names.original.params.constr) > 0){ # are there any free params?
    if(is.null(start.params.constr)){
      start.params.constr <- rep(obj@clv.model@start.param.cov, length(obj@names.original.params.constr))
      names(start.params.constr) <- obj@names.original.params.constr
    }else{
      start.params.constr <- start.params.constr[obj@names.original.params.constr] #ensure order
    }
    start.params.constr        <- clv.model.transform.start.params.cov(obj@clv.model, start.params.constr)
    names(start.params.constr) <- obj@names.prefixed.params.constr
  }


  all.cov.params <- c(start.params.free.life,start.params.free.trans, start.params.constr) # NULL if not needed

  return(c(transformed.start.params.model, all.cov.params))

})
