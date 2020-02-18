#' @include all_generics.R class_clv_fitted_static_cov.R clv_data_reducecovariates.R
setMethod("clv.controlflow.estimate.put.inputs", signature = signature(obj="clv.fitted.static.cov"), definition = function(obj, cl, reg.lambdas, use.cor, names.cov.constr, names.cov.life, names.cov.trans, ...){
  # clv.fitted put estimation
  obj <- callNextMethod()

  # Reduce to user's desired covariates only -----------------------------------------------------------
  obj@clv.data <- clv.data.reduce.covariates(clv.data=obj@clv.data, names.cov.life=names.cov.life,
                                             names.cov.trans=names.cov.trans)

  # is regularization used? ---------------------------------------------------------------------------
  #   Yes:  Indicate and store lambdas
  #   No:   Indicate

  if(!is.null(reg.lambdas) & !all(reg.lambdas == 0)){
    # Regularization: Store
    obj@estimation.used.regularization <- TRUE
    obj@reg.lambda.life  <- reg.lambdas[["life"]]
    obj@reg.lambda.trans <- reg.lambdas[["trans"]]
  }else{
    # No regularization
    obj@estimation.used.regularization <- FALSE
    obj@reg.lambda.life  <- numeric(0)
    obj@reg.lambda.trans <- numeric(0)
  }

  # Are some parameters constraint? --------------------------------------------------------------------
  if(!is.null(names.cov.constr)){
    # Using constraints
    # Set up original and prefixed names for constraint and free
    obj@estimation.used.constraints  <- TRUE

    obj@names.original.params.constr <- names.cov.constr
    obj@names.prefixed.params.constr <- paste("constr", obj@names.original.params.constr,      sep = ".")

    original.free.life  <- unique(setdiff(clv.data.get.names.cov.life(obj@clv.data),  obj@names.original.params.constr)) #unique not needed but be sure
    original.free.trans <- unique(setdiff(clv.data.get.names.cov.trans(obj@clv.data), obj@names.original.params.constr))

    if(length(original.free.life)>0){
      obj@names.original.params.free.life  <- original.free.life
      obj@names.prefixed.params.free.life  <- paste("life",   obj@names.original.params.free.life,   sep = ".")
    }else{
      obj@names.original.params.free.life  <- character(0)
      obj@names.prefixed.params.free.life  <- character(0)
    }

    if(length(original.free.trans)>0){
      obj@names.original.params.free.trans <- original.free.trans
      obj@names.prefixed.params.free.trans <- paste("trans",  obj@names.original.params.free.trans,  sep = ".")
    }else{
      obj@names.original.params.free.trans <- character(0)
      obj@names.prefixed.params.free.trans <- character(0)
    }

  }else{
    # No constraints used
    obj@estimation.used.constraints       <- FALSE

    obj@names.original.params.free.life   <- clv.data.get.names.cov.life(obj@clv.data)
    obj@names.original.params.free.trans  <- clv.data.get.names.cov.trans(obj@clv.data)
    obj@names.original.params.constr      <- character(0)

    obj@names.prefixed.params.free.life   <- paste("life",  obj@names.original.params.free.life,  sep = ".")
    obj@names.prefixed.params.free.trans  <- paste("trans", obj@names.original.params.free.trans, sep = ".")
    obj@names.prefixed.params.constr      <- character(0)
  }

  # independent of using constraints or not. These are used in LL and Reg interlayer after the
  #   potential constraint interlayer and therefore need to contain all names
  obj@names.prefixed.params.after.constr.life  <- paste("life",  clv.data.get.names.cov.life(obj@clv.data),  sep=".")
  obj@names.prefixed.params.after.constr.trans <- paste("trans", clv.data.get.names.cov.trans(obj@clv.data), sep=".")

  return(obj)
})
