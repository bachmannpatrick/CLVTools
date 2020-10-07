# . clv.controlflow.estimate.check.inputs ------------------------------------------------------------------------------
setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted,  start.params.model, optimx.args, verbose, # clv.fitted input args
                                                                                                                                                     names.cov.life, names.cov.trans,
                                                                                                                                                     start.params.life, start.params.trans,
                                                                                                                                                     reg.lambdas,
                                                                                                                                                     names.cov.constr, start.params.constr, cl, ...){

  # check input for clv.fitted.transactions
  callNextMethod()

  # Additional covariates input args checks
  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_namescov_reduce(names.cov = names.cov.life,  data.cov.dt = clv.fitted@clv.data@data.cov.life,  name.of.cov = "Lifetime"))
  err.msg <- c(err.msg, check_user_data_namescov_reduce(names.cov = names.cov.trans, data.cov.dt = clv.fitted@clv.data@data.cov.trans, name.of.cov = "Transaction"))
  check_err_msg(err.msg)

  # Get names as needed for startparams
  #   no name was provided / NULL:  use all covariate names
  #   some name provided:           use this name(s)
  if(length(names.cov.life)==0)
    names.cov.life  <- clv.data.get.names.cov.life(clv.fitted@clv.data)
  if(length(names.cov.trans)==0)
    names.cov.trans <- clv.data.get.names.cov.trans(clv.fitted@clv.data)

  # Check first - if names are wrong they will be wrong as in put to check_startparams as well
  err.msg <- c(err.msg, check_user_data_startparamscov(start.params.cov=start.params.life,  names.params.cov = names.cov.life,  name.of.cov="Lifetime"))
  err.msg <- c(err.msg, check_user_data_startparamscov(start.params.cov=start.params.trans, names.params.cov = names.cov.trans, name.of.cov="Transaction"))

  # Check reg lambdas
  err.msg <- c(err.msg, check_user_data_reglambdas(reg.lambdas=reg.lambdas))

  # Check constraint params input
  err.msg <- c(err.msg, check_user_data_namesconstr(clv.fitted=clv.fitted, names.cov.constr = names.cov.constr))
  check_err_msg(err.msg) # check because names are needed for start params
  err.msg <- c(err.msg, check_user_data_startparamconstr(start.params.constr = start.params.constr, names.cov.constr = names.cov.constr))
  check_err_msg(err.msg)

  # if any param is constraint, it may not be given as start parameters in the free cov params
  if(length(names.cov.constr)>0){
    if(any(names.cov.constr %in% names(start.params.life)))
      err.msg <- c("There may be no start parameters for the constraint parameters in start.params.life")
    if(any(names.cov.constr %in% names(start.params.trans)))
      err.msg <- c("There may be no start parameters for the constraint parameters in start.params.trans")
  }
  check_err_msg(err.msg)

  # Do not warn if there are additional args in ... as they can be for a model
  #   without ... in clv.fitted.transactions.static.cov estimate not possible
})

# . clv.controlflow.estimate.put.inputs ------------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod("clv.controlflow.estimate.put.inputs", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, verbose, reg.lambdas, names.cov.constr, names.cov.life, names.cov.trans, ...){

  # clv.fitted put inputs
  clv.fitted <- callNextMethod()

  # Reduce to user's desired covariates only -----------------------------------------------------------
  clv.fitted@clv.data <- clv.data.reduce.covariates(clv.data=clv.fitted@clv.data, names.cov.life=names.cov.life,
                                                    names.cov.trans=names.cov.trans)

  # is regularization used? ---------------------------------------------------------------------------
  #   Yes:  Indicate and store lambdas
  #   No:   Indicate

  if(!is.null(reg.lambdas) & !all(reg.lambdas == 0)){
    # Regularization: Store
    clv.fitted@estimation.used.regularization <- TRUE
    clv.fitted@reg.lambda.life  <- reg.lambdas[["life"]]
    clv.fitted@reg.lambda.trans <- reg.lambdas[["trans"]]
  }else{
    # No regularization
    clv.fitted@estimation.used.regularization <- FALSE
    clv.fitted@reg.lambda.life  <- numeric(0)
    clv.fitted@reg.lambda.trans <- numeric(0)
  }

  # Are some parameters constraint? --------------------------------------------------------------------
  if(!is.null(names.cov.constr)){
    # Using constraints
    # Set up original and prefixed names for constraint and free
    clv.fitted@estimation.used.constraints  <- TRUE

    clv.fitted@names.original.params.constr <- names.cov.constr
    clv.fitted@names.prefixed.params.constr <- paste("constr", clv.fitted@names.original.params.constr,      sep = ".")

    original.free.life  <- unique(setdiff(clv.data.get.names.cov.life(clv.fitted@clv.data),  clv.fitted@names.original.params.constr)) #unique not needed but be sure
    original.free.trans <- unique(setdiff(clv.data.get.names.cov.trans(clv.fitted@clv.data), clv.fitted@names.original.params.constr))

    if(length(original.free.life)>0){
      clv.fitted@names.original.params.free.life  <- original.free.life
      clv.fitted@names.prefixed.params.free.life  <- paste("life",   clv.fitted@names.original.params.free.life,   sep = ".")
    }else{
      clv.fitted@names.original.params.free.life  <- character(0)
      clv.fitted@names.prefixed.params.free.life  <- character(0)
    }

    if(length(original.free.trans)>0){
      clv.fitted@names.original.params.free.trans <- original.free.trans
      clv.fitted@names.prefixed.params.free.trans <- paste("trans",  clv.fitted@names.original.params.free.trans,  sep = ".")
    }else{
      clv.fitted@names.original.params.free.trans <- character(0)
      clv.fitted@names.prefixed.params.free.trans <- character(0)
    }

  }else{
    # No constraints used

    clv.fitted@estimation.used.constraints       <- FALSE

    clv.fitted@names.original.params.free.life   <- clv.data.get.names.cov.life(clv.fitted@clv.data)
    clv.fitted@names.original.params.free.trans  <- clv.data.get.names.cov.trans(clv.fitted@clv.data)
    clv.fitted@names.original.params.constr      <- character(0)

    clv.fitted@names.prefixed.params.free.life   <- paste("life",  clv.fitted@names.original.params.free.life,  sep = ".")
    clv.fitted@names.prefixed.params.free.trans  <- paste("trans", clv.fitted@names.original.params.free.trans, sep = ".")
    clv.fitted@names.prefixed.params.constr      <- character(0)
  }

  # independent of using constraints or not. These are used in LL and Reg interlayer after the
  #   potential constraint interlayer has doubled the parameters and therefore need to contain all names
  clv.fitted@names.prefixed.params.after.constr.life  <- paste("life",  clv.data.get.names.cov.life(clv.fitted@clv.data),  sep=".")
  clv.fitted@names.prefixed.params.after.constr.trans <- paste("trans", clv.data.get.names.cov.trans(clv.fitted@clv.data), sep=".")

  return(clv.fitted)
})


# . clv.controlflow.estimate.generate.start.params ------------------------------------------------------------------------------
setMethod("clv.controlflow.estimate.generate.start.params", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"),
          # original signature: clv.fitted, start.params.model
          definition = function(clv.fitted,
                                start.params.model,
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

            # any free params?
            if(length(clv.fitted@names.original.params.free.life) > 0){

              # start params given?
              if(is.null(start.params.life)){
                start.params.free.life        <- rep(clv.fitted@clv.model@start.param.cov, length(clv.fitted@names.original.params.free.life))
                names(start.params.free.life) <- clv.fitted@names.original.params.free.life
              }else{
                # correct order
                start.params.free.life <- start.params.life[clv.fitted@names.original.params.free.life]
              }
              # apply model transformation to start params
              start.params.free.life        <- clv.model.transform.start.params.cov(clv.fitted@clv.model, start.params.free.life)
              names(start.params.free.life) <- clv.fitted@names.prefixed.params.free.life
            }

            # any free params?
            if(length(clv.fitted@names.original.params.free.trans) > 0){
              if(is.null(start.params.trans)){
                start.params.free.trans        <- rep(clv.fitted@clv.model@start.param.cov, length(clv.fitted@names.original.params.free.trans))
                names(start.params.free.trans) <- clv.fitted@names.original.params.free.trans
              }else{
                # correct order
                start.params.free.trans <- start.params.trans[clv.fitted@names.original.params.free.trans]
              }

              # apply model transformation to start params
              start.params.free.trans        <- clv.model.transform.start.params.cov(clv.fitted@clv.model, start.params.free.trans)
              names(start.params.free.trans) <- clv.fitted@names.prefixed.params.free.trans
            }

            # any constrain params?
            if(length(clv.fitted@names.original.params.constr) > 0){
              if(is.null(start.params.constr)){
                # none user given
                start.params.constr        <- rep(clv.fitted@clv.model@start.param.cov, length(clv.fitted@names.original.params.constr))
                names(start.params.constr) <- clv.fitted@names.original.params.constr
              }else{
                start.params.constr <- start.params.constr[clv.fitted@names.original.params.constr] #ensure order
              }
              # apply model transformation to start params
              start.params.constr        <- clv.model.transform.start.params.cov(clv.fitted@clv.model, start.params.constr)
              names(start.params.constr) <- clv.fitted@names.prefixed.params.constr
            }


            # NULL if not needed
            all.cov.params <- c(start.params.free.life, start.params.free.trans, start.params.constr)

            return(c(transformed.start.params.model, all.cov.params))

          })


# . clv.controlflow.estimate.prepare.optimx.args ------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod("clv.controlflow.estimate.prepare.optimx.args", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"),
          def=function(clv.fitted, start.params.all){

            # Call clv.fitted prepare.optimx.args
            prepared.nocov.optimx.args <- callNextMethod()

            # Add covariates interlayer parameters ---------------------------------------------------------------------
            #   keep.null = T, needed so that if reg.lambda or names.original.params.constr params are NULL,
            #                  they are given to optimx/interlayer_manager as well

            # Everything to call the regularization layer
            optimx.args <- modifyList(prepared.nocov.optimx.args,
                                      list(use.interlayer.reg        = clv.fitted@estimation.used.regularization,
                                           names.prefixed.params.after.constr.trans = clv.fitted@names.prefixed.params.after.constr.trans,
                                           names.prefixed.params.after.constr.life  = clv.fitted@names.prefixed.params.after.constr.life,
                                           reg.lambda.life           = clv.fitted@reg.lambda.life,
                                           reg.lambda.trans          = clv.fitted@reg.lambda.trans,
                                           num.observations          = nobs(object = clv.fitted)),
                                      keep.null = TRUE)



            # Everything to call the constraints layer
            optimx.args <- modifyList(optimx.args, list(use.interlayer.constr        = clv.fitted@estimation.used.constraints,
                                                        names.original.params.constr = clv.fitted@names.original.params.constr,
                                                        names.prefixed.params.constr = clv.fitted@names.prefixed.params.constr),
                                      keep.null = TRUE)
            return(optimx.args)
          })

