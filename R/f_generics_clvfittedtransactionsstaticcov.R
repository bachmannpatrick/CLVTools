# . clv.controlflow.predict.set.prediction.params ------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted){
  # Get no cov model params
  clv.fitted <- callNextMethod()

  # Set life and trans params
  #   All params:         Re-name to original names to match name of data
  #   Constraint params:  The prediction params for both processes need to contain the constraint params (in original names)
  #   Ordering:           Ensure same ordering as data

  # Covariate params in coef() are always prefixed to distinguish them per process

  # This could be achieved much simpler by relying on character(0) in param names but be explicit for clarity
  if(clv.fitted@estimation.used.constraints){

    named.params.constr <- setNames(coef(clv.fitted)[clv.fitted@names.prefixed.params.constr],
                                    clv.fitted@names.original.params.constr)

    # concatenate with free params, if there are any.
    #   Else only from constr params
    if(length(clv.fitted@names.prefixed.params.free.life)>0){
      prediction.params.life <- c(setNames(coef(clv.fitted)[clv.fitted@names.prefixed.params.free.life],
                                           clv.fitted@names.original.params.free.life),
                                  named.params.constr)
    }else{
      prediction.params.life <- named.params.constr # no free in lifetime, only constr
    }

    if(length(clv.fitted@names.prefixed.params.free.trans)>0){
      prediction.params.trans <- c(setNames(coef(clv.fitted)[clv.fitted@names.prefixed.params.free.trans],
                                            clv.fitted@names.original.params.free.trans),
                                   named.params.constr)
    }else{
      prediction.params.trans <- named.params.constr # no free in transaction, only constr
    }

  }else{
    # no constraints is simple
    prediction.params.life  <- setNames(coef(clv.fitted)[clv.fitted@names.prefixed.params.free.life],  clv.fitted@names.original.params.free.life)
    prediction.params.trans <- setNames(coef(clv.fitted)[clv.fitted@names.prefixed.params.free.trans], clv.fitted@names.original.params.free.trans)
  }

  # Order like data
  prediction.params.life  <- prediction.params.life[clv.data.get.names.cov.life(clv.fitted@clv.data)]
  prediction.params.trans <- prediction.params.trans[clv.data.get.names.cov.trans(clv.fitted@clv.data)]

  clv.fitted@prediction.params.life  <- prediction.params.life
  clv.fitted@prediction.params.trans <- prediction.params.trans

  return(clv.fitted)
})


# . clv.controlflow.check.prediction.params -----------------------------------------------------------------
setMethod("clv.controlflow.check.prediction.params", signature = signature(clv.fitted = "clv.fitted.transactions.static.cov"), function(clv.fitted){
  # Check model prediction params
  callNextMethod()

  # Do not check coef() because correlation coef may be NA and can still predict
  if(anyNA(clv.fitted@prediction.params.life) | anyNA(clv.fitted@prediction.params.trans)){
    check_err_msg("Cannot proceed because there are NAs in the estimated covariate coefficients!")
  }
})


# . clv.controlflow.check.newdata ------------------------------------------------------------------------------
setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.transactions.static.cov"), definition = function(clv.fitted, user.newdata, prediction.end, ...){
  # Do nocov newdata checks first
  #   newdata fulfills all basic properties after this
  callNextMethod()

  err.msg <- c()

  # Check that it does have the same covariates as the ones used for fitting
  #   nocov already checked that is of correct class
  if(!setequal(user.newdata@names.cov.data.life, clv.fitted@clv.data@names.cov.data.life))
    err.msg <- c(err.msg, "Not all Lifetime covariates used for fitting are present in the 'newdata' object!")

  if(!setequal(user.newdata@names.cov.data.trans, clv.fitted@clv.data@names.cov.data.trans))
    err.msg <- c(err.msg, "Not all Transaction covariates used for fitting are present in the 'newdata' object!")

  check_err_msg(err.msg)
})
