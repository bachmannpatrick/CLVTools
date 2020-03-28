# Entry point called by optimx.
#   Puts together all interlayers needed depending on the specified parameters
#
#
# Interlayers are put together depending on the parameters given.
# No additional input checks are performed on any given parameter
#
# @param ... All other arguments to be given to the LL function
#
# LL.params needs to be the first argument as it will receive the parameters from the optimizer
#' @importFrom utils modifyList
interlayer_manager <- function(LL.params, LL.function.sum,
                               use.interlayer.constr,
                               use.interlayer.reg, reg.lambda.trans, reg.lambda.life,
                               use.cor,
                               ...){

  all.other.args <- list(...)

  # Put together the interlayers -----------------------------------------------
  #   Depends on the parameters given
  #     Content: use interlayer
  #     NULL: dont use interlayer
  #
  #   Interlayers will be called recursively, using the
  #     one on top (first position) as next.
  #
  #   Last interlayer to call therefore always is _callLL()
  #
  #   If some parameters are constraint, they need to be the very first,
  #     as this interlayer only generates the parameters which will then be
  #       used by the other interlayers
  #     if covs were constraint, the names for all params are needed per
  #       process in _callLL and regularization interlayer. These are generated
  #       here from the original cov data names

  # Add callLL as first and append all other interlayers before that
  #   (callLL will be called as last)
  interlayers.to.use <- list("callLL"=interlayer_callLL)

  # Add correlation interlayer if needed
  if(use.cor == T & !anyNA(use.cor))
    interlayers.to.use <- c("correlation"=interlayer_correlation, interlayers.to.use)

  # Add regularization interlayer if needed
  if(use.interlayer.reg == T &
     !is.null(reg.lambda.trans) & !anyNA(reg.lambda.trans) &
     !is.null(reg.lambda.life)  & !anyNA(reg.lambda.life)){
       interlayers.to.use <- c("regularization"=interlayer_regularization, interlayers.to.use)
  }

  # Add constraint infront of all other interlayers, if needed
  if(use.interlayer.constr == TRUE){
      interlayers.to.use <- c("constraints"=interlayer_constraints, interlayers.to.use)
  }


  # Start calling the interlayers ----------------------------------------------
  #   Use do call to integrate ... args
  interlayer.call.args <- list(next.interlayers = interlayers.to.use,
                               LL.function.sum = LL.function.sum,
                               LL.params   = LL.params,
                               reg.lambda.life  = reg.lambda.life,
                               reg.lambda.trans = reg.lambda.trans)

  interlayer.call.args <- modifyList(interlayer.call.args,
                                     all.other.args)

  return(do.call(what = interlayer_callnextinterlayer, args = interlayer.call.args))
}
