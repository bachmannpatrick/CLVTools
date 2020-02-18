setMethod("clv.controlflow.estimate.prepare.optimx.args", signature = signature(obj="clv.fitted.static.cov"),
  def=function(obj, start.params.all){

  # Call clv.fitted prepare.optimx.args
  prepared.nocov.optimx.args <- callNextMethod()

  # Add covariates interlayer parameters ---------------------------------------------------------------------
  #   keep.null =T, needed so that if reg.lambda or names.original.params.constr params are NULL,
  #                 they are given to optimx/interlayer_manager as well

  # Everything to call the regularization layer
  optimx.args <- modifyList(prepared.nocov.optimx.args,
                            list(use.interlayer.reg        = obj@estimation.used.regularization,
                                 names.prefixed.params.after.constr.trans = obj@names.prefixed.params.after.constr.trans,
                                 names.prefixed.params.after.constr.life  = obj@names.prefixed.params.after.constr.life,
                                 reg.lambda.life           = obj@reg.lambda.life,
                                 reg.lambda.trans          = obj@reg.lambda.trans,
                                 num.observations          = nobs(object = obj)),
                            keep.null = TRUE)



  # Everything to call the constraints layer
  optimx.args <- modifyList(optimx.args, list(use.interlayer.constr        = obj@estimation.used.constraints,
                                              names.original.params.constr = obj@names.original.params.constr,
                                              names.prefixed.params.constr = obj@names.prefixed.params.constr),
                            keep.null = TRUE)
  return(optimx.args)
})
