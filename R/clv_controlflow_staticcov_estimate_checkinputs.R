setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(obj="clv.fitted.static.cov"),
          definition = function(obj,  start.params.model, use.cor, start.param.cor, optimx.args, # clv.fitted input args
                                verbose,
                                names.cov.life, names.cov.trans,
                                start.params.life, start.params.trans,
                                reg.lambdas,
                                names.cov.constr, start.params.constr, cl, ...){

  # check clv.fitted input
  #   only forward what would have been passed if nocov model was estimated because in the parent method
  #   it is warned if there are addtional unneded params in ...
  callNextMethod(obj=obj,use.cor=use.cor, start.param.cor=start.param.cor, start.params.model=start.params.model, optimx.args=optimx.args, verbose=verbose)

  # Additional covariates input args checks
  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_namescov_reduce(names.cov = names.cov.life,  data.cov.dt = obj@clv.data@data.cov.life,  name.of.cov = "Lifetime"))
  err.msg <- c(err.msg, check_user_data_namescov_reduce(names.cov = names.cov.trans, data.cov.dt = obj@clv.data@data.cov.trans, name.of.cov = "Transaction"))
  check_err_msg(err.msg)

  # Get names as needed for startparams
  #   no name was provided / NULL:  use all covariate names
  #   some name provided:           use this name(s)
  if(length(names.cov.life)==0)
    names.cov.life  <- clv.data.get.names.cov.life(obj@clv.data)
  if(length(names.cov.trans)==0)
    names.cov.trans <- clv.data.get.names.cov.trans(obj@clv.data)

  # Check first - if names are wrong they will be wrong as in put to check_startparams as well
  err.msg <- c(err.msg, check_user_data_startparamscov(start.params.cov=start.params.life,  names.params.cov = names.cov.life,  name.of.cov="Lifetime"))
  err.msg <- c(err.msg, check_user_data_startparamscov(start.params.cov=start.params.trans, names.params.cov = names.cov.trans, name.of.cov="Transaction"))

  # Check reg lambdas
  err.msg <- c(err.msg, check_user_data_reglambdas(reg.lambdas=reg.lambdas))

  # Check constraint params input
  err.msg <- c(err.msg, check_user_data_namesconstr(obj=obj, names.cov.constr = names.cov.constr))
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
  #   without ... in clv.fitted.static.cov estimate not possible
})
