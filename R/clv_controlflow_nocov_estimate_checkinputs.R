setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(obj="clv.fitted"),
          definition = function(obj,  start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){

  l.args <- list(...)

  # Check only basic structure
  err.msg <- c()
  if(!is.null(start.params.model)) # may be NULL = use model default
    err.msg <- c(err.msg, check_user_data_startparams(start.params = start.params.model,
                                                      vector.names = obj@clv.model@names.original.params.model,
                                                      param.names = "model start parameter"))


  # Check cor input
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name ="verbose"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=use.cor, var.name ="use.cor"))
  err.msg <- c(err.msg, check_user_data_startparamcorm(start.param.cor=start.param.cor, use.cor=use.cor))
  # Check additional optimx args
  err.msg <- c(err.msg, check_user_data_optimxargs(optimx.args=optimx.args))
  check_err_msg(err.msg)

  # warn that this model has no covariates but names/start.params/lambdas/cor/constr were given

  if(length(l.args) > 0){
    if("start.params.life" %in% names(l.args))
      if(length(l.args$start.params.life)>0)
        warning("The start parameters given for covariates will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("start.params.trans" %in% names(l.args))
      if(length(l.args$start.params.trans)>0)
        warning("The start parameters given for covariates will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("names.cov.life" %in% names(l.args))
      if(length(l.args$names.cov.life)>0)
        warning("The covariates names given will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("names.cov.trans" %in% names(l.args))
      if(length(l.args$names.cov.trans)>0)
        warning("The covariates names given will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("reg.lambdas" %in% names(l.args))
      if(length(l.args$reg.lambdas)>0)
        warning("The regularization lambdas will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("use.cor" %in% names(l.args))
      if(is.logical(l.args$use.cor) & !anyNA(l.args$use.cor))
        if(l.args$use.cor == TRUE)
          warning("The use.cor parameter will be ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("start.params.constr" %in% names(l.args))
      if(length(l.args$start.params.constr) > 0)
        warning("The given parameters to constrain and their start parameters are ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)
    if("names.cov.constr" %in% names(l.args))
      if(length(l.args$names.cov.constr) > 0)
        warning("The given parameters to constrain and their start parameters are ignored because this is a no covariate model!", call. = FALSE, immediate. = TRUE)

    # Do not warn if anything else unneeded is passed - could be something for a model
  }
})
