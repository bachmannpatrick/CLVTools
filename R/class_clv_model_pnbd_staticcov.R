# Class --------------------------------------------------------------------------------------------------------------------------------
#' @include class_clv_model_basestrategy.R class_clv_model_pnbd_nocov.R
setClass(Class = "clv.model.pnbd.static.cov", contains = "clv.model.pnbd.no.cov",
         slots = list(start.param.cov = "numeric"),
         prototype = list(start.param.cov = 1,
                          # New model defaults
                          optimx.defaults  = list(method = "L-BFGS-B",
                                                  itnmax = 3000),
                          name.model       = "Pareto NBD with Static Covariates"))


# Methods --------------------------------------------------------------------------------------------------------------------------------

#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition =
            function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose,
                                names.cov.life, names.cov.trans,
                                start.params.life, start.params.trans,
                                names.cov.constr,start.params.constr,
                                reg.lambdas, ...){

  # Check start.params.model in pnbd.no.cov function
  #   but with no cov specific inputs only
  callNextMethod(clv.model=clv.model, clv.fitted=clv.fitted, start.params.model=start.params.model, use.cor=use.cor,
                 start.param.cor=start.param.cor, optimx.args=optimx.args, verbose=verbose)

  if(length(list(...)) > 0)
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)

  # Nothing model-specific to check about all other user inputs
  # Nothing to return
})

#   Use pnbd.no.cov methods, dont need to overwrite
# setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, clv.fitted, ...){
#   return(callNextMethod())
# })

setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, start.params.cov){
  # no transformation needed
  return(start.params.cov)
})

setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, prefixed.params.cov){
  # no transformation needed
  return(prefixed.params.cov)
})



setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.pnbd.static.cov"),
  definition = function(clv.model, clv.fitted, prepared.optimx.args,...){

  # Do not call the no.cov function as the LL is different

  # Everything to call the LL function
  optimx.args <- modifyList(prepared.optimx.args,
                            list(
                              obj = clv.fitted,
                              LL.function.sum = pnbd_staticcov_LL_sum,
                              LL.function.ind = pnbd_staticcov_LL_ind, # if doing correlation

                              # For cpp static cov the param order is: model, life, trans
                              LL.params.names.ordered = c(clv.model@names.prefixed.params.model,
                                                          clv.fitted@names.prefixed.params.after.constr.life,
                                                          clv.fitted@names.prefixed.params.after.constr.trans),
                              vX      = clv.fitted@cbs$x,
                              vT_x    = clv.fitted@cbs$t.x,
                              vT_cal  = clv.fitted@cbs$T.cal,
                              # Covariate data as matrix!
                              mCov_life  = clv.data.get.matrix.data.cov.life(clv.fitted@clv.data),
                              mCov_trans = clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data)))
  return(optimx.args)
})



setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.pnbd.static.cov"),
definition = function(clv.model, clv.fitted, prefixed.params){
  # Get corrections from nocov model
  m.diag.model <- callNextMethod()

  # No transformations for static covs: Set diag to 1 for all static cov params

  # Gather names of cov param
  names.cov.prefixed.params  <- c(clv.fitted@names.prefixed.params.free.life,
                                  clv.fitted@names.prefixed.params.free.trans)
  if(clv.fitted@estimation.used.constraints)
    names.cov.prefixed.params <- c(names.cov.prefixed.params, clv.fitted@names.prefixed.params.constr)

  # Set to 1
  m.diag.model[names.cov.prefixed.params,
               names.cov.prefixed.params] <- diag(x = 1,
                                                  nrow = length(names.cov.prefixed.params),
                                                  ncol = length(names.cov.prefixed.params))
  return(m.diag.model)
})
