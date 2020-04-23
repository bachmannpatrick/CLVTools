#' @name ggomnbd
#' @title Gamma-Gompertz/NBD models - Work In Progress
#'
#' @description Fits Gamma-Gompertz/NBD models on transactional data with static and without covariates.
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
NULL

#' @exportMethod ggomnbd
setGeneric("ggomnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("ggomnbd"))



#' @include class_clv_data.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                     start.params.model=c(),
                                                                                     optimx.args=list(),
                                                                                     verbose=TRUE,...){
  cl        <- sys.call(1)
  obj <- clv.ggomnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(obj=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})


#' @include class_clv_data_staticcovariates.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                       start.params.model=c(),
                                                                                                       optimx.args=list(),
                                                                                                       verbose=TRUE,
                                                                                                       names.cov.life=c(), names.cov.trans=c(),
                                                                                                       start.params.life=c(), start.params.trans=c(),
                                                                                                       names.cov.constr=c(), start.params.constr=c(),
                                                                                                       reg.lambdas = c(), ...){
  cl        <- sys.call(1)
  obj <- clv.ggomnbd.static.cov(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(obj=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})



#' @include class_clv_data_dynamiccovariates.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        names.cov.life=c(), names.cov.trans=c(),
                                                                                                        start.params.life=c(), start.params.trans=c(),
                                                                                                        names.cov.constr=c(),start.params.constr=c(),
                                                                                                        reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

