#' @exportMethod gg
setGeneric("gg", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("gg"))


#' @name gg
#' @title Gamma/Gamma model - Work In Progress
#'
#' @description Fits Gamma/Gamma model on transactional data. Not yet implemented.
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @return No value is returned.
#'
#' @include class_clv_data.R
setMethod("gg", signature = signature(clv.data="clv.data"), def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                                                           optimx.args=list(), verbose=TRUE, ...){
  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.gg(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted = obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})


#' @include class_clv_data_staticcovariates.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                         start.params.model=c(),
                                                                                                         optimx.args=list(),
                                                                                                         verbose=TRUE,
                                                                                                         names.cov.life=c(), names.cov.trans=c(),
                                                                                                         start.params.life=c(), start.params.trans=c(),
                                                                                                         names.cov.constr=c(), start.params.constr=c(),
                                                                                                         reg.lambdas = c(), ...){
  stop("This model has not yet been implemented!")
})



#' @include class_clv_data_dynamiccovariates.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                          start.params.model=c(),
                                                                                                          optimx.args=list(),
                                                                                                          verbose=TRUE,
                                                                                                          names.cov.life=c(), names.cov.trans=c(),
                                                                                                          start.params.life=c(), start.params.trans=c(),
                                                                                                          names.cov.constr=c(),start.params.constr=c(),
                                                                                                          reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

