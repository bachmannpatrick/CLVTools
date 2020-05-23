#' @exportMethod gg
setGeneric("gg", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("gg"))


#' @name gg
#' @title Gamma/Gamma model - Work In Progress
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description Fits Gamma/Gamma model on transactional data. Not yet implemented.
#'
#' @param use.cor (Not yet supported) Whether the correlation between the transaction and lifetime process should be estimated.
#' @param start.param.cor (Not yet supported) Start parameter for the optimization of the correlation.
#'
#' @details If no start parameters are given, p = 1, q = 1, gamma = 1 is used.
#' All model start parameters are required to be > 0.
#'
#' @return No value is returned.
#' @aliases gg gg,clv.data-method
#' @include class_clv_data.R class_clv_model_gg.R
setMethod("gg", signature = signature(clv.data="clv.data"), def = function(clv.data,
                                                                           start.params.model=c(),
                                                                           use.cor = FALSE,
                                                                           start.param.cor=c(),
                                                                           optimx.args=list(),
                                                                           verbose=TRUE,...){
  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.gg(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted = obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})


#' @include class_clv_data_staticcovariates.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                    start.params.model=c(),
                                                                                                    use.cor = FALSE,
                                                                                                    start.param.cor=c(),
                                                                                                    optimx.args=list(),
                                                                                                    verbose=TRUE,
                                                                                                    names.cov.life=c(), names.cov.trans=c(),
                                                                                                    start.params.life=c(), start.params.trans=c(),
                                                                                                    names.cov.constr=c(),start.params.constr=c(),
                                                                                                    reg.lambdas = c(), ...){
  stop("This model has not yet been implemented!")
})



#' @include class_clv_data_dynamiccovariates.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                     start.params.model=c(),
                                                                                                     use.cor = FALSE,
                                                                                                     start.param.cor=c(),
                                                                                                     optimx.args=list(),
                                                                                                     verbose=TRUE,
                                                                                                     ...){
  stop("This model cannot be fitted on this type of data!")
})

