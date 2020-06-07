#' @exportMethod ggomnbd
setGeneric("ggomnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("ggomnbd"))


#' @name ggomnbd
#' @aliases ggomnbd,clv.data.dynamic.covariates-method
#'
#' @title Gamma-Gompertz/NBD models
#'
#' @description Fits Gamma-Gompertz/NBD models on transactional data with static and without covariates.
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @template template_details_paramsggomnbd
#'
#' @details If no start parameters are given, r = 1, alpha = 1, beta = 1, b = 1, s = 1 is used.
#' The model start parameters are required to be > 0.
#'
#' \subsection{The GGompertz / NBD model}{
#' **TODO
#' }
#'
#' @return
#' \code{ggomnbd} returns an object of
#' class \code{clv.ggomnbd}.
#'
#' The function \code{\link[CLVTools:summary.clv.fitted]{summary}} can be used to obtain and print a summary of the results.
#' The generic accessor functions \code{coefficients}, \code{fitted},
#' \code{residuals}, \code{vcov}, \code{logLik}, \code{AIC}, \code{BIC}, and \code{nobs} are available.
#'
#' @seealso \code{\link[CLVTools:clvdata]{clvdata}} to create a clv data object
#' @seealso \code{\link[CLVTools:predict.clv.fitted]{predict}} to predict expected transactions, probability of being alive, and customer lifetime value for every customer
#' @seealso \code{\link[CLVTools:plot.clv.fitted]{plot}} to plot the unconditional expectation as predicted by the fitted model
#' @seealso The generic functions \code{\link[CLVTools:summary.clv.fitted]{summary}} and \code{\link[CLVTools:fitted.clv.fitted]{fitted}}.
#'
#' @template template_references_ggomnbd
#'
#' @templateVar vec_startparams_model c(r=0.5, alpha=15, b=5, beta=10, s=0.5)
#' @templateVar name_model_short ggomnbd
#' @template template_examples_nocovmodelinterface
#' @templateVar name_model_short ggomnbd
#' @template template_examples_staticcovmodelinterface
NULL


#' @include class_clv_data.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                       start.params.model=c(),
                                                                                       optimx.args=list(),
                                                                                       verbose=TRUE,...){
  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.ggomnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model, use.cor = FALSE,
                                           start.param.cor = c(), optimx.args = optimx.args, verbose=verbose, ...))
})


#' @rdname ggomnbd
#' @include class_clv_data_staticcovariates.R
setMethod("ggomnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                         start.params.model=c(),
                                                                                                         optimx.args=list(),
                                                                                                         verbose=TRUE,
                                                                                                         names.cov.life=c(), names.cov.trans=c(),
                                                                                                         start.params.life=c(), start.params.trans=c(),
                                                                                                         names.cov.constr=c(), start.params.constr=c(),
                                                                                                         reg.lambdas = c(), ...){
  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.ggomnbd.static(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model,
                                           use.cor = FALSE,
                                           start.param.cor = c(),
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})



#' @include class_clv_data_dynamiccovariates.R
#' @keywords internal
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

