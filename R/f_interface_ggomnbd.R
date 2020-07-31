#' @exportMethod ggomnbd
setGeneric("ggomnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("ggomnbd"))


#' @name ggomnbd
#' @aliases ggomnbd,clv.data.dynamic.covariates-method
#'
#' @title Gamma-Gompertz/NBD model
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
#' All model start parameters are required to be > 0. If no start values are given for the covariate parameters,
#' 0.1 is used.
#'
#' Note that the DERT expression has not been derived (yet) and it consequently is not possible to calculated
#' values for DERT and CLV.
#'
#' \subsection{The Gamma-Gompertz/NBD model}{
#' There are two key differences of the gamma/Gompertz/NBD (GGom/NBD) model compared to the relative to the well-known Pareto/NBD
#' model: (i) its probability density function can exhibit a mode at zero or an interior mode, and (ii) it can be skewed
#' to the right or to the left. Therefore, the GGom/NBD model is more flexible than the Pareto/NBD model.
#' According to Bemmaor and Glady (2012) can indicate substantial differences in expected residual lifetimes compared to the Pareto/NBD.
#' The GGom/NBD tends to be appropriate when firms are reputed and their offerings are differentiated.
#' }
#'
#' @return Depending on the data object on which the model was fit, \code{ggomnbd} returns either an object of
#' class \linkS4class{clv.ggomnbd} or \linkS4class{clv.ggomnbd.static.cov}.
#'
#' @template template_clvfitted_returnvalue
#'
#' @template template_clvfittedtransactions_seealso
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
  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.ggomnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose=verbose))
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
  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.ggomnbd.static(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas))
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

