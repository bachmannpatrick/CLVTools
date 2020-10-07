#' @exportMethod bgnbd
setGeneric("bgnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("bgnbd"))



#' @name bgnbd
#' @aliases bgnbd,clv.data.dynamic.covariates-method
#'
#' @title BG/NBD models
#'
#' @template template_params_estimate
#' @template template_param_verbose
#' @template template_params_estimate_cov
#' @template template_param_dots
#'
#'
#' @description
#' Fits BG/NBD models on transactional data without and with static covariates.
#'
#'
#' @template template_details_paramsbgnbd
#'
#' @details If no start parameters are given, r = 1, alpha = 3, a = 1, b = 3 is used.
#' All model start parameters are required to be > 0. If no start values are given for the
#' covariate parameters, 0.1 is used.
#'
#' Note that the DERT expression has not been derived (yet) and it consequently is not possible to calculated
#' values for DERT and CLV.
#'
#'
#'
#' \subsection{The BG/NBD model}{
#' The BG/NBD is an "easy" alternative to the Pareto/NBD model that is easier to implement. The BG/NBD model slight adapts
#' the behavioral "story" associated with the Pareto/NBD model in order to simplify the implementation. The BG/NBD model uses a beta-geometric and
#' exponential gamma mixture distributions to model customer behavior. The key difference to the Pareto/NBD model is that a customer can only
#' churn right after a transaction. This simplifies computations significantly, however has the drawback that a customer cannot churn until he/she
#' makes a transaction. The Pareto/NBD model assumes that a customer can churn at any time.
#' }
#'
#' \subsection{BG/NBD model with static covariates}{
#' The standard BG/NBD model captures heterogeneity was solely using Gamma distributions.
#' However, often exogenous knowledge, such as for example customer demographics, is available.
#' The supplementary knowledge may explain part of the heterogeneity among the customers and
#' therefore increase the predictive accuracy of the model. In addition, we can rely on these
#' parameter estimates for inference, i.e. identify and quantify effects of contextual factors
#' on the two underlying purchase and attrition processes. For technical details we refer to
#' the technical note by Fader and Hardie (2007).
#'
#' The likelihood function is the likelihood function associated with the basic model where
#' alpha, a, and b are replaced with alpha = alpha0*exp(-g1z1), a = a_0*exp(g2z2), and b = b0*exp(g3z2)
#' while r remains unchanged. Note that in the current implementation, we constrain the covariate parameters
#' and data for the lifetime process to be equal (g2=g3 and z2=z3).
#' }
#'
#' @return Depending on the data object on which the model was fit, \code{bgnbd} returns either an object of
#' class \linkS4class{clv.bgnbd} or \linkS4class{clv.bgnbd.static.cov}.
#'
#' @template template_clvfitted_returnvalue
#'
#' @template template_clvfittedtransactions_seealso
#'
#' @template template_references_bgnbd
#'
#' @templateVar name_model_short bgnbd
#' @templateVar vec_startparams_model c(r=0.5, alpha=15, a = 2, b=5)
#' @template template_examples_nocovmodelinterface
#' @templateVar name_model_short bgnbd
#' @template template_examples_staticcovmodelinterface
NULL


#' @rdname bgnbd
#' @include class_clv_data.R
setMethod("bgnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                     start.params.model=c(),
                                                                                     optimx.args=list(),
                                                                                     verbose=TRUE,...){

  check_err_msg(check_user_data_emptyellipsis(...))

  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.bgnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose=verbose))
})

#' @rdname bgnbd
#' @include class_clv_data_staticcovariates.R
setMethod("bgnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                       start.params.model=c(),
                                                                                                       optimx.args=list(),
                                                                                                       verbose=TRUE,
                                                                                                       names.cov.life=c(), names.cov.trans=c(),
                                                                                                       start.params.life=c(), start.params.trans=c(),
                                                                                                       names.cov.constr=c(),start.params.constr=c(),
                                                                                                       reg.lambdas = c(), ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.bgnbd.static.cov(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas))
})


#' @include class_clv_data_dynamiccovariates.R
#' @keywords internal
setMethod("bgnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        ...){
  stop("This model cannot be fitted on this type of data!")
})
