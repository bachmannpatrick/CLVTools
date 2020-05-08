#' @name bgbb
#' @title BG/BB models - Work In Progress
#'
#' @description Fits BG/BB models on transactional data with static and without covariates. Not yet implemented.
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @return No value is returned.
#'
NULL

#' @exportMethod bgbb
setGeneric("bgbb", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("bgbb"))



#' @include class_clv_data.R
#' @rdname bgbb
setMethod("bgbb", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                    start.params.model=c(),
                                                                                    optimx.args=list(),
                                                                                    verbose=TRUE,...){
  stop("This model has not yet been implemented!")
})


#' @include class_clv_data_staticcovariates.R
#' @rdname bgbb
setMethod("bgbb", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
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
#' @rdname bgbb
setMethod("bgbb", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                       start.params.model=c(),
                                                                                                       optimx.args=list(),
                                                                                                       verbose=TRUE,
                                                                                                       names.cov.life=c(), names.cov.trans=c(),
                                                                                                       start.params.life=c(), start.params.trans=c(),
                                                                                                       names.cov.constr=c(),start.params.constr=c(),
                                                                                                       reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

