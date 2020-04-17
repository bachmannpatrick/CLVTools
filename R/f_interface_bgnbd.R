#' @name bgnbd
#' @title BG/NBD models - Work In Progress
#' @description Fits BG/NBD models on transactional data with static and without covariates.
NULL

#' @exportMethod bgnbd
setGeneric("bgnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("bgnbd"))



#' @include class_clv_data.R
#' @rdname bgnbd
setMethod("bgnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                    start.params.model=c(),
                                                                                    optimx.args=list(),
                                                                                    verbose=TRUE,...){
  stop("This model has not yet been implemented!")
})

#' @include class_clv_data_staticcovariates.R
#' @rdname bgnbd
setMethod("bgnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
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
#' @rdname bgnbd
setMethod("bgnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                       start.params.model=c(),
                                                                                                       optimx.args=list(),
                                                                                                       verbose=TRUE,
                                                                                                       names.cov.life=c(), names.cov.trans=c(),
                                                                                                       start.params.life=c(), start.params.trans=c(),
                                                                                                       names.cov.constr=c(),start.params.constr=c(),
                                                                                                       reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

