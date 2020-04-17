#' @name gnbd
#' @title GG/NBD models - Work In Progress
#' @description Fits GG/NBD models on transactional data with static and without covariates.
NULL

#' @exportMethod gnbd
setGeneric("gnbd", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("gnbd"))



#' @include class_clv_data.R
#' @rdname gnbd
setMethod("gnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                     start.params.model=c(),
                                                                                     optimx.args=list(),
                                                                                     verbose=TRUE,...){
  stop("This model has not yet been implemented!")
})


#' @include class_clv_data_staticcovariates.R
#' @rdname gnbd
setMethod("gnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
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
#' @rdname gnbd
setMethod("gnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        names.cov.life=c(), names.cov.trans=c(),
                                                                                                        start.params.life=c(), start.params.trans=c(),
                                                                                                        names.cov.constr=c(),start.params.constr=c(),
                                                                                                        reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

