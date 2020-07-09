#' CLV Model without support for life-trans correlation
#'
#' @description
#' This class serves as a parent class to other clv.model classes that cannot be fit with correlation between the
#' lifetime and transaction process.
#'
#' @seealso Parent class \linkS4class{clv.model}
#' @seealso CLV model subclasses  \linkS4class{clv.model.bgnbd.no.cov}, \linkS4class{clv.model.ggomnbd.no.cov}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include all_generics.R
setClass(Class = "clv.model.no.correlation", contains = c("clv.model", "VIRTUAL"))


setMethod("clv.model.supports.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

setMethod("clv.model.estimation.used.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

# Nothing to store additionally
setMethod("clv.model.put.estimation.input", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model, ...){
  return(clv.model)
})

setMethod(f = "clv.model.generate.start.param.cor", signature = signature(clv.model="clv.model.no.correlation"), definition = function(clv.model, start.param.cor, transformed.start.params.model){
  return(transformed.start.params.model)
})


setMethod(f = "clv.model.coef.add.correlation", signature = signature(clv.model="clv.model.no.correlation"), definition = function(clv.model, last.row.optimx.coef, original.scale.params){
  return(original.scale.params)
})
