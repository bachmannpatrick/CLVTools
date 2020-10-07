#' CLV Model providing life-trans correlation related functionalities
#'
#' @description
#' This class serves as a parent class to other clv.model classes and provides the functionality needed
#' to fit a model with correlation between the lifetime and transaction process.
#'
#' @slot estimation.used.correlation Single boolean whether the correlation was estimated.
#' @slot name.prefixed.cor.param.m Single character vector of the internal name used for the correlation parameter during optimization.
#' @slot name.correlation.cor Single character vector of the external name used for the correlation parameter.
#'
#' @seealso Parent class \linkS4class{clv.model}
#' @seealso CLV model subclasses  \linkS4class{clv.model.pnbd.no.cov}, \linkS4class{clv.model.pnbd.static.cov}, \linkS4class{clv.model.pnbd.dynamic.cov}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include all_generics.R
setClass(Class = "clv.model.with.correlation", contains = c("clv.model", "VIRTUAL"),
         slots = list(
           estimation.used.correlation  = "logical",
           name.prefixed.cor.param.m    = "character",
           name.correlation.cor         = "character"),

         # Have to use prototype because cannot instantiate (ie constructor function) a virtual class
         prototype = list(
           estimation.used.correlation = logical(0), # not known yet
           name.prefixed.cor.param.m   = "correlation.param.m",
           name.correlation.cor        = "Cor(life,trans)"))


setMethod("clv.model.supports.correlation", signature = signature(clv.model="clv.model.with.correlation"), def = function(clv.model){
  return(TRUE)
})

setMethod("clv.model.estimation.used.correlation", signature = signature(clv.model="clv.model.with.correlation"), def = function(clv.model){
  return(clv.model@estimation.used.correlation)
})


setMethod("clv.model.put.estimation.input", signature = signature(clv.model="clv.model.with.correlation"), def = function(clv.model, use.cor, ...){

  # Should correlation be calculated? -----------------------------------------------------------------
  if(use.cor){
    clv.model@estimation.used.correlation <- TRUE
  }else{
    clv.model@estimation.used.correlation <- FALSE
  }

  return(clv.model)
})


setMethod(f = "clv.model.coef.add.correlation", signature = signature(clv.model="clv.model.with.correlation"), definition = function(clv.model, last.row.optimx.coef, original.scale.params){
  if(clv.model@estimation.used.correlation){
    prefixed.params.model  <- last.row.optimx.coef[1, clv.model@names.prefixed.params.model, drop=TRUE]
    param.m                <- last.row.optimx.coef[1, clv.model@name.prefixed.cor.param.m,   drop=TRUE]
    param.cor              <- clv.model.m.to.cor(clv.model = clv.model,
                                                 prefixed.params.model = prefixed.params.model,
                                                 param.m = param.m)
    names(param.cor)       <- clv.model@name.correlation.cor
    original.scale.params  <- c(original.scale.params, param.cor)
  }
  return(original.scale.params)
})

# function(clv.model, start.param.cor, transformed.start.params)
setMethod(f = "clv.model.generate.start.param.cor", signature = signature(clv.model="clv.model.with.correlation"), definition = function(clv.model, start.param.cor, transformed.start.params.model){

  # Correlation param m
  if(clv.model@estimation.used.correlation){

    # Transform correlation to param m
    #   do model-specific transformation with the generated and transformed model parameters
    if(is.null(start.param.cor)){
      # Use cor=0 if none given
      start.param.cor.param.m <- clv.model.cor.to.m(clv.model=clv.model,
                                                    prefixed.params.model=transformed.start.params.model,
                                                    param.cor = 0)
    }else{
      start.param.cor.param.m <- clv.model.cor.to.m(clv.model=clv.model,
                                                    prefixed.params.model=transformed.start.params.model,
                                                    param.cor = start.param.cor)
    }

    # Name and add to all start params
    names(start.param.cor.param.m) <- clv.model@name.prefixed.cor.param.m
    transformed.start.params.model <- c(transformed.start.params.model, start.param.cor.param.m)
  }

  return(transformed.start.params.model)
})

