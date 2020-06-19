#' @slot estimation.used.correlation Single boolean whether the correlation was estimated.
#' @slot name.prefixed.cor.param.m Single character vector of the internal name used for the correlation parameter during optimization.
#' @slot name.correlation.cor Single character vector of the external name used for the correlation parameter.
setClass(Class = "clv.model.supports.correlation", contains = c("clv.model", "VIRTUAL"),
         slots = list(
           estimation.used.correlation  = "logical",
           name.prefixed.cor.param.m    = "character",
           name.correlation.cor         = "character"),

         # Have to use prototype because cannot instantiate (ie constructor function) a virtual class
         prototype = list(
           estimation.used.correlation = logical(0), # not known yet
           name.prefixed.cor.param.m   = "correlation.param.m",
           name.correlation.cor        = "Cor(life,trans)"))


setMethod("clv.model.supports.correlation", signature = signature(clv.model="clv.model.supports.correlation"), def = function(clv.model){
  return(TRUE)
})

setMethod("clv.model.estimation.used.correlation", signature = signature(clv.model="clv.model.supports.correlation"), def = function(clv.model){
  return(clv.model@estimation.used.correlation)
})


setMethod("clv.model.put.estimation.input", signature = signature(clv.model="clv.model.supports.correlation"), def = function(clv.model, use.cor){

  # Should correlation be calculated? -----------------------------------------------------------------
  if(use.cor){
    clv.model@estimation.used.correlation <- TRUE
  }else{
    clv.model@estimation.used.correlation <- FALSE
  }

  return(clv.model)
})

# setMethod("clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.supports.correlation"), def = function(){
#   s
# })

