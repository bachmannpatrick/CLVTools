setClass(Class = "clv.model.no.correlation", contains = c("clv.model", "VIRTUAL"))


setMethod("clv.model.supports.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

setMethod("clv.model.estimation.used.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

# Nothing to store additionally
setMethod("clv.model.put.estimation.input", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model, use.cor){
  return(clv.model)
})

setMethod(f = "clv.model.generate.start.param.cor", signature = signature(clv.model="clv.model.no.correlation"), definition = function(clv.model, start.param.cor, transformed.start.params.model){
  return(transformed.start.params.model)
})


setMethod(f = "clv.model.coef.add.correlation", signature = signature(clv.model="clv.model.no.correlation"), definition = function(clv.model, last.row.optimx.coef, original.scale.params){
  return(original.scale.params)
})
