setClass(Class = "clv.model.no.correlation", contains = c("clv.model", "VIRTUAL"))


setMethod("clv.model.supports.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

setMethod("clv.model.estimation.used.correlation", signature = signature(clv.model="clv.model.no.correlation"), def = function(clv.model){
  return(FALSE)
})

# Nothing to store additionally
setMethod("clv.model.put.estimation.input", signature = signature(clv.model="clv.model.supports.correlation"), def = function(clv.model, use.cor){
  return(clv.model)
})
