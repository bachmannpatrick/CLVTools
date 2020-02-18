#' @importFrom methods setClass
setClass(Class = "clv.model", contains = "VIRTUAL",
         slots = c(
           # Anything that will be used from main execution code.
           #  Enforce it through slots instead of relying on setting it in model generics
           name.model                  = "character",
           names.original.params.model = "character",
           names.prefixed.params.model = "character",
           start.params.model          = "numeric",
           # The standard method is overwritten with Nelder-Mead if correlation is used because
           #  if param.m is out of bounds, Inf is returned.
           optimx.defaults             = "list"))

# **Separate clv.covariate.model virtual class to hard-code method arguments?
# ** or move to clv.fitted.static.cov?

# Default / fallback methods for all models --------------------------------------------------------
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){
  if(length(list(...)) > 0)
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
})

setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, verbose, ...){
  # do nothing
  return(clv.fitted)
})

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  stop("The method clv.model.prepare.optimx.args needs to be implemented for all models!")
})

setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model"), definition = function(clv.model, original.start.params.model){
  # return start params as given
  return(original.start.params.model)
})

setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.model){
  # return as optimized
  return(prefixed.params.model)
})

setMethod("clv.model.put.optimx.output", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, res.optimx){
  # do nothing
  return(clv.fitted)
})

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, prefixed.params){
  # No transformation needed (because also untransformed), only 1s in diag
  m.diag <- diag(x = 1, nrow = length(prefixed.params))
  rownames(m.diag) <- colnames(m.diag) <- names(prefixed.params)
  return(m.diag)
})


setMethod("clv.model.predict.clv", signature(clv.model="clv.model"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  stop("The method clv.model.predict.clv needs to be implemented for all models!")
})

setMethod("clv.model.expectation", signature(clv.model="clv.model"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  stop("The method clv.model.expectation needs to be implemented for all models!")
})

# Default covariate model steps ----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model"), definition = function(clv.model, start.params.cov){
  # no transformation
  return(start.params.cov)
})

setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.cov){
  # no back transformation
  return(prefixed.params.cov)
})



