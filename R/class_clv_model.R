#' CLV Model providing model related functionalities
#'
#' @description
#' Objects of class \code{clv.fitted} contain an instance of class clv.model that is called
#' whenever a model-related step is performed (Strategy pattern).
#'
#' This class is the abstract parent class from which new clv models inherit. It implements
#' only stubs (with stop()) for all methods that need to be implemented in order
#' to obtain a functional \code{clv.fitted} object.
#'
#' @slot name.model Name of the model as it should be displayed
#' @slot names.original.params.model character vector that defines the names of the model parameters as they should be reported
#' @slot names.prefixed.params.model character vector that defines the names of the model parameters as they are named during LL optimization
#' @slot start.params.model numeric vector of default values at original scale that should be used for the LL optimization if the user does not provide start parameters. Named with \code{names.original.params.model}.
#' @slot optimx.defaults list of default arguments for calling \code{\link[optimx]{optimx}} with \code{do.call}. Named after the respective arguments in optimx.
#'
#' @seealso CLV model subclasses with and without support for fitting with correlation \linkS4class{clv.model.with.correlation}, \linkS4class{clv.model.no.correlation}
#' @seealso Class using its instances: \linkS4class{clv.fitted}
#'
#' @keywords internal
#' @include all_generics.R
#' @importFrom methods setClass
setClass(Class = "clv.model", contains = "VIRTUAL",
         slots = list(
           # Anything that will be used from main execution code.
           #  Enforce it through slots instead of relying on setting it in model generics
           name.model                  = "character",

           names.original.params.model = "character",
           names.prefixed.params.model = "character",
           start.params.model          = "numeric",
           # The standard method is overwritten with Nelder-Mead if correlation is used because
           #  if param.m is out of bounds, Inf is returned.
           optimx.defaults             = "list"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           name.model                  = character(0),

           names.original.params.model = character(0),
           names.prefixed.params.model = character(0),
           start.params.model          = numeric(0),

           optimx.defaults             = list()))


# No constructor as should not be created

# Default / fallback methods for all models --------------------------------------------------------

# . clv.model.check.input.args -----------------------------------------------------------------------------
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, start.params.model, optimx.args, verbose, ...){
  # Example:
  # if(length(list(...)) > 0)
  #   stop("Any further parameters passed in ... are not needed for this model.", call. = FALSE)
  stop("The method clv.model.check.input.args has not been implemented by this model!")
})

# . clv.model.put.estimation.input -----------------------------------------------------------------------------
setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model"), definition = function(clv.model, ...){
  # Example: do nothing
  # return(clv.model)
  stop("The method clv.model.put.estimation.input has not been implemented by this model!")
})

# . clv.model.transform.start.params.model -----------------------------------------------------------------------------
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model"), definition = function(clv.model, original.start.params.model){
  # Example: return start params as given
  # return(original.start.params.model)
  stop("The method clv.model.transform.start.params.model has not been implemented by this model!")
})


# . clv.model.backtransform.estimated.params.model ---------------------------------------------------------------------
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.model){
  # Example: return as optimized
  # return(prefixed.params.model)
  stop("The method clv.model.backtransform.estimated.params.model has not been implemented by this model!")
})

# . clv.model.prepare.optimx.args -----------------------------------------------------------------------------
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, prepared.optimx.args){
  stop("The method clv.model.prepare.optimx.args has not been implemented by this model!")
})


# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, res.optimx){
  # Example: do nothing
  # No additional step needed (ie store model specific stuff, extra process)
  # return(clv.fitted)
  stop("The method clv.model.process.post.estimation has not been implemented by this model!")
})

# . clv.model.cor.to.m ----------------------------------------------------------------------------------------
setMethod(f="clv.model.cor.to.m", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.model, param.cor){
  # Example:
  # res.m <- param.cor / .XXX
  # return unnamed as otherwise still called "cor"
  # return(unname(res.m))
  stop("The method clv.model.cor.to.m has not been implemented by this model!")
})

# . clv.model.m.to.cor ----------------------------------------------------------------------------------------
setMethod(f="clv.model.m.to.cor", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.model, param.m){
  # Example:
  # res.cor <- param.m * XXX
  # return unnamed as otherwise still called "m"
  # return(unname(res.cor))
  stop("The method clv.model.m.to.cor has not been implemented by this model!")
})

# . clv.model.vcov.jacobi.diag ------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, prefixed.params){
  # Example: No transformation needed (because also untransformed), only 1s in diag
  # m.diag <- diag(x = 1, nrow = length(prefixed.params))
  # rownames(m.diag) <- colnames(m.diag) <- names(prefixed.params)
  # return(m.diag)
  stop("The method clv.model.vcov.jacobi.diag has not been implemented by this model!")
})

# . clv.model.predict ------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){
  stop("The method clv.model.predict has not been implemented by this model!")
})

# . clv.model.expectation ------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  stop("The method clv.model.expectation has not been implemented by this model!")
})

# . clv.model.process.newdata ------------------------------------------------------------------------------------------
setMethod("clv.model.process.newdata", signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, user.newdata, verbose){
  stop("The method clv.model.process.newdata has not been implemented by this model!")
})


# Default covariate model steps ----------------------------------------------------------------------------------------------------
# . clv.model.transform.start.params.cov -------------------------------------------------------------------------------------------
setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model"), definition = function(clv.model, start.params.cov){
  # Example: no transformation
  # return(start.params.cov)
  stop("The method clv.model.transform.start.params.cov has not been implemented by this model!")
})

# . clv.model.backtransform.estimated.params.cov ----------------------------------------------------------------------------------------
setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model"), definition = function(clv.model, prefixed.params.cov){
  # Example: no back transformation
  # return(prefixed.params.cov)
  stop("The method clv.model.backtransform.estimated.params.cov has not been implemented by this model!")
})

# .clv.model.probability.density -------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.probability.density", signature = signature(clv.model="clv.model"), definition = function(clv.model, x, clv.fitted){
  stop("The method clv.model.probability.density has not been implemented for this model!")
})


