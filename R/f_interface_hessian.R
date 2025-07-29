
#' @name hessian
#' @title Calculate hessian for a fitted model
#'
#' @description Calculate a numerical approximation to the Hessian matrix at
#' the final estimated parameters using \code{numDeriv::hessian}.
#'
#' @param clv.fitted Fitted model
#' @param method.args List of options forwarded to the numerical approximation
#' method. See \link[numDeriv:hessian]{numDeriv::hessian}.
#'
#' @returns The hessian matrix, with column and row names set to the parameter
#' names used to call the LL.
NULL


#' @rdname hessian
#' @importFrom numDeriv hessian
#' @importFrom utils tail
#' @importFrom methods formalArgs
#' @exportS3Method numDeriv::hessian
hessian.clv.fitted <- function(object, method.args = list()){

  # Register for dispatch on a method defined in another package by using
  # @exportS3Method which adds `S3method(numDeriv::hessian,clv.fitted)` to NAMESPACE

  clv.controlflow.check.prediction.params(clv.fitted = object)

  # To calculate the Hessian, the LL needs to be called with the final parameters
  # Calling the LL with the exact same inputs/specification as when the fitting
  # is however not trivial as there are plenty of options.
  # To reproduce it, the exact steps of the estimation are repeated here.

  # To get coefficients at the same scale (log-transformed) and names as when
  # estimating the model, get them from the optimx results.
  # Cannot use coef() as the reported parameters are transformed back and named
  # differently
  final.coefs <- drop(tail(coef(object@optimx.estimation.output), n=1))

  prepared.optimx.args <- clv.controlflow.estimate.prepare.optimx.args(
    clv.fitted=object,
    start.params.all=final.coefs)

  prepared.optimx.args <- clv.model.prepare.optimx.args(
    clv.model=object@clv.model,
    clv.fitted=object,
    prepared.optimx.args=prepared.optimx.args)

  prepared.optimx.args[["LL.param.names.to.optimx"]] <- names(prepared.optimx.args$par)

  # In the estimation procedure, the user can also supply custom `optimx.args`
  # which override `prepared.optimx.args` here. Because optimx is not called
  # here, there is no need to add `optimx.args` here.

  # The generated args also contain parameters for optimx. These are not required
  # for the LL and need to be removed.
  names.optimx.args <- setdiff(formalArgs(optimx), "...")
  call.args <- prepared.optimx.args[!(names(prepared.optimx.args) %in% names.optimx.args)]


  # Wrapper to call the LL with the original args and `par` given by numDeriv
  fn.with.call.args <- function(par){
    call.args$LL.params <- par
    return(do.call(prepared.optimx.args$fn, call.args))
  }

  # Have to refer to `numDeriv` namespace directly (`::`) as `hessian()` would
  # dispatch to `CLVTools::hessian` and fail if `numDeriv` is not attached
  H <- numDeriv::hessian(
    func=fn.with.call.args,
    x=final.coefs,
    method="Richardson",
    method.args = method.args)
  # Names as in optimx (log-scale etc)
  colnames(H) <- rownames(H) <- names(final.coefs)

  return(H)
}


# In order to be able to use `hessian()` without having `numDeriv` loaded or even
# installed, define and export `hessian()` as a generic in CLVTools.
# The S4 generic is NOT defined with the exact same signature as the
# S3 generic `numDeriv::hessian <- function(func, x, method, method.args, ...){ ... }`.
#
# The numDeriv package exports an S3 generic `hessian()` what masks the generic (whether S3
# or S4) exported by CLVTools if the numDeriv package is loaded after CLVTools.
# Therefore, define and export also as a S3 method `CLVTools::hessian.clv.fitted`.
#
# ?Methods_for_Nongenerics on dispatching an S4 object to S3 generics method in
# another package: Recommends to define both methods: The S3 method and also supply
# the identical function as the definition of the S4 method.
#' @rdname hessian
#' @exportMethod hessian
setGeneric(name = "hessian", def=function(object, ...)
  standardGeneric("hessian"))


#' @rdname hessian
#' @include all_generics.R
#' @exportMethod hessian
setMethod("hessian", signature(object="clv.fitted"), definition = hessian.clv.fitted)

