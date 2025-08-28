
#' @name hessian
#' @title Calculate hessian for a fitted model
#'
#' @description Calculate a numerical approximation to the Hessian matrix at
#' the final estimated parameters using \code{numDeriv::hessian}.
#'
#' @param object Fitted model
#' @param method.args List of options forwarded to the numerical approximation
#' method. See \link[numDeriv:hessian]{numDeriv::hessian}.
#' @template template_param_dots
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

  # Get final parameters
  # To get coefficients at the same scale (log-transformed) and names as when
  # estimating the model, get them directly from the optimx output.
  # Cannot use coef() as the reported parameters are transformed back and named
  # differently.
  final.coefs <- drop(tail(coef(object@optimx.estimation.output), n=1))

  if(anyNA(final.coefs)){
    check_err_msg("Cannot proceed because there are NAs in the estimated coefficients!")
  }

  # Get LL
  fn.LL <- clv.fitted.get.LL(object)

  # fn.call.LL.named <- function(par){
  #   names(par) <- names(final.coefs)
  #   return(fn.LL(par))
  # }

  # Have to refer to `numDeriv` namespace directly (`::`) as `hessian()` would
  # dispatch to `CLVTools::hessian` and fail if `numDeriv` is not attached
  H <- numDeriv::hessian(
    func=fn.LL,
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

