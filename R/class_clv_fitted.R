# Register an S4 class for optimx to be able to save the estimation output in the clv S4 class' slot
#' @import optimx
setOldClass("optimx")


#' Fitted model without covariates
#'
#' @description
#' The class stores the transaction data and various optimization outputs and options.
#' It is created already when model fitting is initiated and is then used to perform no covariate specific steps
#' during the estimation process.
#' Serves as parent classes to fitted transaction and spending models.
#'
#' Created with an existing clv.data and clv.model object (or subclasses thereof).
#'
#' @slot call Single language of the call used to create the object
#' @slot clv.model Single object of (sub-) class \code{clv.model} that determines model-specific behavior.
#' @slot clv.data Single object of (sub-) class \code{clv.data} that contains the data and temporal information to fit the model to.
#' @slot model.specification.args Model specification given by the user with which the model was fit. Used to re-fit the model on new data (bootstrapping).
#' @slot prediction.params.model Numeric vector of the model parameters, set and used solely when predicting. Named after model parameters in original scale and derived from \code{coef()}.
#' @slot optimx.estimation.output A single object of class \code{optimx} as returned from method \code{optimx::optimx} after optimizing the log-likelihood fitting the model.
#' @slot optimx.hessian Single matrix that is the hessian extracted from the last row of the optimization output stored in the slot \code{optimx.estimation.output}.
#'
#' @seealso \linkS4class{clv.fitted.spending}, \linkS4class{clv.fitted.transactions}, \linkS4class{clv.fitted.transactions.static.cov}, \linkS4class{clv.fitted.transactions.dynamic.cov}
#'
#' @importFrom methods setClass
#' @keywords internal
#' @include class_clv_model.R class_clv_data.R
setClass(Class = "clv.fitted", contains = "VIRTUAL",
         slots = c(
           call      = "language",
           clv.model = "clv.model",
           clv.data  = "clv.data",
           model.specification.args = "list",

           prediction.params.model = "numeric",

           # Can save optimx result as optimx class because setOldClass (optimx) is
           #  done before
           optimx.estimation.output = "optimx",
           optimx.hessian           = "matrix"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           model.specification.args = list(),
           prediction.params.model = numeric(0),

           optimx.estimation.output = structure(data.frame(), class="optimx"),
           optimx.hessian           = matrix(data = numeric(0))))




clv.fitted.get.LL <- function(clv.fitted){

  # Calling the LL with the exact same inputs/specification as when the fitting
  # is not trivial as there are plenty of options.
  # To reproduce it, the exact steps of the estimation are repeated here.

  # Start parameters are not really required, they are just stored as item
  # `par` for `optimx()`
  final.coefs <- drop(tail(coef(clv.fitted@optimx.estimation.output), n=1))

  prepared.optimx.args <- clv.controlflow.estimate.prepare.optimx.args(
    clv.fitted=clv.fitted,
    start.params.all= final.coefs)

  prepared.optimx.args <- clv.model.prepare.optimx.args(
    clv.model=clv.fitted@clv.model,
    clv.fitted=clv.fitted,
    prepared.optimx.args=prepared.optimx.args)

  prepared.optimx.args[["LL.param.names.to.optimx"]] <- names(prepared.optimx.args$par)

  # In the estimation procedure, the user can also supply custom `optimx.args`
  # which override `prepared.optimx.args` here. Because optimx is not called
  # here, there is no need to add `optimx.args` here.

  # The generated args also contain parameters for optimx. These are not required
  # for the LL and need to be removed. This also removes the LL itself (`fn`).
  names.optimx.args <- setdiff(formalArgs(optimx), "...")
  call.args <- prepared.optimx.args[!(names(prepared.optimx.args) %in% names.optimx.args)]

  # Could save memory as the returned method is a closure and has the environment
  # in which is was defined attached. Hence all variables in this method here
  # which may be large. However, it can also be useful to have these objects.

  # Wrapper to call the LL with the original args.
  # It is preferred to return a method rather than calling it immediately
  # because generating the call args may take time.
  LL <- function(params){
    req.names <- call.args$LL.param.names.to.optimx

    # Ensure named same as original
    if(!(identical(sort(names(params)), sort(req.names)))){
      check_err_msg(paste0(
        "'params' has to be named ",
        paste(req.names, collapse = ", "),
        ". Often, `drop(coef(model@optimx.estimation.output))` is useful."))
    }

    # Ensure name and position are the same as original order. This is required as
    # any param input will be re-named by slapping `LL.param.names.to.optimx` on
    # them (using names() <- ) in the first interlayer and then further accessed
    # by name.
    call.args$LL.params <- params[req.names] # bring to correct order
    return(do.call(what = prepared.optimx.args$fn, args = call.args))
  }

  return(LL)
}
