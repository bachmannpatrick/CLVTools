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

           prediction.params.model = "numeric",

           # Can save optimx result as optimx class because setOldClass (optimx) is
           #  done before
           optimx.estimation.output = "optimx",
           optimx.hessian           = "matrix"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           prediction.params.model = numeric(0),

           optimx.estimation.output = structure(data.frame(), class="optimx"),
           optimx.hessian           = matrix(data = numeric(0))))


