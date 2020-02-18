# Register an S4 class for optimx to be able to save the estimation output in the clv S4 class' slot
#' @import optimx
setOldClass("optimx")


#' Fitted CLV Model without covariates
#'
#'
#' The class stores the transaction data and various optimization outputs and options.
#' It is created already when model fitting is initiated and is then used to perform no covariate specific steps
#' during the estimation process.
#' Created from an existing clv.data and clv.model object (or subclasses thereof).
#'
#' Extends the c to perform steps in the estimation that are specific to static covariates models.
#' Contains additional slots to accommodate the various further optimization options that can be used for covariates models.
#'
#' @slot call Single language of the call used to create the object
#' @slot clv.model Single object of (sub-) class \code{clv.model} that determines model-specific behavior.
#' @slot clv.data Single object of (sub-) class \code{clv.data} that contains the data and temporal information to fit the model to.
#' @slot cbs Single \code{data.table} that is the Customer-By-Sufficiency matrix with information about each customers' recency, frequency, and total time observed.
#' @slot prediction.params.model Numeric vector of the model parameters, set and used solely when predicting. Named after model parameters in original scale and derived from \code{coef()}.
#' @slot estimation.used.correlation Single boolean whether the correlation was estimated.
#' @slot name.prefixed.cor.param.m Single character vector of the internal name used for the correlation parameter during optimization.
#' @slot name.correlation.cor Single character vector of the external name used for the correlation parameter.
#' @slot optimx.estimation.output A single object of class \code{optimx} as returned from method \code{optimx::optimx} after optimizing the log-likelihood fitting the model.
#' @slot optimx.hessian Single matrix that is the hessian extracted from the last row of the optimization output stored in the slot \code{optimx.estimation.output}.
#' @importFrom methods setClass
#'
#' @keywords internal
#' @include class_clv_model_basestrategy.R class_clv_data_no_covariates.R
setClass(Class = "clv.fitted", # contains = "VIRTUAL",
         slots = c(
           call      = "language",
           clv.model = "clv.model",
           clv.data  = "clv.data",

           prediction.params.model = "numeric",

           estimation.used.correlation  = "logical",
           name.prefixed.cor.param.m    = "character",
           name.correlation.cor         = "character",

           optimx.estimation.output = "optimx",
           optimx.hessian           = "matrix"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           # Set in new()
           # clv.model
           # call
           # clv.data

           prediction.params.model = numeric(0),

           estimation.used.correlation = logical(0),
           name.prefixed.cor.param.m   = "correlation.param.m",
           name.correlation.cor        = "Cor(life,trans)",

           optimx.estimation.output = structure(data.frame(), class="optimx"),
           optimx.hessian     = matrix(data = numeric(0))))


# Convenience constructor to encapsulate all steps for object creation and
#   enforce all required inputs
#   no generic needed because always constructed in same way from transaction data and model
clv.fitted <- function(cl, clv.model, clv.data){
  # need to assign directly in constructor to have valid object
  # Deep copy of clv.data if ever modified by reference later on
  return(new("clv.fitted", call=cl, clv.model = clv.model,
             clv.data=data.table::copy(clv.data)))
}
