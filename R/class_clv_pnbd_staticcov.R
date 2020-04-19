#' Result of fitting the Pareto/NBD model with static covariates
#'
#' @description
#' Output from fitting the Pareto/NBD model on data with static covariates. It constitutes the
#' estimation result and is returned to the user to use it as input to other methods such as
#' to make predictions or plot the unconditional expectation.
#'
#' Inherits from \code{clv.fitted.static.cov} in order to execute all steps required for fitting a model
#' with static covariates and it contains an instance of class \code{clv.model.pnbd.static.cov} which
#' provides the required Pareto/NBD (static covariates) specific functionalities.
#'
#' @template template_slot_pnbdcbs
#'
#' @seealso \link{clv.fitted.static.cov-class}, \link{clv.model.pnbd.static.cov-class}, \link{clv.pnbd-class}, \link{clv.pnbd.dynamic.cov-class}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_pnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.pnbd.static.cov", contains = "clv.fitted.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


# Convenience constructor to encapsulate all steps for object creation
#' @importFrom methods new
#' @include class_clv_data.R class_clv_model_pnbd_staticcov.R
clv.pnbd.static.cov <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_cbs(clv.data = clv.data)
  clv.model   <- clv.model.pnbd.static.cov()

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.pnbd.static.cov",
             clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}
