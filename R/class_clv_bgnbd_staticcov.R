#' Result of fitting the BG/NBD model with static covariates
#'
#' @description
#' Output from fitting the BG/NBD model on data with static covariates. It constitutes the
#' estimation result and is returned to the user to use it as input to other methods such as
#' to make predictions or plot the unconditional expectation.
#'
#' Inherits from \code{clv.fitted.static.cov} in order to execute all steps required for fitting a model
#' with static covariates and it contains an instance of class \code{clv.model.bgnbd.static.cov} which
#' provides the required BG/NBD (static covariates) specific functionalities.
#'
#' @template template_slot_bgnbdcbs
#'
#' @seealso \link{clv.fitted.static.cov-class}, \link{clv.model.bgnbd.static.cov-class}, \link{clv.bgnbd-class}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_bgnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.bgnbd.static.cov", contains = "clv.fitted.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.bgnbd.static.cov <- function(cl, clv.data){

  dt.cbs.bgnbd <- bgnbd_cbs(clv.data = clv.data)
  clv.model    <- clv.model.bgnbd.static.cov()

  return(new("clv.bgnbd.static.cov",
             clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.bgnbd))
}
