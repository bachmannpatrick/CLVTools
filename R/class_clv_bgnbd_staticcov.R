#' @importFrom methods setClass
#' @include class_clv_model_bgnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.bgnbd.static.cov", contains = "clv.fitted.static.cov",
         slots = c(
           cbs = "data.table"),
         prototype = list(
           cbs = data.table()))


# Convenience constructor to encapsulate all steps for object creation
#' @importFrom methods new
#' @include class_clv_data.R class_clv_model_bgnbd_staticcov.R
clv.bgnbd.static.cov <- function(cl, clv.data){

  dt.cbs.bgnbd <- bgnbd_cbs(clv.data = clv.data)
  clv.model <- new("clv.model.bgnbd.static.cov")

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.bgnbd.static.cov",
             clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.bgnbd))
}
