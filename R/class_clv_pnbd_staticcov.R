#' @importFrom methods setClass
#' @include class_clv_model_pnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.pnbd.static.cov", contains = "clv.fitted.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


# Convenience constructor to encapsulate all steps for object creation
#' @include class_clv_data.R class_clv_model_pnbd_staticcov.R
clv.pnbd.static.cov <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_cbs(clv.data = clv.data)
  clv.model <- new("clv.model.pnbd.static.cov")

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.pnbd.static.cov",
             clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}
