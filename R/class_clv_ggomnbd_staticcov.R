#' @importFrom methods setClass
#' @include class_clv_model_ggomnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.ggomnbd.static.cov", contains = "clv.fitted.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


# Convenience constructor to encapsulate all steps for object creation
clv.ggomnbd.static <- function(cl, clv.data){

  dt.cbs.ggomnbd <- ggomnbd_cbs(clv.data = clv.data)
  clv.model <- new("clv.model.ggomnbd.static.cov")

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.ggomnbd.static.cov",
             clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.ggomnbd))
}
