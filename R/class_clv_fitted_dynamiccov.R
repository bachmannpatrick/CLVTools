#' Fitted CLV Model with Dynamic covariates
#'
#' @description
#'
#' Extends the class \code{\link[CLVTools:clv.fitted.static.cov-class]{clv.fitted.static.cov}} but adds no
#' additional slots to it. The purpose of this class rather is to perform steps during the fitting
#' process that are specific to dynamic covariates models.
#'
#' @importFrom methods setClass
#' @include all_generics.R class_clv_data_dynamiccovariates.R class_clv_fitted_staticcov.R class_clv_fitted.R
setClass(Class = "clv.fitted.dynamic.cov", contains = "clv.fitted.static.cov")


# Constructor. Creates object through fitted.static.cov.
clv.fitted.dynamic.cov <- function(cl, clv.model, clv.data){
  return(
    new("clv.fitted.dynamic.cov",
        clv.fitted.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data)))
}
