#' Fitted CLV Model with Dynamic covariates
#'
#' @description
#'
#' Extends the class \linkS4class{clv.fitted.transactions.static.cov} but adds no
#' additional slots to it. The purpose of this class rather is to perform steps during the fitting
#' process that are specific to dynamic covariates models.
#'
#' @seealso \linkS4class{clv.fitted}, \linkS4class{clv.fitted.transactions.static.cov}
#'
#' @importFrom methods setClass
#' @include all_generics.R class_clv_data_dynamiccovariates.R class_clv_fitted_transactions_staticcov.R class_clv_fitted_transactions.R
#' @keywords internal
setClass(Class = "clv.fitted.transactions.dynamic.cov", contains = "clv.fitted.transactions.static.cov")


#' @importFrom methods new
clv.fitted.transactions.dynamic.cov <- function(cl, clv.model, clv.data){
  return(new("clv.fitted.transactions.dynamic.cov",
             clv.fitted.transactions.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data)))
}
