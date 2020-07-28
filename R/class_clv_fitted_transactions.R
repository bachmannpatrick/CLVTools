#' Fitted Transaction Model without covariates
#'
#' Extends the class \code{clv.fitted} to performs steps during the
#' estimation, prediction and plotting process that are specific to all transaction models.
#'
#' @seealso Definition of the parent class \linkS4class{clv.fitted}
#' @seealso For spending models \linkS4class{clv.fitted.spending}
#'
#' @include class_clv_fitted.R
#' @keywords internal
setClass(Class = "clv.fitted.transactions", contains = "clv.fitted")


#' @importFrom methods new
clv.fitted.transactions <- function(cl, clv.model, clv.data){

  # Deep copy of clv.data if ever modified by reference later on
  return(new("clv.fitted.transactions",
             call      = cl,
             clv.model = clv.model,
             clv.data  = copy(clv.data)))
}



