#' Fitted CLV Transaction Model without covariates
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



