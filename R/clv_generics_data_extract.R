#' @importFrom stats nobs
#' @export
nobs.clv.data   <- function(object, ...){
  # Observations are number of customers
  return(as.integer(object@descriptives.transactions[Name=="Number of customers"]$Total))
}
