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


clv.fitted.transactions.add.expectation.data <- function(clv.fitted.transactions, dt.expectation.seq, cumulative, verbose){
  expectation <- i.expectation <- NULL

  #   Pass copy of expectation table file because will be modified and contain column named expecation
  dt.model.expectation <- clv.model.expectation(clv.model=clv.fitted.transactions@clv.model, clv.fitted=clv.fitted.transactions,
                                                dt.expectation.seq=copy(dt.expectation.seq), verbose = verbose)

  # Only the expectation data
  dt.model.expectation <- dt.model.expectation[, c("period.until", "expectation")]

  if(cumulative)
    dt.model.expectation[, expectation := cumsum(expectation)]

  # add expectation to plot data
  #   name columns by model
  dt.expectation.seq[dt.model.expectation, expectation := i.expectation, on = "period.until"]
  return(dt.expectation.seq)
}
