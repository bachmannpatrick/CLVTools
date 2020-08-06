pmf.clv.fitted <- function(clv.fitted, x, plot = FALSE){

  return(clv.template.controlflow.pmf(clv.fitted=clv.fitted, x=x, plot=plot))
}

#' @exportMethod pmf
setMethod(f = "pmf", signature = signature(clv.fitted="clv.fitted"), definition = pmf.clv.fitted)
