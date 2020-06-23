#' @include class_clv_fitted.R
setClass(Class = "clv.fitted.spending", contains = "clv.fitted")

#' @importFrom methods new
clv.fitted.spending <- function(cl, clv.model, clv.data){

  # Deep copy of clv.data if ever modified by reference later on
  return(new("clv.fitted.spending",
             call      = cl,
             clv.model = clv.model,
             clv.data  = copy(clv.data)))
}

# inputchecks & check newdata: needs spending


