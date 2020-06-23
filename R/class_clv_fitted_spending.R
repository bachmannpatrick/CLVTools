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

#  check newdata: needs spending


setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted,  start.params.model, optimx.args, verbose, ...){
  err.msg <- c()

  if(!clv.data.has.spending(clv.fitted@clv.data))
    err.msg <- c(err.msg, "Spending models can only be fit on clv.data objects with spending data!")

  check_err_msg(err.msg)
})
