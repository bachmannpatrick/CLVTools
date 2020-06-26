#' @include class_clv_fitted.R
#' @keywords internal
setClass(Class = "clv.fitted.spending", contains = "clv.fitted",
         slots     = list(estimation.removed.first.transaction = "logical"),
         prototype = list(estimation.removed.first.transaction = logical(0)))

#' @importFrom methods new
clv.fitted.spending <- function(cl, clv.model, clv.data){
  # Deep copy of clv.data if ever modified by reference later on
  return(new("clv.fitted.spending",
             call      = cl,
             clv.model = clv.model,
             clv.data  = copy(clv.data)))
}



setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(clv.fitted="clv.fitted.spending"), definition = function(clv.fitted,  start.params.model, optimx.args, verbose, remove.first.transaction, ...){
  # clv.fitted inputchecks
  callNextMethod()

  err.msg <- c()

  if(!clv.data.has.spending(clv.fitted@clv.data))
    err.msg <- c(err.msg, "Spending models can only be fit on clv.data objects with spending data!")

  # Have to check in interface because already required when building cbs
  # err.msg <- c(err.msg, .check_user_data_single_boolean(b = remove.first.transaction, var.name = "remove.first.transaction"))

  check_err_msg(err.msg)
})
