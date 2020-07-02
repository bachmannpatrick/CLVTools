#' @param remove.first.transaction consequence: All zero-repeaters are excluded when fitting the model
#' @exportMethod gg
setGeneric("gg", def = function(clv.data, start.params.model=c(), optimx.args=list(), remove.first.transaction = TRUE, verbose=TRUE, ...)
  standardGeneric("gg"))



#' @include class_clv_data.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                  start.params.model=c(),
                                                                                  optimx.args=list(),
                                                                                  remove.first.transaction = TRUE,
                                                                                  verbose=TRUE,
                                                                                  ...){

  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  # Check remove first here already because needed to build cbs
  err.msg <- c(err.msg, .check_user_data_single_boolean(remove.first.transaction, var.name = "remove.first.transaction"))
  err.msg <- c(err.msg, check_user_data_containsspendingdata(clv.data = clv.data))
  check_err_msg(err.msg)

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)
  obj <- clv.gg(cl=cl, clv.data=clv.data, remove.first.transaction = remove.first.transaction)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose = verbose,
                                           remove.first.transaction = remove.first.transaction))
})
