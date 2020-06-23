
#' @exportMethod gg
setGeneric("gg", def = function(clv.data, start.params.model=c(), optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("gg"))



#' @include class_clv_data.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                    start.params.model=c(),
                                                                                    optimx.args=list(),
                                                                                    verbose=TRUE,...){

  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)
  obj <- clv.gg(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model, optimx.args = optimx.args, verbose=verbose))
})
