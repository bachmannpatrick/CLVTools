# Call the single next layer in next.interlayers and pass all parameters
interlayer_callnextinterlayer <- function(next.interlayers, LL.params, LL.function.sum, ...){

  all.other.args <- list(...)

  # Put together call ----------------------------------------------------------
  #   remove the current interlayer
  #   Call layer on top
  interlayer.call.args <- list(next.interlayers = next.interlayers[-1],
                               LL.params        = LL.params,
                               LL.function.sum  = LL.function.sum)
  interlayer.call.args <- modifyList(interlayer.call.args, all.other.args)

  return(do.call(what = next.interlayers[[1]], args = interlayer.call.args))
}
