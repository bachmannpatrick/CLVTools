# There is no next.interlayer
#' @importFrom utils modifyList
interlayer_callLL <- function(LL.function.sum, LL.params, LL.params.names.ordered, ...){

  all.other.args <- list(...)

  # Order LL params --------------------------------------------------------------
  #   As the order of the LL params may differ by function (ie trans vs life),
  #     bring them in the needed order
  #   This will also remove any param which was not specified in
  #     LL.params.names.ordered what could be desirable
  LL.params <- LL.params[LL.params.names.ordered]


  # Call LL ----------------------------------------------------------------------
  #   Call the specified LL function with the new params

  allowed.LL.function.arg.names <- formalArgs(def = LL.function.sum)

  # Create the args to call the LL function
  #   the params need to be in first position
  LL.function.args      <- list(LL.params)
  LL.function.args      <- modifyList(LL.function.args,
                                      # Only pass the params allowed by the LL
                                      all.other.args[intersect(allowed.LL.function.arg.names,
                                                               names(all.other.args))])

  LL.res <- do.call(LL.function.sum, LL.function.args)

  return(LL.res)
}
