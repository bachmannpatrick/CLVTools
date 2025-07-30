#' @importFrom utils modifyList
interlayer_regularization <- function(next.interlayers, LL.params, LL.function.sum,
                                      names.prefixed.params.after.constr.life, names.prefixed.params.after.constr.trans,
                                      reg.weight.life, reg.weight.trans, num.observations, ...){

  all.other.args <- list(...)


  # Calculate LL value --------------------------------------------------------
  #   The LL value is calculate by the next interlayer and may pass through
  #     additional layers before actually being calculated
  #   Use do.call to integrate ... args

  next.interlayer.call.args <- list(next.interlayers = next.interlayers,
                                    LL.params        = LL.params,
                                    LL.function.sum  = LL.function.sum)

  next.interlayer.call.args <- modifyList(next.interlayer.call.args,
                                          all.other.args)

  LL.value     <- do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args)
  if(!is.finite(LL.value))
    return(LL.value)


  # Regularization --------------------------------------------------------------
  #   LL value + regularization term
  #
  #   regularization term := weight.trans * t(cov.trans) * params.trans +
  #                          weight.life  * t(cov.life)  * params.life

  params.cov.life  <- LL.params[names.prefixed.params.after.constr.life]
  params.cov.trans <- LL.params[names.prefixed.params.after.constr.trans]

  reg.term <- reg.weight.trans*t(params.cov.trans)%*%params.cov.trans + reg.weight.life*t(params.cov.life)%*%params.cov.life
  reg.term <- as.vector(reg.term)

  # Although we use the compressed CBS and only calculate the LL for unique data,
  # num.observations is the real number of all customers and LL.value is already
  # multiplied with vN. Therefore, nothing needs to be adapted here.
  avg.LL.value <- LL.value / num.observations

  # Return sum of LL and regterm
  #   Add reg term to pull negative avg LL value towards 0
  return(avg.LL.value + reg.term)
}
