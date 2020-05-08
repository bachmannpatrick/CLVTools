#' @importFrom utils modifyList
interlayer_regularization <- function(next.interlayers, LL.params, LL.function.sum,
                                      names.prefixed.params.after.constr.life, names.prefixed.params.after.constr.trans,
                                      reg.lambda.life, reg.lambda.trans, num.observations, ...){

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
  #   regularization term := lambda.trans * t(cov.trans) * params.trans +
  #                          lambda.life  * t(cov.life)  * params.life

  params.cov.life  <- LL.params[names.prefixed.params.after.constr.life]
  params.cov.trans <- LL.params[names.prefixed.params.after.constr.trans]

  reg.term <- reg.lambda.trans*t(params.cov.trans)%*%params.cov.trans + reg.lambda.life*t(params.cov.life)%*%params.cov.life
  reg.term <- as.vector(reg.term)

  avg.LL.value <- LL.value / num.observations

  # Return sum of LL and regterm
  #   Add reg term to pull negative avg LL value towards 0
  return(avg.LL.value + reg.term)
}
