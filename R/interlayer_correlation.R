#' @importFrom utils modifyList
interlayer_correlation <- function(next.interlayers, LL.params, LL.function.sum, LL.function.ind, name.prefixed.cor.param.m, check.param.m.bounds, ...){
  # Catch all other args in elipsis
  all.other.args <- list(...)

  # Read out pnbd model params
  alpha_0 <- exp(LL.params[["log.alpha"]])
  r       <- exp(LL.params[["log.r"]])
  beta_0  <- exp(LL.params[["log.beta"]])
  s       <- exp(LL.params[["log.s"]])

  param.m <- LL.params[[name.prefixed.cor.param.m]]

  # Laplace transformations
  LA <- (alpha_0 / (1 + alpha_0))^r
  LB <- (beta_0  / (1 + beta_0 ))^s


  if(check.param.m.bounds){
    # Restrain m to be in the interval [lowerbound, upperbound]
    upperbound <-  1 / max(  LA*(1- LB), (1-LA)*LB )
    lowerbound <- -1 / max(  LA*LB     , (1-LA)*(1-LB))

    if( param.m > upperbound || param.m < lowerbound){
      return(NA_real_)
    }
  }


  # Each model param + 1 while keeping the others unchanged
  params.00 <- LL.params
  params.10 <- LL.params
  params.01 <- LL.params
  params.11 <- LL.params

  optimx.names.model <- all.other.args[["obj"]]@clv.model@names.prefixed.params.model
  params.00[optimx.names.model] <- log(c(r, alpha_0,   s, beta_0))
  params.10[optimx.names.model] <- log(c(r, alpha_0+1, s, beta_0))
  params.01[optimx.names.model] <- log(c(r, alpha_0,   s, beta_0+1))
  params.11[optimx.names.model] <- log(c(r, alpha_0+1, s, beta_0+1))


  # Call individual LL for each of these parameter combinations --------------------------------------
  next.interlayer.call.args <- list(next.interlayers = next.interlayers,
                                    LL.function.sum  = LL.function.ind) # use the individual LL function

  next.interlayer.call.args <- modifyList(next.interlayer.call.args,
                                          all.other.args)

  next.interlayer.call.args.00 <- modifyList(next.interlayer.call.args, list(LL.params = params.00))
  next.interlayer.call.args.10 <- modifyList(next.interlayer.call.args, list(LL.params = params.10))
  next.interlayer.call.args.01 <- modifyList(next.interlayer.call.args, list(LL.params = params.01))
  next.interlayer.call.args.11 <- modifyList(next.interlayer.call.args, list(LL.params = params.11))

  LL.00 <- do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args.00)
  LL.01 <- do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args.01)
  LL.10 <- do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args.10)
  LL.11 <- do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args.11)

  # Catch errors before they crash the addition below due to unqual extent
  if(any(!is.finite(LL.00), !is.finite(LL.01), !is.finite(LL.10), !is.finite(LL.11))){
    return(NA_real_)
  }

  # Return summed cor LL values
  vLL <- exp(LL.00) + param.m*LA*LB * (exp(LL.00) + exp(LL.11) - exp(LL.10) - exp(LL.01))
  vLL <- log(vLL)

  return(-sum(vLL))
}
