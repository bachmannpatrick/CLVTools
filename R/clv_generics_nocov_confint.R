#' @export
confint.clv.fitted <- function(object, parm, level = 0.95, ...){
  # This largely follows stats:::confint.lm to exhibit the exact same behavior

  # # Get SE
  # SE <- sqrt(diag(vcov(object)))
  #
  # CI.low  <- params - qnorm(1-alpha/2) * SE
  # CI.high <- params + qnorm(1-alpha/2) * SE

  estim.coefs <- coef(object)

  # Param selection --------------------------------------------------------------------------------
  if(missing(parm))
    # Use all by default
    parm <- names(estim.coefs)
  else
    if(is.numeric(parm))
      # Make numbers to respective names
      parm <- names(estim.coefs)[parm]

  # CI calc ----------------------------------------------------------------------------------------
  req.a <- (1-level) / 2
  req.a <- c(req.a, 1 - req.a)

  zs <- qnorm(p = req.a, mean = 0, sd = 1)
  ci <- estim.coefs[parm] + sqrt(diag(vcov(object)))[parm] %o% zs

  # Return ----------------------------------------------------------------------------------------
  # from stats:::format.perc - cannot call with ::: as gives CRAN note
  names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
  res[] <- ci
  return(res)
}
