pnbd_dyncov_palive <- function (clv.fitted){

  cbs.x <- i.x <- Bksum <- DkT <- A1sum <- palive <-  NULL

  # Params, not logparams
  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]

  # write to LLdata for nicer calculation
  LLdata <- copy(clv.fitted@LL.data)
  LLdata[clv.fitted@cbs, cbs.x := i.x, on="Id"]

  # Implementation with improved numerical stability using LL rather than the consitutents of the LL
  # A1sum = log(Akprod)
  LLdata[, F1 := A1sum + lgamma(r+cbs.x) - lgamma(r) + r * (log(alpha_0) - log(alpha_0 + Bksum)) + cbs.x * (-log(alpha_0 + Bksum)) + s*(log(beta_0) - log(beta_0+DkT))]
  LLdata[, palive := exp(F1 - LL)]

  # return as data.table
  return(LLdata[, c("Id", "palive")])
}
