pnbd_dyncov_palive <- function (clv.fitted){

  cbs.x <- rsx <- i.x <- Bksum <- DkT <- Z <- palive <-  NULL

  # Params, not logparams
  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]

  LLdata <- copy(clv.fitted@LL.data)
  cbs    <- copy(clv.fitted@cbs)

  # write to LLdata for nicer calculation
  # Z in the notes: F.2 in LL function
  LLdata[cbs, cbs.x := i.x, on="Id"]
  LLdata[,    rsx   := s/(r+s+cbs.x)]

  LLdata[, palive := 1/((Bksum+alpha_0)^(cbs.x+r) * (DkT+beta_0)^s * rsx * Z + 1)]

  # return as data.table
  return(LLdata[, c("Id", "palive")])
}
