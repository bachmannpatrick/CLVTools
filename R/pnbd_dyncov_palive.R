pnbd_dyncov_palive <- function (clv.fitted){

  # Params, not logparams
  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]

  LLdata <- copy(clv.fitted@LL.data)
  cbs <- copy(clv.fitted@cbs)

  # write to LLdata for nicer calculation
  # Z in the notes: F.2 in LL function
  LLdata[cbs, cbs.x := i.x, on="Id"]
  LLdata[,    rsx   := s/(r+s+cbs.x)]

  # print(paste0("cbs vs LLdata: ", all.equal(cbs$Id, LLdata$Id)))
  LLdata[, palive := 1/((Bksum+alpha_0)^(cbs.x+r) * (DkT+beta_0)^s * rsx * Z + 1)]

  return(LLdata[, c("Id", "palive")]) # return as data.table

  # Jeff's new implementation:
  # PAlive<-  1/( (LLdata$Bksum+alpha_0)^(cbs$x+r)* (LLdata$DkT+beta_0)^s * cbs$Zrsx+1) #the cb$Zrsx is wrong

  # Old implementation:
  # PAlive<-  LLdata$Akprod/(exp(LLdata$LL)) *(gamma(cbs$x+r)*alpha_0^(r) * beta_0^(s))/(gamma(r)*(LLdata$Bksum+alpha_0)^(cbs$x+r)* (LLdata$DkT+beta_0)^s)

  # return(PAlive)
}
