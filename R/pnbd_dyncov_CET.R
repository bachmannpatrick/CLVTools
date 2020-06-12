pnbd_dyncov_CET <- function(clv.fitted, predict.number.of.periods, prediction.end.date, only.return.input.to.CET=FALSE){

  i <- S <- Ai <- T.cal <- Ci <- Dbar_i <- Bbar_i <- bT_i <- DkT <- i.DkT <- Bksum <- i.Bksum <- palive <- i.palive <- NULL
  i.S <- F1 <- x <- Id <- F2 <- i.F2.noS <- CET <- NULL


  t <- predict.number.of.periods

  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]
  clv.time <- clv.fitted@clv.data@clv.time
  # This is d_1 in the formulas
  d <- clv.time.interval.in.number.tu(clv.time=clv.time,
                                      interv = interval(start = clv.time@timepoint.estimation.end,
                                                        end   = clv.time.ceiling.date(clv.time=clv.time,
                                                                                      timepoint=clv.time@timepoint.estimation.end)))

  # table with A,Bbar,C,Dbar
  dt.ABCD <- pnbd_dyncov_ABCD(clv.fitted = clv.fitted, prediction.end.date = prediction.end.date)

  # S ------------------------------------------------------------------------------------------
  # Distinguish cases depending on the number of covariates that are active during the prediction
  #   kTTt==1: Single expression, also save in S as it is added at the same place
  #   kTTt>=2: Sum of S
  #   kTTt is the number of active covariates from holdout.start until
  #     i here is the covariate number for covariates in the prediction period (seq(N) by=Id from Cov.Date>=floor(estimation.end))
  #       because all customers' Cov.Dates start at floor(estimation.end), their i is also the same (and at least 1)

  kTTt <- dt.ABCD[, max(i)]

  if(kTTt >= 2){

    dt.ABCD[i == 1,
            S := ((Ai*(T.cal*s  + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci*T.cal)^s)) -
              ((Ai*((T.cal+d)*s + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci*(T.cal+d))^s)]


    # ** What if i == 2? Is then overwritten?
    dt.ABCD[i > 1, bT_i := T.cal+d+(i-2)]

    dt.ABCD[i > 1, # also do for i==max(i), subsetting to !max(i) is presumably slower
            S:= ((Ai*( bT_i *s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci* bT_i   )^s) -
              ((Ai*((bT_i+1)*s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci*(bT_i+1))^s)]

    dt.ABCD[i == max(i),
            S:= ((Ai*( bT_i  *s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci* bT_i    )^s)) -
              ((Ai*((T.cal+t)*s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci*(T.cal+t))^s))]

  }else{
    dt.ABCD[i == 1,
            S := ((Ai*(T.cal *s + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci* T.cal   )^s)) -
              ((Ai*((T.cal+t)*s + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ( Dbar_i+beta_0+Ci*(T.cal+t))^s)]
  }

  # To test correctness of intermediate results
  if(only.return.input.to.CET){
    return(dt.ABCD)
  }

  dt.S <- dt.ABCD[, list(S = sum(S)), keyby="Id"]

  # PAlive ------------------------------------------------------------------------------------------
  dt.palive <- pnbd_dyncov_palive(clv.fitted=clv.fitted)


  # CET ---------------------------------------------------------------------------------------------

  # Merge data in single table by Id
  dt.result <- clv.fitted@cbs[, c("Id","x", "t.x", "T.cal")]
  dt.result[clv.fitted@LL.data, DkT   := i.DkT, on="Id"]
  dt.result[clv.fitted@LL.data, Bksum := i.Bksum, on="Id"]
  dt.result[dt.palive, palive := i.palive, on="Id"]
  dt.result[dt.S, S := i.S, on="Id"]

  # F1
  #   Bksum has BkT correctly included
  dt.result[, F1 := ((r+x) * (beta_0+DkT)^s)   /  ((Bksum + alpha_0) * (s-1))]
  # F2
  dt.F2.noS <- dt.ABCD[i == max(i), list(Id, F2.noS = ((Bbar_i + Ai*(T.cal+t) )*(s-1)) / (Dbar_i + Ci*(T.cal+t) + beta_0)^s)]

  # S is different for kTTt==1 and kTTt>=2
  dt.result[dt.F2.noS, F2 := i.F2.noS + S, on = "Id"]

  dt.result[, CET :=  palive * F1 * F2]
  return(dt.result)
}
