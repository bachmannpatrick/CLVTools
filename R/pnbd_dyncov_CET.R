pnbd_dyncov_CET <- function(clv.fitted, predict.number.of.periods, prediction.end.date){

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
  dt.ABCD[i == 1,
          S := ((Ai*(T.cal*s     + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci*T.cal)^s)) -
            ((Ai*((T.cal+d)*s + 1 / Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci*(T.cal+d))^s)]


  dt.ABCD[i>1, bT_i := T.cal+d+(i-2)]
  dt.ABCD[i > 1, # also do for i==max(i), subsetting is presumably slower
          S:= ((Ai*( bT_i   *s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci* bT_i   )^s) -
            ((Ai*((bT_i+1)*s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / (Dbar_i+beta_0+Ci*(bT_i+1))^s)]

  # **ASK JEFF: how is t defined vs i? Should t==i in this case? Apparently not always the case
  dt.ABCD[i == max(i),
          S:= ((Ai*( bT_i    *s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci* bT_i    )^s)) -
            ((Ai*((T.cal+t)*s + 1/Ci*(Dbar_i+beta_0)) + Bbar_i*(s-1)) / ((Dbar_i+beta_0+Ci*(T.cal+t))^s))]

  dt.S <- dt.ABCD[, .(S = sum(S)), keyby="Id"]

  # PAlive ------------------------------------------------------------------------------------------
  dt.palive <- pnbd_dyncov_palive(clv.fitted=clv.fitted)


  # CET ---------------------------------------------------------------------------------------------

  # Merge data in single table by Id
  dt.result <- clv.fitted@cbs[, c("Id","x", "t.x", "T.cal")]
  dt.result[clv.fitted@LL.data, DkT := i.DkT, on="Id"]
  dt.result[clv.fitted@LL.data, Bksum := i.Bksum, on="Id"]
  dt.result[dt.palive, palive := i.palive, on="Id"]
  dt.result[dt.S, S := i.S, on="Id"]

  # F1
  #   Bksum has BkT correctly included
  dt.result[, F1 := ((r+x) * (beta_0+DkT)^s)   /  ((Bksum + alpha_0) * (s-1))]
  # F2
  dt.F2.noS <- dt.ABCD[i == max(i), .(Id, F2.noS = ((Bbar_i + Ai*(T.cal+t) )*(s-1)) / (Dbar_i + Ci*(T.cal+t) + beta_0)^s)]
  dt.result[dt.F2.noS, F2 := i.F2.noS + S, on = "Id"]

  dt.result[, CET :=  palive * F1 * F2]
  return(dt.result)
}
