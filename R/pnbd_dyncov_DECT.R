# Note that for a constant prediction period, the difference between DECT and DERT increases as the discount factor decreases.
pnbd_dyncov_DECT <- function(clv.fitted, predict.number.of.periods, prediction.end.date, continuous.discount.factor){

  # cran silence
  i.S <- bT_i <- T.cal <- d1 <- i <- param.s <- d1 <- delta <- Ci <- Dbar_i <- palive <- i.palive <- F1 <- S <- DECT <-  NULL
  Ai <- Dbar_i <- i.DkT  <- BkSum <- i.BkSum <- x <- DkT <- Bksum <- i.Bksum <- NULL

  clv.time <- clv.fitted@clv.data@clv.time

  # delta is the discount rate \Delta in derivations.
  delta <- continuous.discount.factor

  t <- predict.number.of.periods

  r       <- clv.fitted@prediction.params.model["r"]
  alpha_0 <- clv.fitted@prediction.params.model["alpha"]
  s       <- clv.fitted@prediction.params.model["s"]
  beta_0  <- clv.fitted@prediction.params.model["beta"]

  # This is d_1 in the formulas
  d <- clv.time.interval.in.number.tu(clv.time=clv.time,
                                      interv = interval(start = clv.time@timepoint.estimation.end,
                                                        end =  clv.time.ceiling.date(clv.time = clv.time,
                                                                                     timepoint = clv.time@timepoint.estimation.end)))

  # S ---------------------------------------------------------------------------------------------------------


  dt.ABCD <- pnbd_dyncov_ABCD(clv.fitted = clv.fitted, prediction.end.date = prediction.end.date)
  dt.ABCD[, param.s := s] # add s as vector to pass to vec_hyper2f0 function that relies on vectors
  dt.ABCD[, bT_i := T.cal + d1 + (i-2)]

  dt.ABCD[i==1,
          S := (                   .f_confhypergeo_secondkind(param.s, param.s, (delta*(Ci*T.cal      + Dbar_i + beta_0))/Ci)) -
                (exp(-delta*d1) *  .f_confhypergeo_secondkind(param.s, param.s, (delta*(Ci*(T.cal+d1) + Dbar_i + beta_0))/Ci))]

  dt.ABCD[i>1 & i!= max(i),
          S := ( exp(-delta*(d1+i-2)) * .f_confhypergeo_secondkind(param.s,param.s, (delta*(Ci* bT_i   +Dbar_i+beta_0)) / Ci)) -
                (exp(-delta*(d1+i-1)) * .f_confhypergeo_secondkind(param.s,param.s, (delta*(Ci*(bT_i+1)+Dbar_i+beta_0)) / Ci))]

  # T  = T.cal
  # T2 = T.cal + t
  # T2-T = t
  # ***TODO: Is t == max(i) really the same. It can often then result in negative expressions ***
  dt.ABCD[i== max(i),
          S := (exp(-delta*(d1+i-2)) * .f_confhypergeo_secondkind(param.s, param.s, (delta * (Ci*bT_i      + Dbar_i + beta_0)) / Ci)) -
               (exp(-delta*(t))      * .f_confhypergeo_secondkind(param.s, param.s, (delta * (Ci*(T.cal+t) + Dbar_i + beta_0)) / Ci))]

  dt.ABCD[, S := S * (Ai / (Ci^s))]
  dt.S <- dt.ABCD[, list(S = sum(S)), keyby="Id"]


  # Aggregate results ------------------------------------------------------------------------------------------------
  dt.result <- clv.fitted@cbs[, c("Id", "x")]
  dt.result[clv.fitted@LL.data, DkT := i.DkT, on="Id"]
  dt.result[clv.fitted@LL.data, Bksum := i.Bksum, on="Id"]

  dt.palive <- pnbd_dyncov_palive(clv.fitted = clv.fitted)
  dt.result[dt.palive, palive := i.palive, on = "Id"]

  dt.result[, F1 := delta^(s-1) * ((r+x)*  (beta_0+DkT)^s)   /  (Bksum + alpha_0)]
  dt.result[dt.S, S := i.S, on = "Id"]

  dt.result[, DECT := palive * F1 * S]
  setkeyv(dt.result, "Id")

  return(dt.result)
}

# The confluent hypergeometric function of the second kind as defined on:
# http://mathworld.wolfram.com/ConfluentHypergeometricFunctionoftheSecondKind.html
.f_confhypergeo_secondkind <- function (a, b, z)
{
  # Verified for a number of parameters to yield the same results as Wolfram's HypergeometricU[] and Matlab's kummerU().
  return(Re(z^(-a)* vec_gsl_hyp2f0_e(vA = a, vB = 1 + a - b, vZ = -z^(-1))$value))
}
