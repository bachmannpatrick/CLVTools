#' @importFrom utils txtProgressBar setTxtProgressBar
pnbd_dyncov_expectation <- function(clv.fitted, dt.expectation.seq, verbose, only.return.input.to.expectation=FALSE){
  # cran silence
  expectation <- exp.gX.P <- i.exp.gX.P <- exp.gX.L <- d_omega <- i.d_omega <- NULL
  i <- Ai <- Bi <- Ci <- Di <- Dbar_i <- Bbar_i <- period.num <- d1 <- NULL


  tp.last.period.end <- dt.expectation.seq[, max(period.until)]
  max.period.no      <- dt.expectation.seq[, max(period.num)]

  if(max.period.no <=2)
    stop("Have to plot at least 3 periods!", call. = FALSE)

  # For more readable code
  clv.time <- clv.fitted@clv.data@clv.time

  # Create ABCD ---------------------------------------------------------------------------------------------
  # Calculate Ai, Bbar_i, Ci, Dbar_i
  #   i is per customer, since when coming alive
  #   i=1 in period when customer turns alive
  # Upper max cov period is where the last expectation date lies in
  #   If max(dates.periods) falls directly onto the start of a covariate,
  #     the covariate is active then and is included as well.
  #     => max cov: floor_tu(max(dates.periods))

  date.last.cov  <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.last.period.end)

  l.covs <- pnbd_dyncov_alivecovariates(clv.fitted = clv.fitted, date.upper.cov = date.last.cov)
  dt.trans <- l.covs[["dt.trans"]]
  dt.life  <- l.covs[["dt.life"]]

  # Merge into single table
  dt.ABCD <- dt.life[, c("Id", "Cov.Date", "exp.gX.L")]
  setkeyv(dt.ABCD, cols = c("Id", "Cov.Date"))
  dt.ABCD[dt.trans, exp.gX.P := i.exp.gX.P, on = c("Id", "Cov.Date")]


  # . i --------------------------------------------------------------------------------------------------------
  # Number of active covariates since customer came alive
  #   = relative to when alive
  # First is the one which was active when the customer had its first transaction
  #   The data is already cut to only these dates when a customer was alive

  # Order with smallest Cov.Date up
  #   Needed here and in all following parts
  setorderv(dt.ABCD, cols = "Cov.Date", order=1L)
  # Add i per customer
  dt.ABCD[, i := seq.int(from = 1, to = .N), by="Id"]

  # Add data needed in Dbar_i and Bbar_i
  dt.ABCD[clv.fitted@cbs, d_omega := i.d_omega, on="Id"]

  # . Ai & Ci ---------------------------------------------------------------------------------------------------
  # They are both simply the gamma*cov values in the prediction period, for all Ids and dates
  dt.ABCD[, Ai := exp.gX.P]
  dt.ABCD[, Ci := exp.gX.L]

  # . Bbar_i ----------------------------------------------------------------------------------------------------

  # d1: For this case here d1 = d_omega
  dt.ABCD[, d1 := d_omega]

  dt.ABCD[,       Bbar_i := exp.gX.P]
  dt.ABCD[i == 1, Bbar_i := exp.gX.P * d1]

  # Already ordered when creating dt.ABCD
  dt.ABCD[,  Bbar_i := cumsum(Bbar_i), by="Id"]

  # At i, exp.gX.P_i is already contained through cumsum.
  #   Therefore subtract again
  # i=1: Bbar_i = 0 because (Bbar_i - exp.gX.P) + exp.gX.P * (-d1)
  dt.ABCD[, Bbar_i := (Bbar_i - exp.gX.P) + exp.gX.P * (-d1 - (i-2))]
  dt.ABCD[i == 1, Bbar_i := 0]

  # . Dbar_i ----------------------------------------------------------------------------------------------------
  dt.ABCD[,       Dbar_i := exp.gX.L]
  dt.ABCD[i == 1, Dbar_i := exp.gX.L*d_omega]

  dt.ABCD[, Dbar_i := cumsum(Dbar_i), by="Id"]

  # i=1: Dbar_i = 0 because (Dbar_i - exp.gX.L) + exp.gX.L * (-d_omega)]
  dt.ABCD[      , Dbar_i := (Dbar_i - exp.gX.L) + exp.gX.L * (-d_omega - (i-2))]
  dt.ABCD[i == 1, Dbar_i := 0]


  if(only.return.input.to.expectation){
    return(dt.ABCD)
  }

  # Do expectation -----------------------------------------------------------------------------------------------
  # For every date in the expectation table, calculate the expectation separately

  if(verbose){
    progress.bar <- txtProgressBar(max = max.period.no, style = 3)
    update.pb    <- function(n){setTxtProgressBar(pb=progress.bar, value = n)}
  }

  # For every period, do unconditional expectation (sumF)
  #   Do for loop because more expressive than doing by="period.num" in table
  for(p.no in dt.expectation.seq$period.num){
    period.until <- dt.expectation.seq[period.num == p.no, period.until]

    expectation_i <- .pnbd_dyncov_unconditionalexpectation(clv.fitted = clv.fitted,
                                                           dt.ABCD = dt.ABCD,
                                                           period.until = period.until)

    dt.expectation.seq[period.num == p.no, expectation := expectation_i]

    if(verbose)
      update.pb(p.no)
  }


  # Cumulative to incremental --------------------------------------------------------------------------
  #   First entry is already correct, because cumulative = incremental there, and cannot be
  #   infered using "diff". Therefore let first entry as is, rest is diff
  dt.expectation.seq[order(period.num, decreasing = FALSE), expectation := c(0, diff(expectation))]
  return(dt.expectation.seq)
}



# **** JEFF: t = TUs from alive until date.expectation.period.start oder date.expectation.period.end?
# **** JEFF: cut: At date.expectation.period.start oder date.expectation.period.end?
.pnbd_dyncov_unconditionalexpectation <- function(clv.fitted, dt.ABCD, period.until){

  # cran silence
  i <- Ai <- Bbar_i <- Ci <- Dbar_i <- S <- i.S <- f <- Id  <- NULL
  num.periods.alive.expectation.date <- i.num.periods.alive.expectation.date <- date.first.actual.trans <- Cov.Date <- NULL


  # Read out needed params
  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]

  # More readable code
  clv.time <- clv.fitted@clv.data@clv.time

  # Prepare dt.ABCD -------------------------------------------------------------------------------------
  #   Only alive customers

  # consider only customers alive already at expectation date
  # According to Jeff's email: (0, t_i], ie <=
  dt.alive.customers <- clv.fitted@cbs[date.first.actual.trans <= period.until,
                                       c("Id", "date.first.actual.trans")]

  # Keep only who is alive already at period.until
  dt.ABCD.alive <- dt.ABCD[dt.alive.customers, on="Id", nomatch = NULL]


  # Exact time from coming alive until end of expectation period
  dt.alive.customers[, num.periods.alive.expectation.date :=
                       clv.time.interval.in.number.tu(clv.time=clv.time,
                                                      interv=interval(start = date.first.actual.trans,
                                                                      end   = period.until))]
  # Add to every cov period
  dt.ABCD.alive[dt.alive.customers, num.periods.alive.expectation.date := i.num.periods.alive.expectation.date,
                on = "Id"]

  # Cut data to maximal range
  # Consider all covariates which are active before and during the period for which the expectation is
  #   calculated (incl / <= because period.until is the beginning of the covariate period)
  dt.ABCD.alive <- dt.ABCD.alive[Cov.Date <= period.until]


  # S --------------------------------------------------------------------------------------------------------
  dt.S <- .pnbd_dyncov_unconditionalexpectation_alive_customers_S(dt.ABCD.alive = dt.ABCD.alive, s=s, beta_0 = beta_0)


  # F --------------------------------------------------------------------------------------------------------

  # Add everything else needed
  #   For all customers Ak0t/Bk0t/Ck0t/Dk0t is the last ABCD value (with max(i) where max(Cov.Date))
  dt.ABCD_k0t <- dt.ABCD.alive[Cov.Date == max(Cov.Date),
                               list(Id, A_k0t=Ai, Bbar_k0t=Bbar_i, C_k0t=Ci, Dbar_k0t=Dbar_i, i)]
  dt.alive.customers <- dt.alive.customers[dt.ABCD_k0t, on = "Id"]
  dt.alive.customers[dt.S, S := i.S, on = "Id"]


  dt.f <- .pnbd_dyncov_unconditionalexpectation_alive_customers_f(dt.alive.customers=dt.alive.customers, r=r, alpha_0 = alpha_0, s=s, beta_0=beta_0)

  return(dt.f[, sum(f)])
}


.pnbd_dyncov_unconditionalexpectation_alive_customers_f <- function(dt.alive.customers, r, alpha_0, s, beta_0){
  f <- only.alive.in.1.period <- NULL
  num.periods.alive.expectation.date <- A_k0t <- C_k0t <- Dbar_k0t <- only.alive.in.1.period <- S <- NULL

  # Only alive for 1 period is a special case
  #   Mark who is alive for only one period
  #     dt.alive.customers is at max(Cov.Date) for every customer and hence i == max(i) and only one entry per customer
  dt.alive.customers[, only.alive.in.1.period := i == 1]
  # dt.alive.customers[, only.alive.in.1.period := num.periods.alive.expectation.date <= 1]

  # F value
  #   t is exact (partial) time from alive until expectation end
  #   (Bk0tbar + t.customer*Ak0t) == (Bbar_i + t.customer*Ai) == Bi, which is needed, and not Bbar_i
  #   analogously for Di
  dt.alive.customers[, f := ((beta_0)^s * r )/ ((s-1) * alpha_0)]
  dt.alive.customers[only.alive.in.1.period == TRUE,
                     f := f * ((A_k0t*num.periods.alive.expectation.date*(s-1)) / (beta_0+C_k0t*num.periods.alive.expectation.date)^s + (A_k0t/C_k0t)/beta_0^(s-1) -
                                 (A_k0t*(num.periods.alive.expectation.date*s + 1/C_k0t*beta_0))/(beta_0+C_k0t*num.periods.alive.expectation.date)^s)]
  dt.alive.customers[only.alive.in.1.period == FALSE,
                     # f * (. +S)
                     f := f * ( (((A_k0t*num.periods.alive.expectation.date+Bbar_k0t) *(s-1)) /
                                   (beta_0 + (C_k0t*num.periods.alive.expectation.date + Dbar_k0t))^s) + S)]

  return(dt.alive.customers[, list(Id, f)])
}


.pnbd_dyncov_unconditionalexpectation_alive_customers_S <- function(dt.ABCD.alive, s, beta_0){
  S <- Cov.Date <- num.periods.alive.expectation.date <- Ai <- Bbar_i <- Ci <- Dbar_i <- d1 <- i <- NULL
  # S_i is relative to when alive, ie by i
  # d1 is first.purchase until ceiling_tu(first.purchase) = d_omega
  #   Already added for Bbar_i

  # helper to calculate S value by:
  #   s.fct.expectation(term.1) - s.fct.expectation(term.2)
  # A = Ai, B = Bbar_i, C = Ci, D = Dbar_i
  s.fct.expectation <- function(term, A, B, C, D, beta_0, s){
    return( (A * (term * s + 1/C * (beta_0+D)) + B*(s-1)) / (beta_0 + D + C * term)^s )
  }

  # term 1 = 0 (yes!), term 2 = d
  dt.ABCD.alive[i==1,
                S:= s.fct.expectation(term = 0, A=Ai, B=Bbar_i,C=Ci,D=Dbar_i,beta_0=beta_0,s=s) -
                  s.fct.expectation(term = d1, A=Ai, B=Bbar_i,C=Ci,D=Dbar_i,beta_0=beta_0,s=s)]

  dt.ABCD.alive[i>1,
                S:= s.fct.expectation(term = (d1 + i - 2), A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s) -
                  s.fct.expectation(term = (d1 + i - 1), A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s)]

  # Last = max(i) is per customer, but only after cutting to expectation date!
  #   After cutting to expectation date, all have the same max date!
  # **JEFF: Wird davon ausgegangen, dass num.periods.alive.expectation.date > i ist?
  # **JEFF: For first period d1+i-2 is negative..? or exactly -d1 and num.periods.alive.expectation.date = d1, hence = 0?
  dt.ABCD.alive[Cov.Date == max(Cov.Date),
                S := s.fct.expectation(term = (d1 + i - 2), A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s) -
                  s.fct.expectation(term = num.periods.alive.expectation.date, A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s)]

  # S may be NA for customers alive only for <=1 period.
  #   Their f value is calculated without S then
  dt.S <- dt.ABCD.alive[, list(S = sum(S)), keyby="Id"]

  return(dt.S)
}



pnbd_dyncov_newcustomer_expectation <- function(clv.fitted, t, tp.first.transaction, dt.cov.life, dt.cov.trans){
  Cov.Date <- exp.gX <- exp.gX.P <- i.exp.gX <- i <- d_omega <- Ai <- Ci <- d1 <- Bbar_i <- Dbar_i <- NULL
  Id <- .N <- S <- i.S <- num.periods.alive.expectation.date <- NULL

  # TODO[test]: If covs are static, the date of transaction should not matter (same outcome regardless of tp.first.transaction)
  # TODO[test]: Compare against dt.ABCD in expectation
  # TODO[test]: Test that covariates before first transaction are cut off
  # TODO[test]: Test that covariates after prediction end are cut off in dt.ABCD.alive
  # TODO[test]: Test that i starts (and i==1) in the period of the first transaction

  r       <- clv.fitted@prediction.params.model[["r"]]
  alpha_0 <- clv.fitted@prediction.params.model[["alpha"]]
  s       <- clv.fitted@prediction.params.model[["s"]]
  beta_0  <- clv.fitted@prediction.params.model[["beta"]]

  # readability
  clv.time <- clv.fitted@clv.data@clv.time


  # We cannot use the preparation from the ordinary expectation because too many things
  # are relying on clv.data and the cbs. Therefore, re-implement (copy paste) and adapt were necessary



  # Create ABCD ---------------------------------------------------------------------------------------------
  # pnbd_dyncov_alivecovariates(): Uses date.first.transaction from cbs
  # Instead of using pnbd_dyncov_alivecovariates(), only use the relevant parts: Calculating exp.gX.P and exp.gX.L.

  tp.first.cov <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.first.transaction)
  dt.cov.life  <- dt.cov.life[Cov.Date >= tp.first.cov]
  dt.cov.trans <- dt.cov.trans[Cov.Date >= tp.first.cov]


  dt.cov.life <- pnbd_dyncov_add_expgX(
    dt.cov=dt.cov.life,
    names.cov=clv.fitted@clv.data@names.cov.data.life,
    params.cov=clv.fitted@prediction.params.life)

  dt.cov.trans <- pnbd_dyncov_add_expgX(
    dt.cov=dt.cov.trans,
    names.cov=clv.fitted@clv.data@names.cov.data.trans,
    params.cov=clv.fitted@prediction.params.trans)

  # Merge into single table
  #   there are no Ids
  dt.ABCD <- dt.cov.life[, list(Cov.Date, exp.gX.L=exp.gX)]
  dt.ABCD[dt.cov.trans, exp.gX.P := i.exp.gX, on = "Cov.Date"]



  # . copied ----------------------------------------------------------------------------------------------
  # The following parts are copied from pnbd_dyncov_expectation().
  # See comments there, they are removed here to improve maintainability

  # There is no cbs entry for new customers, hence no d_omega.
  # d_omega is not known for a new customer. Assume it to be 0

  # . i
  setorderv(dt.ABCD, cols = "Cov.Date", order=1L)
  dt.ABCD[, i := seq.int(from = 1, to = .N)]  # remove by="Id"

  # d_omega: Not read from cbs but calculated from given timepoint of first transaction
  dt.ABCD[, d_omega := pnbd_dyncov_walk_d(clv.time=clv.time, tp.relevant.transaction = tp.first.transaction)]

  # . Ai & Ci
  dt.ABCD[, Ai := exp.gX.P]
  dt.ABCD[, Ci := exp.gX.L]

  # . Bbar_i
  dt.ABCD[, d1 := d_omega]
  dt.ABCD[,       Bbar_i := exp.gX.P]
  dt.ABCD[i == 1, Bbar_i := exp.gX.P * d1]
  dt.ABCD[,  Bbar_i := cumsum(Bbar_i)]
  dt.ABCD[, Bbar_i := (Bbar_i - exp.gX.P) + exp.gX.P * (-d1 - (i-2))]
  dt.ABCD[i == 1, Bbar_i := 0]

  # . Dbar_i
  dt.ABCD[,       Dbar_i := exp.gX.L]
  dt.ABCD[i == 1, Dbar_i := exp.gX.L*d_omega]
  dt.ABCD[, Dbar_i := cumsum(Dbar_i)]
  dt.ABCD[      , Dbar_i := (Dbar_i - exp.gX.L) + exp.gX.L * (-d_omega - (i-2))]
  dt.ABCD[i == 1, Dbar_i := 0]

  print("dt.ABCD prepared")
  print(dt.ABCD)

  # . alive --------------------------------------------------------------------------------------------------
  # There is no need to select out customers which are alive as there is only 1 single customer
  # which by definition is alive


  # Cut data to maximal range as is done in the expectation loop (period.until)
  # Consider all covariates which are active before and during the period for which the expectation is
  #   calculated (incl / <= because Cov.Date is the beginning of the covariate period)
  tp.cov.until <- tp.first.transaction + clv.time.number.timeunits.to.timeperiod(clv.time = clv.time, user.number.periods = t)
  dt.ABCD <- dt.ABCD[Cov.Date <= tp.cov.until]

  # Calculating S and f requires a Id and num.periods.alive.expectation.date
  dt.ABCD[, Id := "<placeholder>"]
  dt.ABCD[, num.periods.alive.expectation.date := t]

  print("dt.ABCD alive")
  print(dt.ABCD)

  # S --------------------------------------------------------------------------------------------------------
  dt.S <- .pnbd_dyncov_unconditionalexpectation_alive_customers_S(dt.ABCD.alive = dt.ABCD, s=s, beta_0 = beta_0)
  dt.ABCD[dt.S, S := i.S, on = "Id"]


  # F --------------------------------------------------------------------------------------------------------
  # Add everything else needed
  #   For all customers Ak0t/Bk0t/Ck0t/Dk0t is the last ABCD value (with max(i) where max(Cov.Date))
  dt.ABCD_k0t <- dt.ABCD[Cov.Date == max(Cov.Date),
                         list(Id, A_k0t=Ai, Bbar_k0t=Bbar_i, C_k0t=Ci, Dbar_k0t=Dbar_i, i, num.periods.alive.expectation.date, S)]

  dt.f <- .pnbd_dyncov_unconditionalexpectation_alive_customers_f(dt.alive.customers=dt.ABCD_k0t, r=r, alpha_0 = alpha_0, s=s, beta_0=beta_0)

  return(dt.f$f)
}
