pnbd_dyncov_pmf_hyp2f1 <- function(a, b, c, z){
  return(vec_gsl_hyp2f1_e(a, b, c, z)$value)
}


pnbd_dyncov_pmf_A_i <- function(i, dt.data.period.customer){
  exp.gX.P <- Date <- NULL

  # According to Patrick: i refers to offset inside period u,tu
  # dt.A <- dt.data[order(Date)][seq.int(from=1, to=i)]
  # dt.A[, A := exp.gX.P]
  # Ai <- dt.A[, sum(A)]
  # Ai <- dt.data.period.customer[order(Date)][seq.int(from=1, to=i), sum(exp.gX.L)]
  Ai <- dt.data.period.customer[order(Date)][i, sum(exp.gX.P)]
  return(Ai)
}

pnbd_dyncov_pmf_C_i <- function(i, dt.data.period.customer){
  Date <- exp.gX.L <- NULL

  # Ci = exp(gX_{k0,u+i-1}) = defines what period?
  # According to Patrick: i refers to offset inside period u,tu
  # dt.C <- dt.data[Date>=date.u & Date<=date.tu][order(Date)][seq.int(from=1, to=i)]
  # dt.C[, Ci := exp.gX.L]
  # Ci <- dt.C[, sum(Ci)]

  # Ci <- dt.data.period.customer[order(Date)][seq.int(from=1, to=i), sum(exp.gX.L)]
  Ci <- dt.data.period.customer[order(Date)][i, sum(exp.gX.L)]
  return(Ci)
}

pnbd_dyncov_pmf_Bbar_i <- function(i, dt.data.period.customer){
  Date <- exp.gX.P <- Bbar <- d1 <- ui <- NULL

  # Formula: Bbar_i= exp(gamma1*X1)d1+sum_l=2_i-1{exp(gamma1*Xl)}+exp(gamma1*Xi)[-u-d1-I(i>=2)(i-2)], i=1...k_utu
  # Only do additional multiplications to 1 and k_utu

  # According to Patrick: These are relativ notation, i referring to inside (u, t].
  dt.Bbar <- dt.data.period.customer[order(Date)][seq.int(from=1, to=i)]
  # cut to i!
  dt.Bbar[, Bbar := exp.gX.P]
  # B1 is first period
  dt.Bbar[Date == min(Date), Bbar := Bbar * d1]

  if(i >= 2){
    dt.Bbar[Date == max(Date), Bbar := Bbar * (-ui-d1-(i-2))]
  }else{
    dt.Bbar[Date == max(Date), Bbar := Bbar * (-ui-d1)]
  }

  Bbar_i <- dt.Bbar[, sum(Bbar)]
  return(Bbar_i)
}

pnbd_dyncov_pmf_Dbar_i <- function(i, dt.data.period.customer, dt.data.since.alive.customer){ # dt.data.all.periods.customer
  k0u <- exp.gX.L <- Date <- Dbar <- d_omega <- NULL

  # any k0u works, they are all the same
  k0u <- dt.data.period.customer[1, k0u]
  # i = 1,..., k_u,tu
  # **k0u+i = what?? Assumption: From alive customer until end of period (u, t+u).
  #   ??ast entry of dt.Dbar should be the one to multiply with (*domega)
  # dt.Dbar <- dt.data.period.customer[order(Date)][seq.int(from=1,to=i)]
  dt.Dbar <- dt.data.since.alive.customer[order(Date)][seq.int(from = 1, to=i+k0u-1)]
  dt.Dbar[, Dbar := exp.gX.L]
  dt.Dbar[Date == min(Date), Dbar := Dbar*d_omega]


  if(k0u+i >= 3)
    dt.Dbar[Date == max(Date), Dbar := Dbar *(-d_omega-(k0u + i - 3))]
  else
    dt.Dbar[Date == max(Date), Dbar := Dbar *(-d_omega)]

  Dbar_i <- dt.Dbar[, sum(Dbar)]

  return(Dbar_i)
}

pnbd_dyncov_pmf_b0_i <-function(d1, i){
  if(i<2){
    stop("i < 2 in pnbd_dyncov_pmf_b0_i!")
  }

  return(d1+(i-2))
}


pnbd_dyncov_pmf_bu_i <- function(ui, i, d1){
  # p.60
  # bu_i = u+d1+i-2, i=2,...,k_u,tu
  if(i < 2){
    stop("i may not be < 2 in bu_i")
  }

  return(ui+d1+i-2)
}

# dt.data with both (L & P) dyn cov params and data
pnbd_dyncov_pmf_S1_per_customer <- function(dt.data.period.customer,
                                            dt.data.since.alive.customer,
                                            x, a, b, r, s,
                                            t){
  ui <- NULL

  ui <- dt.data.period.customer[1, ui]
  # p. 61
  i.kutu <- nrow(dt.data.period.customer) # I[Date == max(Date)]

  A_kutu <- pnbd_dyncov_pmf_A_i(i=i.kutu, dt.data.period.customer = dt.data.period.customer)
  C_kutu <- pnbd_dyncov_pmf_C_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer)

  Bbar_kutu <- pnbd_dyncov_pmf_Bbar_i(i=i.kutu, dt.data.period.customer = dt.data.period.customer)
  B_kutu <- A_kutu*(t+ui) + Bbar_kutu

  # ** what is u,tu? wrong spelling of k_{u,tu}?
  Dbar_utu <- pnbd_dyncov_pmf_Dbar_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer,
                                     dt.data.since.alive.customer=dt.data.since.alive.customer)
  D_kutu <- C_kutu*(t+ui)+Dbar_utu

  # Cannot calculate: (D_kutu+b)^s

  S1 <- (B_kutu^x / factorial(x)) * (gamma(x+r) / gamma(r)) * ((a^r* b^s) / ((B_kutu+a)^(x+r)*(D_kutu+b)^s))
  return(S1)
}

pnbd_dyncov_pmf_S2_1j_per_customer <- function(dt.data.period.customer, j,
                                               dt.data.since.alive.customer,
                                               x, a, b, r, s){
  ui <- d1 <- NULL

  A1 <- pnbd_dyncov_pmf_A_i(i=1, dt.data.period.customer=dt.data.period.customer)
  C1 <- pnbd_dyncov_pmf_C_i(i=1, dt.data.period.customer=dt.data.period.customer)

  Bbar_1 <- pnbd_dyncov_pmf_Bbar_i(i = 1, dt.data.period.customer = dt.data.period.customer)
  Dbar_1 <- pnbd_dyncov_pmf_Dbar_i(i = 1, dt.data.period.customer = dt.data.period.customer,
                                   dt.data.since.alive.customer=dt.data.since.alive.customer)

  # p.60
  # bu_i = u+d1+i-2, i=2,...,k_u,tu
  # any d1 works, all the same
  ui <- dt.data.period.customer[1, ui]
  d1 <- dt.data.period.customer[1, d1]
  bu2 <- pnbd_dyncov_pmf_bu_i(ui = ui, i = 2, d1 = d1)

  # I{Bbar1+alpha > (Dbar1+beta)*A1/C1}
  if((Bbar_1+a) > (Dbar_1+b)*A1/C1){

    fct.greater <- function(u_part, n){
      ((u_part^n*a^r*b^s) / (Bbar_1+A1*u_part+a)^(r+s+j+n)) *
        ((gamma(r+s+j+n))/(gamma(r)*gamma(s))) *
        ((A1 ^(s-x+j+n))/(C1^(s+1))) *
        ((gamma(s+1)*gamma(r+x)) / (gamma(r+s+x+1))) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, s+1, r+s+x+1, (Bbar_1+a-(Dbar_1+b)*(A1/C1)) / (Bbar_1+A1*u_part+a))
    }

    fct.sum <- function(n){
      ((1 / factorial(n))* (fct.greater(u_part = ui, n=n) - fct.greater(u_part = bu2,n=n)))
    }


    S2_1j <- (Bbar_1^j*A1^(x-j) * C1 / factorial(j)) * sum(sapply(seq(from=0,to=(x-j)), fct.sum))
    return(S2_1j)

  }else{
    fct.smaller <- function(u_part, n){
      ((u_part^n*a^r*b^s) / ((Dbar_1+C1*u_part+b)*A1/C1)^(r+s+j+n)) *
        ((gamma(r+s+j+n))/(gamma(r)*gamma(s))) *
        ((A1 ^(s-x+j+n))/(C1^(s+1))) *
        ((gamma(s+1)*gamma(r+x)) / (gamma(r+s+x+1))) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, r+x, r+s+x+1, (((Dbar_1+b)*A1/C1)-(Bbar_1+a)) / ((Dbar_1+C1*u_part+b)*A1/C1))
    }

    fct.sum <- function(n){1/factorial(n) *
        (fct.smaller(u_part = ui, n=n) - fct.smaller(u_part = bu2, n=n))}

    S2_1j <- (Bbar_1^j*A1^(x-j) * C1 / factorial(j) * sum(sapply(seq(from=0, to=(x-j)), fct.sum)))
    return(S2_1j)
  }
}


pnbd_dyncov_pmf_S2_ij_per_customer <- function(dt.data.period.customer, i,j,
                                               dt.data.since.alive.customer,
                                               x, a, b, r, s){
  ui <- d1 <- NULL

  Ai <- pnbd_dyncov_pmf_A_i(i=i, dt.data.period.customer=dt.data.period.customer)
  Ci <- pnbd_dyncov_pmf_C_i(i=i, dt.data.period.customer=dt.data.period.customer)

  Bbar_i <- pnbd_dyncov_pmf_Bbar_i(i=i, dt.data.period.customer = dt.data.period.customer)
  Dbar_i <- pnbd_dyncov_pmf_Dbar_i(i=i, dt.data.period.customer = dt.data.period.customer,
                                   dt.data.since.alive.customer = dt.data.since.alive.customer)

  # d1 can be any, all the same
  ui <- dt.data.period.customer[1, ui]
  d1 <- dt.data.period.customer[1, d1]
  bu_i <- pnbd_dyncov_pmf_bu_i(ui = ui, d1 = d1, i = i)


  if(Bbar_i+a > (Dbar_i+b)*Ai/Ci){
    fct.greater <- function(u_part, n){
      ((u_part^n*a^r*b^s) / (Bbar_i + Ai*u_part+a)^(r+s+j+n)) *
        (gamma(r+s+j+n) / (gamma(r)*gamma(s))) *
        (Ai^(s-x+j+n)/Ci^(s+1)) *
        ((gamma(s+1)*gamma(r+x))/gamma(r+s+x+1)) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, s+1,r+s+x+1, (Bbar_i+a-(Dbar_i+b)*Ai/Ci)/(Bbar_i+Ai*u_part+a))
    }

    fct.sum <- function(n){
      1/factorial(n) * (fct.greater(u_part = bu_i, n=n) - fct.greater(u_part=(bu_i+1), n=n))}

    S2_ij <- Bbar_i^j * Ai^(x-j)*Ci / factorial(j) * sapply(seq(from=0, to=(x-j)), fct.sum)

    return(S2_ij)
  }else{

    fct.smaller <- function(u_part, n){
      ((u_part^n*a^r*b^s) / ((Dbar_i+Ci*u_part+b)*Ai/Ci)^(r+s+j+n)) *
        (gamma(r+s+j+n)/(gamma(r)*gamma(s))) *
        ((Ai^(s-x+j+n)) / (Ci^(s+1))) *
        ((gamma(s+1)*gamma(r+x)) / gamma(r+s+x+1)) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, r+x, r+s+x+1, (((Dbar_i+b)*Ai/Ci - (Bbar_i+a)) / ((Dbar_i+Ci*u_part+b)*Ai/Ci)))
    }

    fct.sum <- function(n){
      1/factorial(n) * (fct.smaller(u_part = bu_i, n=n) - fct.smaller(u_part=(bu_i+1), n=n))}
    S2_ij <- Bbar_i^j * Ai^(x-j)*Ci / factorial(j) * sapply(seq(from=0, to=(x-j)), fct.sum)

    return(S2_ij)
  }
}

pnbd_dyncov_pmf_S2_kutuj_per_customer <- function(dt.data.period.customer, j,
                                                  dt.data.since.alive.customer,
                                                  x, r, a, s, b, t){
  ui <- d1 <- NULL

  i.kutu <- nrow(dt.data.period.customer)

  A_kutu    <- pnbd_dyncov_pmf_A_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer)
  C_kutu    <- pnbd_dyncov_pmf_C_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer)

  Bbar_kutu <- pnbd_dyncov_pmf_Bbar_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer)
  Dbar_kutu <- pnbd_dyncov_pmf_Dbar_i(i = i.kutu, dt.data.period.customer = dt.data.period.customer,
                                      dt.data.since.alive.customer = dt.data.since.alive.customer)

  # Any d1 is ok, they are all the same
  ui <- dt.data.period.customer[1, ui]
  d1 <- dt.data.period.customer[1, d1]
  bu_kutu <- pnbd_dyncov_pmf_bu_i(ui = ui, d1 = d1, i = i.kutu)

  if((Bbar_kutu+a) > ((Dbar_kutu+b)*A_kutu/C_kutu) ){

    fct.greater <- function(u_part, n){
      ((u_part^n*a^r*b^s)/(Bbar_kutu+A_kutu*u_part+a)^(r+s+j+n)) *
        ((gamma(r+s+j+n))/(gamma(r)*gamma(s))) *
        ((A_kutu^(s-x+j+n)) / (C_kutu^(s+1))) *
        ((gamma(s+1)*gamma(r+x)) / gamma(r+s+x+1)) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, s+1, r+s+x+1, (Bbar_kutu+a-(Dbar_kutu+b)*A_kutu/C_kutu)/(Bbar_kutu+A_kutu*u_part+a))
    }

    fct.sum <- function(n){1/factorial(n) * (fct.greater(u_part=bu_kutu, n=n) - fct.greater(u_part = t+ui,n=n))}

    S2_kutuj <- Bbar_kutu^j*A_kutu^(x-j)*C_kutu/factorial(j) * sum( sapply(seq.int(from=0, to=x-j), fct.sum) )
    return(S2_kutuj)

  }else{
    fct.smaller <- function(u_part, n){
      ((u_part^n*a^r*b^s) / (((Dbar_kutu+C_kutu*u_part+b)*A_kutu/C_kutu)^(r+s+j+n))) *
        ((gamma(r+s+j+n)/(gamma(r)*gamma(s)))) *
        (A_kutu^(s-x+j+n)/C_kutu^(s+1)) *
        ((gamma(s+1)*gamma(r+x)) / gamma(r+s+x+1)) *
        pnbd_dyncov_pmf_hyp2f1(s+r+j+n, r+x, r+s+x+1, ((Dbar_kutu+b)*A_kutu/C_kutu - (Bbar_kutu+a))/((Dbar_kutu+C_kutu*u_part+b)*A_kutu/C_kutu))
    }

    fct.sum <- function(n){1/factorial(n) * (fct.smaller(u_part=bu_kutu, n=n) - fct.smaller(u_part = t+ui,n=n))}

    S2_kutuj <- Bbar_kutu^j*A_kutu^(x-j)*C_kutu/factorial(j) * sum(sapply(seq.int(from=0, to=(x-j)), fct.sum))
    return(S2_kutuj)
  }
}


pnbd_dyncov_pmf_per_customer <- function(dt.data.period.customer,
                                         dt.data.since.alive.all,
                                         x, a, b, r, s, t){
  Id <- NULL

  dt.data.since.alive.customer <- unique(dt.data.since.alive.all[Id == dt.data.period.customer[1,"Id"]])

  S1 <- pnbd_dyncov_pmf_S1_per_customer(dt.data.period.customer = dt.data.period.customer, x=x, a=a, b=b,r=r, s=s, t=t,
                                        dt.data.since.alive.customer = dt.data.since.alive.customer)

  fct.sum.s2.ij <- function(i, j){
    S2_ij <- pnbd_dyncov_pmf_S2_ij_per_customer(i=i,j=j,dt.data.period.customer = dt.data.period.customer,
                                                dt.data.since.alive.customer = dt.data.since.alive.customer,
                                                x = x, a = a, b = b, r = r, s = s)
    return(S2_ij)
  }


  i.kutu <- nrow(dt.data.period.customer)

  fct.sum.j <- function(j){
    S2_1j <- pnbd_dyncov_pmf_S2_1j_per_customer(j = j,dt.data.period.customer = dt.data.period.customer,
                                                dt.data.since.alive.customer = dt.data.since.alive.customer,
                                                x = x, a = a, b = b, r = r, s = s)
    S2_kutu_j <- pnbd_dyncov_pmf_S2_kutuj_per_customer(j = j, dt.data.period.customer = dt.data.period.customer,
                                                       dt.data.since.alive.customer = dt.data.since.alive.customer,
                                                       x = x, r=r, a = a, s = s, b = b, t = t)
    S2_ij_sum <- sum(sapply(seq.int(from=2, to=i.kutu-1), fct.sum.s2.ij, j=j))

    return(S2_1j + S2_kutu_j + S2_ij_sum)
  }

  S2_sums <- sum(sapply(seq.int(from=0, to = x), fct.sum.j))
  return(S1 + S2_sums)
}

pnbd_dyncov_pmf_r <- function(object, x){
  Date <- exp.gX.L <- exp.gX.P <- i.exp.gX.L <- i.exp.gX.P <- Date.first.trans <- date.tu <- Date.next.cov <- NULL
  tmp.Date <- Date.next.covariate.after.first.trans <- d_omega <- date.u <- i.Date.first.trans <- is.alive <- NULL
  k0u <- ui <- d.i <- pmf <- Id <- d1 <- N <- NULL

  #extract the start and end date of the estimation period
  date.u <- object@clv.data@clv.time@timepoint.estimation.start
  date.tu <- object@clv.data@clv.time@timepoint.estimation.end

  # extract the base model parameters
  r <- object@prediction.params.model[["r"]]
  a <- object@prediction.params.model[["alpha"]]
  s <- object@prediction.params.model[["s"]]
  b <- object@prediction.params.model[["beta"]]

  # extract the covariate model parameters
  vec.gamma.trans <- object@prediction.params.trans
  vec.gamma.life <- object@prediction.params.life

  cols.data.trans <- object@names.original.params.free.trans
  cols.data.life <- object@names.original.params.free.life

  #extract transactional data
  dt.transactions <- copy(object@clv.data@data.transactions)
  dt.transactions[, Date := as.Date(Date)]

  # extract covariate data
  dt.data.cov.life <- copy(object@clv.data@data.cov.life)
  dt.data.cov.trans <- copy(object@clv.data@data.cov.trans)
  setnames(dt.data.cov.life, "Cov.Date", "Date")
  setnames(dt.data.cov.trans, "Cov.Date", "Date")
  dt.data.cov.life[, Date := as.Date(Date)]
  dt.data.cov.trans[, Date := as.Date(Date)]

  # set the time unit used
  clv.time <- object@clv.data@clv.time

  date.estimation.start <- as.Date(dt.transactions[, min(Date)])


  # Reduce to relevant columns in right order
  dt.data.cov.life  <- dt.data.cov.life[,  .SD, .SDcols = c("Id", "Date", cols.data.life)]
  dt.data.cov.trans <- dt.data.cov.trans[, .SD, .SDcols = c("Id", "Date", cols.data.trans)]

  # Calculate all exp(gamma%*%X) already here so need less data, make less errors,
  # and can use less variables
  # use sum(g*X) because matrix mult (%*%) does not work on data frames
  # because sum(vec.gamma * .SD), by=c("Id", "Date") is extreeeemely slow, add gammas as columns
  # and then do data.frame on data.frame element wise multiplication and row-wise summing
  # dt.data.cov.life[, exp.gX.L := sum(vec.gamma.life * .SD), .SDcols=cols.data.life, by=c("Id", "Date")]

  # bring gammas to same order as columns
  vec.gamma.life  <- vec.gamma.life[cols.data.life]
  vec.gamma.trans <- vec.gamma.trans[cols.data.trans]

  # rename with prefix to not have duplicate names (same as cov data)
  cols.gamma.life  <- paste0("life.gamma.",  names(vec.gamma.life ))
  cols.gamma.trans <- paste0("trans.gamma.", names(vec.gamma.trans))

  # Add gammas to tables
  dt.data.cov.life[,  (cols.gamma.life)  := as.data.table(t(vec.gamma.life))]
  dt.data.cov.trans[, (cols.gamma.trans) := as.data.table(t(vec.gamma.trans))]

  # multiply gammas with cov data and sum (= matrix multiplication)
  dt.data.cov.life[,  exp.gX.L := exp(rowSums(dt.data.cov.life[,  .SD, .SDcols = cols.gamma.life]  * dt.data.cov.life[,  .SD, .SDcols = cols.data.life]))]
  dt.data.cov.trans[, exp.gX.P := exp(rowSums(dt.data.cov.trans[, .SD, .SDcols = cols.gamma.trans] * dt.data.cov.trans[, .SD, .SDcols = cols.data.trans]))]


  # Reduce in a single DT to the relevant columns only: Id, Date, exp.gX.L, exp.gX.P
  setkeyv(dt.data.cov.life,  c("Id", "Date"))
  setkeyv(dt.data.cov.trans, c("Id", "Date"))
  dt.data <- unique(dt.data.cov.life[, c("Id", "Date")])
  # add gamma*cov to id and dates
  dt.data[dt.data.cov.life,  exp.gX.L := i.exp.gX.L, on=c("Id", "Date")]
  dt.data[dt.data.cov.trans, exp.gX.P := i.exp.gX.P, on=c("Id", "Date")]

  # only who had first transaction by end of interval (at date.tu)
  dt.first.transactions    <- dt.transactions[, list(Date.first.trans = min(Date)), by="Id"]
  dt.first.trans.before.tu <- dt.first.transactions[Date.first.trans < date.tu, "Id"]

  if(nrow(dt.first.trans.before.tu) == 0){
    stop("Nobody had first transaction by then!")
  }

  # remove customers which were not alive in period
  # (=only keep such that had before date.tu)
  dt.data <- dt.data[dt.first.trans.before.tu, on="Id", nomatch=0]

  # last.transaction.before.cov
  # - d1: fractional part from customer's very first transaction to next full tu = next covariates
  # - d_omega: fractional part from u until next full tu = ceiling_date
  #             (ie to make up the difference if transaction is inbetween covariates)
  #

  # Try to match on Covariate data on Date.first.trans. If no exact match, roll next covariate Date back
  # For rolling, Date HAS to be last!
  # rolls the whole row, but keeps the original value of the "on" columns.
  #   Therefore needs to be same value but in different column
  dt.data[, tmp.Date := Date]
  dt.next.cov <- dt.data[dt.first.transactions, list(Id, Date.next.cov = tmp.Date),
                         on=c("Id", "Date"="Date.first.trans"), roll=-Inf]
  dt.data[, tmp.Date := NULL]
  dt.first.transactions[dt.next.cov, Date.next.covariate.after.first.trans := Date.next.cov, on="Id"]
  dt.first.transactions[, d1 := clv.time.interval.in.number.tu(clv.time,
                                                               interval(start=Date.first.trans,
                                                                        end=Date.next.covariate.after.first.trans))]
  dt.data[dt.first.transactions, d1 := d1, on = "Id"]

  dt.first.transactions[, date.u := date.u]
  dt.first.transactions[, d_omega := clv.time.interval.in.number.tu(clv.time,
                                                                    interval(start=date.u,
                                                                             end=clv.time.ceiling.date(clv.time, date.u)))]

  dt.data[dt.first.transactions, d_omega := d_omega, on = "Id"]

  if(dt.data[, any(d1>=1)]){
    stop("d1 is too large!")
  }
  if(dt.data[, any(d1<0)]){
    stop("d1 is too small!")
  }

  if(dt.data[, any(d_omega>=1)]){
    stop("d_omega is too large!")
  }
  if(dt.data[, any(d_omega<0)]){
    stop("d_omega is too small!")
  }

  if(any(dt.data < 0)){
    stop("Literally all entries need to be > 0!")
  }

  # Cut to period only after calculating other variables

  # verify correctness
  if(anyNA(dt.data)){
    stop("NA in data")
  }
  # verify exactly same number of covs for every customer
  if(dt.data[, .N, by="Id"][, uniqueN(N)] != 1){
    stop("Not all customers have same data!")
  }
  if(dt.data[, uniqueN(Id)] != nrow(dt.first.trans.before.tu)){
    stop("some customers got lost..?")
  }


  dt.data.alive.forDi <- copy(dt.data)
  dt.data.alive.forDi <- dt.data.alive.forDi[ Date <= clv.time.floor.date(clv.time, date.tu)]
  dt.data.alive.forDi[dt.first.transactions, Date.first.trans:=i.Date.first.trans, on="Id"]
  dt.data.alive.forDi[, is.alive := Date >= clv.time.floor.date(clv.time, Date.first.trans)]
  dt.data.alive.forDi <- dt.data.alive.forDi[is.alive == TRUE]

  # Do PMF for all customers that are alive before date.u
  # For customers only turning alive in (u, t+u]:
  #   loop over period (u, tu) and see who becomes alive then
  #     the PMF() already does all periods withing, no need for extra loop. Only needs the data
  #   calculate pmf for these customers that become alive at this point and
  #   only over the still available periods

  # Only for customers alive already:
  dt.alive.cust <- copy(dt.first.transactions[Date.first.trans <= date.u, ])
  dt.data.alive <- dt.data[dt.alive.cust[, "Id"], on="Id", nomatch=NULL]

  # Only (u, t+u] period
  dt.data.alive <- dt.data.alive[Date>=clv.time.floor.date(clv.time, date.u) &
                                   Date <= clv.time.floor.date(clv.time, date.tu)]
  setkeyv(dt.data, c("Id", "Date"))

  # k0u: Number of time units between customer's first transaction and beginning of (u,u+t]
  dt.alive.cust[, date.u := date.u]
  dt.alive.cust[, k0u := clv.time.interval.in.number.tu(clv.time,
                                                        interval(end = clv.time.ceiling.date(clv.time, date.u),
                                                                 start = clv.time.floor.date(clv.time, Date.first.trans)))]
  dt.alive.cust[, ui := clv.time.interval.in.number.tu(clv.time,
                                                       interval(end = date.u,
                                                                start = Date.first.trans))]
  dt.data.alive[dt.alive.cust, k0u := k0u, on = "Id"] # add to data used in calculations
  dt.data.alive[dt.alive.cust, ui := ui, on = "Id"] # add to data used in calculations

  t <- clv.time.interval.in.number.tu(clv.time, interval(start=date.u, end=date.tu))

  #loop through every date of the estimation period to catch all customer with different start dates
  results.tmp <- list()
  period.dates <- seq(from=date.u, to = date.tu, by="days")
  for(ii in 1:length(period.dates)){
    d.i <- period.dates[ii]
    #   # see who turns alive on EXACTLY this date
    dt.data.i <- copy(dt.first.transactions[Date.first.trans == d.i, ])
    if(nrow(dt.data.i) == 0){
      next
    }

    dt.alive.i  <- dt.data[dt.data.i[, "Id"], on="Id", nomatch=NULL]

    dt.alive.i <- dt.alive.i[Date>=clv.time.floor.date(clv.time, d.i) &
                               Date <= clv.time.floor.date(clv.time, date.tu)]
    setkeyv(dt.data, c("Id", "Date"))

    # k0u: Number of time units between customer's first transaction and beginning of (u,u+t]
    dt.data.i[, date.u := d.i]
    dt.data.i[, k0u := clv.time.interval.in.number.tu(clv.time,
                                                      interval(end = clv.time.ceiling.date(clv.time, d.i),
                                                               start = clv.time.floor.date(clv.time, d.i)))]
    dt.data.i[, ui := clv.time.interval.in.number.tu(clv.time,
                                                     interval(end = d.i, start = Date.first.trans))]
    dt.alive.i[dt.data.i, k0u := k0u, on = "Id"] # add to data used in calculations
    dt.alive.i[dt.data.i, ui := ui, on = "Id"] # add to data used in calculations

    # u: Period number of start of pmf: estimation start until date.u
    # t: Number of periods in period. Has to be calculated here because in pmf from nrow(data) can only be
    #     full periods
    t <- clv.time.interval.in.number.tu(clv.time, interval(start=d.i, end=date.tu))


    # pmf for customers becoming alive at this date
    results.tmp[[ii]] <- dt.alive.i[, list(pmf = pnbd_dyncov_pmf_per_customer(dt.data.period.customer=.SD,
                                                                           dt.data.since.alive.all = dt.data.alive.forDi,
                                                                           x=x, a=a, b=b, r=r, s=s,t=t)),
                                    by="Id", .SDcols = c("Id", "Date", "exp.gX.L", "exp.gX.P", "d1", "d_omega","k0u", "ui")]
  }

  results.tmp <- rbindlist(results.tmp)
  return(results.tmp)
}


pnbd_dyncov_prepare_data <- function(object) {
  Date <- exp.gX.L <- exp.gX.P <- i.exp.gX.L <- i.exp.gX.P <- Date.first.trans <- NULL
  date.tu <- Date.next.cov <- tmp.Date <- Date.next.covariate.after.first.trans <- NULL
  d_omega <- date.u <- i.Date.first.trans <- is.alive <- k0u <- ui <- d1 <- NULL

  clv.time <- object@clv.data@clv.time
  date.u <- clv.time@timepoint.estimation.start
  date.tu <- clv.time@timepoint.estimation.end

  dt.data.cov.life <- copy(object@clv.data@data.cov.life)
  dt.data.cov.trans <- copy(object@clv.data@data.cov.trans)

  setnames(dt.data.cov.life, "Cov.Date", "Date")
  setnames(dt.data.cov.trans, "Cov.Date", "Date")
  vec.gamma.life <- object@prediction.params.life
  vec.gamma.trans <- object@prediction.params.trans
  m.cov.life <- as.matrix(dt.data.cov.life[, .SD, .SDcols = names(vec.gamma.life)])
  m.cov.trans <- as.matrix(dt.data.cov.trans[, .SD, .SDcols = names(vec.gamma.trans)])
  dt.data.cov.life[, exp.gX.L := exp(m.cov.life %*% vec.gamma.life)]
  dt.data.cov.trans[, exp.gX.P := exp(m.cov.trans %*% vec.gamma.trans)]
  dt.data <- merge(dt.data.cov.life[, c("Id", "Date", "exp.gX.L")],
                   dt.data.cov.trans[, c("Id", "Date", "exp.gX.P")],
                   by = c("Id", "Date"), all = TRUE)

  dt.transactions <- copy(object@clv.data@data.transactions)
  dt.first.transactions <- dt.transactions[, list(Date.first.trans = min(Date)), by = "Id"]
  dt.data <- merge(dt.data, dt.first.transactions, by = "Id")

  dt.data <- dt.data[Date.first.trans < date.tu]

  dt.data[, tmp.Date := Date]
  dt.next.cov <- dt.data[dt.first.transactions, list(Id, Date.next.cov = tmp.Date),
                         on = c("Id", "Date" = "Date.first.trans"), roll = -Inf]
  dt.data[, tmp.Date := NULL]
  dt.first.transactions[dt.next.cov, Date.next.cov.after.first.trans := Date.next.cov, on = "Id"]
  dt.first.transactions[, d1 := clv.time.interval.in.number.tu(clv.time,
                                                               interval(start = Date.first.trans,
                                                                        end = Date.next.cov.after.first.trans))]
  dt.first.transactions[, d_omega := clv.time.interval.in.number.tu(clv.time,
                                                                    interval(start = date.u,
                                                                             end = clv.time.ceiling.date(clv.time, date.u)))]
  dt.first.transactions[, k0u := clv.time.interval.in.number.tu(clv.time,
                                                                interval(end = clv.time.ceiling.date(clv.time, date.u),
                                                                         start = clv.time.floor.date(clv.time, Date.first.trans)))]
  dt.first.transactions[, ui := clv.time.interval.in.number.tu(clv.time,
                                                               interval(end = date.u,
                                                                        start = Date.first.trans))]

  dt.data <- merge(dt.data, dt.first.transactions, by = c("Id", "Date.first.trans"))
  setkeyv(dt.data, c("Id", "Date"))
  
  return(dt.data)
}

pnbd_dyncov_calculate_pmf_cpp <- function(dt_customer_set, dt_since_alive_data,
                                          r, s, a, b, x, t) {
  if (nrow(dt_customer_set) == 0) {
    return(data.table(Id = character(0), pmf = numeric(0)))
  }
  
  results_list <- lapply(unique(dt_customer_set$Id), function(customer_id) {
    dt_customer_period <- dt_customer_set[Id == customer_id][order(Date)]
    dt_customer_sincealive <- dt_since_alive_data[Id == customer_id][order(Date)]
    
    if (nrow(dt_customer_period) == 0) return(NULL)
    
    pmf_value <- .Call('_CLVTools_pnbd_dyncov_pmf_per_customer',
                       PACKAGE = 'CLVTools',
                       dt_customer_period$exp.gX.L,
                       dt_customer_period$exp.gX.P,
                       dt_customer_sincealive$exp.gX.L,
                       dt_customer_sincealive$exp.gX.P,
                       r, a, s, b,
                       as.numeric(x),
                       t,
                       dt_customer_period$d1[1],
                       dt_customer_period$d_omega[1],
                       dt_customer_period$k0u[1],
                       dt_customer_period$ui[1])
                       
    return(data.table(Id = customer_id, pmf = pmf_value))
  })
  
  return(rbindlist(results_list))
}

pnbd_dyncov_pmf <- function(object, x, use_r = FALSE) {
  if (length(x) > 1 && !use_r) {
    warning("Vector support for `x` is only available in the R implementation. Switching to R implementation.")
    use_r <- TRUE
  }
  if (use_r) {
    return(pnbd_dyncov_pmf_r(object, x))
  }
  
  dt.prepared.data <- pnbd_dyncov_prepare_data(object)

  r <- object@prediction.params.model[["r"]]
  a <- object@prediction.params.model[["alpha"]]
  s <- object@prediction.params.model[["s"]]
  b <- object@prediction.params.model[["beta"]]

  clv.time <- object@clv.data@clv.time
  date.u <- clv.time@timepoint.estimation.start
  date.tu <- clv.time@timepoint.estimation.end
  
  dt.already.alive.period <- dt.prepared.data[Date.first.trans < date.u & Date >= date.u & Date <= date.tu]

  dt.newly.alive.period <- dt.prepared.data[Date.first.trans >= date.u & Date >= Date.first.trans & Date <= date.tu]

  dt.since.alive.data <- dt.prepared.data[Date >= Date.first.trans & Date <= date.tu]

  t_full <- clv.time.interval.in.number.tu(clv.time, interval(start = date.u, end = date.tu))
  results_already_alive <- pnbd_dyncov_calculate_pmf_cpp(
    dt_customer_set = dt.already.alive.period,
    dt_since_alive_data = dt.since.alive.data,
    r = r, s = s, a = a, b = b, x = x, t = t_full
  )
  
  results_newly_alive_list <- lapply(unique(dt.newly.alive.period$Id), function(id){
      dt_cust_new <- dt.newly.alive.period[Id == id]
      cust_start_date <- dt_cust_new$Date.first.trans[1]
      t_new <- clv.time.interval.in.number.tu(clv.time, interval(start=cust_start_date, end=date.tu))
      pnbd_dyncov_calculate_pmf_cpp(dt_cust_new, dt.since.alive.data, r,s,a,b,x,t_new)
  })

  return(rbindlist(c(list(results_already_alive), results_newly_alive_list)))
}