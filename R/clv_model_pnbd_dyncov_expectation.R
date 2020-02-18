#' @importFrom utils txtProgressBar setTxtProgressBar
#' @include all_generics.R class_clv_model_pnbd_dynamiccov.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.dynamic.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

  # Create ABCD for expectation
  #   i starts at when becoming alive
  #
  # Loop over expectation dates
  #   Subset ABCD to who had transactions before (strictly?) this date
  #   Calc expectation for these already alive

  tp.last.period.start <- dt.expectation.seq[, max(period.first)]
  max.period.no <- dt.expectation.seq[, max(period.num)]

  if(max.period.no <=2)
    stop("Have to plot at least 3 periods!", call. = FALSE)

  # err.msg <- check_user_data_dyncovlongenough(clv.fitted=clv.fitted, tp.end = tp.last.period.start)
  # check_err_msg(err.msg)

  # For more readable code
  clv.time <- clv.fitted@clv.data@clv.time

  # Create ABCD ---------------------------------------------------------------------------------------------
  # Calculate Ai, Bbar_i, Ci, Dbar_i
  #   i=1 in period when customer turns alive
  # Upper max cov period is where the last expectation date lies in
  #   If max(dates.periods) falls directly onto the start of a covariate,
  #     the covariate is active then and is included as well.
  #     => max cov: floor_tu(max(dates.periods))

  date.last.cov  <- clv.time.floor.date(clv.time=clv.time, timepoint=tp.last.period.start)

  l.covs <- pnbd_dyncov_alivecovariates(clv.fitted = clv.fitted, date.upper.cov = date.last.cov)
  dt.trans <- l.covs[["dt.trans"]]
  dt.life  <- l.covs[["dt.life"]]

  # Merge into single table
  dt.ABCD <- dt.life[, c("Id", "Cov.Date", "exp.gX.L")]
  setkeyv(dt.ABCD, cols = c("Id", "Cov.Date"))
  dt.ABCD[dt.trans, exp.gX.P := i.exp.gX.P, on = c("Id", "Cov.Date")]

  # Add all other needed data
  dt.ABCD[clv.fitted@cbs, d_omega := i.d_omega, on="Id"]


  # . i --------------------------------------------------------------------------------------------------------
  # Number of covariates since customer came alive
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

  # **TEST: Dbar_i == Bbar_i if covariates data & parameters are the same! (or gamma=0)

  # Do expectation -----------------------------------------------------------------------------------------------
  # For every date in the expectation table, calculate the expectation separatley

  if(verbose){
    progress.bar <- txtProgressBar(max = max.period.no, style = 3)
    update.pb    <- function(n){setTxtProgressBar(pb=progress.bar, value = n)}
  }

  # For every period, do unconditional expectation (sumF)
  #   Do for loop because more expressive than doing by="period.num" in table
  for(p.no in dt.expectation.seq$period.num){
    period.first <- dt.expectation.seq[period.num == p.no, period.first]

    expectation_i <- .pnbd_dyncov_unconditionalexpectation(clv.fitted = clv.fitted,
                                                           dt.ABCD = dt.ABCD,
                                                           period.first = period.first)

    dt.expectation.seq[period.num == p.no, expectation := expectation_i]

    if(verbose)
      update.pb(p.no)
  }


  # Cumulative to incremental --------------------------------------------------------------------------
  #   First entry is already correct, because cumulative = incremental there, and cannot be
  #   infered using "diff". Therefore let first entry as is, rest is diff
  dt.expectation.seq[order(period.num, decreasing = FALSE), expectation := c(expectation[[1]], diff(expectation))]

  return(dt.expectation.seq)
})



# **** JEFF: t = TUs from alive until date.expectation.period.start oder date.expectation.period.end?
# **** JEFF: cut: At date.expectation.period.start oder date.expectation.period.end?
.pnbd_dyncov_unconditionalexpectation <- function(clv.fitted, dt.ABCD, period.first){

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
  # ***JEFF: Is this < or <= ?
  dt.alive.customers <- clv.fitted@cbs[date.first.actual.trans <= period.first,
                                       c("Id", "date.first.actual.trans")]

  # Keep only who is alive already at period.first
  dt.ABCD.alive <- dt.ABCD[dt.alive.customers, on="Id", nomatch = NULL]


  # Exact time from coming alive until end of expectation period
  dt.alive.customers[, num.periods.alive.expectation.date :=
                       clv.time.interval.in.number.tu(clv.time=clv.time,
                                                      interv=interval(start = date.first.actual.trans,
                                                                      end   = period.first))]
  # Add to every cov period
  dt.ABCD.alive[dt.alive.customers, num.periods.alive.expectation.date := i.num.periods.alive.expectation.date,
                on = "Id"]

  # Cut data to maximal range
  # **JEFF: Which covariates are allowed to be considered? All of current period? Only before that?
  #   Only covariates until period of period.first may be considered or covariates that
  #     are going to be active during t?
  # Consider all covariates which are active before and during the period for which the expectation is
  #   calculated (because period.first is the beginning of the covariate period)
  dt.ABCD.alive <- dt.ABCD.alive[Cov.Date <= period.first]


  # S --------------------------------------------------------------------------------------------------------
  # S_i is relative to when alive, ie by i
  # d1 is first.purchase until ceiling_tu(first.purchase) = d_omega
  #   Already added for Bbar_i

  #helper to calculate S value by:
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
  dt.ABCD.alive[Cov.Date == max(Cov.Date),
                S := s.fct.expectation(term = (d1 + i - 2), A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s) -
                  s.fct.expectation(term = num.periods.alive.expectation.date, A=Ai, B=Bbar_i, C=Ci, D=Dbar_i, beta_0=beta_0, s=s)]

  # S may be NA for customers alive only for <=1 period.
  #   Their f value is calculated without S then
  dt.S <- dt.ABCD.alive[, .(S = sum(S)), keyby="Id"]


  # F --------------------------------------------------------------------------------------------------------

  # Add everything else needed
  #   For all customers Ak0t/Bk0t/Ck0t/Dk0t is the last ABCD value (with max(i) where max(Cov.Date))
  dt.ABCD_k0t <- dt.ABCD.alive[Cov.Date == max(Cov.Date),
                               .(Id, A_k0t=Ai, Bbar_k0t=Bbar_i, C_k0t=Ci, Dbar_k0t=Dbar_i, i)]
  dt.alive.customers <- dt.alive.customers[dt.ABCD_k0t, on = "Id"]
  dt.alive.customers[dt.S, S := i.S, on = "Id"]


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

  return(dt.alive.customers[, sum(f)])
}

# 1-vs-1 session ------------------------------------------------------------------------------------------------------------

# # i: relative to when alive
# # Dbar_i:
# # From ABCD
# dt.Dbar <- clv.data@data.cov.life[Cov.Date >= clv.time.floor.date(clv.time=clv.time,timepoint=clv.time@timepoint.estimation.start)]
# # All cov=1 for gamma=0!
# dt.Dbar[, exp.gX.L := 1]
# dt.Dbar[, exp.gX.P := 1]
# # Only cov since and including the one that awoke the customer (ie which covariate was active when became customer)
# dt.Dbar[dt.cbs, date.first.actual.trans := i.date.first.actual.trans, on="Id"]
# dt.Dbar[, is.alive := Cov.Date >=  clv.time.floor.date(clv.time=clv.time,timepoint=date.first.actual.trans)]
# dt.Dbar <- dt.Dbar[is.alive == TRUE]
#
# # Until the one active at prediction end
# dt.Dbar[Cov.Date <= prediction.end.date, ]
#
# dt.Dbar[order(Cov.Date), i:=seq(.N), by="Id"]
#
# # Same as ABCD, but k0T=0
# dt.cbs[, d_omega := clv.time.interval.in.number.tu(clv.time=clv.time,
#                                                    interv = interval(start = date.first.actual.trans,
#                                                                      end = clv.time.ceiling.date(clv.time=clv.time,
#                                                                                                  date.first.actual.trans))),
#        by = "Id"]
# dt.Dbar[dt.cbs, d_omega := i.d_omega, on="Id"]
#
# # Dbar_i
# dt.Dbar[i == 1,Dbar_i := exp.gX.L*d_omega]
# dt.Dbar[i > 1, Dbar_i := exp.gX.L]
# # Not only cumsum i>1!
# dt.Dbar[, Dbar_i := cumsum(Dbar_i), by="Id"]
#
# dt.Dbar[i == 1, Dbar_i := 0] #In this case=0. (Dbar_i - exp.gX.L) + exp.gX.L * (-d_omega)]
# dt.Dbar[i > 1, Dbar_i := (Dbar_i - exp.gX.L) + exp.gX.L * (-d_omega - (i-2))]
#
# # Bbar_i
# dt.Dbar[, d1 := d_omega] # for this case only
# dt.Dbar[i == 1,  Bbar_i := exp.gX.P*d1]
# dt.Dbar[i > 1,   Bbar_i := exp.gX.P]
# # ORDER!
# dt.Dbar[i > 1,  Bbar_i := cumsum(Bbar_i), by="Id"]
#
# dt.Dbar[i == 1, Bbar_i := 0] #In this case = 0. (Bbar_i - exp.gX.P) + exp.gX.P * (-d1)]
# dt.Dbar[i  > 1, Bbar_i := (Bbar_i - exp.gX.P) + exp.gX.P * (-d1 - (i-2))]
#
# dt.Dbar[, Ai := exp.gX.P]
# dt.Dbar[, Ci := exp.gX.L]
#
# # Remove everybody which is not alive yet
# #   Only keep who is alive already at t
# dt.Dbar <- dt.Dbar[date.first.actual.trans <= prediction.end.date]
#
# # S-VALUE
# # s.fct.expectation <- function(term, A, B, C, D, beta_0, s){
# #   return( (A * (term * s + 1/C * (beta_0+D)) + B*(s-1)) / (beta_0 + D + C * term)^s )
# # }
# # s.fct.expectation(term = 0, A=A[i, p+2], B=B[i, p+2],C=C[i, p+2],D=D[i, p+2],beta_0=beta_0,s=s)
# # - s.fct.expectation(term = d[i], A=A[i, p+2], B=B[i, p+2],C=C[i, p+2],D=D[i, p+2],beta_0=beta_0,s=s))
# fct.S1.part1 <- function(){ (A1 / C1 * (beta_0+D1) + Bbar_1*(s-1)) / (beta_0+Dbar_1)^s }
# fct.S1.part2 <- function(){ (A1 / C1 * (beta_0+D1) + Bbar_1*(s-1)) / (beta_0+Dbar_1)^s }
# dt.Dbar[i==1, S:= s.fct.expectation(term = , A = Ai, B = Bbar_i, C = Ci, D = Dbar_i, beta_0 = beta_0, s = s) -
#           s.fct.expectation(A = Ai, B = Bbar_i, C = Ci, D = Dbar_i, beta_0 = beta_0, s = s), by=c("Id")]
# dt.Dbar[i != 1 & i != max(i), S:= S(), by=c("Id", "i")] # wrong because max() is overall and not per customer!
#
# dt.S <- dt.Dbar[, sum(S), by="Id"]
#
# # F
# #   Different for how long alive. Only alive 1 period is special case
# dt.fvaue <-dt.Dbar[is.max.i == T]
# dt.fvalue[dt.S, S := i.S, on = "Id"]
# dt.fvalue
# dt.Dbar[, is.max.i := i == max(i), by="Id"]
# dt.Dbar[only.1.period.alive == 1, ]
# # Jeff: (Bk0tbar + t.customer*Ak0t) == (Bbar_i + t.customer*Ai) == Bi (and not Bbar_i)
# #   t.customer: exact (partial) time from alive until expectation end
# # Dk0tbar: last Dbar == Dbar_k0t==Dbar_max(i)
# # Ck0t: last Cbar == Cbar_k0t==Ci_max(i)
# # C1 = Ci 1
# # A1 = Ai 1 = A[i==1]
# # TEST: Dbar_i == Bbar_i if covariates data & parameters are the same! (or gamma=0)
# # period.num: how long alive.
# f.value[period.num == 1, f := f * ((A1*t.customer*(s-1)) / (beta_0+C1*t.customer)^s + (A1/C1)/beta_0^(s-1) - (A1*(t.customer*s + 1/C1*beta_0))/(beta_0+C1*t.customer)^s)]
# f.value[period.num >  1, f := f * (( (Bk0tbar + t.customer*Ak0t)       *(s-1)) / (beta_0+Dk0tbar  + t.customer*Ck0t      )^s + S.value)]

#the sorting is not by ID anymore, but by period.num!
#Doesnt matter as returned summed -> not in this version, careful!
# return(f.value$f)
# return(f.value[, sum(f, na.rm = TRUE)])



# OLD CODE ------------------------------------------------------------------------------------------------------------

# .pnbd_ConditionalExpectedTransactions_DynCov_gen.calc_adj_m <- function(data.cov, cov.gammas, covariate.names, upper.date, lower.date){
#   Id <- Cov.Date <- NULL
#
#   #first get all the covariates in the obj in matrix form
#   # lapply over all cov names and get these columns from cov table in obj
#   #    acast (=make matrix form) all covariates with date after cal.end
#   all.cov.mats <-
#     lapply(X =covariate.names, FUN=function(name){
#       #only the covariates with date after
#
#       # replace reshape2's acast function
#       cast.dt <- dcast(data = data.cov[Cov.Date >= lower.date & Cov.Date <= upper.date],
#                        formula = Id~Cov.Date, value.var = name)
#       setkeyv(cast.dt, "Id")
#       m <- as.matrix(cast.dt[, !"Id"])
#       rownames(m) <- cast.dt$Id
#       return(m)
#     })
#
#
#   #then calc the adjusted covariate matrix
#   # = exp(cov.m.1*cov.g.1 + cov.m.2*cov.g.2 + cov.m.3*cov.g.3 + ... )
#   adj.m <- exp(Reduce("+", Map("*", all.cov.mats, cov.gammas)))
#
#   # adj.m[is.na(adj.m)] <- 0
#
#   return(adj.m)
# }
#
# #
# # @importFrom parallel detectCores
# # @importFrom doParallel registerDoParallel stopImplicitCluster
# # @importFrom foreach %dopar%
# # @importFrom utils txtProgressBar setTxtProgressBar
# # @importFrom lubridate dseconds
# # @include all_generics.R class_clv_model_pnbd_dynamiccov.R
# setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.dynamic.cov"), function(clv.model, clv.fitted, dates.periods, verbose){
#
#   period.no <- expectation <- p.no <- NULL
#
#   # Check Input
#   # --------------------------------------------------------------------------------
#   max.period.no <- length(dates.periods)
#   if(max.period.no <=2)
#     stop("Have to plot at least 2 periods!", call. = FALSE)
#
#
#   # err.msg <- check_user_data_dyncovlongenough(clv.fitted=clv.fitted, date.end = max(dates.periods))
#   # check_err_msg(err.msg)
#
#
#   # Periods
#   #   build the points in time for which to calculate
#   #     from the dates given in dates.periods
#   #
#   # Calculate expectation for every period separatley
#   #   do parallel
#   #   tryCatch finally to cleanup when it crashes
#   # --------------------------------------------------------------------------------
#
#   expectation.dt <- data.table(date.period = dates.periods, period.no = seq_along(dates.periods), expectation = NA_real_)
#
#   # registerDoParallel(max(detectCores()-1, 1))
#
#   # progress.bar <- txtProgressBar(max = max.period.no, style = 3)
#   # update.pb    <- function(n){setTxtProgressBar(pb=progress.bar, value = n)}
#   message("Max periods:", max.period.no)
#   # sum.Fs <-
#   #   tryCatch( foreach(p.no = seq.int(from=2, to=max.period.no),
#   #                     .combine=c,
#   #                     .inorder = TRUE)%do%{
#   #                       print(p.no)
#   #                       # print("going to calculate")
#   #                       sumF <- .pnbd_dyn_cov_expectation_sumF(clv.fitted=clv.fitted, dates.periods = dates.periods[seq(p.no)])
#   #                       # print("calculation done.")
#   #                       # update.pb(p.no)
#   #                       str(sumF)
#   #                       return(sumF)
#   #                     },
#   #
#   #             finally = stopImplicitCluster())
#
#   # order periods
#   dates.periods <- dates.periods[order(dates.periods)]
#   sum.Fs <- sapply(seq.int(from=3, to=max.period.no), function(p.no){
#     sumF <- .pnbd_dyn_cov_expectation_sumF(clv.fitted=clv.fitted, dates.periods = dates.periods[seq(p.no)])
#     # update.pb(p.no)
#     str(sumF)
#     return(sumF)
#   })
#
#
#   print("FINISHED EXPECTATION")
#   str(sum.Fs)
#   # close(progress.bar)
#
#   # Transform cumulative back to incremental
#   #   First entry is already correct and cannot be infered using "diff"
#   #   Therefore let first entry as is
#   #   ***Patrik: And what do we write into period.no==2 (and now also 3)??
#   # --------------------------------------------------------------------------------
#   expectation.dt[period.no == 1, expectation := sum.Fs[[1]]]
#   expectation.dt[period.no != 1 & period.no != 2 & period.no != 3, expectation := diff(sum.Fs)]
#
#   print(expectation.dt)
#   return(expectation.dt)
# })
#
#
# #helper to calculate S value by:
# #   s.fct.expectation(term.1) - s.fct.expectation(term.2)
# s.fct.expectation <- function(term, A, B, C, D, beta_0, s){
#   return( (A * (term * s + 1/C * (beta_0+D)) + B*(s-1)) / (beta_0 + D + C * term)^s )
# }
#
# .pnbd_dyn_cov_expectation_sumF <- function(clv.fitted, dates.periods){
#
#   Id <- Date <- floorDate <- t.customer <- f <- Bk0tbar <- Ak0t <- Dk0tbar <- Ck0t <- NULL
#
#   logparams    <- log(clv.fitted@prediction.params.model)
#   trans.gammas <- clv.fitted@prediction.params.trans
#   life.gammas  <- clv.fitted@prediction.params.life
#   periods      <- length(dates.periods)
#   message("Num periods to expect for: ", periods)
#
#   # prediction.end.date <- date.prediction.end
#
#   # Patrik: Its 3 periods! The code implicitely assumes 3 periods!
#   if(periods < 3)
#     stop("Can only calculate expectation for >= 3 periods!", call. = FALSE)
#
#   cal.start.date      <- min(dates.periods)
#   prediction.end.date <- max(dates.periods)
#   message("prediction.end.date: ", prediction.end.date)
#
#   clv.data <- clv.fitted@clv.data
#   clv.time <- clv.data@clv.time
#
#
#   ######################################################################## COPIED CODE ########################################################################
#   # perids: t in the formulas, eg how much in the "future" we want to predict (starting from 0)
#
#
#   # Note Jeff: I am somewhat concerned about the order of the different vectors/matrices we define here. Have to check that it is
#   # always in the same order
#
#   # In this function we also "calculate" everything for customer who have not yet started byuing! This is certainly redundant, and it also
#   # gives weird results (as we actually calculate things which do not exist.). Change in next version!
#
#   # The function seems to be correct: The only thing not fully tested is the case period.num=1!!
#
#   # This must be exp of logparams!
#   r       <- exp(logparams[1])
#   alpha_0 <- exp(logparams[2])
#   s       <- exp(logparams[3])
#   beta_0  <- exp(logparams[4])
#
#
#   # cal.start.date      <- floor_date(ymd(obj@durations$cal.start, tz="UTC"), unit=obj@durations$measurement.scale)
#   # k0T.date            <- floor_date(ymd(obj@durations$cal.end, tz="UTC"), unit=obj@durations$measurement.scale)
#   # cal.start.date   <-  floor_tu(obj=obj, d=obj@date.estimation.start)
#   # k0T.date         <-  floor_tu(obj=obj, d=obj@date.estimation.end)
#   cal.start.date   <-  clv.time.floor.date(clv.time=clv.time, timepoint=clv.time@timepoint.estimation.start)
#   k0T.date         <-  clv.time.floor.date(clv.time=clv.time, timepoint=clv.time@timepoint.estimation.end)
#
#   # #Prediction always starts at cal.end and no need for full period (ie ceiling_date)
#   # prediction.end.date <- switch(EXPR = obj@durations$measurement.scale,
#   #                                 day   = obj@durations$cal.start + days(periods),
#   #                                 week  = obj@durations$cal.start + weeks(periods),
#   #                                 month = obj@durations$cal.start + months(periods),
#   #                                 year  = obj@durations$cal.start + years(periods))
#
#
#   # transaction.adj.m <-  .pnbd_ConditionalExpectedTransactions_DynCov_gen.calc_adj_m(obj = obj, cov.gammas = trans.gammas,
#   #                                                                                   covariate.names = obj@names.cov.data.trans,
#   #                                                                                   lower.date = cal.start.date,
#   #                                                                                   upper.date = prediction.end.date)
#   #
#   # lifetime.adj.m <-     .pnbd_ConditionalExpectedTransactions_DynCov_gen.calc_adj_m(obj = obj, cov.gammas = life.gammas,
#   #                                                                                   covariate.names = obj@names.cov.data.life,
#   #                                                                                   lower.date = cal.start.date,
#   #                                                                                   upper.date = prediction.end.date)
#   transaction.adj.m <-  .pnbd_ConditionalExpectedTransactions_DynCov_gen.calc_adj_m(data.cov = clv.data@data.cov.trans,
#                                                                                     cov.gammas = trans.gammas,
#                                                                                     covariate.names = clv.data@names.cov.data.trans,
#                                                                                     lower.date = cal.start.date,
#                                                                                     upper.date = prediction.end.date)
#
#   lifetime.adj.m <-     .pnbd_ConditionalExpectedTransactions_DynCov_gen.calc_adj_m(data.cov = clv.data@data.cov.life,
#                                                                                     cov.gammas = life.gammas,
#                                                                                     covariate.names = clv.data@names.cov.data.life,
#                                                                                     lower.date = cal.start.date,
#                                                                                     upper.date = prediction.end.date)
#
#
#   # find the first purchase for every customer
#   # First purchase is always in calibration period
#   first.purchase <- clv.data@data.transactions[, list(Date=base::min(Date)), by="Id"]
#
#   # Now I would do something more here, namely round down to the next sunday!
#   # first.purchase[, floorDate:= flofloor_date(first.purchase$Date, obj@durations$measurement.scale)]
#   # first.purchase[, floorDate:= floor_tu(obj=obj, d=first.purchase$Date)]
#   first.purchase[, floorDate:= clv.time.floor.date(clv.time=clv.time, timepoint=first.purchase$Date)]
#
#   # Now need to use floordate and the ceiling data for cal.start -> this is to make sure in every last special case the number of periods is taken out correctly
#   # time.to.first.purchase <- interval(start=ceiling_date(obj@durations$cal.start, obj@durations$measurement.scale), end=first.purchase$floorDate)
#   # time.to.first.purchase <- interval(start= ceiling_tu(obj=obj, d=obj@date.estimation.start), end=first.purchase$floorDate)
#   time.to.first.purchase <- interval(start = clv.time.ceiling.date(clv.time=clv.time,
#                                                                    timepoint=clv.time@timepoint.estimation.start),
#                                      end = first.purchase$floorDate)
#
#   # Number of periods (starting from the beginning) until the first purchase per customer
#   # periods.until.first.purchase <- time_length(x=time.to.first.purchase, unit=obj@time.unit)
#   periods.until.first.purchase <- clv.time.interval.in.number.tu(clv.time=clv.time,
#                                                                  interv= time.to.first.purchase)
#
#   #It is time until exact first purchase: only "full" periods needed, cut off any fractions
#   periods.until.first.purchase <- as.integer(periods.until.first.purchase)
#
#   # Get t.customer (number of weeks we between prediction.end.date and start of customer observation (eg, first purchase))
#   # first.purchase[,t.customer:=difftime(prediction.end.date, Date,units = obj@durations$measurement.scale)]
#   # first.purchase[,t.customer:=as.numeric(t.customer)*(t.customer>=0)]
#   # first.purchase[,t.customer:=difftime(prediction.end.date, Date,units = obj@time.unit)]
#   # first.purchase[,t.customer:=as.numeric(t.customer)*(t.customer>=0)]
#   # ***Patrik: Why is (t.customer>=0) needed? Do we need to remove some
#   #   customers for which the expectation should not be done at all??
#   first.purchase[,t.customer:=clv.time.interval.in.number.tu(clv.time=clv.time,
#                                                              interv=interval(start = Date,
#                                                                              end = prediction.end.date))]
#   first.purchase[t.customer < 0, t.customer := 0]
#
#   # This is d_1 in the formulas # Different for each customer! -> Watch out for order!
#   # d.time <- interval(first.purchase$Date, ceiling_date(first.purchase$Date + dseconds(1), obj@durations$measurement.scale))
#   # d <- as.numeric(d.time, unit=obj@durations$measurement.scale)
#   # d.time <- interval(first.purchase$Date, ceiling_tu(obj=obj, d= first.purchase$Date + dseconds(1)))
#   # d <-interval_in_number_tu(obj=obj, interv=d.time)
#   d.time <- interval(first.purchase$Date, clv.time.ceiling.date(clv.time=clv.time,
#                                                                 timepoint = first.purchase$Date + dseconds(1)))
#   d <-clv.time.interval.in.number.tu(clv.time=clv.time, interv=d.time)
#
#   period.num=c()
#   # matrix of structure [(1,1,..), (2,2,...), (n,n,...)] for later usage (index) instead of looping
#   # This matrix is also different for each customer, depending on when they started
#   i.matrix <- matrix(data = seq(1, ncol(lifetime.adj.m)),
#                      ncol=ncol(lifetime.adj.m), nrow=nrow(lifetime.adj.m), byrow = TRUE)
#   i.matrix[,1]<-2
#
#   # This is super bad, but it shall suffice for now: Define A.2, and C.2 for the sum in between (for b.2, d.2)
#   A.2<-transaction.adj.m
#   C.2<-lifetime.adj.m
#   # Need to define b.1/d.1/A1/C1 correctly for those guys actually starting in the same period as cal.start
#   b.1<-transaction.adj.m * matrix(data=d,nrow=length(d),ncol=ncol(transaction.adj.m))
#   d.1<-lifetime.adj.m * matrix(data=d,nrow=length(d),ncol=ncol(lifetime.adj.m))
#   A1<-transaction.adj.m[,1, drop=FALSE]
#   C1<-lifetime.adj.m[,1, drop=FALSE]
#
#
#   for(i in seq(1, length(periods.until.first.purchase))){
#     p <- periods.until.first.purchase[i]
#
#     #new format: 0,0,0,0,0,v1,v2,v3,...
#     #make the first p periods before first purchase 0 so they will not be summed up but include the rest of the row
#     if(p>=0){
#
#       if(ncol(transaction.adj.m)>p+1){
#
#         transaction.adj.m[i, ] <- c(rep(0, (p+1)), transaction.adj.m[i, -(1:(p+1)) ])
#         lifetime.adj.m[i, ] <- c(rep(0, (p+1)), lifetime.adj.m[i, -(1:(p+1)) ])
#         # I think setting this stuff to 0 is not even necessary, we actually take out the right things anyway!
#         # Correct also for the case p<0! (as this is defined above)
#
#         # For the sum in between also delete the first one
#         A.2[i, ] <- c(rep(0, (p+2)), transaction.adj.m[i, -(1:(p+2)) ])
#         C.2[i, ] <- c(rep(0, (p+2)), lifetime.adj.m[i, -(1:(p+2)) ])
#         # Correct also for the case p<0! (as this is defined below)
#
#         # Need to create an i.matrix which accounts for the fact that each customer starts at a different time point
#         i.matrix[i,]<-c(rep(0, (p+1)),seq(1, ncol(lifetime.adj.m)-(p+1)))# For those with (transaction.adj.m)<=p), it does not matter what we have in the i.matrix
#         # Idea (maybe change!): Account for i=1 (which is again at different date for each customer) by setting first element of i.matrix for each customer to 2
#         i.matrix[i,p+2]<-2
#         # Correct also for the case p<0! (as this is defined above)
#
#         # Fish out the first element from the matrix
#         b.1[i,] <- c(rep(0, (p+1)),matrix(data=transaction.adj.m[i, p+2], nrow=1, ncol=ncol(transaction.adj.m)-p-1) * d[i])
#         d.1[i,] <- c(rep(0, (p+1)),matrix(data=lifetime.adj.m[i, p+2], nrow=1, ncol=ncol(lifetime.adj.m)-p-1) * d[i])
#         # Correct also for the case p<0! (as this is defined above)
#
#
#         # This is for the f function in the end (A1 and C1 should have the right order)
#         A1[i]=transaction.adj.m[i,p+2]
#         C1[i]=lifetime.adj.m[i,p+2]
#         # it is also ok that they have NA, though we could also set it to 0
#         # Correct also for the case p<0! (as this is defined above)
#
#       } else{
#         transaction.adj.m[i, ] <- 0
#         lifetime.adj.m[i, ] <- 0
#         b.1[i,]<-0
#         d.1[i,]<-0
#       }
#     }
#
#     if(ncol(transaction.adj.m)>p+1){
#       # Also if p<0: For the sum in between also delete the first one
#       A.2[i, ] <- c(rep(0, (p+2)), transaction.adj.m[i, -(1:(p+2)) ])
#       C.2[i, ] <- c(rep(0, (p+2)), lifetime.adj.m[i, -(1:(p+2)) ])
#     }
#
#     # These "Covariance" matricies are simply beautiful! With it we can easily get the actual number of k_{0,t} (=period.num)
#     # Achtung different again for each customer
#     period.num[i] <- max(ncol(transaction.adj.m)-(p+1), 0)
#   }
#
#
#
#
#
#
#
#   #***ID in Matrix not ordered same as in data.table ****
#   # order row IDs to same as IDs in cbs (=data.table sorting)
#   transaction.adj.m     <- transaction.adj.m[clv.fitted@cbs$Id, ,drop=FALSE]
#   lifetime.adj.m        <- lifetime.adj.m[clv.fitted@cbs$Id, ,drop=FALSE]
#
#
#
#   # A
#   A <- transaction.adj.m
#
#   # B
#   # is made of 3 "layers": b1, bi, bt
#   # before, each row was calculated as following: (mainly in _Bi)
#   # what was in _Bi before, t only single ints
#   # := b1 + bi + bt
#   #   b1: Always same
#   #   bt: different for first col
#   #   bi: (if nol<2: 0)
#   #
#   #   b1:= adj.m[1] * d
#   #   bt:= adj.m*
#
#   #b.1: all cols = transaction.adj.m[, 1]
#   # of course if people start at different dates this is now also not correct -.-
#
#   #b.2
#   # first col:  0
#   # second col: 0
#   # after: sum of previous columns in transaction.adj.m
#   # prediction period (=ncol) < 3 not allowed in predict
#   b.2 <- matrix(0, nrow=nrow(transaction.adj.m), ncol=ncol(transaction.adj.m))
#
#   ## Here we have an error (same in d.2)!!
#   b.2<- cbind(  rep(0, nrow(A.2)), #first column is 0
#                 rep(0, nrow(A.2)), #second column is 0
#                 #matrix returned from sapply
#                 #Sum A from second col only (cannot 2:(i-1) as rowSums fails with single col)
#                 sapply(3:ncol(A.2), function(i){rowSums(A.2[, 1:(i-1), drop=FALSE], na.rm=TRUE)} )) ### Ah shit: Problem is that we need to get rid of the first value which is nonzero!
#
#   # b3
#   # all cols:         transaction.adj.m[, period.num]
#   # first col:        * (-obj@clv.cbs$T.cal - d)
#   # col after first:  * (-obj@clv.cbs$T.cal - d - (col.index-2))
#   b.3 <- A
#
#   b.3 <- b.3 * (- d - (i.matrix - 2))
#
#   #old:
#   #b.3[, 1] <- b.3[, 1]   * (- d)                   # This is now wrong again, as first position is again different for each customer
#   #b.3[, -1] <- b.3[, -1] * (- d - (i.matrix[,-1] - 2)) # With corrected i.matrix above this is correct!
#
#   #make actual B
#   B <- b.1 + b.2 + b.3
#
#
#
#
#   # C
#   C <- lifetime.adj.m
#
#   # D: D is just exactly the same as B in this case (only that we use C instead of A and lifetime.adj.m instead of transaction.adj.m!)
#
#
#   #d.2
#   # first col:  0
#   # second col: 0
#   # after: sum of previous columns in transaction.adj.m
#   # prediction period (=ncol) < 3 not allowed in predict
#   d.2 <- matrix(0, nrow=nrow(lifetime.adj.m), ncol=ncol(lifetime.adj.m))
#
#
#   ## Here we have an error (same in d.2)!!
#   d.2<- cbind(  rep(0, nrow(C.2)), #first column is 0
#                 rep(0, nrow(C.2)), #second column is 0
#                 #matrix returned from sapply
#                 #Sum C from second col only (cannot 2:(i-1) as rowSums fails with single col)
#                 sapply(3:ncol(C.2), function(i){rowSums(C.2[, 1:(i-1)], na.rm=T)} )) ### Ah shit: Problem is that we need to get rid of the first value which is nonzero!
#
#   # d3
#   # all cols:         transaction.adj.m[, period.num]
#   # first col:        * (-obj@clv.cbs$T.cal - d)
#   # col after first:  * (-obj@clv.cbs$T.cal - d - (col.index-2))
#   d.3 <- C
#
#   d.3 <- d.3 * (- d - (i.matrix - 2))
#
#   #old:
#   #d.3[, 1] <- d.3[, 1]   * (- d)                   # This is now wrong again, as first position is again different for each customer
#   #d.3[, -1] <- d.3[, -1] * (- d - (i.matrix[,-1] - 2)) # With corrected i.matrix above this is correct!
#
#   #make actual D
#   D <- d.1 + d.2 + d.3
#
#   #
#   # calculate S value for ALL rows and subset for k later
#   # --------------------------------------------------------
#
#
#
#
#   S <- matrix(data=0, ncol=ncol(A), nrow = nrow(A))
#   K=ncol(S) # Just arbitrary name
#
#
#   # We start at a different period for each customer, which throws everything über den Haufen!
#
#   for(i in seq(1, length(periods.until.first.purchase))){
#     p <- periods.until.first.purchase[i]
#
#
#
#     if(K>p+1){ # First col is in p+2, so it makes sense that we only look at K>p+1 (or even K>p+2, as otherwise S[i,p+2]=S[,ncol(S)], but doesn't matter)
#
#       #S for first col
#       #term 1 = 0 (yes!) term 2 = d
#       S[i, p+2] <- ( s.fct.expectation(term = 0, A=A[i, p+2], B=B[i, p+2],C=C[i, p+2],D=D[i, p+2],beta_0=beta_0,s=s)
#                      - s.fct.expectation(term = d[i], A=A[i, p+2], B=B[i, p+2],C=C[i, p+2],D=D[i, p+2],beta_0=beta_0,s=s))
#
#
#       if (K>p+3){ # Only if for a certain customer period.num>2, which is the same as K>p+3 do this middle part
#
#
#         #S middle cols -> need again be different for each customer
#         middle.ind <- seq(from = p+3, to = (K-1))
#         S[i, middle.ind] <- c(rep(0, length(middle.ind)-length((p+3):(K-1))) , (s.fct.expectation(term = (d[i] + i.matrix[i,(p+3):(K-1)] - 2), A=A[i, (p+3):(K-1)], B=B[i, (p+3):(K-1)],C=C[i, (p+3):(K-1)],D=D[i, (p+3):(K-1)],beta_0=beta_0,s=s) - s.fct.expectation(term = (d[i] + i.matrix[i,(p+3):(K-1)] - 1), A=A[i, (p+3):(K-1)], B=B[i, (p+3):(K-1)],C=C[i, (p+3):(K-1)],D=D[i, (p+3):(K-1)],beta_0=beta_0,s=s)))
#
#       }
#
#     }
#
#
#   }
#
#
#
#   #S last col
#   S[, ncol(S)] <- ( s.fct.expectation(term = (d + period.num - 2), A=A[, ncol(S)], B=B[,ncol(S)],C=C[, ncol(S)],D=D[, ncol(S)],beta_0=beta_0,s=s)
#                     - s.fct.expectation(term = first.purchase[,t.customer],A=A[, ncol(S)], B=B[,ncol(S)],C=C[, ncol(S)],D=D[, ncol(S)],beta_0=beta_0,s=s))
#   # There are NA's for people who did not start yet (eg the very first purchase happens after our prediction period)
#   # They get removed below, but their value does not matter, as in the relevant f below they are set back to NA
#
#   # sum only until where k>i
#   # use sieve matrix to remove entries in S where k < i
#   # S.value <- rowSums(S * sieve.matrix, na.rm=T)
#   #
#   S.value <- rowSums(S, na.rm=T)
#
#
#
#   ## Stopped here! Seems to be correct!
#
#
#   #
#   # calculate F value depending on period.num (k)
#   # --------------------------------------------------------
#
#
#   # Achtung: Again for all customers Ak0t/Bk0t/Ck0t/Dk0t are in the last colum of A/B/C/D-> ok!
#   # However: A1 is in different column for each customer!
#   f.value <- data.table(period.num = period.num,  S.value = S.value, t.customer = first.purchase$t.customer, A1=A1, C1=C1, Bk0tbar=B[,ncol(B)], Ak0t=A[,ncol(A)], Dk0tbar=D[,ncol(D)], Ck0t=C[,ncol(C)])
#   setkeyv(f.value, "period.num") # order changes now!
#
#   # It makes sense to put in NA for those people which did not start yet!
#   f.value[period.num >  0, f := ((beta_0)^s * r )/ ((s-1) * alpha_0)]
#
#   f.value[period.num == 1, f := f * ((A1*t.customer*(s-1)) / (beta_0+C1*t.customer)^s + (A1/C1)/beta_0^(s-1) - (A1*(t.customer*s + 1/C1*beta_0))/(beta_0+C1*t.customer)^s)]
#   f.value[period.num >  1, f := f * (( (Bk0tbar + t.customer*Ak0t)       *(s-1)) / (beta_0+Dk0tbar  + t.customer*Ck0t      )^s + S.value)]
#
#   #the sorting is not by ID anymore, but by period.num!
#   #Doesnt matter as returned summed -> not in this version, careful!
#   # return(f.value$f)
#   return(f.value[, sum(f, na.rm = TRUE)])
# }
#
