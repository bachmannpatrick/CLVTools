# Separate function for output of single table:
#   Calculate exp.gX.P/L
#   Cut to alive periods per customer
#   Cut to maximum date
#     No covariate after date.upper.cov is considered. Do floor_timeunit() as required before calling
# Do it for life and trans together because it is much easier to pass in
#   single clv.fitted and not all parts alone
pnbd_dyncov_alivecovariates <- function(clv.fitted, date.upper.cov){

  # For more readable code
  clv.time <- clv.fitted@clv.data@clv.time

  names.cov.life  <- clv.fitted@clv.data@names.cov.data.life
  names.cov.trans <- clv.fitted@clv.data@names.cov.data.trans

  # Initial cutting:
  #   Values are calculated for all available data since start of estimation
  #     and only cut to required shorter length later as required
  #     -> min lower: floor_tu(estimation.start)
  #   No covariate after date.upper.cov is considered
  date.first.cov <- clv.time.floor.date(clv.time=clv.time,timepoint=clv.time@timepoint.estimation.start)

  # Only keep cov data between first and last allowable date
  dt.life   <- clv.fitted@clv.data@data.cov.life[Cov.Date  >= date.first.cov & Cov.Date <= date.upper.cov, .SD, .SDcols = c("Id", "Cov.Date", names.cov.life)]
  dt.trans  <- clv.fitted@clv.data@data.cov.trans[Cov.Date >= date.first.cov & Cov.Date <= date.upper.cov, .SD, .SDcols = c("Id", "Cov.Date", names.cov.trans)]

  # matrix multiply per date and customer ---------------------------------------------------------------------
  #   matrix multiplication by=c("date", "Id") is extremely slow, but can do cov1*g1+cov2*g2+cov3*g3+...
  #   However, row-wise vec*DT is not possible unless also a data.frame/table of same dimension.
  #     Therefore, add gammas as separate columns to have them as data.table as well

  # Gammas need to have separate names from cov data
  #   Also, this step ensures same order of names and gammas when using as .SDcols when multiplying
  names.gamma.life  <- paste0("gamma.", names.cov.life)
  names.gamma.trans <- paste0("gamma.", names.cov.trans)

  # Write gamma values in gamma columns (one whole column per gamma value)
  #   Subset with names.cov.* to ensure the same order as names.gamma.*
  dt.life[,  (names.gamma.life)  := data.table(t(clv.fitted@prediction.params.life[names.cov.life]))]
  dt.trans[, (names.gamma.trans) := data.table(t(clv.fitted@prediction.params.trans[names.cov.trans]))]

  # Multiply data * gammas element-wise and sum row-wise
  #   actually multiplies data.table*data.table element-wise and then rowSums is like sum per c("Id","Cov.Date")
  dt.life[,  exp.gX.L := exp(rowSums(dt.life[, .SD,  .SDcols=names.gamma.life]  * dt.life[,  .SD, .SDcols=names.cov.life]))]
  dt.trans[, exp.gX.P := exp(rowSums(dt.trans[, .SD, .SDcols=names.gamma.trans] * dt.trans[, .SD, .SDcols=names.cov.trans]))]

  # Cut to when alive ---------------------------------------------------------------------------------
  # Calculate the period when customer became alive.
  #   Do this outside dt.life once for every customer in a separate table to avoid calling
  #     clv.time.floor.date (S4 function) for every row separately
  dt.customer.alive <- clv.fitted@cbs[, c("Id", "date.first.actual.trans")]
  dt.customer.alive[, date.cov.period.coming.alive := clv.time.floor.date(clv.time=clv.time,
                                                                          timepoint=date.first.actual.trans)]

  # Add when customers became alive to every cov data
  dt.life[ dt.customer.alive, date.cov.period.coming.alive := i.date.cov.period.coming.alive, on="Id"]
  dt.trans[dt.customer.alive, date.cov.period.coming.alive := i.date.cov.period.coming.alive, on="Id"]

  # Only the covariates at which the customer is alive
  #   Including the one active when customer became alive (by doing floor_date before)
  dt.life[, is.alive.in.period := Cov.Date >= date.cov.period.coming.alive]
  dt.trans[, is.alive.in.period := Cov.Date >= date.cov.period.coming.alive]
  dt.life  <- dt.life[is.alive.in.period == TRUE]
  dt.trans <- dt.trans[is.alive.in.period == TRUE]

  return(list(dt.trans = dt.trans, dt.life = dt.life))
}




pnbd_dyncov_ABCD <- function(clv.fitted, prediction.end.date){
  # For more understandable, clean code
  clv.time <- clv.fitted@clv.data@clv.time

  # Covariate Values ---------------------------------------------------------------------------------------------------
  # Calculate values from covariates and gammas, ie exp(gammas*cov) and cuts to only these when customer is alive
  #
  # The Ai, Bi, Ci, Di parts have to be built, so that the last (=the ith)
  #   covariate is active when prediction.end.date is
  #   If prediction.end.date falls directly onto the start of a covariate,
  #     the covariate is active then and included as well.
  #     => max cov: floor_tu(prediction.end.date)
  #
  #   The first covariate in the prediction (i=1) is the one active when estimation.end is
  #     The covariate start date hence can be before the holdout.start and is also already
  #       used in the fitting
  #       => i=1: floor_tu(estimation.end)

  date.last.cov  <- clv.time.floor.date(clv.time=clv.time, timepoint=prediction.end.date)

  l.covs <- pnbd_dyncov_alivecovariates(clv.fitted = clv.fitted, date.upper.cov = date.last.cov)
  dt.trans <- l.covs[["dt.trans"]]
  dt.life  <- l.covs[["dt.life"]]


  # i ---------------------------------------------------------------------------------------------------
  # Define prediction period number i per customer
  # As per definition, the first prediction period is the period where estimation.end lies in.
  #   Even if it was possible to properly separate estimation and prediction periods
  #     (ie they estimation.end and holdout.start are in separate periods) this is enforced because the
  #   notation relies on it and corrects for it (ie i is period num k0T+i-1, minus 1 to avoid double counting this overlap of periods)

  # First period is the cov period in which the estimation end lies! (not the holdout.start)
  date.first.prediction.period.start <- clv.time.floor.date(clv.time = clv.time,
                                                            timepoint = clv.time@timepoint.estimation.end)

  # Order by covariate date!
  #   1 = Smallest date up
  setorderv(dt.life,  cols = "Cov.Date", order = 1)
  setorderv(dt.trans, cols = "Cov.Date", order = 1)

  # Write all i to data, by Id!
  #   Leave all i before prediction start intenionally as NA
  #   .N is nrow() - after cut to prediction period
  dt.life[Cov.Date  >= date.first.prediction.period.start, i := seq.int(from = 1, to = .N), by="Id"]
  dt.trans[Cov.Date >= date.first.prediction.period.start, i := seq.int(from = 1, to = .N), by="Id"]

  # Table with results data ----------------------------------------------------------------------------------
  #   Values for each Id/Cov combination in the prediction period
  #     in the prediction period = has value for i
  dt.ABCD <- dt.life[!is.na(i), c("Id", "Cov.Date", "exp.gX.L", "i")]
  setkeyv(dt.ABCD, cols = c("Id", "Cov.Date", "i"))

  # Add transaction g*cov data
  setkeyv(dt.trans, cols = c("Id", "Cov.Date"))
  dt.ABCD[dt.trans, exp.gX.P := i.exp.gX.P, on=c("Id", "Cov.Date")]


  # Ai & Ci ---------------------------------------------------------------------------------------------------
  # They are both simply the gamma*cov values in the prediction period, for all Ids and dates
  dt.ABCD[, Ai := exp.gX.P]
  dt.ABCD[, Ci := exp.gX.L]


  # Bbar_i ------------------------------------------------------------------------------------------------------
  #   Only the prediction part, from i=1
  #   k0T=0, only use i
  #   nothing that is before prediction start
  #   role of domega: d1 = partial period from Tcal to ceiling_tu(Tcal)

  # d1 is same value for all customers and periods
  d1 <- clv.time.interval.in.number.tu(interv = interval(start = clv.time@timepoint.estimation.end,
                                                         end = clv.time.ceiling.date(clv.time,
                                                                                     timepoint = clv.time@timepoint.estimation.end)),
                                       clv.time = clv.time)
  dt.ABCD[, d1 := d1]
  dt.ABCD[,     Bbar_i := exp.gX.P]
  dt.ABCD[i==1, Bbar_i := exp.gX.P*d1]

  # Bbar is all cov data of lower i's (previous periods) summed up - per customer!
  #   Cannot do cumsum(exp.gX.P) because also need to include *d1 in cumsum
  dt.ABCD[, Bbar_i := cumsum(Bbar_i), by="Id"]

  # Add T.cal from cbs
  dt.ABCD[clv.fitted@cbs, T.cal := i.T.cal, on="Id"]

  # Every last cov data needs to be calculated differently
  # All i's mark the last period of 1..i and therefore need to be adapted
  #   Subtract cov value at i because already included in cumsum
  #   These adaptions are done for every i>1, unspecific to customer (ie can be done row-wise)
  dt.ABCD[i > 1, Bbar_i := (Bbar_i - exp.gX.P) + exp.gX.P*(-T.cal - d1 - (i-2))]

  # First also needs to be adapted
  #   i=1: exp.gX.P * -Tcal
  dt.ABCD[i == 1, Bbar_i :=  exp.gX.P*(-T.cal)]


  # Dbar_i ----------------------------------------------------------------------------------------------------
  # Based on all cov data since the customer came alive / the cov that awoke the customer / the covariate that was active when became customer
  #   = also before prediction period -> Use dt.life and not just dt.ABCD
  #
  # A note on the notation:
  # With k0T+i-1 the notation refers to the current period of Dbar_i because the last estimation period and
  #   first prediction period are forced to always overlap and otherwise would be counted double when
  #   summing k0T and i.
  # For the same reason it is -3 and not -2 when adapting the last i-th cov
  # k0T+i-3 hence counts the summed middle-elements
  #   Counting k0T is however somewhat error-prone because of the involved period definitions and
  #     edge-cases. Therefore, the number of middle cov periods are calculated instead by counting
  #     all periods since becoming alive until i and substracting 2. (ie number periods since alive - 2)

  dt.Dbar <- dt.life

  # Dbar is summed up lifetime cov data
  dt.Dbar[, Dbar_i := exp.gX.L]

  # First period when coming alive is *d_omega
  #   Needs to be added because not in dt.life, only in dt.ABCD
  dt.Dbar[clv.fitted@cbs, d_omega := i.d_omega, on="Id"]
  # min(cov.date) per Id!
  dt.Dbar[, is.customers.first.cov := Cov.Date == min(Cov.Date), by="Id"]
  dt.Dbar[is.customers.first.cov == TRUE, Dbar_i := exp.gX.L*d_omega]

  # At every period: Sum of all previous lifetime cov data
  setorderv(x = dt.Dbar, cols = "Cov.Date", order = 1)
  dt.Dbar[, Dbar_i := cumsum(Dbar_i) , by="Id"]

  # Every i is a "last" that needs to be adapted
  #   Instead of calculating the number of periods to subtract (k0T+i-3), they are counted from becoming alive
  #   Current covdata at i is already wrongly in Dbar_i through cumsum() therefore subtract it from Dbar_i
  #
  #   k0T+i > 2 : * (-d.omega - (k0T+i-3))
  #   k0T+i <= 2: * (-d.omega)
  #     k0T+i <= 2: "coming alive only just in the period of estimation.end = first prediction period"
  #   k0T+i-3 = num.periods.alive-2 => k0T+i = num.periods.alive+1

  # Count of periods alive at every point
  #   Still ordered by cov.date
  dt.Dbar[, num.period.alive := seq.int(from = 1, to = .N), by="Id"]

  # Only keep prediction period
  dt.Dbar <- dt.Dbar[!is.na(i)]

  # Last period adaption
  dt.Dbar[, Dbar_i := (Dbar_i- exp.gX.L) + exp.gX.L*(-d_omega - (num.period.alive-2))]
  # Special case: k0T+i <= 2 <=> num.periods.alive+1 <= 2  <=> "only alive 1 period": *(-d_omega) only
  dt.Dbar[num.period.alive+1 <= 2, Dbar_i := (Dbar_i- exp.gX.L) + exp.gX.L*(-d_omega)]

  # Write results to final results table
  dt.ABCD[dt.Dbar, Dbar_i:= Dbar_i, on=c("Id", "i")]

  # Return ------------------------------------------------------------------------------------
  setkeyv(dt.ABCD, c("Id", "Cov.Date", "i"))

  return(dt.ABCD)
}

# # **TEST: that all cov date < (and <= if required) prediction end
# # **TEST: Gamma=0:
# # Dbar_i == 0
# stopifnot(dt.ABCD[, all(Dbar_i < sqrt(.Machine$double.eps))])
# # Bbar_i == -T.cal
# #   Actually, if all covs are same: Bbar_i = exp(gXP)*(-Tcal)
# stopifnot(dt.ABCD[, all(Bbar_i+T.cal < sqrt(.Machine$double.eps))])


# CODE k0T+i for DBar_i -----------------------------------------------------------------------------------------
#   # . Part based on cov before prediction ---------------------------------------------------------------------
#   dt.Dbar.before.prediction <- dt.life[is.alive.in.period == TRUE]
#
#   # Sum up everything before D_i=1 (ie before prediction started)
#   #   This is where i == NA
#   dt.Dbar.before.prediction <- dt.Dbar.before.prediction[is.na(i)]
#
#
#   # Sum all cov data (exp.gX.L) in this before-prediction-data
#   #   except the first of every customer which has to be *d_omega
#
#   # All other than first Dbar are exp.gX.L
#   dt.Dbar.before.prediction[, part.before := exp.gX.L]
#
#   # First cov a customer has been alive: *d_omega
#   #   Add d_omega from cbs
#   dt.Dbar.before.prediction[clv.fitted@cbs, d_omega := i.d_omega, on="Id"]
#   dt.Dbar.before.prediction[, is.customers.first.cov := Cov.Date == min(Cov.Date), by="Id"]
#   dt.Dbar.before.prediction[is.customers.first.cov == TRUE, part.before := exp.gX.L*d_omega]
# print(dt.Dbar.before.prediction[Id=="10000094635"|Id=="1"])
#   # Sum all by customer
#   dt.Dbar.before.sum <- dt.Dbar.before.prediction[, .(cov.sum.before.prediction = sum(part.before)), by="Id"]
#
#   # Add to overall table to use for Dbar
#   dt.ABCD[dt.Dbar.before.sum, cov.sum.before.prediction := i.cov.sum.before.prediction, on="Id"]
#
#   # . k0T -----------------------------------------------------------------------------------------------
#   # k0T is needed in the prediction part
#   # k0T: Number of covariates touched since becoming alive until estimation end
#   #       = Number of covariates that affected customer between becoming alive and estimation end
#   #   Should be <= because if on Cov.Date, the Cov has an effect (ie Cov.Date is when the Cov is active the first time, when it starts)
#
#   # Cannot use dt.Dbar.before.prediction because it is strictly withouth prediction period.
#   #   But cov of estimation end is always also part of prediction period (as i=1) and
#   #     hence not in dt.Dbar.before.prediction. Therefore, use dt.life.
#   #   dt.life has already marked covs when alive
#   #   Count these covariates until estimation.end for every customer
#   dt.k0T <- dt.life[is.alive.in.period == TRUE &
#                       Cov.Date <= clv.time@timepoint.estimation.end, .(k0T =.N), by="Id"]
#
#   print(dt.life[is.alive.in.period == TRUE &
#                   Cov.Date <= clv.time@timepoint.estimation.end][Id == "10000094635"|Id=="1"])
#
#   dt.ABCD[dt.k0T, k0T := i.k0T, on="Id"]
#   # **TEST correctness: For count of k0T
#
#   # . Prediction part of Dbar --------------------------------------------------------------------------
#
#   # Make every Dbar_i the sum of
#   #   - all previous covs in the prediction period
#   #   and
#   #   - all cov data before prediction period
#   setorderv(x = dt.ABCD, cols = "Cov.Date", order = 1)
#   dt.ABCD[, Dbar_alluntilandincludingthis := cumsum(exp.gX.L) + cov.sum.before.prediction, by="Id"]
#
#
#   # Every i is a "last" that needs to be adapted
#   #   Current covdata at i is already wrongly in Dbar_i through cumsum()
#   #     therefore subtract it from Dbar_i
#   #   k0T+i > 2 : * (-d.omega - (k0T+j-3))
#   #   k0T+i <= 2: * (-d.omega)
#   # ***Check vs BTYD with estimation.end at different points that k0T+i actually happens
#   dt.ABCD[k0T+i > 2,  Dbar_i := (Dbar_alluntilandincludingthis-exp.gX.L) + exp.gX.L*(-d_omega-(k0T + i - 3))]
#   dt.ABCD[k0T+i <= 2, Dbar_i := (Dbar_alluntilandincludingthis-exp.gX.L) + exp.gX.L*(-d_omega)]



