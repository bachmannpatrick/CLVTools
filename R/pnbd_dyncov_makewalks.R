pnbd_dyncov_creatwalks_add_tjk <- function(dt.walk, clv.time){
  # time between Trans and the previous Trans / from date.lagged to date
  dt.walk[, tjk := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                  interv = interval( start = tp.previous.trans,
                                                                     end   = tp.this.trans))]
  return(dt.walk)
}

pnbd_dyncov_creatwalks_add_d <- function(dt.walk, clv.time){
  # **TODO: +1 or not?
  # time between date.lagged and period end (ceiling(data.lagged+1))
  # d shall be 1 if it is exactly on the time.unit boundary!
  # Plus.Eps is already "+ 1"
  dt.walk[, d := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                interv = interval( start = tp.previous.trans,
                                                                   end   = clv.time.ceiling.date(clv.time=clv.time,
                                                                                                 timepoint=tp.previous.trans)))]
  return(dt.walk)
}

pnbd_dyncov_creatwalks_add_delta <- function(dt.walk){
  dt.walk[, delta := NA_real_]
  # **TODO: implement, requires walk_id?
  return(dt.walk)
}

# pnbd_dyncov_extractwalk <- function(dt.cov.id, tp.lower, tp.upper){
#   # Extract covariates which were active for this transaction (tp.upper)
#   # **TODO: What if tp.lower == tp.upper? Such as when last trans on estimation end (for aux walk)
#
#   dt.walk <- dt.cov.id[Cov.Date > tp.lower & Cov.Date <= tp.upper]
#
#   # ** TODO: Or only add to table at the very end when all walks are combined
#   dt.walk <- pnbd_dyncov_extractwalk_add_d(dt.walk)
#
#   dt.walk <- pnbd_dyncov_extractwalk_add_tjk(dt.walk)
#
#   # ** TODO: What counts as Num.Walk? Actual content or 1 cov (walk1 + max.walk = 1 or 2)
#   # delta: if Num.Walk > 1 -> 1, otherwise 0
#   if(nrow(dt.walk) > 1){
#     dt.walk[, delta := 1]
#   }else{
#     dt.walk[, delta := 0]
#   }
#
#   dt.walk[, tp.transaction := tp.upper]
#
#   return(dt.walk)
# }


pnbd_dyncov_creatwalks_matchcovstocuts <- function(dt.cov, dt.cuts, names.cov){
  by.covs <- c("Id", "tp.cov.lower", "tp.cov.upper")
  by.cuts <- c("Id", "tp.cut.lower", "tp.cut.upper")
  setkeyv(dt.cov, by.covs)
  setkeyv(dt.cuts, by.cuts)

  dt.matched <- foverlaps(x = dt.cuts, y = dt.cov, by.x = by.cuts, by.y = by.covs,
                          type="any", mult="all", nomatch = NULL)
  return(dt.matched)
}

pnbd_dyncov_createwalks_singletrans <- function(dt.cov, dt.tp.first.last,
                                                name.lower, name.upper,
                                                names.covs, clv.time){
  dt.cuts <- copy(dt.tp.first.last)
  dt.cuts[, tp.cut.lower := get(name.lower) + clv.time.epsilon(clv.time)]
  dt.cuts[, tp.cut.upper := get(name.upper)]
  dt.cuts[get(name.lower) == get(name.upper), tp.cut.lower := get(name.lower)]

  dt.walks <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov, dt.cuts = dt.cuts,
                                                     names.cov = names.covs)

  dt.walks[, tp.this.trans := get(name.upper)]
  dt.walks[, tp.previous.trans := get(name.lower)]

  dt.walks <- pnbd_dyncov_creatwalks_add_delta(dt.walks)
  dt.walks <- pnbd_dyncov_creatwalks_add_tjk(dt.walks, clv.time = clv.time)
  dt.walks <- pnbd_dyncov_creatwalks_add_d(dt.walks, clv.time = clv.time)

  return(dt.walks)
}

pnbd_dyncov_covariate_add_interval_bounds <- function(dt.cov, clv.time){
  # Covariate intervals are closed intervals + Cov.Date marks beginning (Covs are forward-looking)
  #   => [Cov.Date, Next Cov.Date - eps] <=> [Cov.Date, Cov.Date + 1 Period - eps]

  dt.cov[, tp.cov.lower := Cov.Date]

  # Do not use shift() because leaves NA which will then have to be fixed
  single.timeperiod <- clv.time.number.timeunits.to.timeperiod(clv.time, user.number.periods=1L)
  dt.cov[, tp.cov.upper := tp.cov.lower + single.timeperiod - clv.time.epsilon(clv.time)]

  # **TODO: test: same as if shifted + short manual
  #         test: no NAs in tp columns
  # dt.cov[, tp.cov.upper := shift(tp.cov.lower, n=-1L), by="Id"]
  # dt.cov[, tp.cov.upper := tp.cov.upper - clv.time.epsilon(clv.time)]

  return(dt.cov)
}

pnbd_dyncov_createwalks_trans <- function(clv.data){

  # For every customer, find tp of first and last transaction
  dt.trans <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.tp.first.last <- dt.trans[, list(tp.first.trans = min(Date),
                                      tp.last.trans = max(Date),
                                      num.trans = .N),
                               by="Id"]

  # clv.data.get.covariate.data.with.intervals.trans
  dt.cov <- clv.data@data.cov.trans
  names.covs <- clv.data.get.names.cov.trans(clv.data)

  # Real Walks -----------------------------------------------------------------------
  # Covariates affecting repeat-transactions
  #   Zero-repeaters have 0 real walks (**TODO: what, really..??)
  #   No walk for the first tranaction (**TODO: what, really, also..??)

  # remove zero-repeaters
  dt.cuts.real <- dt.trans[dt.tp.first.last[num.trans > 1, "Id"], on="Id", nomatch=NULL]
  dt.tp.first.last[, num.trans := NULL]

  setkeyv(dt.cuts.real, cols=c("Id", "Date"))
  # Cannot/Should have no 2 transactions on same tp because are aggregated,
  #   min dist is 1 eps, can fall together again
  dt.cuts.real[, tp.this.trans := Date]
  dt.cuts.real[, tp.previous.trans := shift(tp.this.trans, n=1), by="Id"]
  dt.cuts.real[, tp.cut.lower := tp.previous.trans + clv.time.epsilon(clv.data@clv.time)]
  dt.cuts.real[, tp.cut.upper := tp.this.trans]

  # remove cut for first transaction
  #   - which has NA in tp.previous.trans (cannot match to cov)
  #   - for which no walk shall be created
  # dt.cuts.real <- dt.cuts.real[!is.na(tp.previous.trans)]
  dt.cuts.real[, is.first := Date == min(Date), by="Id"]
  dt.cuts.real <- dt.cuts.real[is.first == FALSE]
  dt.cuts.real[, is.first := NULL]

  # **TODO: test: Even if trans only 1 eps apart, gives walk
  #         test: Number of walks = num repeat trans
  # dt.cuts.real[tp.cut.lower > tp.cut.upper, tp.cut.lower := tp.cut.upper]??
  # print(dt.cuts.real)
  # print(dt.cuts.real[is.na(tp.cut.lower)])

  dt.walks.real <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov,
                                                          dt.cuts = dt.cuts.real,
                                                          names.cov = names.covs)

  dt.walks.real <- pnbd_dyncov_creatwalks_add_delta(dt.walks.real)
  dt.walks.real <- pnbd_dyncov_creatwalks_add_tjk(dt.walks.real, clv.data@clv.time)
  dt.walks.real <- pnbd_dyncov_creatwalks_add_d(dt.walks.real, clv.data@clv.time)
  dt.walks.real[, AuxTrans := FALSE]


  # Aux Walks ------------------------------------------------------------------------
  dt.tp.first.last[, tp.estimation.end := clv.data@clv.time@timepoint.estimation.end]
  dt.walks.aux <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov,
                                                      name.lower="tp.last.trans",
                                                      name.upper="tp.estimation.end",
                                                      dt.tp.first.last=dt.tp.first.last,
                                                      names.covs=names.covs,
                                                      clv.time=clv.data@clv.time)
  dt.walks.aux[, AuxTrans := TRUE]

  cols.walks <- c("Id", "tp.this.trans", "AuxTrans", "tp.cov.lower", "Cov.Date", names.covs,
                  "tjk", "d", "delta")
  return(rbindlist(list(dt.walks.real[, .SD, .SDcols = cols.walks],
                        dt.walks.aux[, .SD, .SDcols = cols.walks])))
}

pnbd_dyncov_createwalks_life <- function(clv.data){

  # For every customer, find tp of first and last transaction
  dt.trans <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.tp <- dt.trans[, list(tp.first.trans = min(Date),
                           tp.last.trans = max(Date)), by="Id"]

  dt.cov <- clv.data@data.cov.life
  names.covs <- clv.data.get.names.cov.life(clv.data)

  # Real Walks -----------------------------------------------------------------------
  # **TOOD: Keep Zero-repeaters? Because for transaction process, only repeat-transactions (zero repeaters may have no walks)
  dt.walks.real <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov, dt.tp.first.last=dt.tp,
                                                       name.lower="tp.first.trans",
                                                       name.upper="tp.last.trans",
                                                       names.covs=names.covs,
                                                       clv.time=clv.data@clv.time)
  dt.walks.real[, AuxTrans := FALSE]


  # Aux Walks ------------------------------------------------------------------------
  dt.tp[, tp.estimation.end := clv.data@clv.time@timepoint.estimation.end]
  dt.walks.aux <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov, dt.tp.first.last=dt.tp,
                                                      name.lower="tp.last.trans",
                                                      name.upper="tp.estimation.end",
                                                      names.covs=names.covs,
                                                      clv.time=clv.data@clv.time)
  dt.walks.aux[, AuxTrans := TRUE]

  cols.walks <- c("Id", "tp.this.trans", "AuxTrans", "tp.cov.lower", "Cov.Date", names.covs,
                  "tjk", "d", "delta")
  return(rbindlist(list(dt.walks.real[, .SD, .SDcols = cols.walks],
                        dt.walks.aux[, .SD, .SDcols = cols.walks])))
}





# Creates all walks for
#' @importFrom lubridate force_tz
#' @importFrom methods is
pnbd_dyncov_makewalks <-function(clv.data){

  Id <- Date <- Cov.Date <- Price <- AuxTrans <- Mapping.Transaction.Id <- is.first.trans <- NULL
  data.dyn.cov.life  <- copy(clv.data@data.cov.life)
  data.dyn.cov.trans <- copy(clv.data@data.cov.trans)

  names.cov.life  <- clv.data@names.cov.data.life
  names.cov.trans <- clv.data@names.cov.data.trans

  #copy as will be manipulated by ref
  trans.dt  <- copy(clv.data@data.transactions[, c("Id", "Date")])

  # NEW IN cleanup-dyncov --------------------------------------------
  # CONVERT DATES TO POSIXCT!
  #   Force the Dates at "facevalue" to posixct and cutoff any time
  if(is(clv.data@clv.time, "clv.time.date")){
    data.dyn.cov.life[,  Cov.Date := floor_date(force_tz(as.POSIXct.Date(Cov.Date), tzone = "UTC"), unit="day")]
    data.dyn.cov.trans[, Cov.Date := floor_date(force_tz(as.POSIXct.Date(Cov.Date), tzone = "UTC"), unit="day")]
    trans.dt[, Date := floor_date(force_tz(as.POSIXct.Date(Date), tzone = "UTC"), unit="day")]
  }

  #
  #do not restrain covariates to estimation period here - needed longer for interval building


  # Include AuxTrans for all customers -------------------------------
  #   Add AuxTrans on Estimation end date, 1 for every customer
  #   Needs to be done before removing the first transactions
  # ** This may lead to 2 transaction on the same date when last trans is on estimation.end !!?? **

  # Add column for all real transactions first
  trans.dt[, AuxTrans:=FALSE]

  #Add artificial aux transactions ------------------------------------
  # NEW IN cleanup-dyncov
  # CONVERT DATES TO POSIXCT!
  if(is(clv.data@clv.time, "clv.time.date")){
    dt.aux.transactions <- data.table( Id   = trans.dt[,unique(Id)],
                                       Date = floor_date(force_tz(as.POSIXct(clv.data@clv.time@timepoint.estimation.end), tzone = "UTC"), unit="day"),
                                       AuxTrans=TRUE)
  }else{
    dt.aux.transactions <- data.table( Id   = trans.dt[,unique(Id)],
                                       Date = clv.data@clv.time@timepoint.estimation.end,
                                       AuxTrans=TRUE)
  }

  #Append dt.aux.transactions to ordinary transactions
  trans.dt          <- rbindlist(list(trans.dt, dt.aux.transactions), use.names = TRUE)


  # Temporary unique transaction Id ----------------------------------------
  #   - Is needed to subset by
  #   - Will be written to every covariate
  #     between transactions to identicate
  #       which covs belongs to which trans!

  #add unique transaction id
  trans.dt[, Mapping.Transaction.Id:=seq.int(from=1, to=nrow(trans.dt))]

  # Inidicate first transaction here as this is needed in many cases
  #   AuxTrans needs to be FALSE, as sometimes AuxTrans falls together with first trans
  trans.dt[, is.first.trans := ifelse(Date == min(Date) & AuxTrans == FALSE, TRUE, FALSE), by=Id]




  # Create Walks for transaction covariate --------------------------------
  #   Only if there are trans covs

  if(!is.null(names.cov.trans)){
    # create actual walks
    trans.walks <- pnbd_dyncov_createwalks(clv.time=clv.data@clv.time, data.transactions = trans.dt, data.dyn.cov = data.dyn.cov.trans, names.dyn.cov = names.cov.trans)
    #rename covariate on transaction date
    for(w.dt in trans.walks){setnames(w.dt, "Cov.on.trans.date","transaction.cov.dyn")}
  }else{
    trans.walks <- list()
  }



  # Create Walks for lifetime covariate -----------------------------------
  #   Only if there are life covs

  if(!is.null(names.cov.life)){

    # Only 2 Transactions per customer are relevant -----------------------
    #
    #   But transaction table needs these trans:
    #     - (1) AuxTrans
    #     - (2) Last Trans before AuxTrans
    #     - (3) Very first trans
    #   (3) is needed to create cov interval from 0-x
    #       (same as in implementation before)
    #   (3) will be removed in CreateWalk - only 2 Trans left
    #   As (2) and (3) can be the same, but (2) may not be removed in
    #      CreateWalks, set its "is.first.trans" = FALSE (=will not remove)

    # Date=max(Date) only correct if Date<=date.estimation.end (!)
    if(is(clv.data@clv.time, "clv.time.date")){
      before.aux <- trans.dt[AuxTrans == FALSE & Date <= floor_date(force_tz(as.POSIXct.Date(clv.data@clv.time@timepoint.estimation.end), tzone = "UTC"), unit="day"), .SD[Date == max(Date)], by=Id]
    }else{
      before.aux <- trans.dt[AuxTrans == FALSE & Date <= clv.data@clv.time@timepoint.estimation.end, .SD[Date == max(Date)], by=Id]
    }

    # do not remove in CreateWalks, in case it is the same as the very first trans (1)
    before.aux[, is.first.trans := FALSE]

    life.cov.trans <- rbindlist(list( trans.dt[is.first.trans == TRUE],  #(3)
                                      before.aux,                        #(2)
                                      trans.dt[AuxTrans == TRUE]),       #(1)
                                use.names = TRUE, fill=FALSE)

    # create actual walks
    life.walks <- pnbd_dyncov_createwalks(clv.time=clv.data@clv.time, data.transactions = life.cov.trans,
                                          data.dyn.cov = data.dyn.cov.life, names.dyn.cov = names.cov.life)

    #rename covariate on transaction date
    for(w.dt in life.walks){setnames(w.dt, "Cov.on.trans.date","lifetime.cov.dyn")}
  }else{
    life.walks <- list()
  }


  return(list(data.walks.life  = life.walks,
              data.walks.trans = trans.walks))
}
