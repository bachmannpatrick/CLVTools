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
