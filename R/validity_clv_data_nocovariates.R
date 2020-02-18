setValidity(Class = "clv.data", method = function(object){

  # Do not callNext()

  err.msg <- c()

  # Only when it contains transactions, the object can be considered initialized, before that
  #   not validity checking makes sense as all the checks are based on this assumption of having
  #   transactions. (and NAs break if in comparison)
  if(nrow(object@data.repeat.trans) == 0)
    return(TRUE)

  # name -------------------------------------------------------------------
  if(anyNA(object@name))
    err.msg <- c(err.msg, "The name for a clv data object may not contain any NA")

  if(length(object@name) != 1)
    err.msg <- c(err.msg, "Only a single name can be given for a clv data object!")

  if(nchar(object@name) == 0)
    err.msg <- c(err.msg, "The clv data object must be named")

  # has.holdout ------------------------------------------------------------
  # verify correctnes of has.holdout, but also needs stuff from clv.time

  if(object@has.holdout){
    if(object@clv.time@holdout.period.in.tu <= 0)
      err.msg <- c(err.msg, "Holdout period needs to be > 0 if there is a holdout period")
  }else{
    # no holdout
    if(object@clv.time@timepoint.holdout.end != object@clv.time@timepoint.holdout.start)
      err.msg <- c(err.msg, "Holdout start and end need to be same")

    if(object@clv.time@timepoint.holdout.end != object@clv.time@timepoint.estimation.end)
      err.msg <- c(err.msg, "Holdout start and end both need to be estimation.end if there is no holdout")

    if(object@clv.time@holdout.period.in.tu != 0)
      err.msg <- c(err.msg, "Holdout period needs to be == 0, if there is no holdout period")
  }

  # Data transactions -----------------------------------------------------
  if(!all(c("Id", "Date") %in% colnames(object@data.transactions)))
    err.msg <- c(err.msg, "Transaction data needs to contain the following columns: Id, Date")

  if(!object@data.transactions[, is.Date(Date) | is.POSIXct(Date)])
    err.msg <- c(err.msg, "Date column in data.transactions needs to be either of class posixct or Date")

  if( (object@data.transactions[, is.Date(Date)] & !is.Date(object@clv.time@timepoint.estimation.start)) |
      (object@data.transactions[, is.POSIXct(Date)] & !is.POSIXct(object@clv.time@timepoint.estimation.start)))
    err.msg <- c(err.msg, "Date column in data.transactions has to be of the same type as the timepoints in clv.time")


  if(!object@data.transactions[, is.character(Id)])
    err.msg <- c(err.msg, "Id column in data.transactions needs to be of class character")

  if(ncol(object@data.transactions) == 3){
    if(!"Price" %in% colnames(object@data.transactions))
      return("The spending column in the transaction data needs to be named Price")

    if(!object@data.transactions[, is.numeric(Price)])
      err.msg <- c(err.msg, "Price column in data.transactions needs to be numeric")
  }


  # Data repeat transactions ------------------------------------------------
  if(!all(c("Id", "Date") %in% colnames(object@data.repeat.trans)))
    err.msg <- c(err.msg, "Transaction data needs to contain the following columns: Id, Date")

  if(!object@data.repeat.trans[, is.Date(Date) | is.POSIXct(Date) ])
    err.msg <- c(err.msg, "Date column in data.repeat.trans needs to be either of class posixct or Date")

  if( (object@data.repeat.trans[, is.Date(Date)] & !is.Date(object@clv.time@timepoint.estimation.start)) |
      (object@data.repeat.trans[, is.POSIXct(Date)] & !is.POSIXct(object@clv.time@timepoint.estimation.start)))
    err.msg <- c(err.msg, "Date column in data.repeat.trans has to be of the same type as the timepoints in clv.time")

  if(!object@data.repeat.trans[, is.character(Id)])
    err.msg <- c(err.msg, "Id column in data.repeat.trans needs to be of class character")

  if(ncol(object@data.repeat.trans) == 3){
    if(!"Price" %in% colnames(object@data.repeat.trans))
      return("The spending column in the transaction data needs to be named Price")

    if(!object@data.repeat.trans[, is.numeric(Price)])
      err.msg <- c(err.msg, "Price column in data.repeat.trans needs to be numeric")
  }

  # descriptives.transactions --------------------------------------------------
  if(!object@descriptives.transactions[, all(sapply(.SD, is.character))])
    err.msg <- c(err.msg, "All columns in descriptives.transactions need to be characters")

  if(!all(c("Name" ,"Estimation", "Holdout", "Total") %in% colnames(object@descriptives.transactions)))
    err.msg <- c(err.msg, "descriptives.transactions has to contain columns named Name, Estimation, Holdout, Total")

  if(!object@has.holdout){
    if(object@descriptives.transactions[, !all(Holdout == "-")])
      err.msg <- c(err.msg, "There may be no values in column Holdout (except -) in descriptives.transactions if there is no holdout")
  }else{
    if(object@descriptives.transactions[, all(Holdout == "-")])
      err.msg <- c(err.msg, "There must be values in column Holdout (not -) in descriptives.transactions if there is a holdout period")
  }


  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})

# VALIDITY **TODO**
# nrow(trans) - length(unique(Id)) = nrow(repeat.trans)
# is repeat.trans[, min(Date),by="Id"] == cbs[, first.repeat.trans]
# total.descr$#cust = estimation.descr$#cust
# Always has exactly columns Id, Date, Price


