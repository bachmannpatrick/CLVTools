#' Transactional data to fit CLV models
#'
#' @description
#' Stores the processed transactional data and holds an object of class \linkS4class{clv.time}
#' which stores further information about the split in an estimation and holdout sample.
#'
#' A \code{clv.data} object serves as input into the various model fitting functions.
#'
#' @slot call Single language of the call used to create the object
#' @slot name Human-readable name of the type of transactional data
#' @slot clv.time clv.time object that stores and is used for processing all timepoint related information
#' @slot data.transactions Single \code{data.table} containing the original transaction data, with columns renamed to 'Id', 'Date', 'Price'
#' @slot data.repeat.trans Single \code{data.table} containing only the repeat transactions
#' @slot has.spending Single logical whether the data contains information about the amount spent per transaction
#' @slot has.holdout Single logical whether the data is split in a holdout and estimation period
#'
#' @seealso \linkS4class{clv.time}
#'
#' @keywords internal
#' @include all_generics.R class_clv_time.R
setClass(Class = "clv.data",
         slots = c(
           call = "language",
           name = "character",

           clv.time          = "clv.time",

           data.transactions = "data.table",
           data.repeat.trans = "data.table",

           has.spending   = "logical",
           has.holdout    = "logical"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           name = character(0),

           data.transactions  = data.table(),
           data.repeat.trans  = data.table(),

           has.spending       = logical(0),
           has.holdout        = logical(0)))


#' @importFrom methods new
clv.data <- function(call, data.transactions, data.repeat.trans, has.spending, clv.time){

  has.holdout <- clv.time.has.holdout(clv.time)

  setkeyv(data.transactions, c("Id", "Date"))
  setkeyv(data.repeat.trans, c("Id", "Date"))

  return(new("clv.data",
             name       = "CLV Transaction Data",
             call       = call,
             clv.time   = clv.time,
             data.transactions = copy(data.transactions),
             data.repeat.trans = copy(data.repeat.trans),
             has.spending = has.spending,
             has.holdout  = has.holdout))
}

clv.data.has.holdout <- function(clv.data){
  return(clv.data@has.holdout)
}

clv.data.has.spending <- function(clv.data){
  return(clv.data@has.spending)
}

clv.data.has.negative.spending <- function(clv.data){
  Price <- NULL

  if(clv.data.has.spending(clv.data = clv.data) == FALSE)
    return(FALSE)

  return(clv.data@data.transactions[Price < 0, .N] > 0)
}

clv.data.get.transactions.in.estimation.period <- function(clv.data){
  Date <- NULL
  return(clv.data@data.transactions[Date <= clv.data@clv.time@timepoint.estimation.end])
}

clv.data.get.transactions.in.holdout.period <- function(clv.data){
  Date <- NULL
  stopifnot(clv.data.has.holdout(clv.data))
  return(clv.data@data.transactions[Date >= clv.data@clv.time@timepoint.holdout.start])
}

clv.data.make.repeat.transactions <- function(dt.transactions){
  Date <- previous <- NULL

  # Copy because alters table
  dt.repeat.transactions <- copy(dt.transactions)

  dt.repeat.transactions[order(Date), previous := shift(x=Date, n = 1L, type = "lag"), by="Id"]
  # Remove first transaction: Have no previous (ie is NA)
  dt.repeat.transactions <- dt.repeat.transactions[!is.na(previous)]
  dt.repeat.transactions[, previous := NULL]

  # Alternative:
  #   Works only because all on same Date were aggregated. Otherwise, there could be more than one removed
  # dt.repeat.transactions[, is.first.trans := (Date == min(Date), by="Id"]
  # dt.repeat.transactions <- dt.trans[is.first.trans == FALSE]

  return(dt.repeat.transactions)
}

# Aggregate what is on same smallest scale representable by time
#   Spending is summed, if present
#   aggregating what is in same time.unit does not not make sense
#   Date: on same day
#   posix: on same second
clv.data.aggregate.transactions <- function(dt.transactions, has.spending){
  Price <- NULL

  if(has.spending){
    dt.aggregated.transactions <- dt.transactions[, list("Price" = sum(Price)), by=c("Id", "Date")]
  }else{
    # Only keep one observation, does not matter which
    # head(.SD) does not work because Id and Date both in by=
    # unique() has the same effect because there are only 2 columns
    dt.aggregated.transactions <- unique(dt.transactions, by=c("Id", "Date"))
  }

  return(dt.aggregated.transactions)
}

# Interpurchase time, for repeaters only
#   Time between consecutive purchases of each customer - convert to intervals then time units
#   If zero-repeaters (only 1 trans) set NA to ignore it in mean / sd calculations
#' @importFrom lubridate int_diff
clv.data.mean.interpurchase.times <- function(clv.data, dt.transactions){
  Id <- num.trans <- Date <- NULL

  num.transactions <- dt.transactions[, list(num.trans = .N), by="Id"]

  return(rbindlist(list(
    # 1 Transaction = NA
    dt.transactions[Id %in% num.transactions[num.trans == 1,Id], list(interp.time = NA_real_, Id)],
    dt.transactions[Id %in% num.transactions[num.trans >  1,Id],
                    list(interp.time = mean(clv.time.interval.in.number.tu(clv.time = clv.data@clv.time,
                                                                        interv = int_diff(Date)))),
                    by="Id"]
  ), use.names = TRUE))
}

#' @importFrom stats sd
#' @importFrom lubridate time_length
clv.data.make.descriptives <- function(clv.data, Ids){

  Id <- Date <- .N <- N <- Price <- interp.time<- Name <- Holdout <- NULL

  # readability
  clv.time <- clv.data@clv.time

  Ids <- unique(Ids)

  # Make descriptives ------------------------------------------------------------------------------
  fct.make.descriptives <- function(dt.data, sample.name){

    # Subset transaction data to relevant Ids
    if(!is.null(Ids)){
      dt.data <- dt.data[Id %in% Ids]

      # print warning only once
      if(sample.name == "Total" & dt.data[, uniqueN(Id)] != length(unique(Ids))){
        warning("Not all given Ids were found in the transaction data.", call. = FALSE)
      }

    }
    dt.interp <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions = dt.data)
    dt.num.trans.by.cust <- dt.data[, .N, by="Id"]

    l.desc <- list(
      "Number of customers"           = if(sample.name=="Total"){nrow(dt.num.trans.by.cust)}else{"-"},
      "First Transaction in period"   = clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.data[, min(Date)]),
      "Last Transaction in period"    = clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.data[, max(Date)]),
      "Total # Transactions"          = nrow(dt.data),
      "Mean # Transactions per cust"  = dt.num.trans.by.cust[, mean(N)],
      "(SD)"                          = dt.num.trans.by.cust[, sd(N)])

    if(clv.data.has.spending(clv.data)){
      l.desc <- c(l.desc, list(
        "Mean Spending per Transaction"  = dt.data[, mean(Price)],
        "(SD) "                          = dt.data[, sd(Price)],
        "Total Spending"                 = dt.data[, sum(Price)]))
    }

    l.desc <- c(l.desc, list(
      # Zero-repeaters can only be in Total ()
      "Total # zero repeaters"        = if(sample.name == "Total"){dt.num.trans.by.cust[N == 1, uniqueN(Id)]}else{"-"},
      "Percentage # zero repeaters"   = if(sample.name == "Total"){dt.num.trans.by.cust[, mean(N==1)]}else{"-"},

      # Inter-purchase time
      #   Remove NAs resulting from zero-repeaters
      "Mean Interpurchase time"       = dt.interp[, mean(interp.time, na.rm=TRUE)],
      "(SD)   "                       = dt.interp[, sd(interp.time, na.rm=TRUE)]))

    # Format numbers
    l.desc <- format(l.desc, digits=3, nsmall=3)

    return(l.desc)
  }

  l.desc.estimation <- fct.make.descriptives(dt.data = clv.data.get.transactions.in.estimation.period(clv.data),
                                             sample.name="Estimation")
  l.desc.total      <- fct.make.descriptives(dt.data = clv.data@data.transactions,
                                             sample.name="Total")

  dt.summary <- cbind(
    data.table(Estimation=l.desc.estimation),
    data.table(Total=l.desc.total))

  # Add holdout descriptives, if
  #   - has holdout sample period
  #   - has transactions in holdout sample (might not have if make descriptives for single customer)
  dt.summary[, Holdout := "-"]
  if(clv.data.has.holdout(clv.data)){
    dt.trans.holdout <- clv.data.get.transactions.in.holdout.period(clv.data = clv.data)
    # Need to subset to Ids here already to check if there actually are transactions in holdout period
    if(!is.null(Ids)){
      dt.trans.holdout <- dt.trans.holdout[Id %in% Ids]
    }
    if(nrow(dt.trans.holdout) > 0){
      dt.summary[, Holdout := fct.make.descriptives(dt.data = dt.trans.holdout, sample.name="Holdout")]
    }
  }

  dt.summary[, Name := names(l.desc.estimation)]
  setcolorder(dt.summary, c("Name", "Estimation", "Holdout", "Total"))

  return(dt.summary)

}
