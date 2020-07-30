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
clv.data.make.descriptives <- function(clv.data){

  Id <- Date <- .N <- N <- Price <- interp.time<- NULL

  # readability
  clv.time <- clv.data@clv.time


  # Data preparation ---------------------------------------------------------------------------------
  # If there is no holdout period, give the estimation period data as input to be able to calculate values.
  #   Then replace them with "-" in the end before returning

  data.transactions.total      <- clv.data@data.transactions

  data.transactions.estimation <- clv.data.get.transactions.in.estimation.period(clv.data = clv.data)

  if(clv.data.has.holdout(clv.data=clv.data)){
    data.transactions.holdout  <- clv.data.get.transactions.in.holdout.period(clv.data = clv.data)
  }else{
    data.transactions.holdout  <- data.transactions.estimation
  }

  no.trans.by.cust.total       <- data.transactions.total[,      .N, by="Id"]
  no.trans.by.cust.estimation  <- data.transactions.estimation[, .N, by="Id"]
  no.trans.by.cust.holdout     <- data.transactions.holdout[,    .N, by="Id"]


  interp.est   <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions = data.transactions.estimation)
  interp.hold  <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions = data.transactions.holdout)
  interp.total <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions = data.transactions.total)


  # Make descriptives ------------------------------------------------------------------------------

  list.of.list <- list(
    "Number of customers"  =
      list(Estimation = "-",
           Holdout    = "-",
           Total      = nrow(no.trans.by.cust.total)),
    "First Transaction in period"   =
      list(Estimation = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.estimation[, min(Date)]),
           Holdout    = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.holdout[,   min(Date)]),
           Total      = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.total[,     min(Date)])),

    "Last Transaction in period"    =
      list(Estimation = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.estimation[, max(Date)]),
           Holdout    = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.holdout[,    max(Date)]),
           Total      = clv.time.format.timepoint(clv.time=clv.time, timepoint=data.transactions.total[,      max(Date)])),
    "Total # Transactions"          =
      list(Estimation = nrow(data.transactions.estimation),
           Holdout    = nrow(data.transactions.holdout),
           Total      = nrow(data.transactions.total)),
    "Mean # Transactions per cust"  =
      list(Estimation = no.trans.by.cust.estimation[, mean(N)],
           Holdout    = no.trans.by.cust.holdout[,    mean(N)],
           Total      = no.trans.by.cust.total[,      mean(N)]),
    "(SD)" =
      list(Estimation = no.trans.by.cust.estimation[, sd(N)],
           Holdout    = no.trans.by.cust.holdout[,    sd(N)],
           Total      = no.trans.by.cust.total[,      sd(N)]))

  if(clv.data.has.spending(clv.data)){
    list.of.list <- c(list.of.list, list(
      "Mean Spending per Transaction"    =
        list(Estimation = data.transactions.estimation[, mean(Price)],
             Holdout    = data.transactions.holdout[,    mean(Price)],
             Total      = data.transactions.total[,      mean(Price)]),
      "(SD) " =
        list(Estimation  = data.transactions.estimation[, sd(Price)],
             Holdout    = data.transactions.holdout[,     sd(Price)],
             Total      = data.transactions.total[,       sd(Price)]),
      "Total Spending" =
        list(Estimation  = data.transactions.estimation[, sum(Price)],
             Holdout    = data.transactions.holdout[,     sum(Price)],
             Total      = data.transactions.total[,       sum(Price)])))
  }

  #   Total:      buy exactly once, ever
  #   Estimation: buy exactly once, in estimation period
  #   Holdout:    the ones who dont buy in holdout, ie only in estimation
  list.of.list <- c(list.of.list, list(
    "Total # zero repeaters"        =
      list(  Estimation = nrow(no.trans.by.cust.estimation[N == 1]),
             Holdout    = nrow(fsetdiff(no.trans.by.cust.total[, "Id"], no.trans.by.cust.holdout[, "Id"])),
             Total      = nrow(no.trans.by.cust.total[     N == 1])),
    "Percentage # zero repeaters"        =
      list( Estimation = nrow(no.trans.by.cust.estimation[N == 1])                                        / nrow(no.trans.by.cust.total),
            Holdout    = nrow(fsetdiff(no.trans.by.cust.total[, "Id"], no.trans.by.cust.holdout[, "Id"])) / nrow(no.trans.by.cust.total),
            Total      = nrow(no.trans.by.cust.total[     N == 1])                                        / nrow(no.trans.by.cust.total)),
    # Interpurchase time
    # Remove NAs indicating zero-repeaters!
    "Mean Interpurchase time"       =
      list( Estimation = interp.est[,   mean(interp.time, na.rm=TRUE)],
            Holdout    = interp.hold[,  mean(interp.time, na.rm=TRUE)],
            Total      = interp.total[, mean(interp.time, na.rm=TRUE)]),

    "(SD)   "       =
      list( Estimation = interp.est[,   sd(interp.time, na.rm=TRUE)],
            Holdout    = interp.hold[,  sd(interp.time, na.rm=TRUE)],
            Total      = interp.total[, sd(interp.time, na.rm=TRUE)])))




  # Format output ----------------------------------------------------------------------------------

  # Make cut digits
  list.of.list <-   lapply(list.of.list, function(x)format(x, digits=3, nsmall=3))

  # Make data.table
  dt.summary <- as.data.table(list.of.list)
  dt.summary <- transpose(dt.summary)

  colnames(dt.summary) <- c("Estimation", "Holdout", "Total")
  # Rownames are discouraged in data.table
  #   instead insert a column
  dt.summary[, "Name" := names(list.of.list)]

  setcolorder(dt.summary, c("Name", "Estimation", "Holdout", "Total"))


  # No Holdout ------------------------------------------------------------------------------------
  #   Remove values in holdout if there is no holdout
  #   In this case, the estimation data was used
  if(!clv.data.has.holdout(clv.data)){
    dt.summary[, "Holdout" := "-"]
  }

  return(dt.summary)

}
