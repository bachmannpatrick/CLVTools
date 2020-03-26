#' Transactional data to fit CLV models
#'
#' @description
#' Stores the processed transactional data and holds an object of class \code{\link[CLVTools:clv.time-class]{clv.time}}
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
#' @slot descriptives.transactions Single \code{data.table} with descriptive statistics about the given transaction data
#' @slot has.holdout Single logical whether the data is split in a holdout and estimation period
#'
#' @seealso \code{\link[CLVTools:clv.time-class]{clv.time}}
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
           has.spending = "logical",

           descriptives.transactions = "data.table",

           has.holdout    = "logical"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           data.transactions  = data.table(),
           data.repeat.trans  = data.table(),

           descriptives.transactions = data.table(),

           has.spending       = logical(0),
           has.holdout        = logical(0)))


clv.data <- function(call, data.transactions, data.repeat.trans, has.spending, clv.time){

  has.holdout <- (clv.time@holdout.period.in.tu > 0)

  descriptives.transactions <- clv.data.make.descriptives(clv.time=clv.time, data.transactions = data.transactions,
                                                          has.holdout = has.holdout, has.spending = has.spending)

  setkeyv(data.transactions, c("Id", "Date"))
  setkeyv(data.repeat.trans, c("Id", "Date"))

  return(new(Class = "clv.data",
             name = "CLV Transaction Data",
             call=call,
             data.transactions = copy(data.transactions),
             data.repeat.trans = data.repeat.trans,
             has.spending = has.spending,
             clv.time = clv.time,
             has.holdout = has.holdout,
             descriptives.transactions=descriptives.transactions))
}



#' @importFrom stats sd
#' @importFrom lubridate time_length
clv.data.make.descriptives <- function(clv.time, data.transactions, has.holdout, has.spending){

  Id <- Date <- .N <- N <- Price <- interp.time<- NULL


  # Data preparation
  #
  # If there is no holdout period, give the estimation period data as input to be able to calculate values
  # Then replace them with "-" in the end before returning
  # ------------------------------------------------------------------------------------------------

  data.transactions.estimation <- data.transactions[Date >= clv.time@timepoint.estimation.start &
                                                      Date <= clv.time@timepoint.estimation.end]
  if(has.holdout)
    data.transactions.holdout  <- data.transactions[Date >= clv.time@timepoint.holdout.start &
                                                      Date <= clv.time@timepoint.holdout.end]
  else
    data.transactions.holdout  <- data.transactions.estimation

  no.trans.by.cust.total       <- data.transactions[,            .N, by="Id"]
  no.trans.by.cust.estimation  <- data.transactions.estimation[, .N, by="Id"]
  no.trans.by.cust.holdout     <- data.transactions.holdout[,    .N, by="Id"]


  # Interpurchase time, for repeaters only
  #   Time between consecutive purchases of each customer - convert to intervals then time units
  #   If zero-repeaters (only 1 trans) set NA to ignore it in mean / sd calculations
  #
  #   Cannot use int_diff as s4 is created for every customer which is very slow - use base::diff
  # ------------------------------------------------------------------------------------------------
  # .calc.interp.time <- function(data.trans){
  #   mean.interp.time.per.cust <- data.trans[, list(interp.time =
  #                                                  ifelse(.N > 1, mean(time_length(base::diff.POSIXt(Date), obj@clv.time@time.unit)),   NA_real_)), by="Id"]
  #   return(mean.interp.time.per.cust)
  # }
  # interp.est   <- .calc.interp.time(data.trans = data.transactions.estimation)
  # interp.hold  <- .calc.interp.time(data.trans = data.transactions.holdout)
  # interp.total <- .calc.interp.time(data.trans = data.transactions)


  # select non-zero repeaters (N>1)
  # order by Date
  # by Id
  #


  # Make descriptives
  # ------------------------------------------------------------------------------------------------

  list.of.list <- list(
    "Number of customers"  =
      list(Estimation = "-",
           Holdout    = "-",
           Total      = nrow(no.trans.by.cust.total)),
    "First Transaction in period"   =
      list(Estimation= as.character(data.transactions.estimation[, min(Date)]),
           Holdout    = as.character(data.transactions.holdout[,    min(Date)]),
           Total      = as.character(data.transactions[,            min(Date)])),

    "Last Transaction in period"    =
      list(Estimation = as.character(data.transactions.estimation[, max(Date)]),
           Holdout    = as.character(data.transactions.holdout[,    max(Date)]),
           Total      = as.character(data.transactions[,            max(Date)])),
    "Total # Transactions"          =
      list(Estimation = nrow(data.transactions.estimation),
           Holdout    = nrow(data.transactions.holdout),
           Total      = nrow(data.transactions)),
    "Mean # Transactions per cust"  =
      list(Estimation = no.trans.by.cust.estimation[, mean(N)],
           Holdout    = no.trans.by.cust.holdout[,    mean(N)],
           Total      = no.trans.by.cust.total[,      mean(N)]),
    "(SD)" =
      list(Estimation = no.trans.by.cust.estimation[, sd(N)],
           Holdout    = no.trans.by.cust.holdout[,    sd(N)],
           Total      = no.trans.by.cust.total[,      sd(N)]))

  if(has.spending)
    list.of.list <- c(list.of.list, list(
      "Mean Spending per Transaction"    =
        list(Estimation = data.transactions.estimation[, mean(Price)],
             Holdout    = data.transactions.holdout[,    mean(Price)],
             Total      = data.transactions[,            mean(Price)]),
      "(SD) " =
        list(Estimation  = data.transactions.estimation[, sd(Price)],
             Holdout    = data.transactions.holdout[,    sd(Price)],
             Total      = data.transactions[,            sd(Price)]),
      "Total Spending"                =
        list(Estimation  = data.transactions.estimation[, sum(Price)],
             Holdout    = data.transactions.holdout[,    sum(Price)],
             Total      = data.transactions[,            sum(Price)])))

  #   Total:      buy exactly once, ever
  #   Estimation: buy exactly once, in estimation period
  #   Holdout:    the ones who dont buy in holdout, ie only in estimation
  list.of.list <- c(list.of.list, list(
    "Total # zero repeaters"        =
      list(  Estimation = nrow(no.trans.by.cust.estimation[N == 1]),
             Holdout    = nrow(fsetdiff(no.trans.by.cust.total[, "Id"], no.trans.by.cust.holdout[, "Id"])),
             Total      = nrow(no.trans.by.cust.total[     N == 1])),
    "Percentage # zero repeaters"        =
      list( Estimation = nrow(no.trans.by.cust.estimation[N == 1])                                               / nrow(no.trans.by.cust.total),
            Holdout    = nrow(fsetdiff(no.trans.by.cust.total[, "Id"], no.trans.by.cust.holdout[, "Id"]))        / nrow(no.trans.by.cust.total),
            Total      = nrow(no.trans.by.cust.total[     N == 1])                                               / nrow(no.trans.by.cust.total))))
  # # Interpurchase time
  # # Remove NAs indicating zero-repeaters!
  # "Mean Interpurchase time"       =
  #                           list( Estimation = interp.est[,   mean(interp.time, na.rm=T)],
  #                                 Holdout    = interp.hold[,  mean(interp.time, na.rm=T)],
  #                                 Total      = interp.total[, mean(interp.time, na.rm=T)]),
  #
  # "(SD)   "       =
  #                           list( Estimation = interp.est[,   sd(interp.time, na.rm=T)],
  #                                 Holdout    = interp.hold[,  sd(interp.time, na.rm=T)],
  #                                 Total      = interp.total[, sd(interp.time, na.rm=T)]))




  # Format output ----------------------------------------------------------------------------------

  # Make cut digits
  list.of.list <-   lapply(list.of.list, function(x)format(x, digits=3, nsmall=3))

  # Make data.table
  summary.dt <- as.data.table(list.of.list)
  summary.dt <- transpose(summary.dt)

  colnames(summary.dt) <- c("Estimation", "Holdout", "Total")
  # Rownames are discouraged in data.table
  #   instead insert a column
  summary.dt[, "Name" := names(list.of.list)]

  setcolorder(summary.dt, c("Name", "Estimation", "Holdout", "Total"))


  # No Holdout ------------------------------------------------------------------------------------
  #   Remove values in holdout if there is no holdout
  #   In this case, the estimation data was used
  if(!has.holdout)
    summary.dt[, "Holdout" := "-"]

  return(summary.dt)

}
