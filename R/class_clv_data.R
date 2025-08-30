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

clv.data.get.repeat.transactions.in.estimation.period <- function(clv.data){
  Date <- NULL
  return(clv.data@data.repeat.trans[Date <= clv.data@clv.time@timepoint.estimation.end])
}

clv.data.get.transactions.in.holdout.period <- function(clv.data){
  Date <- NULL
  stopifnot(clv.data.has.holdout(clv.data))
  return(clv.data@data.transactions[Date >= clv.data@clv.time@timepoint.holdout.start])
}


# to allow for sampling with replacement, the name of ids appearing multiple times have to be changed
# because they are otherwise aggregated in clv.data
clv.data.select.customer.data.duplicating.ids <- function(dt.data, ids){
  .N <- id_nth <- new_Id <- Id <- NULL

  # Because this method is used to select transactions as well as covariate data,
  # it is paramount that the ordering of ids is preserved to ensure the
  # <Id> <=> <new_Id> mapping remains the same whenever this method is called.
  # Otherwise, customers receive the wrong covariate data.

  if(anyDuplicated(ids)){
    dt.ids <- data.table(Id=ids, key = "Id")
    dt.ids[, id_nth := seq(.N), by="Id"]
    dt.ids[, new_Id := paste(Id, id_nth, sep = '_BOOTSTRAP_ID_')]
    dt.ids[id_nth == 1, new_Id := Id] # only use new name if there are multiple ids
    # we are duplicating data which may result in more than nrow(x)+nrow(i) rows
    # which will raise an error if allow.cartesian=FALSE (default)
    dt.data <- dt.ids[dt.data, on="Id", nomatch=NULL, mult = "all", allow.cartesian=TRUE]
    dt.data[, Id := new_Id]
    dt.data[, new_Id := NULL]
    dt.data[, id_nth := NULL]
  }else{
    dt.data <- dt.data[SJ(Id=ids), on="Id", nomatch=NULL]
  }

  return(dt.data)
}



clv.data.make.repeat.transactions <- function(dt.transactions){
  Date <- trans_num <- .N <- NULL

  # Copy because alters table by temporarily adding a column
  dt.repeat.transactions <- copy(dt.transactions)

  # Mark for drop approach
  # profiled to be faster & more memory efficient than alternatives
  # Do not have to sort if table is already (physically) sorted by Date
  if("Date" %in% key(dt.repeat.transactions)){
    dt.repeat.transactions[,            trans_num := seq(.N), by="Id"]
  }else{
    dt.repeat.transactions[order(Date), trans_num := seq(.N), by="Id"]
  }
  dt.repeat.transactions <- dt.repeat.transactions[trans_num > 1]
  dt.repeat.transactions[, trans_num := NULL]

  # Previous implementation: Shift/lag approach
  # dt.repeat.transactions[order(Date), previous := shift(x=Date, n = 1L, type = "lag"), by="Id"]
  # # Remove first transaction: Have no previous (ie is NA)
  # dt.repeat.transactions <- dt.repeat.transactions[!is.na(previous)]
  # dt.repeat.transactions[, previous := NULL]

  # Alternative
  # More understandable but uses more memory and takes longer, probably
  # because allocates many small data.table for each group
  # dt.repeat.transactions <- dt.transactions[order(Date), tail(.SD, n=.N-1), by="Id"]
  # using .SD[-1L] inplace of tail() is slower

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
#' @importFrom lubridate interval
clv.data.mean.interpurchase.times <- function(clv.data, dt.transactions){
  Id <- num.trans <- Date <- next.date <- interp.time <- NULL

  num.transactions <- dt.transactions[, list(num.trans = .N), by="Id"]

  # Single Transaction = NA
  dt.interp.zrep <- dt.transactions[Id %in% num.transactions[num.trans == 1,Id], list(interp.time = NA_real_, Id)]

  # For > 1 transaction, calculate interpurchase time
  # copy because manipulating in-place with `:=`
  # do mem-heavy ops (shift) not in `by='Id'` but in single step
  dt.interp <- copy(dt.transactions[Id %in% num.transactions[num.trans >  1,Id]])
  # needs to be sorted by Id and Date for shift() to produce correct results.
  # Most of the times, the input is already keyed on exactly these.
  setkeyv(dt.interp, c('Id', 'Date'))



  # Replace: dt.interp[, next.date := shift(Date, type = 'lead'), by='Id']
  # Markus' idea: Avoid doing shift() `by=Id` by shift()ing across whole table and
  # correcting last transaction for each customer which is wrong. It now
  # contains transaction date of next customer but should be NA instead.
  # This was benchmarked to be about 4-5x faster.
  dt.interp[, next.date := shift(Date, type = 'lead')]

  # Find position (index) of last transaction of each customer by num of transactions.
  idx.last.trans <- cumsum(dt.interp[, .N, keyby='Id']$N)
  dt.interp[idx.last.trans, next.date := NA]


  dt.interp[, interp.time := clv.time.interval.in.number.tu(clv.time = clv.data@clv.time, interv=interval(start=Date, end = next.date))]
  dt.interp <- dt.interp[, list(interp.time = mean(interp.time, na.rm=TRUE)), by='Id']

  # Using this single-liner would as intermediate results also require to allocate the same memory (Id, Date, next.Date)
  # dt.transactions[Id%in% num.transactions[num.trans >  1,Id], .(Id, Date, next.date = shift(Date, type = 'lead')), by='Id'][, .(Id, interp = time_length(interval(start=Date, end = next.date), 'week'))][, mean(interp, na.rm=T), by='Id']

  return(rbindlist(list(dt.interp.zrep, dt.interp), use.names = TRUE))
}

#' @importFrom stats sd
#' @importFrom lubridate time_length
clv.data.make.descriptives <- function(clv.data, ids){

  Id <- Date <- .N <- N <- Price <- interp.time<- Name <- Holdout <- NULL

  # readability
  clv.time <- clv.data@clv.time

  ids <- unique(ids)

  # Make descriptives ------------------------------------------------------------------------------
  #   Do not simply overwrite all NA/NaN with "-", only where these are expected (num obs = 1).
  #     Let propagate otherwise to help find errors
  fct.make.descriptives <- function(dt.data, sample.name){

    # Subset transaction data to relevant ids
    if(!is.null(ids)){
      dt.data <- dt.data[Id %in% ids]

      # print warning only once
      if(sample.name == "Total" & dt.data[, uniqueN(Id)] != length(unique(ids))){
        warning("Not all given ids were found in the transaction data.", call. = FALSE)
      }

    }
    dt.interp <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions = dt.data)
    dt.num.trans.by.cust <- dt.data[, .N, by="Id"]

    tp.period.start <- switch(
      sample.name,
      Estimation=clv.time@timepoint.estimation.start,
      Holdout=clv.time@timepoint.holdout.start,
      Total=clv.time@timepoint.estimation.start)

    tp.period.end <- switch(
      sample.name,
      Estimation=clv.time@timepoint.estimation.end,
      Holdout=clv.time@timepoint.holdout.end,
      Total=clv.time@timepoint.holdout.end)

    l.desc <- list(
      "Period Start"                  = clv.time.format.timepoint(clv.time=clv.time, timepoint=tp.period.start),
      "Period End"                    = clv.time.format.timepoint(clv.time=clv.time, timepoint=tp.period.end),
      "Number of customers"           = if(sample.name=="Total"){nrow(dt.num.trans.by.cust)}else{"-"},
      "First Transaction in period"   = clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.data[, min(Date)]),
      "Last Transaction in period"    = clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.data[, max(Date)]),
      "Total # Transactions"          = nrow(dt.data),
      "Mean # Transactions per cust"  = dt.num.trans.by.cust[, mean(N)],
      "(SD)"                          = if(nrow(dt.num.trans.by.cust) > 1){dt.num.trans.by.cust[, sd(N)]}else{"-"})

    if(clv.data.has.spending(clv.data)){
      l.desc <- c(l.desc, list(
        "Mean Spending per Transaction"  = dt.data[, mean(Price)],
        # SD is calculated not across customers but across transactions
        "(SD) "                          = if(dt.data[, .N] > 1){dt.data[, sd(Price)]}else{"-"},
        "Total Spending"                 = dt.data[, sum(Price)]))
    }

    num.interp.obs <- dt.interp[!is.na(interp.time), .N]
    l.desc <- c(l.desc, list(
      # Zero-repeaters can only be in Estimation ()
      "Total # zero repeaters"        = if(sample.name == "Estimation"){dt.num.trans.by.cust[, sum(N==1)]}else{"-"},
      "Percentage of zero repeaters"   = if(sample.name == "Estimation"){dt.num.trans.by.cust[, mean(N==1)*100]}else{"-"},

      # Inter-purchase time
      #   Remove NAs resulting from zero-repeaters
      "Mean Interpurchase time"       = if(num.interp.obs > 0){dt.interp[, mean(interp.time, na.rm=TRUE)]}else{"-"},
      # Need 2 obs to calculate SD
      "(SD)   "                       = if(num.interp.obs > 1){dt.interp[, sd(interp.time, na.rm=TRUE)]}else{"-"}))

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
    # Need to subset to ids here already to check if there actually are transactions in holdout period
    if(!is.null(ids)){
      dt.trans.holdout <- dt.trans.holdout[Id %in% ids]
    }
    if(nrow(dt.trans.holdout) > 0){
      dt.summary[, Holdout := fct.make.descriptives(dt.data = dt.trans.holdout, sample.name="Holdout")]
    }
  }

  dt.summary[, Name := names(l.desc.estimation)]
  setcolorder(dt.summary, c("Name", "Estimation", "Holdout", "Total"))

  return(dt.summary)

}



# default.choices might differ in order
clv.data.select.sample.data <- function(clv.data, sample, choices){
  # check if sample is valid
  check_err_msg(.check_userinput_matcharg(char=sample, choices=choices, var.name="sample"))

  sample <- match.arg(arg = tolower(sample), choices = choices)
  if(sample == "holdout" & !clv.data.has.holdout(clv.data)){
    check_err_msg("The given clv.data object has no holdout data!")
  }

  return(switch(sample,
                "full" = copy(clv.data@data.transactions),
                "estimation" = clv.data.get.transactions.in.estimation.period(clv.data),
                "holdout" = clv.data.get.transactions.in.holdout.period(clv.data)))
}


# Add the number of repeat transactions to the given dt.date.seq
clv.data.add.repeat.transactions.to.periods <- function(clv.data, dt.date.seq, cumulative){

  num.repeat.trans <- i.num.repeat.trans <- Date <- period.until <- NULL

  # Add period at every repeat transaction (and therefore copy)
  dt.repeat.trans  <- copy(clv.data@data.repeat.trans)

  # join (roll: -Inf=NOCF) period number onto all repeat transaction by dates
  #   ie assign each repeat transaction the next period number to which it belongs
  dt.repeat.trans <- dt.date.seq[dt.repeat.trans, on = c("period.until"="Date"), roll=-Inf, rollends=c(FALSE, FALSE)]
  # !period.until now is missleading, as it stands for the repeat transaction date!

  # Count num rep trans in every time unit
  dt.repeat.trans <- dt.repeat.trans[, list(num.repeat.trans = .N), by="period.num"]
  setorderv(dt.repeat.trans, order = 1L, cols = "period.num") # sort in ascending order

  # make double to avoid coercion warning in melt
  dt.date.seq[dt.repeat.trans, num.repeat.trans := as.numeric(i.num.repeat.trans), on = "period.num"]

  # set 0 where there are no transactions
  #   for when there are transactions again later on
  dt.date.seq[is.na(num.repeat.trans), num.repeat.trans := 0]

  # After last transaction, there are no more transactions.
  #   dt.expectation can however be longer. Set these intentionally to NA so that
  #   nothing is plotted (setting 0 plots a line at the bottom)
  date.last.repeat.transaction <- clv.data@data.repeat.trans[, max(Date)]
  dt.date.seq[period.until > date.last.repeat.transaction, num.repeat.trans := NA_real_]

  if(cumulative)
    dt.date.seq[, num.repeat.trans := cumsum(num.repeat.trans)]

  return(dt.date.seq)
}
