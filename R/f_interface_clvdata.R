#' @name clvdata
#' @md
#'
#' @title Create an object for transactional data required to estimate CLV
#'
#' @param data.transactions Transaction data as \code{data.frame} or \code{data.table}. See details.
#' @param date.format Character string that indicates the format of the date variable in the data used. See details.
#' @param time.unit What time unit defines a period. May be abbreviated, capitalization is ignored. See details.
#' @param estimation.split Indicates the length of the estimation period. See details.
#' @param name.id Column name of the customer id in \code{data.transaction}.
#' @param name.date Column name of the transaction date in \code{data.transaction}.
#' @param name.price Column name of price in \code{data.transaction}. NULL if no spending data is present.
#'
#'
#' @description
#' Creates a data object that contains the prepared transaction data and that is used as input for
#' model fitting. The transaction data may be split in an estimation and holdout sample if desired.
#' The model then will only be fit on the estimation sample.
#'
#' If covariates should be used when fitting a model, covariate data can be added
#' to an object returned from this function.
#'
#' @details
#' \code{data.transactions} A \code{data.frame} or \code{data.table} with customers' purchase history.
#' Every transaction record consists of a purchase date and a customer id.
#' Optionally, the price of the transaction may be included to also allow for prediction
#' of future customer spending.
#'
#' \code{time.unit} The definition of a single period. Currently available are \code{"hours"}, \code{"days"}, \code{"weeks"}, and \code{"years"}.
#' May be abbreviated.
#'
#' \code{date.format} A single format to use when parsing any date that is given as character input. This includes
#' the dates given in \code{data.transaction}, \code{estimation.split}, or as an input to any other function at
#' a later point, such as \code{prediction.end} in \code{predict}.
#' The function \code{\link[lubridate]{parse_date_time}} of package \code{lubridate} is used to parse inputs
#' and hence all formats it accepts in argument \code{orders} can be used. For example, a date of format "year-month-day"
#' (i.e., "2010-06-17") is indicated with \code{"ymd"}. Other combinations such as \code{"dmy"}, \code{"dym"},
#' \code{"ymd HMS"}, or \code{"HMS dmy"} are possible as well.
#'
#' \code{estimation.split} May be specified as either the number of periods since the first transaction or the timepoint
#' (either as character, Date, or POSIXct) at which the estimation period ends. The indicated timepoint itself will be part of the estimation sample.
#' If no value is provided or set to \code{NULL}, the whole dataset will used for fitting the model (no holdout sample).
#'
#' @details ## Aggregation of Transactions
#'
#' Multiple transactions by the same customer that occur on the minimally representable temporal resolution are aggregated to a
#' single transaction with their spending summed. For time units \code{days} and any other coarser \code{Date}-based
#' time units (i.e. \code{weeks}, \code{years}), this means that transactions on the same day are combined.
#' When using finer time units such as \code{hours} which are based on \code{POSIXct}, transactions on the same second are aggregated.
#'
#' For the definition of repeat-purchases, combined transactions are viewed as a single transaction. Hence, repeat-transactions
#' are determined from the aggregated transactions.
#'
#' @return
#' An object of class \code{clv.data}.
#' See the class definition \linkS4class{clv.data}
#' for more details about the returned object.
#'
#' The function \code{summary} can be used to obtain and print a summary of the data.
#' The generic accessor function \code{nobs} is available to read out the number of customers.
#'
#' @seealso \code{\link[CLVTools:SetStaticCovariates]{SetStaticCovariates}} to add static covariates
#' @seealso \code{\link[CLVTools:SetDynamicCovariates]{SetDynamicCovariates}} for how to add dynamic covariates
#' @seealso \code{\link[CLVTools:plot.clv.data]{plot}} to plot the repeat transactions
#' @seealso \code{\link[CLVTools:summary.clv.data]{summary}} to summarize the transaction data
#' @seealso \code{\link[CLVTools:pnbd]{pnbd}} to fit Pareto/NBD models on a \code{clv.data} object
#'
#' @examples
#'
#' data("cdnow")
#'
#' # create clv data object with weekly periods
#' #    and no splitting
#' clv.data.cdnow <- clvdata(data.transactions = cdnow,
#'                           date.format="ymd",
#'                           time.unit = "weeks")
#'
#' # same but split after 37 periods
#' clv.data.cdnow <- clvdata(data.transactions = cdnow,
#'                           date.format="ymd",
#'                           time.unit = "w",
#'                           estimation.split = 37)
#'
#' # same but estimation end on the 15th Oct 1997
#' clv.data.cdnow <- clvdata(data.transactions = cdnow,
#'                           date.format="ymd",
#'                           time.unit = "w",
#'                           estimation.split = "1997-10-15")
#'
#' \donttest{
#'
#' # summary of the transaction data
#' summary(clv.data.cdnow)
#'
#' # plot the transaction data
#' plot(clv.data.cdnow)
#'
#' # create data with the weekly periods defined to
#' #   start on Mondays
#'
#' \dontrun{
#' # set start of week to Monday
#' oldopts <- options("lubridate.week.start"=1)
#'
#' # create clv.data while Monday is the beginning of the week
#' clv.data.cdnow <- clvdata(data.transactions = cdnow,
#'                           date.format="ymd",
#'                           time.unit = "weeks")
#'
#' # Dynamic covariates now have to be supplied for every Monday
#'
#' # set week start to what it was before
#' options(oldopts)
#' }
#'
#'}
#'
#'
#' @export
clvdata <- function(data.transactions, date.format, time.unit, estimation.split=NULL, name.id="Id", name.date="Date", name.price="Price"){
  # silence CRAN notes
  Date <- Price <- Id <- x <- previous <- date.first.actual.trans <- NULL

  cl <- match.call(expand.dots = TRUE)

  # Check input parameters ---------------------------------------------------------

  # Before breaking anything
  if(!is.data.frame(data.transactions))
    stop("Only transaction data of type data.frame or data.table can be processed!", call. = FALSE)

  # Check user data. This also checks the given column names
  err.msg <- c()
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.date,  data=data.transactions))
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.id,    data=data.transactions))
  if(!is.null(name.price)) # May be NULL to indicate no Spending data
    err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.price, data=data.transactions))
  check_err_msg(err.msg) # check already if colnames are wrong

  err.msg <- c(err.msg, check_userinput_datanocov_timeunit(time.unit=time.unit))

  err.msg <- c(err.msg, .check_userinput_single_character(char=date.format, var.name = "date.format"))
  err.msg <- c(err.msg, check_userinput_datanocov_estimationsplit(estimation.split=estimation.split, date.format=date.format))
  check_err_msg(err.msg)


  # transaction data to data.table  --------------------------------------------------

  # Convert transaction data to data.table already to better process in the check and convert function
  # Copy data because it will be manipulated by reference
  dt.trans <- copy(data.transactions)

  if(!is.data.table(dt.trans))
    dt.trans <- setDT(dt.trans)

  has.spending <- (!is.null(name.price))

  # Reduce to named columns.
  #   This is needed if the data contains a column "Date"/"Id"/"Price" which is not actually selected
  #   as the renaming will leave it undetermined what is then used (2 columns with the same name)
  if(has.spending){
    dt.trans <- dt.trans[, .SD, .SDcols = c(name.id, name.date, name.price)]
    setnames(dt.trans, old = c(name.id, name.date, name.price), new = c("Id", "Date", "Price"))
    # reduce transactions to only 3 columns, in case there is more data
    dt.trans <- dt.trans[, c("Id", "Date", "Price")]
  }else{
    dt.trans <- dt.trans[, .SD, .SDcols = c(name.id, name.date)]
    setnames(dt.trans, old = c(name.id, name.date), new = c("Id", "Date"))
    dt.trans <- dt.trans[, c("Id", "Date")]
  }

  # Check transaction data type  ------------------------------------------------------
  #   check data only after it is data.table because relies on data table syntas
  check_err_msg(check_userinput_datanocov_datatransactions(data.transactions.dt = dt.trans,
                                                           has.spending = has.spending))



  # clv time -----------------------------------------------------------------------

  # a match should be garantueed as allowed input was checked in check_user_data
  clv.t <- switch(EXPR   = match.arg(arg = tolower(time.unit),
                                     choices = tolower(clv.time.possible.time.units())),
                  "hours" = clv.time.hours(time.format=date.format),
                  "days"  = clv.time.days(time.format=date.format),
                  "weeks" = clv.time.weeks(time.format=date.format),
                  "years" = clv.time.years(time.format=date.format))


  # with clv time, can convert transaction data to correct type -------------------------------
  dt.trans[, Id    := .convert_userinput_dataid(id.data = Id)]
  dt.trans[, Date  := clv.time.convert.user.input.to.timepoint(clv.t, user.timepoint = Date)]

  if(has.spending){
    dt.trans[, Price := as.numeric(Price)] # already checked that is numeric
  }

  setkeyv(dt.trans, cols = c("Id", "Date"))


  # Aggregate transactions at the same timepoint ------------------------------------------------
  # Aggregate what is on same smallest scale representable by time
  # aggregating in the same time.unit does not make sense
  #   Date: on same day
  #   posix: on same second
  dt.trans <- clv.data.aggregate.transactions(dt.transactions = dt.trans, has.spending = has.spending)


  # Set estimation and holdout periods ---------------------------------------------------------
  tp.first.transaction <- dt.trans[, min(Date)]
  tp.last.transaction  <- dt.trans[, max(Date)]

  clv.t <- clv.time.set.sample.periods(clv.time = clv.t,
                                       tp.first.transaction = tp.first.transaction,
                                       tp.last.transaction  = tp.last.transaction,
                                       user.estimation.end  = estimation.split)

  if(clv.t@timepoint.estimation.end > dt.trans[, max(Date)])
    stop("Parameter estimation.split needs to indicate a point in the data!", call. = FALSE)

  if(clv.t@estimation.period.in.tu < 1)
    stop("Parameter estimation.split needs to be at least 1 time.unit after the start!", call. = FALSE)


  # Check if the estimation.split is valid ----------------------------------------
  #   - estimation period long enough
  #   - in transaction data
  #   - at least 2 periods

  # Estimation end has to be at least cohort length
  #   otherwise there will be customers in holdout which never were
  #     in cbs (and many other problems)
  # = Everyone's first actual transaction needs to be until calibration end
  everyones.first.trans <- dt.trans[, list(date.first.actual.trans = min(Date)), by="Id"]
  date.last.first.trans <- everyones.first.trans[, max(date.first.actual.trans)]
  if(clv.t@timepoint.estimation.end < date.last.first.trans)
    stop("The estimation split is too short! Not all customers of this cohort had their first actual transaction until the specified estimation.split!", call. = F)



  # Repeat Transactions ------------------------------------------------------------
  #   Save because used when plotting
  #   Remove the first transaction per customer
  dt.repeat.trans <- clv.data.make.repeat.transactions(dt.transactions = dt.trans)


  # Create clvdata object ----------------------------------------------------------
  obj <- clv.data(call=cl,
                  data.transactions = dt.trans,
                  data.repeat.trans = dt.repeat.trans,
                  has.spending = has.spending,
                  clv.time=clv.t)

  return(obj)
}
