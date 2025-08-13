# function to to check if there are error messages and print+stop them
check_err_msg <- function(err.msg){
  if(length(err.msg) > 0)
    stop(c("\n",paste0(err.msg, sep="\n")),call. = FALSE)
}

.check_user_data_single_boolean <- function(b, var.name){
  err.msg <- c()
  if(!is.logical(b))
    return(paste0("The parameter ", var.name, " needs to be of type logical (True/False)!"))
  if(length(b)>1)
    err.msg <- c(err.msg, paste0("The parameter ", var.name, " can only contain 1 element!"))
  if(anyNA(b))
    err.msg <- c(err.msg, paste0("The parameter ", var.name, " cannot be NA!"))
  return(err.msg)
}

.check_userinput_charactervec <- function(char, var.name, n){
  err.msg <- c()

  if(!is.character(char))
    return(paste0(var.name, " needs to be of type character (text)!"))
  if(length(char) != n)
    err.msg <- c(err.msg, paste0(var.name, " must contain exactly ", n, " element(s)!"))
  if(anyNA(char))
    err.msg <- c(err.msg, paste0(var.name, " may not contain any NA!"))

  if(length(err.msg) == 0){
    # is non empty vec, but check is not no text ("")
    if(any(sapply(char, nchar) == 0)){
      err.msg <- c(err.msg, paste0(var.name, " may not contain elements which are empty text!"))
    }
  }

  return(err.msg)
}

.convert_userinput_dataid <- function(id.data){
  return(as.character(id.data))
}

.check_userinput_matcharg <- function(char, choices, var.name){
  if(is.null(char))
    return(paste0("Parameter ",var.name, " cannot be NULL!"))
  if(!is.character(char))
    return(paste0(var.name, " needs to be of type character (text)!"))

  err.msg <- c()
  if(anyNA(char))
    err.msg <- c(err.msg, paste0(var.name, " may not contain any NA!"))

  # use pmatch to match the input against the possible choices
  #   match.arg would throw undescriptive error if not found
  #   this also accounts for empty texts

  if(length(err.msg) == 0) # may fail ungracefully if inproper input
    if(!all(pmatch(x=tolower(char), table=tolower(choices), nomatch = FALSE)))
      err.msg <- c(err.msg, paste0("Please choose one of the following values for ",var.name,": ",
                                   paste(choices, collapse = ", "), "!"))

  return(err.msg)
}

.check_userinput_integer_vector <- function(vec.int, var.name){
  if(is.null(vec.int))
    return(paste0(var.name, " cannot be NULL!"))

  if(anyNA(vec.int))
    return(paste0(var.name, " cannot contain any NA values!"))

  if(length(vec.int) == 0)
    return(paste0(var.name, " has to contain values!"))

  if(!is.numeric(vec.int))
    return(paste0(var.name, " has to be a vector of integer numbers!"))

  # all integers
  if(!all(vec.int == as.integer(vec.int))){
    return(paste0(var.name, " must be all integer numbers!"))
  }
  return(c())
}

check_userinput_datanocov_transbins <- function(trans.bins, count.repeat.trans){

  err.msg <- .check_userinput_integer_vector(vec.int=trans.bins, var.name="trans.bins")
  if(length(err.msg))
    return(err.msg)

  if(count.repeat.trans){
    if(any(trans.bins < 0))
      return("trans.bins has to be a vector of positive integers (>=0)!")
  }else{
    if(any(trans.bins < 1))
      return("trans.bins has to be a vector of strictly positive integers (>=1)!")
  }

  return(c())
}

check_userinput_datanocov_ids <- function(ids){
  err.msg <- c()

  if(is.null(ids)){
    return(err.msg)
  }

  if(anyNA(ids)){
    return("Parameter 'ids' may not contain any NA!")
  }

  if(!is.numeric(ids) & !is.character(ids)){
    return("Parameter 'ids' has to be a single numeric value or a character vector!")
  }


  if(is.numeric(ids)){
    err.msg <- c(err.msg, .check_user_data_single_numeric(ids, var.name = 'ids'))
    # if(any(!is.finite(ids))){
    #   return("ids may not contain any non-finite elements if numeric!")
    # }
    # if(length(ids) != 1){
    #   return("ids has to be of length 1 if numeric!")
    # }
    if(!all(ids > 0)){
      err.msg <- c(err.msg, "Parameter 'ids' has to be strictly positive (>0)")
    }
  }

  if(is.character(ids)){
    if(length(ids) == 0){
      err.msg <- c(err.msg, "Parameter 'ids' has to contain at least 1 element if character vector")
    }else{
      if(any(nchar(ids) == 0)){
        err.msg <- c(err.msg, "Parameter 'ids' may not be empty text")
      }
    }

  }

  return(err.msg)
}

check_userinput_datanocov_columnname <- function(name.col, data){

  if(is.null(name.col))
    return("Column names cannot be NULL!") #return already as NULL will break code

  err.msg <- .check_userinput_charactervec(char=name.col, var.name="Column names", n=1)

  # check if column is exactly in data
  if(length(err.msg) == 0)
    if(!(name.col %in% colnames(data)))
      err.msg <- c(err.msg, paste0("The column named \"", name.col, "\" could not be found in the data!"))

  return(err.msg)
}

check_userinput_datanocov_timeunit <- function(time.unit){

  if(is.null(time.unit))
    return("Time unit cannot be NULL!") #return already as NULL will break code

  err.msg <- .check_userinput_charactervec(char=time.unit, var.name = "time.unit", n=1)


  # use pmatch to match the input againts the possible time units
  #   this also accounts for empty texts
  #   use tolower to allow capital/mixed spelling
  #   match.arg would throw undescriptive error if not found

  if(length(err.msg) == 0) # may fail ungracefully if inproper input
    if(!(pmatch(x=tolower(time.unit), table=tolower(clv.time.possible.time.units()), nomatch = FALSE)))
      err.msg <- c(err.msg, paste0("Please choose one of the following time units: ", paste(clv.time.possible.time.units(), collapse = ", "), "!"))

  return(err.msg)
}



# Check type
# estimation.split can be
# - NULL: no split
# - char: convert to date
# - date: split here
# - numeric: split after this many number of time units
#' @importFrom lubridate is.POSIXt is.Date parse_date_time
check_userinput_datanocov_estimationsplit <- function(estimation.split, date.format){

  if(is.null(estimation.split))
    return(c())

  #cannot use .single_character helper to check because may be numeric or date

  if(length(estimation.split) != 1)
    return("estimation.split must contain exactly one single element!")

  if(anyNA(estimation.split))
    return("estimation.split may not contain any NAs!")

  if(!is.character(estimation.split) & !is.numeric(estimation.split) &
     !is.Date(estimation.split) & !is.POSIXt(estimation.split))
    return("estimation.split needs to either of type character, numeric, or Date (Date or POSIXt)")

  if(is.character(estimation.split))
    if(anyNA(parse_date_time(x=estimation.split, quiet=TRUE, orders=date.format)))
      return("Please provide a valid estimation.split to that can be converted with the given date.format!")

  # Whether estimation.split lies in data will only be checked when it is converted!
  return(c())
}

#' @importFrom lubridate is.POSIXt is.Date parse_date_time
check_userinput_datanocov_observationend <- function(observation.end, date.format){

  # May be NULL
  if(is.null(observation.end))
    return(c())

  if(length(observation.end) != 1)
    return("observation.end must contain exactly one single element!")

  if(anyNA(observation.end))
    return("observation.end may not contain any NAs!")

  if(!is.character(observation.end)
     & !is.Date(observation.end)
     & !is.POSIXt(observation.end))
    return("observation.end needs to either of type character or date-like (Date or POSIXt)")

  if(is.character(observation.end))
    if(anyNA(parse_date_time(x=observation.end, quiet=TRUE, orders=date.format)))
      return("Please provide a valid observation.end to that can be converted with the given date.format!")

  return(c())
}


#' @importFrom lubridate is.POSIXct
check_userinput_datanocov_datatransactions <- function(data.transactions.dt, has.spending){
  Id <- Date <- Price <- NULL

  err.msg <- c()
  if(!is.data.table(data.transactions.dt))
    return("Something went wrong. Transactions could not be converted to data.table!")

  if(nrow(data.transactions.dt) == 0)
    return("Transactions may not be empty!")

  if(any(!c("Id", "Date") %in% colnames(data.transactions.dt)))
    return("The column names could not be set in the transaction data!")

  # Id can be char, number, or factor
  err.msg <- c(err.msg, check_userinput_data_id(dt.data = data.transactions.dt, name.id = "Id", name.var = "transaction data"))
  err.msg <- c(err.msg, check_userinput_data_date(dt.data = data.transactions.dt, name.date = "Date", name.var = "transaction data"))

  # Price can only be numeric
  if(has.spending){
    if(!(data.transactions.dt[,is.numeric(Price)]))
      err.msg <- c(err.msg, "The Price column in the transaction data needs to be of type \"numeric\"!")

    if(data.transactions.dt[, anyNA(Price)])
      err.msg <- c(err.msg, "The \"Price\" column in the transaction data contains NAs!")
  }

  # No NAs
  if(data.transactions.dt[, anyNA(Id)])
    err.msg <- c(err.msg, "The \"Id\" column in the transaction data contains NAs!")

  if(data.transactions.dt[, anyNA(Date)])
    err.msg <- c(err.msg, "The \"Date\" column in the transaction data contains NAs!")


  return(err.msg)
}



check_userinput_datanocov_namescov <- function(names.cov, data.cov.df, name.of.covariate){
  err.msg <- c()

  if(is.null(names.cov))
    return(paste0("Covariate names for the ",name.of.covariate," covariate may not be NULL!"))

  if(!is.character(names.cov))
    return(paste0("Covariate names for the ",name.of.covariate," covariate have to be character vector!"))

  if(length(names.cov) < 1)
    return(paste0("There needs to be at least one covariate name for the ",name.of.covariate," covariate!"))

  if(anyNA(names.cov))
    err.msg <- c(err.msg, "Column names may not contain any NAs!")

  for(n in names.cov)
    if(!(n %in% colnames(data.cov.df)))
      err.msg <- c(err.msg, paste0("The column named ", n, " could not be found in the ",name.of.covariate," covariate data!"))

  if(length(names.cov) != length(unique(names.cov)))
    err.msg <- c(err.msg, paste0("Column names for the ",name.of.covariate," covariate may not contain any duplicates!"))

  return(err.msg)
}


check_userinput_datanocov_datastaticcov <- function(clv.data, dt.data.static.cov, names.cov, name.of.covariate){
  err.msg <- c()

  # Cov data checks ------------------------------------------------------------------------

  # the cov data itself needs to be numeric, char or factor
  if(dt.data.static.cov[, !all(sapply(.SD, is.numeric) | sapply(.SD, is.character) |sapply(.SD, is.factor)),
                        .SDcols = names.cov])
    err.msg <- c(err.msg, paste0("All ",name.of.covariate," covariate data (except Id) needs to be of type numeric, character, or factor!"))

  # Categorical cov data cannot be only a single category
  if(dt.data.static.cov[, any(sapply(.SD, uniqueN) == 1), .SDcols = names.cov])
    err.msg <- c(err.msg, "Covariate variables with only a single category cannot be used as covariates.")

  # Id checks ----------------------------------------------------------------------------

  # Exactly 1 cov per customer
  dt.uniq.id <- unique(clv.data@data.transactions[, "Id"])
  if(nrow(dt.uniq.id) != nrow(dt.data.static.cov))
    err.msg <- c(err.msg, paste0("Every Id has to appear exactly once in the ", name.of.covariate ," covariate data!"))

  # every Id in cbs needs to be in covariate Id
  #   use data.table::fsetdiff which returns a data.table
  if(nrow(fsetdiff(dt.uniq.id, dt.data.static.cov[, "Id"])) > 0)
    err.msg <- c(err.msg, paste("Every Id in the transaction data needs to be in the ",name.of.covariate," covariate data as well!"))

  # No NAs in Id and relevant cov data
  if(dt.data.static.cov[, anyNA(.SD), .SDcols=c("Id", names.cov)])
    err.msg <- c(err.msg, paste0("The ",name.of.covariate," covariate data may not contain any NAs!"))

  return(err.msg)
}


check_userinput_data_id <- function(dt.data, name.id, name.var){
  err.msg <- c()

  if(!dt.data[, (sapply(.SD, is.numeric) | sapply(.SD, is.character) | sapply(.SD, is.factor)), .SDcols=name.id])
    err.msg <- c(paste0("The Id column in the ",name.var," needs to be of type \"numeric\", \"character\", or \"factor\"!"))

  return(err.msg)
}

check_userinput_data_date <- function(dt.data, name.date, name.var){
  err.msg <- c()

  # Date can be Date, character, or Posixct (but not posixlt because of data.table!)
  if(!dt.data[, (sapply(.SD, is.Date) | sapply(.SD, is.character) | sapply(.SD, is.POSIXct)), .SDcols=name.date])
    err.msg <- c(paste0("The Date column in the ",name.var," needs to be of type \"Date\", \"character\", or \"POSIXct\"!"))

  return(err.msg)
}

# The cov data is already cut to range when given
check_userinput_datadyncov_datadyncovspecific <- function(dt.data.dyn.cov, dt.required.dates, clv.time, dt.required.ids, names.cov, name.of.covariate){
  Cov.Date <- Id <- Max.Cov.Date <- is.long.enough <- Min.Cov.Date <- N <- has.req.dates <- num.dates <- NULL

  # Better be sure
  setkeyv(dt.data.dyn.cov, cols = c("Id", "Cov.Date"))

  err.msg <- c()

  # Cov data checks -------------------------------------------------------------------------------

  # Categorical cov data cannot be only a single category
  if(dt.data.dyn.cov[, any(sapply(.SD, uniqueN) == 1), .SDcols = names.cov])
    err.msg <- c(err.msg, "Covariate variables with only a single category cannot be used as covariates.")

  # NA checked outside / before

  # the cov data itself needs to be numeric, char or factor
  if(dt.data.dyn.cov[, !all(sapply(.SD, is.numeric) | sapply(.SD, is.character) |sapply(.SD, is.factor)),
                     .SDcols = names.cov])
    err.msg <- c(err.msg, paste0("All ",name.of.covariate," covariate data (except Id and Date) needs to be of type numeric, character, or factor!"))


  # Id checks ------------------------------------------------------------------------------------

  # every Id needs to be in covariate Id
  if(!fsetequal(
    x = dt.required.ids,
    y = dt.data.dyn.cov[, "Id"],
    all = FALSE # exact same count not required
    )){
    err.msg <- c(err.msg, paste0("Every Id in the transaction data needs to be in the ",name.of.covariate," covariate data and vice versa!"))
  }


  # Date checks -----------------------------------------------------------------------------------

  # Last date, for every Id:
  #   - the last  Date is at least until the specified end
  #   - the last  Date is the same for all Ids
  #   - the first Date is the same for all Ids
  #   - the number of Dates is the same for all customers
  #   - there are no other dates than the required ones
  # last.cov.date.per.cust <- dt.data.dyn.cov[, list("Max.Cov.Date" = max(Cov.Date)), by=Id]

  # Check that every customer has a cov for exactly the required dates


  # there are no other dates than the required ones, across all customers
  #   it only concerns the relevant range because the data was cut to this range
  #   (all=FALSE = does not need to have the required dates multiple times)
  if(!fsetequal(dt.data.dyn.cov[, "Cov.Date"],
                dt.required.dates,
                all=FALSE))
    err.msg <- c(err.msg, paste0("There need to be ",tolower(clv.time.tu.to.ly(clv.time))," covariate data exactly from ",
                                 clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.required.dates[, min(Cov.Date)]),
                                 " until ",
                                 clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.required.dates[, max(Cov.Date)])))

  # It can still be that some customers dont have all these dates, ie have some dates missing
  #   (even only have 1 ) and can still have duplicated
  #   - check that everybody has the same number of Dates
  #     (= together with the last test this implies that these are the required Dates)
  #   - check that everybody has every Date only once

  # Everybody has the correct number of Dates.
  #   all() because could be vector of different values from unique()
  if(!all(dt.data.dyn.cov[, list(num.dates = uniqueN(Cov.Date)), by="Id"][, unique(num.dates)] == nrow(dt.required.dates)))
    err.msg <- c(err.msg, paste0("All customers in the ",name.of.covariate,
                                 " covariate data need to have the same number of Dates: ", nrow(dt.required.dates)))

  # Everybody has the correct number of observations
  #   Needed as otherwise can have duplicate observations
  #   all() because could be vector of different values from unique()
  if(!all(dt.data.dyn.cov[,  .N, by="Id"][, unique(N)] == nrow(dt.required.dates)))
    err.msg <- c(err.msg, paste0("All customers in the ",name.of.covariate,
                                 " covariate data need to have the same number of Dates: ", nrow(dt.required.dates)))

  return(err.msg)
}
