#' @name newcustomer
#' @title New customer prediction data
#'
#' @description
#' The methods documented here are to be used together with
#' \link[CLVTools:predict.clv.fitted.transactions]{predict (transactions)} to obtain
#' the expected number of transactions of an average newly alive customer and
#' with \link[CLVTools:predict.clv.fitted.spending]{predict (spending)} to obtain
#' the expected spending of an average newly alive customer.
#' This prediction is only sensible for (fictional) customers without order history:
#' Customers which just came alive and have not had the chance to reveal any more of their behavior.
#'
#' The methods described here produce the data required as input to
#' \code{predict(newdata=)} to make this new customer prediction.
#' This is mostly covariate data for static and dynamic covariate models.
#' See details for the required format.
#'
#' \code{newcustomer()}, \code{newcustomer.static()}, \code{newcustomer.dynamic()}:
#' To predict the number of transactions a single, fictional, average new customer is expected to make in
#' the \code{num.periods} periods since making the first transaction ("coming alive").
#'
#' \code{newcustomer.spending()}: To estimate how much a single, fictional, average
#' new customer is expected to spend on average per transaction.
#'
#' @param num.periods A positive, numeric scalar indicating the number of periods to predict.
#' @param data.cov.life Numeric-only covariate data for the lifetime process for a single customer, \code{data.table} or \code{data.frame}. See details.
#' @param data.cov.trans Numeric-only covariate data for the transaction process for a single customer, \code{data.table} or \code{data.frame}. See details.
#' @param first.transaction For dynamic covariate models only: The time point of the first transaction of the customer ("coming alive") for which a prediction is made.
#' Has to be within the time range of the covariate data.
#'
#' @seealso \link[CLVTools:predict.clv.fitted.transactions]{predict (transactions)} to use the output of the methods described here.
#' @seealso \link[CLVTools:predict.clv.fitted.spending]{predict (spending)} to use the output of the methods described here.
#'
#' @details
#' The covariate data has to contain one column for every covariate parameter in the fitted model. Only numeric values are allowed, no factors or characters.
#' No customer Id is required because the data on which the model was fit is not used for this prediction.
#'
#' For \code{newcustomer.static()}: One column for every covariate parameter in the estimated model.
#' No column \code{Id}. Exactly 1 row of numeric covariate data. \cr
#' For example: \code{data.frame(Gender=1, Age=30, Channel=0)}.
#'
#' For \code{newcustomer.dynamic()}: One column for every covariate parameter in the estimated model.
#' No column \code{Id}. A column \code{Cov.Date} with time points that mark the start of the period defined by \code{time.unit}.
#' For every \code{Cov.Date}, exactly 1 row of numeric covariate data. \cr
#' For example for weekly covariates: \code{data.frame(Cov.Date=c("2000-01-03", "2000-01-10"), Gender=c(1,1), Channel=c(1, 1), High.Season=c(0,1,0))} \cr
#' If \code{Cov.Date} is of type character, the \code{date.format} given when creating the the \code{clv.data} object is used to parse it.
#' The data has to cover the time from the customer's first transaction \code{first.transaction}
#' to the end of the prediction period given by \code{t}. It does not have to cover the same time range as when fitting the model.
#' See examples.
#'
#' For models with dynamic covariates, the time point of the first purchase (\code{first.transaction}) is
#' additionally required because the exact covariates that are active during the prediction period have
#' to be known.
#'
#' @returns
#' \item{newcustomer()}{An object of class \code{clv.newcustomer.no.cov}}
#' \item{newcustomer.static()}{An object of class \code{clv.newcustomer.static.cov}}
#' \item{newcustomer.dynamic()}{An object of class \code{clv.newcustomer.dynamic.cov}}
#' \item{newcustomer.spending()}{An object of class \code{clv.newcustomer.spending}}
#'
#'
#'
#' @examples
#' \donttest{
#' data("apparelTrans")
#' data("apparelStaticCov")
#' data("apparelDynCov")
#'
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 52)
#' clv.data.static.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelStaticCov,
#'                      names.cov.life = "Gender",
#'                      data.cov.trans = apparelStaticCov,
#'                      names.cov.trans = c("Gender", "Channel"))
#' clv.data.dyn.cov <-
#'   SetDynamicCovariates(clv.data = clv.data.apparel,
#'                        data.cov.life = apparelDynCov,
#'                        data.cov.trans = apparelDynCov,
#'                        names.cov.life = c("High.Season", "Gender"),
#'                        names.cov.trans = c("High.Season", "Gender"),
#'                        name.date = "Cov.Date")
#'
#'
#'
#' # No covariate model
#' p.apparel <- pnbd(clv.data.apparel)
#'
#' # Predict the number of transactions an average new
#' # customer is expected to make in the first 3.68 weeks
#' predict(
#'   p.apparel,
#'   newdata=newcustomer(num.periods=3.68)
#' )
#'
#'
#' # Spending model
#' gg.apparel <- gg(clv.data.apparel)
#' predict(gg.apparel, newdata = newcustomer.spending())
#'
#'
#'
#' # Static covariate model
#' p.apparel.static <- pnbd(clv.data.static.cov)
#'
#' # Predict the number of transactions an average new
#' # customer who is female (Gender=1) and who was acquired
#' # online (Channel=1) is expected to make in the first 3.68 weeks
#' predict(
#'   p.apparel.static,
#'   newdata=newcustomer.static(
#'     num.periods=3.68,
#'     # For the lifetime process, only Gender was used when fitting
#'     data.cov.life=data.frame(Gender=1),
#'     data.cov.trans=data.frame(Gender=1, Channel=0)
#'   )
#' )
#'
#'
#' \dontrun{
#' # Dynamic covariate model
#'
#' p.apparel.dyn <- pnbd(clv.data.dyn.cov)
#'
#' # Predict the number of transactions an average new
#' # customer who is male (Gender=0), who did not purchase during
#' # high.season, and who was
#' # acquired on "2005-02-16" (first.transaction) is expected
#' # to make in the first 2.12 weeks.
#' # Note that the time range is very different from the one used
#' # when fitting the model. Cov.Date still has to match the
#' # beginning of the week.
#' predict(
#'   p.apparel.dyn,
#'   newdata=newcustomer.dynamic(
#'     num.periods=2.12,
#'     data.cov.life=data.frame(
#'       Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
#'       Gender=c(0, 0, 0),
#'       High.Season=c(4, 0, 7)),
#'     data.cov.trans=data.frame(
#'       Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
#'       Gender=c(0, 0, 0),
#'       High.Season=c(4, 0, 7)),
#'     first.transaction = "2051-02-16"
#'   )
#' )
#' }
#' }
#'
NULL

# . clv.newcustomer.base -------------------------------------------------------
# A (near useless) base class from which other 'newcustomer' classes inherit.
# This is required because a class defined without slots and without parents is
# considered VIRTUAL and cannot be instantiated. Inheriting from this class,
# allows to define a class `newcustomer.spending` which has no slots and
# otherwise would be considered VIRTUAL.
# This base class additionally is handy to catch any type of newcustomer
# instance when verifying parameters.
# Making this class virtual is not required as having no slots and parent has
# the same effect but better to be explicit.
setClass("clv.newcustomer.base", contains = "VIRTUAL")

setClass(
  Class = "clv.newcustomer.no.cov",
  representation = list(num.periods="numeric"),
  contains = 'clv.newcustomer.base'
)

clv.newcustomer.no.cov <- function(num.periods){
  return(new("clv.newcustomer.no.cov", num.periods=num.periods))
}


# Covariate data in SetStaticCovariates() can be given as factor/character which are then
# turned into numeric dummies. This was considered here as well but dismissed
# because it requires more than a single observation/level (ie need to
# know 'm' and 'f' to create dummies but only one can be given here).
# If the factors have more than 1 level (or contrasts) set, this would work
# but was also dismissed as rather niche. It also simplifies input checks to not
# convert the data
setClass(
  Class = "clv.newcustomer.static.cov",
  contains = "clv.newcustomer.base",
  representation = list(
    num.periods="numeric",
    data.cov.life="data.table",
    data.cov.trans="data.table"
))

clv.newcustomer.static.cov <- function(num.periods, data.cov.life, data.cov.trans){
  return(new(
    "clv.newcustomer.static.cov",
    num.periods=num.periods,
    data.cov.life=copy(data.cov.life),
    data.cov.trans=copy(data.cov.trans)
    ))
}


clv.newcustomer.static.get.matrix.cov.trans <- function(clv.newcustomer, clv.fitted){
  m <- data.matrix(clv.newcustomer@data.cov.trans)
  return(m[, names(clv.fitted@prediction.params.trans), drop=FALSE])
}

clv.newcustomer.static.get.matrix.cov.life <- function(clv.newcustomer, clv.fitted){
  m <- data.matrix(clv.newcustomer@data.cov.life)
  return(m[, names(clv.fitted@prediction.params.life), drop=FALSE])
}


setClass(
  Class = "clv.newcustomer.dynamic.cov",
  contains = "clv.newcustomer.base",
  representation = list(
    num.periods="numeric",
    data.cov.life="data.table",
    data.cov.trans="data.table",
    # Has to be ANY because can be Date, Posixt, or character because this class is
    # used to transport the data to the clv.fitted object for predicting and it
    # contains the clv.data@clv.time object required to convert first.transaction
    first.transaction="ANY"
  ))

clv.newcustomer.dynamic.cov <- function(num.periods, data.cov.life, data.cov.trans, first.transaction){
  return(new(
    "clv.newcustomer.dynamic.cov",
    num.periods=num.periods,
    data.cov.life=copy(data.cov.life),
    data.cov.trans=copy(data.cov.trans),
    first.transaction=first.transaction
  ))
}

clv.newcustomer.dynamic.cov.convert.time <- function(clv.newcustomer, clv.time){
  Cov.Date <- NULL

  # Convert all objects containing time information to correct data type
  # using provided clv.time
  # will be changed (by ref), therefore deep copy
  dt.cov.life <- copy(clv.newcustomer@data.cov.life)
  dt.cov.trans <- copy(clv.newcustomer@data.cov.trans)

  # Convert Cov.Date to timepoint
  dt.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]
  dt.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.time, user.timepoint = Cov.Date)]

  setkeyv(dt.cov.life, cols = "Cov.Date")
  setkeyv(dt.cov.trans, cols = "Cov.Date")

  tp.first.transaction <- clv.time.convert.user.input.to.timepoint(
    clv.time = clv.time,
    user.timepoint = clv.newcustomer@first.transaction
  )

  # Return new object with converted time information
  return(clv.newcustomer.dynamic.cov(
    num.periods=clv.newcustomer@num.periods,
    data.cov.life=dt.cov.life,
    data.cov.trans=dt.cov.trans,
    first.transaction=tp.first.transaction))
}


# Needs to inherit from a class as it would otherwise be a VIRTUAL class as it
# also has no slots.
setClass(
  Class = "clv.newcustomer.spending",
  contains = 'clv.newcustomer.base'
)

clv.newcustomer.spending <- function(){
  return(new("clv.newcustomer.spending"))
}



#' @rdname newcustomer
#' @export
newcustomer <- function(num.periods){
  check_err_msg(check_user_data_predict_newcustomer_numperiods(num.periods))
  return(clv.newcustomer.no.cov(num.periods))
}

#' @rdname newcustomer
#' @export
newcustomer.static <- function(num.periods, data.cov.life, data.cov.trans){

  check_err_msg(check_user_data_predict_newcustomer_numperiods(num.periods))
  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.life, name.of.covariate='Lifetime'))
  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.trans, name.of.covariate='Transaction'))

  return(clv.newcustomer.static.cov(
    num.periods = num.periods,
    data.cov.life = as.data.table(data.cov.life),
    data.cov.trans = as.data.table(data.cov.trans)
  ))
}

#' @rdname newcustomer
#' @export
newcustomer.dynamic <- function(num.periods, data.cov.life, data.cov.trans, first.transaction){

  check_err_msg(check_user_data_predict_newcustomer_numperiods(num.periods))
  check_err_msg(check_user_data_newcustomer_firsttransaction(first.transaction))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.life, name.of.covariate = "Lifetime"))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.trans, name.of.covariate = "Transaction"))

  dt.cov.life <- as.data.table(data.cov.life)
  dt.cov.trans <- as.data.table(data.cov.trans)

  # Check Cov.Date is allowed type
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.life,  name.date = 'Cov.Date', name.var="Lifetime covariate"))
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.trans, name.date = 'Cov.Date', name.var="Transaction covariate"))

  # Cannot convert Cov.Date because dont have clv.time object.
  # When predicting, use clv.newcustomer.dynamic.cov.convert.time

  return(clv.newcustomer.dynamic.cov(
    num.periods = num.periods,
    data.cov.life = dt.cov.life,
    data.cov.trans = dt.cov.trans,
    first.transaction = first.transaction
  ))
}


#' @rdname newcustomer
#' @export
newcustomer.spending <- function(){
  return(clv.newcustomer.spending())
}



check_user_data_newcustomer_staticcovdatacov <- function(data.cov, name.of.covariate){
  # Check if data has basic properties
  if(!is.data.frame(data.cov)){
    return("Only covariate data of type data.frame or data.table can be processed!")
  }

  if(nrow(data.cov) != 1){
    return("Covariate data has be be given for exacly one single customer (exactly 1 row)!")
  }

  if(anyNA(data.cov)){
    return("There may be no NAs in the covariate data!")
  }

  # Not required because testing that exactly equal param names but be explicit that not required
  if("Id" %in% names(data.cov)){
    return(paste0("There may be no column named 'Id' in the ",name.of.covariate," covariate data!"))
  }

  if(!all(sapply(data.cov, is.numeric))){
    return(paste0("All ",name.of.covariate," covariate data needs to be of type numeric!"))
  }
}


check_user_data_newcustomer_dyncovdatacov <- function(data.cov, names.col, name.of.covariate){
  # Check if data has basic properties
  if(!is.data.frame(data.cov)){
    return("Only covariate data of type data.frame or data.table can be processed!")
  }

  if(nrow(data.cov) == 0){
    return(paste0("The ",name.of.covariate," covariate data may not empty!"))
  }

  if(anyNA(data.cov)){
    return(paste0("There may be no NAs in the ",name.of.covariate," covariate data!"))
  }

  if("Id" %in% colnames(data.cov)){
    return(paste0("There may be no column named 'Id' in the ",name.of.covariate," covariate data!"))
  }

  if(!("Cov.Date" %in% colnames(data.cov))){
    return(paste0("There needs to be a column named 'Cov.Date' in the ",name.of.covariate," covariate data!"))
  }

  # the following does not work for data.table because passing a variable in dt[, j] returns the variable
  # itself and not the column(s), deliberately (should use ..var). Therefore, convert to data.frame.
  if(!all(sapply(as.data.frame(data.cov)[, setdiff(colnames(data.cov), "Cov.Date"), drop=FALSE], is.numeric))){
    return(paste0("All ",name.of.covariate," covariate data needs to be of type numeric!"))
  }

  return(c())
}


#' @importFrom lubridate is.POSIXt
check_user_data_newcustomer_firsttransaction <- function(first.transaction){
  if(missing(first.transaction)){
    return("Parameter first.transaction is required!")
  }

  if(length(first.transaction) != 1){
    return("Parameter first.transaction has to be of length 1!")
  }

  if(anyNA(first.transaction)){
    return("Parameter first.transaction may not contain any NAs!")
  }

  if(!is.character(first.transaction) & !is.Date(first.transaction) & !is.POSIXt(first.transaction)){
    return("Parameter first.transaction has to be of type character, Date, POSIXct, or POSIXlt!")
  }

  return(c())
}
