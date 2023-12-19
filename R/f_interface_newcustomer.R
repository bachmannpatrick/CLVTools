# This class only exists pro-forma to have a class clv.newcustomer.no.cov with no slots.
# Classes with no slots and no parent class are virtual. In order for clv.newcustomer.no.cov to have no slots,
# create this (virtual) base class to inherit from.
setClass(
  Class = "clv.newcustomer",
  representation = list())

setClass(
  Class = "clv.newcustomer.no.cov",
  contains = "clv.newcustomer")

clv.newcustomer.no.cov <- function(){
  return(new("clv.newcustomer.no.cov"))
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
  contains = "clv.newcustomer.no.cov",
  representation = list(
    data.cov.life="data.table",
    data.cov.trans="data.table"
))

clv.newcustomer.static.cov <- function(data.cov.life, data.cov.trans){
  return(new(
    "clv.newcustomer.static.cov",
    data.cov.life=copy(data.cov.life),
    data.cov.trans=copy(data.cov.trans)
    ))
}


setClass(
  Class = "clv.newcustomer.dynamic.cov",
  contains = "clv.newcustomer.static.cov",
  representation = list(
    # Has to be ANY because can be Date, Posixt, or character because this class is
    # used to transport the data to the clv.fitted object for predicting and it
    # contains the clv.data@clv.time object required to convert first.transaction
    first.transaction="ANY"
  ))

clv.newcustomer.dynamic.cov <- function(data.cov.life, data.cov.trans, first.transaction){
  return(new(
    "clv.newcustomer.dynamic.cov",
    data.cov.life=copy(data.cov.life),
    data.cov.trans=copy(data.cov.trans),
    first.transaction=first.transaction
  ))
}


#' @export
newcustomer <- function(){
  return(clv.newcustomer.no.cov())
}

#' @export
newcustomer.static <- function(data.cov.life, data.cov.trans){

  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.life, name.of.covariate='Lifetime'))
  check_err_msg(check_user_data_newcustomer_staticcovdatacov(data.cov=data.cov.trans, name.of.covariate='Transaction'))

  return(clv.newcustomer.static.cov(
    data.cov.life = as.data.table(data.cov.life),
    data.cov.trans = as.data.table(data.cov.trans)
  ))
}

#' @export
newcustomer.dynamic <- function(data.cov.life, data.cov.trans, first.transaction){

  check_err_msg(check_user_data_newcustomer_firsttransaction(first.transaction))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.life, name.of.covariate = "Lifetime"))
  check_err_msg(check_user_data_newcustomer_dyncovdatacov(data.cov=data.cov.trans, name.of.covariate = "Transaction"))

  dt.cov.life <- as.data.table(data.cov.life)
  dt.cov.trans <- as.data.table(data.cov.trans)

  # Check Cov.Date is allowed type
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.life,  name.date = 'Cov.Date', name.var="Lifetime covariate"))
  check_err_msg(check_userinput_data_date(dt.data = dt.cov.trans, name.date = 'Cov.Date', name.var="Transaction covariate"))

  # Cannot convert Cov.Date because dont have clv.time object

  return(clv.newcustomer.dynamic.cov(
    data.cov.life = dt.cov.life,
    data.cov.trans = dt.cov.trans,
    first.transaction = first.transaction
  ))
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
