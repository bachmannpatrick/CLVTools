# This class only exists proforma to have a class clv.newcustomer.no.cov with no slots
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
    data.cov.life=data.cov.life,
    data.cov.trans=data.cov.trans
    ))
}


setClass(
  Class = "clv.newcustomer.dynamic.cov",
  contains = "clv.newcustomer.static.cov",
  representation = list(
    # has to be ANY because can be Date, Posixt, or character as this class is
    # used to transport the data to the clv.fitted object which is used for the prediction and
    # contains the clv.data@clv.time object required to convert first.transaction
    first.transaction="ANY"
  ))

clv.newcustomer.dynamic.cov <- function(data.cov.life, data.cov.trans, first.transaction){
  return(new(
    "clv.newcustomer.dynamic.cov",
    data.cov.life=data.cov.life,
    data.cov.trans=data.cov.trans,
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

  return(clv.newcustomer.dynamic.cov(
    data.cov.life = as.data.table(data.cov.life),
    data.cov.trans = as.data.table(data.cov.trans),
    first.transaction = first.transaction
  ))
}



check_user_data_newcustomer_staticcovdatacov <- function(data.cov, name.of.covariate){
  # Check if data has basic properties
  if(!is.data.frame(data.cov)){
    return("Only covariate data of type data.frame or data.table can be processed!")
  }

  if(nrow(data.cov) != 1){
    return("Covariate data has be be given for exacly one single customer (only 1 row)!")
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

  if("Id" %in% names(data.cov)){
    return(paste0("There may be no column named 'Id' in the ",name.of.covariate," covariate data!"))
  }


  # the following does not work for data.table because passing a variable in dt[, j] returns the variable
  # itself and not the column(s), delibrately (should use ..var). Therefore, convert to data.frame
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
