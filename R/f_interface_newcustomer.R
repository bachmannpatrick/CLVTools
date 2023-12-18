setClass("clv.newcustomer.no.cov", representation = list())

clv.newcustomer <- function(){
  return(new("clv.newcustomer.no.cov"))
}

setClass(
  Class = "clv.newcustomer.static.cov",
  contains = "clv.newcustomer.no.cov",
  representation = list(
    data.cov.life="data.frame",
    data.cov.trans="data.frame"
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
  return(clv.newcustomer)
}

#' @export
newcustomer.static <- function(data.cov.life, data.cov.trans){
  return(clv.newcustomer.static.cov(
    data.cov.life = data.cov.life,
    data.cov.trans = data.cov.trans
  ))
}

#' @export
newcustomer.dynamic <- function(data.cov.life, data.cov.trans, first.transaction){
  return(clv.newcustomer.dynamic.cov(
    data.cov.life = data.cov.life,
    data.cov.trans = data.cov.trans,
    first.transaction = first.transaction
  ))
}
