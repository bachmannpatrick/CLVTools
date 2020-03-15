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



#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.matrix.data.cov.life", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(data.matrix(clv.data@data.cov.life[, .SD, .SDcols=clv.data@names.cov.data.life]))
})


#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.matrix.data.cov.trans", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(data.matrix(clv.data@data.cov.trans[, .SD, .SDcols=clv.data@names.cov.data.trans]))
})

#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.names.cov.life", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(clv.data@names.cov.data.life)
})

#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.names.cov.trans", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(clv.data@names.cov.data.trans)
})


#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.reduce.covariates", signature = signature(clv.data="clv.data.static.covariates"),
          definition = function(clv.data, names.cov.life, names.cov.trans){
            # Reduce covariate data to Id + cov names if told by user

            if(length(names.cov.life) != 0 & !identical(names.cov.life, clv.data@names.cov.data.life)){
              clv.data@names.cov.data.life  <- names.cov.life
              clv.data@data.cov.life        <- clv.data@data.cov.life[,  .SD, .SDcols=c("Id", clv.data@names.cov.data.life)]
            }

            if(length(names.cov.trans) !=0 & !identical(names.cov.trans, clv.data@names.cov.data.trans)){
              clv.data@names.cov.data.trans <- names.cov.trans
              clv.data@data.cov.trans       <- clv.data@data.cov.trans[, .SD, .SDcols=c("Id", clv.data@names.cov.data.trans)]
            }
            return(clv.data)
          })


