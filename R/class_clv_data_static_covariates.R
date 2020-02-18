#' Transactional and static covariates data to fit CLV models
#'
#'
#' Extends the class \code{\link[CLVTools:clv.data-class]{clv.data}} and adds slots to store data and names of
#' static covariates for both processes.
#' An object of this class then serves as input to fit models with static covariates.
#'
#'
#' @slot data.cov.life Single \code{data.table} with all static covariate data for the lifetime process
#' @slot data.cov.trans Single \code{data.table} with all static covariate data for the transaction process
#' @slot names.cov.data.life Character vector with names of the static lifetime covariates.
#' @slot names.cov.data.trans Character vector with names of the static transaction covariates.
#Corresponds to the column names of the \code{data.table} in slot data.cov.life
#'
#' @seealso Definition of the parent class \code{\link[CLVTools:clv.data-class]{clv.data}}.
#' @seealso For fitting covariate models: \code{\link[CLVTools:pnbd]{pnbd}}
#'
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include all_generics.R class_clv_data_no_covariates.R class_clv_time.R
setClass(Class = "clv.data.static.covariates", contains = "clv.data",
         slots = c(
           data.cov.life  = "data.table",
           data.cov.trans = "data.table",

           names.cov.data.life  = "character",
           names.cov.data.trans = "character"),

         prototype = list(
           data.cov.life           = data.table(),
           data.cov.trans          = data.table(),

           names.cov.data.life     = character(0),
           names.cov.data.trans    = character(0)))

# Constructor
clv.data.static.covariates <- function(no.cov.obj,data.cov.life,data.cov.trans, names.cov.data.life,names.cov.data.trans){

  # all the data in the no covariate clv.data object need to be deep copied.
  #   This is only relevant for the data.tables in it (data.transactions)

  obj.cov <- new(Class = "clv.data.static.covariates",
                 data.table::copy(no.cov.obj), # copy construct on deep copy of no cov data
                 name = "CLV Transaction Data with Static Covariates",
                 names.cov.data.life=names.cov.data.life, names.cov.data.trans=names.cov.data.trans,
                 data.cov.life  = data.cov.life,
                 data.cov.trans = data.cov.trans)

  return(obj.cov)
}
