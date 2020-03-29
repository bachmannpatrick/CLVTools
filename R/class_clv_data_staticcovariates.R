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
#' @include all_generics.R class_clv_data.R class_clv_time.R
setClass(Class = "clv.data.static.covariates", contains = "clv.data",
         slots = c(
           data.cov.life  = "data.table",
           data.cov.trans = "data.table",

           names.cov.data.life  = "character",
           names.cov.data.trans = "character"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           data.cov.life           = data.table(),
           data.cov.trans          = data.table(),

           names.cov.data.life     = character(0),
           names.cov.data.trans    = character(0)))

# Constructor
#' @importFrom methods new
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


clv.data.get.matrix.data.cov.life <- function(clv.data){
  return(data.matrix(clv.data@data.cov.life[, .SD, .SDcols=clv.data@names.cov.data.life]))
}

clv.data.get.matrix.data.cov.trans <- function(clv.data){
  return(data.matrix(clv.data@data.cov.trans[, .SD, .SDcols=clv.data@names.cov.data.trans]))
}

clv.data.get.names.cov.life <- function(clv.data){
  return(clv.data@names.cov.data.life)
}

clv.data.get.names.cov.trans <- function(clv.data){
  return(clv.data@names.cov.data.trans)
}


clv.data.reduce.covariates <- function(clv.data, names.cov.life, names.cov.trans){
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
}


#' @importFrom stats model.frame model.matrix reformulate
convert_userinput_covariatedata_dummies <- function(dt.cov.data, names.cov){

  # Use model.frame/model.matrix to convert cov data
  #   numeric to numeric, char/factors to k-1 dummies

  # Always need intercept!
  #   to always get k-1 dummies, as no intercept implies k dummies in the
  #   case of only a single catgorical covariate
  f.covs <- reformulate(termlabels = names.cov,
                        response = NULL,
                        intercept = TRUE)

  mf <- model.frame(f.covs, data = dt.cov.data)
  mm <- model.matrix(object = f.covs, data = dt.cov.data)

  # Combine averything else (Id, maybe Cov.Date) and raw converted numeric covariate data
  dt.cov <- cbind(dt.cov.data[, .SD, .SDcols=setdiff(colnames(dt.cov.data), names.cov)], # everything except cov data
                  mm[, setdiff(colnames(mm), "(Intercept)"), drop=FALSE]) # everything except the Intercept

  names.cov <- setdiff(colnames(dt.cov), c("Id", "Cov.Date"))

  return(list(data.cov = dt.cov, names.cov=names.cov))
}

