#' Transactional and static covariates data to fit CLV models
#'
#'
#' Extends the class \linkS4class{clv.data} and adds slots to store data and names of
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
#' @seealso Definition of the parent class \linkS4class{clv.data}.
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


#' @importFrom methods new
clv.data.static.covariates <- function(no.cov.obj, data.cov.life, data.cov.trans, names.cov.data.life,names.cov.data.trans){

  # Cannot set keys here because only setting "Id" would remove the keys set for dyncov

  # all the data in the no covariate clv.data object need to be deep copied.
  #   This is only relevant for the data.tables in it (data.transactions)
  # Do not call the clv.data constructor function because it would require taking the clv.data object apart to pass
  #   it as single arguments
  return(new("clv.data.static.covariates",
             copy(no.cov.obj), # copy construct on deep copy of no cov data

             name = "CLV Transaction Data with Static Covariates",

             names.cov.data.life  = names.cov.data.life,
             names.cov.data.trans = names.cov.data.trans,

             data.cov.life  = data.cov.life,
             data.cov.trans = data.cov.trans))
}

clv.data.get.matrix.data.cov.life <- function(clv.data, correct.col.names, correct.row.names){
  # .SD returns copy, can use setDF without modifying the original data
  m.cov.data.life <- data.matrix(setDF(clv.data@data.cov.life[, .SD, .SDcols=clv.data@names.cov.data.life],
                                       rownames = clv.data@data.cov.life$Id))

  if(!all(rownames(m.cov.data.life) == correct.row.names))
    stop("Covariate data (life) rows are not sorted correctly. Please file a bug!")

  if(!all(colnames(m.cov.data.life) == correct.col.names))
    stop("Covariate data (life) cols are not sorted correctly. Please file a bug!")

  return(m.cov.data.life)
}

# Returns matrix of transaction cov data
#   with cols sorted same as in vector names.cov.data.trans
clv.data.get.matrix.data.cov.trans <- function(clv.data, correct.col.names, correct.row.names){
  # .SD returns copy, can use setDF without modifying the original data
  m.cov.data.trans <- data.matrix(setDF(clv.data@data.cov.trans[, .SD, .SDcols=clv.data@names.cov.data.trans],
                                        rownames = clv.data@data.cov.trans$Id))

  if(!all(rownames(m.cov.data.trans) == correct.row.names))
    stop("Covariate data (trans) rows are not sorted correctly. Please file a bug!")

  if(!all(colnames(m.cov.data.trans) == correct.col.names))
    stop("Covariate data (trans) columns are not sorted correctly. Please file a bug!")

  return(m.cov.data.trans)
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
convert_userinput_covariatedata <- function(dt.cov.data, names.cov){


  # Make syntactically valid names
  #   Rename data in order to be able to use model.frame() which requires legal names
  original.cov.names <- names.cov
  legal.cov.names    <- make.names(names.cov)
  setnames(dt.cov.data, old = original.cov.names, new = legal.cov.names)


  # Use model.frame/model.matrix to convert cov data
  #   numeric stays numeric, char/factors to k-1 dummies

  # Always need intercept!
  #   to always get k-1 dummies, as no intercept implies k dummies in the
  #   case of only a single categorical covariate
  f.covs <- reformulate(termlabels = legal.cov.names,
                        response = NULL,
                        intercept = TRUE)

  mf <- model.frame(f.covs, data = dt.cov.data)
  mm <- model.matrix(object = f.covs, data = dt.cov.data)

  # Combine everything else (Id, maybe Cov.Date for dyncov) and raw converted numeric covariate data
  dt.cov <- cbind(
    # Id and Cov.Date from original data, everything except actual cov data
    dt.cov.data[, .SD, .SDcols=setdiff(colnames(dt.cov.data), legal.cov.names)],
    # everything except the Intercept: numeric, dummies, etc
    mm[, setdiff(colnames(mm), "(Intercept)"), drop=FALSE])

  # Read final names which in the case of dummies are completely different from
  #   original.cov.names (or legal.cov.names)
  final.names.cov <- setdiff(colnames(dt.cov), c("Id", "Cov.Date"))

  return(list(data.cov = dt.cov, final.names.cov=final.names.cov))
}

