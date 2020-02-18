
#' @export
#' @include all_generics.R class_clv_data_no_covariates.R clv_helpers.R
print.clv.data <- function(x, digits = max(3L, getOption("digits") - 3L), ...){

  nsmall <- 4 # dont leave to user, hardcode

  # Print an overview of the data
  cat(x@name, "\n")

  # cat("\nCall:\n", paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")

  # Rough data set overview of sample only  --------------------------------------------------
  .print.list(list("Total # customers"    = x@descriptives.transactions[Name == "Number of customers",  Total],
                   "Total # transactions" = x@descriptives.transactions[Name == "Total # Transactions", Total]))
  cat("\n")
  print(x@clv.time, digits = digits, ...)
  # cat("\n") # clv.time already prints the newline

  invisible(x)
}

#' @export
#' @include all_generics.R class_clv_data_static_covariates.R clv_helpers.R
print.clv.data.static.covariates <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # print no cov part
  NextMethod()

  cat("Covariates")

  # Print rough covariates overview if it is a covariates model -----------------------------
  #**TODO: maybe put onto separate lines instead of long text
  .print.list(list( "Trans. Covariates   " = paste(x@names.cov.data.trans, collapse=", "),
                    "       # covs"        = length(x@names.cov.data.trans),
                    "Life.  Covariates   " = paste(x@names.cov.data.life,  collapse=", "),
                    "       # covs "       = length(x@names.cov.data.life)))

  invisible(x)
}

#' @export
print.clv.data.dynamic.covariates <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # print static cov part
  NextMethod()

  # Cannot store in object because of datatype issues.
  #   Would need to subclass clv.time or hide in a list in object
  timepoint.first.cov.life  <- x@data.cov.life[,  min(Cov.Date)]
  timepoint.last.cov.life   <- x@data.cov.life[,  max(Cov.Date)]
  timepoint.first.cov.trans <- x@data.cov.trans[, min(Cov.Date)]
  timepoint.last.cov.trans  <- x@data.cov.trans[, max(Cov.Date)]

  # Print first and last day of cov data
  .print.list(list("Cov Date start Life"  = as.character(timepoint.first.cov.life),
                   "Cov Date end   Life"  = as.character(timepoint.last.cov.life),
                   "Cov Date start Trans" = as.character(timepoint.first.cov.trans),
                   "Cov Date end   Trans" = as.character(timepoint.last.cov.trans)))

  invisible(x)
}



#' @include all_generics.R class_clv_data_no_covariates.R
#' @importFrom methods show
#' @export
setMethod(f = "show", signature = signature(object="clv.data"), definition = function(object){
  print(x=object)})

#' @importFrom methods show
#' @include all_generics.R class_clv_data_static_covariates.R
#' @export
setMethod(f = "show", signature = signature(object="clv.data.static.covariates"), definition = function(object){
  print(x=object)})

#' @importFrom methods show
#' @include all_generics.R class_clv_data_dynamic_covariates.R
#' @export
setMethod(f = "show", signature = signature(object="clv.data.dynamic.covariates"), definition = function(object){
  print(x=object)})
