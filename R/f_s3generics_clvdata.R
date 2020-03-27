#' @importFrom stats nobs
#' @export
nobs.clv.data   <- function(object, ...){
  Name <- NULL # cran silence
  # Observations are number of customers
  return(as.integer(object@descriptives.transactions[Name == "Number of customers"]$Total))
}


#' @export
#' @include class_clv_data.R
print.clv.data <- function(x, digits = max(3L, getOption("digits") - 3L), ...){

  Name <- Total  <- NULL

  nsmall <- 4 # dont leave to user, hardcode

  # Print an overview of the data
  cat(x@name, "\n")

  cat("\nCall:\n", paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")

  # Rough data set overview of sample only  --------------------------------------------------
  .print.list(list("Total # customers"    = x@descriptives.transactions[Name == "Number of customers",  Total],
                   "Total # transactions" = x@descriptives.transactions[Name == "Total # Transactions", Total]))
  cat("\n")
  print(x@clv.time, digits = digits, ...)
  # clv.time already prints a newline

  invisible(x)
}

#' @export
#' @include class_clv_data_staticcovariates.R
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

  Cov.Date <- NULL

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


#' @template template_summary_data
#' @export
summary.clv.data <- function(object, ...){
  res <- structure(list(), class="summary.clv.data")

  res$name <- object@name
  res$summary.clv.time <- summary(object@clv.time)
  res$descriptives.transactions <- object@descriptives.transactions
  return(res)
}

#' @export
#' @rdname summary.clv.data
#' @keywords internal
print.summary.clv.data <- function(x, digits=max(3L, getOption("digits")-3L),
                                   signif.stars = getOption("show.signif.stars"), ...){
  nsmall <- 4

  cat(x$name, "\n")

  print(x$summary.clv.time, digits=digits, signif.stars = signif.stars, ...)


  # Print transactions descriptives for each period -------------------------------------------

  # Actual descriptives
  disp <- array(data = NA_character_, dim = list(nrow(x$descriptives.transactions), 3))
  disp[, 1] <- x$descriptives.transactions$Estimation
  disp[, 2] <- x$descriptives.transactions$Holdout
  disp[, 3] <- x$descriptives.transactions$Total

  rownames(disp) <- x$descriptives.transactions$Name
  colnames(disp) <- c("Estimation", "Holdout", "Total")

  cat("\n")
  cat("Transaction Data Summary \n")
  print(disp,quote = FALSE, na.print = "", print.gap = 6)

  cat("\n")
  invisible(x)
}



#' @include class_clv_data.R
#' @importFrom methods show
#' @export
setMethod(f = "show", signature = signature(object="clv.data"), definition = function(object){
  print(x=object)})


