#' @templateVar name_data_text Data Table
#' @templateVar name_data_code data.table
#' @templateVar name_res dt.trans
#' @template template_clvdata_asdatax
#' @template template_param_dots
#' @export
as.data.table.clv.data <- function(x,
                                   Ids = NULL,
                                   sample = c("both", "estimation", "holdout"),
                                   ...){

  check_err_msg(check_user_data_emptyellipsis(...))
  sample <- match.arg(arg = tolower(sample), choices = c("both", "estimation", "holdout"))
  if(sample == "holdout" & !clv.data.has.holdout(x)){
    check_err_msg("The given clv.data object has no holdout data!")
  }

  dt.trans <- switch(sample,
                     "both" = copy(x@data.transactions),
                     "estimation" = clv.data.get.transactions.in.estimation.period(x),
                     "holdout" = clv.data.get.transactions.in.holdout.period(x))

  if(is.null(Ids)){
    return(dt.trans)
  }else{
    dt.trans <- dt.trans[Id %in% Ids]

    # ***TODO: Should stop instead of warn if not all Ids are there?
    if(dt.trans[, uniqueN(Id)] != length(unique(Ids))){
      warning("Not for all of the given Ids transaction data could be found")
    }
    return(dt.trans)
  }
}

#' @templateVar name_data_text Data Frame
#' @templateVar name_data_code data.frame
#' @templateVar name_res df.trans
#' @template template_clvdata_asdatax
#' @template template_param_dots
#' @export
as.data.frame.clv.data <- function(x,
                                   Ids = NULL,
                                   sample = c("both", "estimation", "holdout"),
                                   ...){
  return(as.data.frame(as.data.table.clv.data(x, Ids=Ids, sample=sample, ...=...)))
}


#' Number of observations
#'
#' The number of observations is defined as the number of unique customers in the transaction data.
#'
#' @template template_param_dots
#' @param object An object of class clv.data.
#'
#' @return The number of customers.
#'
#' @importFrom stats nobs
#' @export
nobs.clv.data   <- function(object, ...){
  Id <- NULL
  # Observations are number of customers
  return(as.integer(object@data.transactions[, uniqueN(Id)]))
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
  .print.list(list("Total # customers"    = nobs(x),
                   "Total # transactions" = nrow(x@data.transactions),
                   "Spending information" = clv.data.has.spending(clv.data = x)))
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
  .print.list(list("Cov Date first Life"  = clv.time.format.timepoint(clv.time=x@clv.time, timepoint=timepoint.first.cov.life),
                   "Cov Date last  Life"  = clv.time.format.timepoint(clv.time=x@clv.time, timepoint=timepoint.last.cov.life),
                   "Cov Date first Trans" = clv.time.format.timepoint(clv.time=x@clv.time, timepoint=timepoint.first.cov.trans),
                   "Cov Date last  Trans" = clv.time.format.timepoint(clv.time=x@clv.time, timepoint=timepoint.last.cov.trans)))

  invisible(x)
}


#' @template template_summary_data
#' @export
summary.clv.data <- function(object, ...){
  res <- structure(list(), class="summary.clv.data")

  res$name <- object@name
  res$clv.time         <- object@clv.time # needed for formating when printing
  res$summary.clv.time <- summary(object@clv.time)

  res$descriptives.transactions <- clv.data.make.descriptives(clv.data=object)
  return(res)
}

#' @param x An object of class \code{"summary.clv.data"}, usually, a result of a call to \code{summary.clv.data}.
#' @param digits The number of significant digits to use when printing.
#'
#' @export
#' @rdname summary.clv.data
#' @keywords internal
print.summary.clv.data <- function(x, digits=max(3L, getOption("digits")-3L), ...){
  nsmall <- 4

  cat(x$name, "\n")

  print(x$summary.clv.time, digits=digits, ...)


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
#' @rdname clv.data-class
setMethod(f = "show", signature = signature(object="clv.data"), definition = function(object){
  print(x=object)})



#' Subsetting clv.data
#'
#' Returns a subset of the transaction data stored within the given \code{clv.data} object which meet conditions.
#' The given expression are forwarded to the \code{data.table} of transactions.
#' Possible rows to subset and select are \code{Id}, \code{Date}, and \code{Price} (if present).
#'
#' @param x \code{clv.data} to subset
#' @param subset logical expression indicating rows to keep
#' @param select expression indicating columns to keep
#' @param ... further arguments passed to \code{data.table::subset}
#' @template template_param_sample
#'
#' @return A copy of the \code{data.table} of selected transactions. May contain columns \code{Id}, \code{Date}, and \code{Price}.
#'
#' @seealso \code{data.table}'s \code{\link[data.table:subset]{subset}}
#'
#' @aliases subset
#'
#' @examples
#' data(cdnow)
#'
#' clv.cdnow <- clvdata(cdnow,
#'   date.format="ymd",
#'   time.unit = "week",
#'   estimation.split = "1997-09-30")
#'
#' # all transactions of customer "1"
#' subset(clv.cdnow, Id=="1")
#' subset(clv.cdnow, subset = Id=="1")
#'
#' # all transactions of customer "111" in the estimation period...
#' subset(clv.cdnow, Id=="111", sample="estimation")
#' # ... and in the holdout period
#' subset(clv.cdnow, Id=="111", sample="holdout")
#'
#' # all transactions of customers "1", "2", and "999"
#' subset(clv.cdnow, Id %in% c("1","2","999"))
#'
#' # all transactions on "1997-02-16"
#' subset(clv.cdnow, Date == "1997-02-16")
#'
#' # all transactions between "1997-02-01" and "1997-02-16"
#' subset(clv.cdnow, Date >= "1997-02-01" & Date <= "1997-02-16")
#' # same using data.table's between
#' subset(clv.cdnow, between(Date, "1997-02-01","1997-02-16"))
#'
#' # all transactions with a value between 50 and 100
#' subset(clv.cdnow, Price >= 50 & Price <= 100)
#' # same using data.table's between
#' subset(clv.cdnow, between(Price, 50, 100))
#'
#' # only keep Id of transactions on "1997-02-16"
#' subset(clv.cdnow, Date == "1997-02-16", "Id")
#'
#' @export
subset.clv.data <- function(x,
                            subset,
                            select,
                            sample=c("full", "estimation", "holdout"),
                            ...){

  mc <- match.call(expand.dots = FALSE)

  sample <- match.arg(sample, choices=c("full", "estimation", "holdout"))
  if(sample == "holdout" & !clv.data.has.holdout(x)){
    check_err_msg("The given clv.data object has no holdout data!")
  }

  # replace object and function in call
  mc[[1L]] <- quote(base::subset)
  mc[["x"]] <- switch(sample,
                      "full"       = x@data.transactions,
                      "estimation" = clv.data.get.transactions.in.estimation.period(x),
                      "holdout"    = clv.data.get.transactions.in.holdout.period(x))
  # only keep subset, select to call data.table
  mc <- mc[c(1L, match(c("x", "subset", "select", "..."), names(mc), 0L))]
  return(eval(mc, parent.frame()))

  # NextMethod(object=x@data.transactions) # object has no S3 class attribute (vector)

  # Does not work because subset and select are expressions
  # dt.subset <- data.table:::subset.data.table(x=x@data.transactions, subset=subset, select=select, ...=...)
  # return(dt.subset)
  # if(isTRUE(all.equal(address(dt.subset),address(x@data.transactions))){
  #   return(copy(dt.subset))
  # }else{
  #   return(dt.subset)
  # }
}

#' #'
#' #' @export
#' `[.clv.data` <- function(x, i, j, value){
#'   mc <- match.call(expand.dots = FALSE)
#'   print(names(mc))
#'   mc[[1L]] <-  data.table:::`[.data.table` # base::`[`
#'   mc[["x"]] <- x@data.transactions
#'   print(names(mc))
#'   return(eval(mc, parent.frame()))
#' }

