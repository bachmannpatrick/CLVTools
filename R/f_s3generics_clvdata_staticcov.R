#' @export
#' @rdname summary.clv.data
#' @keywords internal
summary.clv.data.static.covariates <- function(object, ...){

  # get part for no cov data
  res <- NextMethod()
  class(res) <- c("summary.clv.data.static.covariates", class(res))

  # res$name.covariates.type <- "Static Covariates" #printing
  res$names.cov.data.trans <- object@names.cov.data.trans
  res$names.cov.data.life  <- object@names.cov.data.life

  return(res)
}


#' @export
#' @rdname summary.clv.data
print.summary.clv.data.static.covariates <- function(x, digits=max(3L, getOption("digits")-3L), ...){

  NextMethod()

  cat("Covariates")
  # cat(x$name.covariates.type)
  .print.list(list( "Trans. Covariates   " = paste(x$names.cov.data.trans, collapse=", "),
                    "       # covs"        = length(x$names.cov.data.trans),
                    "Life.  Covariates   " = paste(x$names.cov.data.life,  collapse=", "),
                    "       # covs "       = length(x$names.cov.data.life)))

  cat("\n")

  invisible(x)
}


#' @importFrom methods show
#' @include class_clv_data_staticcovariates.R
#' @export
#' @rdname clv.data.static.covariates-class
setMethod(f = "show", signature = signature(object="clv.data.static.covariates"), definition = function(object){
  print(x=object)})
