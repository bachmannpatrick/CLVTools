#' @export
#' @rdname summary.clv.data
summary.clv.data.dynamic.covariates <- function(object, ...){

  # get part for static cov data
  res <- NextMethod()
  class(res) <- c("summary.clv.data.dynamic.covariates", class(res))

  # res$name.covariates.type <- "Dynamic Covariates" #printing

  # Cannot store in object because of datatype issues.
  #   Would need to subclass clv.time or hide in a list in object
  res$timepoint.first.cov.life  <- object@data.cov.life[,  min(Cov.Date)]
  res$timepoint.last.cov.life   <- object@data.cov.life[,  max(Cov.Date)]
  res$timepoint.first.cov.trans <- object@data.cov.trans[, min(Cov.Date)]
  res$timepoint.last.cov.trans  <- object@data.cov.trans[, max(Cov.Date)]

  return(res)
}


#' @export
#' @rdname summary.clv.data
print.summary.clv.data.dynamic.covariates <- function(x, digits=max(3L, getOption("digits")-3L),
                                                      signif.stars = getOption("show.signif.stars"), ...){
  # print static cov part
  NextMethod()

  # Print first and last day of cov data
  .print.list(list("Cov Date start Life"  = as.character(x$timepoint.first.cov.life),
                   "Cov Date end   Life"  = as.character(x$timepoint.last.cov.life),
                   "Cov Date start Trans" = as.character(x$timepoint.first.cov.trans),
                   "Cov Date end   Trans" = as.character(x$timepoint.last.cov.trans)))

  cat("\n")

  invisible(x)
}
