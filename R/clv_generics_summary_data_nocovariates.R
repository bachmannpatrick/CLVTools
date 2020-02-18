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
  # disp <- array(data = NA_character_, dim = list(length(x$descriptives.total), 3))
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


