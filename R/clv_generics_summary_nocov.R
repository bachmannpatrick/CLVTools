#' @rdname summary.clv.fitted
#' @include clv_helpers.R
#' @export
#' @keywords internal
print.summary.clv.fitted <- function(x, digits=max(3L, getOption("digits")-3L),
                                   signif.stars = getOption("show.signif.stars"), ...){
  Total <- Name <- Estimation <- Holdout <- Total <- NULL

  nsmall <- 4

  cat(x$name.model, " Model \n")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  # Print fitting period information ----------------------------------------
  cat("Fitting period:")
  .print.list(nsmall=nsmall,
              l=list("Estimation start"  = as.character(x$tp.estimation.start),
                     "Estimation end"    = as.character(x$tp.estimation.end),
                     "Estimation length" = paste0(format(x$estimation.period.in.tu, digits=digits,nsmall=nsmall), " ", x$time.unit)))

  cat("\n")

  # Print estimated model parameters ---------------------------------------
  cat("Coefficients:\n")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)

  # General Optimization infos ---------------------------------------------
  cat("\nOptimization info:")
  .print.list(nsmall=nsmall,
              l = list("LL"  = x$estimated.LL,
                       "AIC" = x$AIC,
                       "BIC" = x$BIC,
                       "KKT 1" = x$kkt1,
                       "KKT 2" = x$kkt2,
                       "fevals" = x$fevals,
                       "Method" = x$method))

  # Correlation ------------------------------------------------------------
  cat("\nUsed Options:")
  .print.list(nsmall=nsmall,
              l = x$additional.options)

  return(invisible(x))
}


#' @template template_summary
#' @include class_clv_fitted.R
#' @export
summary.clv.fitted <- function(object, ...){
  ## The basis structure
  res <- structure(list(), class = "summary.clv.fitted")

  # Model stuff ---------------------------------------------------------------------
  res$name.model <- object@clv.model@name.model
  res$call       <- object@call

  # Estimation & Transaction --------------------------------------------------------
  # Fitting period
  res$tp.estimation.start     <- object@clv.data@clv.time@timepoint.estimation.start
  res$tp.estimation.end       <- object@clv.data@clv.time@timepoint.estimation.end
  res$estimation.period.in.tu <- object@clv.data@clv.time@estimation.period.in.tu
  res$time.unit               <- object@clv.data@clv.time@name.time.unit

  # Coefficient table --------------------------------------------------------------
  # Return the full coefficient table. The subset is to relevant rows is done in the
  #   printing
  # Jeff:
  # SE <- sqrt(diag(Hinvcov))
  # tval    <- (params-0) / SE
  # pval    <- 2*(1-pnorm(abs(tval)))
  all.est.params  <- coef(object)

  # return NA_ placeholder if cannot calculate vcov
  res$vcov <- tryCatch(vcov(object),
                       error = function(e){
                         h <- object@optimx.hessian
                         h[,] <- NA_real_
                         return(h)})
  se <- tryCatch(suppressWarnings(sqrt(diag(res$vcov))),
                              error = function(e)return(e))
  if(is(se, "error")){
    warning("The standard errors could not be calculated because the vcov contains non-numeric elements.", call. = FALSE)
  }else{
    # only check if not error
    if(anyNA(se))
      warning("For some parameters the standard error could not be calculated.", call. = FALSE)
  }

  # t.val <- (all.est.params-0)/se
  z.val <- (all.est.params-0)/se
  # p.val <- 2*pt(q=-abs(t.val), df=nobs(object)-1)
  p.val <- 2*(1-pnorm(abs(z.val)))

  res$coefficients <- cbind(all.est.params,
                            se,
                            z.val,
                            p.val)
  rownames(res$coefficients) <- names(all.est.params)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-val", "Pr(>|z|)")

  # Optimization -------------------------------------------------------------------
  res$estimated.LL <- as.vector(logLik(object))
  res$AIC <- AIC(object)
  res$BIC <- BIC(object)

  last.row.optimx <- tail(object@optimx.estimation.output, n = 1)
  res$kkt1 <- last.row.optimx$kkt1
  res$kkt2 <- last.row.optimx$kkt2

  res$fevals <- last.row.optimx$fevals
  res$method <- rownames(last.row.optimx)

  # Additional options: Correlation ------------------------------------------------
  res$additional.options <- list("Correlation"=object@estimation.used.correlation)
  # if(object@estimation.used.correlation)
  #   res$additional.options <- c(res$additional.options,
  #                               #unname. cannot store w/o name as is estimated param
  #                               paste0("   ",unname(object@estimated.param.cor)))

  return(res)
}


#' @export
coef.summary.clv.fitted <- function(object, ...){
  return(object$coefficients)
}

