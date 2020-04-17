#' @include class_clv_fitted.R
#' @importFrom stats logLik nobs coef
#' @importFrom utils tail
#' @importFrom optimx coef<-
#' @export
logLik.clv.fitted <- function(object, ...){
  last.row.optimx <- tail(object@optimx.estimation.output, n = 1)
  return(structure( (-1)*(last.row.optimx[["value"]]),
                    nall  = nobs(object),
                    nobs  = nobs(object),
                    df    = length(coef(last.row.optimx)),
                    class ="logLik"))
}


#' @include class_clv_fitted.R
#' @importFrom stats nobs
#' @export
nobs.clv.fitted   <- function(object, ...){
  # Observations are number of customers
  return(nrow(object@cbs))
}




#' @include class_clv_fitted.R
#' @importFrom stats coef
#' @importFrom optimx coef<-
#' @importFrom utils tail
#' @export
coef.clv.fitted <- function(object, complete=TRUE, ...){

  last.row.optimx.coef <- tail(coef(object@optimx.estimation.output),n=1)

  # model params from clv.model (backtransform) --------------------------------------------------------
  #   Backtransform estimated model params from opimizer to original scale
  prefixed.params.model       <- last.row.optimx.coef[1, object@clv.model@names.prefixed.params.model,drop=TRUE]
  original.scale.model.params <- clv.model.backtransform.estimated.params.model(clv.model = object@clv.model,
                                                                                prefixed.params.model = prefixed.params.model)
  # Set original scale names and ensure order
  original.scale.model.params <- setNames(original.scale.model.params[object@clv.model@names.prefixed.params.model],
                                          object@clv.model@names.original.params.model)
  original.scale.params <- original.scale.model.params

  # Correlation param ---------------------------------------------------------------------------------------
  if(object@estimation.used.correlation & complete==TRUE){
    last.row.optimx.coef   <- tail(coef(object@optimx.estimation.output),n=1)
    param.m                <- last.row.optimx.coef[1, object@name.prefixed.cor.param.m, drop=TRUE]
    param.cor              <- clv.model.m.to.cor(clv.model = object@clv.model, prefixed.params.model=prefixed.params.model,
                                                 param.m = param.m)
    names(param.cor)       <- object@name.correlation.cor
    original.scale.params  <- c(original.scale.params, param.cor)
  }

  return(original.scale.params)
}



#' @title Calculate Variance-Covariance Matrix for CLV Models fitted with Maximum Likelihood Estimation
#'
#' @param object a fitted clv model object
#' @param ... ignored, for consistency with the generic function.
#'
#'
#' @description
#' Returns the variance-covariance matrix of the parameters of the fitted model object.
#' The variance-covariance matrix is derived from the Hessian that results from the optimization procedure.
#' First, the Moore–Penrose generalized inverse of the Hessian is used to obtain an estimate of the
#' variance-covariance matrix. If the result is not positive definite, \link[Matrix:nearPD]{nearPD} is used
#' with standard settings to find the nearest positive definite matrix.
#'
#' Because some parameters may be transformed for the purpose of restricting their value during
#' the log-likelihood estimation, it requires that the variance estimates are adapted as well to
#' be comparable to the reported coefficient estimates. See the references for more details on how this is done.
#'
#' If multiple estimation methods were used, the Hessian of the last method is used.
#'
#' @return
#' A matrix of the estimated covariances between the parameter estimates of the model.
#' The row and column names correspond to the parameter names given by the \code{coef} method.
#'
#' @references
#' Jeff's "Note on p-values"
#'
#' @seealso \link[MASS]{ginv}, \link[Matrix]{nearPD}
#'
#'
#' @importFrom stats vcov
#' @importFrom utils tail
#' @importFrom Matrix nearPD
#' @importFrom MASS ginv
#'
#' @export
vcov.clv.fitted <- function(object, ...){

  if(any(!is.finite(object@optimx.hessian)))
    stop("The vcov matrix cannot be calulated because the hessian contains non-finite values!", call. = FALSE)

  # Get the vcov as the inverse of hessian
  #   log(thetahat) ~ N(theta,Hinv)!
  #
  #   Jeff:
  #   negative hessian or hessian?!
  #     -> Depends on optimization! If we have the loglikelihood we need the negative Hessian.
  #   However we have the negative (!) Log likelihood, so we need to take the Hessian directly

  # Moore-Penrose inverse of Hessian
  #   Results in the regular inverse if invertible
  m.hessian.inv <- ginv(object@optimx.hessian)


  # Make positive definite if it is not already
  #   Returns unchanged if the matrix is PD already
  m.hessian.inv <- as.matrix(nearPD(m.hessian.inv)$mat)


  # Apply Jeff's delta method to account for the transformations of the parameters
  #   See Jeff's Note on how to derive p-values
  #   The variances are for the transformed parameters (ie log-scale)

  # Get the numbers to put in diag() for back transformation from the model
  #   Jeff: Apply the transformation on optimizer-scale parameters
  prefixed.params  <- tail(coef(object@optimx.estimation.output), n=1)[1, ,drop = TRUE]
  m.delta.diag     <- clv.model.vcov.jacobi.diag(clv.model=object@clv.model, clv.fitted=object,
                                                 prefixed.params=prefixed.params)

  stopifnot(all(colnames(m.hessian.inv) == colnames(m.delta.diag)))
  stopifnot(all(rownames(m.hessian.inv) == rownames(m.delta.diag)))
  m.vcov <- m.delta.diag %*% m.hessian.inv %*% m.delta.diag

  # Naming and sorting
  #   Sorting:  Correct because directly from optimx hessian and for delta.diag from coef(optimx)
  #   Naming:   Has to match coef(). model + cor: original
  #                                  any cov:     leave prefixed
  #             Change the names of model+cor to display name because
  #               the reported vcov is in original scale as well

  # Set all names of vcov to these of hessian (prefixed)
  rownames(m.vcov) <- colnames(m.vcov) <- colnames(object@optimx.hessian)

  # prefixed names to replace with original names
  names.prefixed.all <- object@clv.model@names.prefixed.params.model
  names.original.all <- object@clv.model@names.original.params.model
  if(object@estimation.used.correlation){
    names.prefixed.all <- c(names.prefixed.all, object@name.prefixed.cor.param.m)
    names.original.all <- c(names.original.all, object@name.correlation.cor)
  }

  # position of these prefixed names
  pos.prefixed.names <- match(x = names.prefixed.all, table=rownames(m.vcov))
  # replace with original names
  rownames(m.vcov)[pos.prefixed.names] <- names.original.all
  colnames(m.vcov)[pos.prefixed.names] <- names.original.all

  # Of utmost importance: Ensure same sorting as coef()
  names.coef <- names(coef(object = object))
  m.vcov     <- m.vcov[names.coef, names.coef]

  # **TODO: Add argument "complete" to be comparable to coef()!

  return(m.vcov)
}

#' @importFrom stats qnorm vcov coef
#' @export
confint.clv.fitted <- function(object, parm, level = 0.95, ...){
  # This largely follows stats:::confint.lm to exhibit the same behavior

  # Get SE
  # SE <- sqrt(diag(vcov(object)))
  #
  # CI.low  <- params - qnorm(1-alpha/2) * SE
  # CI.high <- params + qnorm(1-alpha/2) * SE

  estim.coefs <- coef(object)

  # Param selection --------------------------------------------------------------------------------
  if(missing(parm))
    # Use all by default
    parm <- names(estim.coefs)
  else
    if(is.numeric(parm))
      # Make numbers to respective names
      parm <- names(estim.coefs)[parm]

    # CI calc ----------------------------------------------------------------------------------------
    req.a <- (1-level) / 2
    req.a <- c(req.a, 1 - req.a)

    zs <- qnorm(p = req.a, mean = 0, sd = 1)
    ci <- estim.coefs[parm] + sqrt(diag(vcov(object)))[parm] %o% zs

    # Return ----------------------------------------------------------------------------------------
    # from stats:::format.perc - cannot call with ::: because gives CRAN note
    names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
    res[] <- ci
    return(res)
}




#' @export
#' @importFrom utils tail
#' @importFrom stats coef
#' @include class_clv_fitted.R
print.clv.fitted <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # Short print similar to lm
  # Only print the main model coefs
  cat(x@clv.model@name.model, " Model\n")

  cat("\nCall:\n", paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")

  cat("\nCoefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L,
                quote = FALSE)
  last.row.optimx <- tail(x@optimx.estimation.output, n = 1)
  cat("KKT1:", last.row.optimx$kkt1, "\n")
  cat("KKT2:", last.row.optimx$kkt2, "\n")

  cat("\n")
  cat("Used Options:\n")
  cat("Correlation:    ", x@estimation.used.correlation, "\n")

  invisible(x)
}





#' @include all_generics.R class_clv_fitted.R
#' @importFrom methods show
#' @export
#' @rdname clv.fitted-class
setMethod(f = "show", signature = signature(object="clv.fitted"), definition = function(object){
  print(x=object)})



# helper to convert list to printable array
.list2array <- function(l, col.n="", row.n=names(l), nsmall=4){
  disp           <- array(data=NA_character_, dim=list(length(l), 1))
  disp[, 1]      <- unlist(format(l, na.encode = FALSE, digits=nsmall, nsmall=nsmall, scientific=FALSE))
  rownames(disp) <- row.n
  colnames(disp) <- col.n
  return(disp)
}


.print.list <- function(l, col.n = "", row.n = names(l), nsmall=4){
  disp.arr <- .list2array(l, col.n=col.n, row.n=row.n, nsmall=nsmall)
  print(disp.arr, na.print = "",  quote = FALSE)
}



#' @rdname summary.clv.fitted
#' @order 3
#' @importFrom stats printCoefmat
#' @export
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
#' @order 1
#' @importFrom stats coef vcov AIC BIC logLik pnorm
#' @importFrom utils tail
#' @importFrom methods is
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

  # Jeff: z.val - norm
  z.val <- (all.est.params-0)/se
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

  return(res)
}


#' @export
coef.summary.clv.fitted <- function(object, ...){
  return(object$coefficients)
}


#' @export
vcov.summary.clv.fitted <- function(object, ...){
  return(object$vcov)
}



#' @title Extract Unconditional Expectation
#' @param object A fitted clv model for which the unconditional expectation is desired.
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Extract the unconditional expectation (future transactions unconditional on beein "alive") from a fitted clv model.
#'
#' @template template_details_predictionend
#'
#' @include class_clv_fitted.R
#' @importFrom stats fitted
#' @export
fitted.clv.fitted <- function(object, prediction.end=NULL, verbose=FALSE, ...){

  dt.expectation.seq <- clv.time.expectation.periods(clv.time = object@clv.data@clv.time,
                                                     user.tp.end = prediction.end)
  object <- clv.controlflow.predict.set.prediction.params(object)

  dt.model.expectation <- clv.model.expectation(clv.model=object@clv.model, clv.fitted=object,
                                                dt.expectation.seq=dt.expectation.seq, verbose=verbose)

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.model.expectation[]
  return(dt.model.expectation)
}
