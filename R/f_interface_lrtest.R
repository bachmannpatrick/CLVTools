
#' @export
#' @importFrom methods is slot
#' @importFrom stats nobs logLik pchisq
lrtest.clv.fitted <- function(object, ..., name = NULL){
  # This very closely follows lmtest::lrtest.default in package version 0.9-40
  # See lmtest::lrtest() on the CRAN github repo https://github.com/cran/lmtest/blob/master/R/lrtest.R
  # Specifically@0.9-40: https://github.com/cran/lmtest/blob/1a5c1de8629e650226091ee3c0be33f0b8a42b33/R/lrtest.R

  objects <- list(object, ...)
  nmodels <- length(objects)

  # Verify there are more than 1 object
  if(nmodels <= 1){
    check_err_msg("At least 2 fitted models are required!")
  }
  # Verify all objects are clv.fitted
  if(!all(sapply(objects, is, "clv.fitted"))){
    check_err_msg("All models must be CLV models (objects of class `clv.fitted`)")
  }
  # Verify all models are nested (clv.model inherits from the same)
  # all are pnbd
  objects.clv.models <- lapply(objects, slot, name="clv.model")

  if(!any(
    all(sapply(objects.clv.models, is, "clv.model.pnbd.no.cov")), # all pnbd
    all(sapply(objects.clv.models, is, "clv.model.bgnbd.no.cov")), # all bgnbd
    all(sapply(objects.clv.models, is, "clv.model.ggomnbd.no.cov")) # all ggomnbd
  )){
    check_err_msg("Only nested models may be compared with each other! (i.e.: Models may only be compared across their no / static / dyn cov specification)")
  }

  # Verify sample size
  if(length(unique(sapply(objects, nobs))) != 1){
    check_err_msg("All models have to be fitted on the same number of customers (same transction data but not same covariate data)!")
  }

  # Verify names are given for each model
  if(!is.null(name) & (length(name) != length(objects))){
    check_err_msg("Names must be provided exactly as many as models!")
  }


  # Copy-pasta from `lmtest::lrtest`

  ## setup ANOVA matrix
  rval <- matrix(rep(NA, 5 * nmodels), ncol = 5)
  colnames(rval) <- c("#Df", "LogLik", "Df", "Chisq", "Pr(>Chisq)")
  rownames(rval) <- 1:nmodels

  logL <- lapply(objects, logLik)
  rval[,1] <- as.numeric(sapply(logL, function(x) attr(x, "df")))
  rval[,2] <- sapply(logL, as.numeric)
  rval[2:nmodels, 3] <- rval[2:nmodels, 1] - rval[1:(nmodels-1), 1]
  rval[2:nmodels, 4] <- 2 * abs(rval[2:nmodels, 2] - rval[1:(nmodels-1), 2])
  rval[,5] <- pchisq(rval[,4], round(abs(rval[,3])), lower.tail = FALSE)

  if(is.null(name)){
    # Read actual model names and do not deparse call to method
    variables <- sapply(objects.clv.models, slot, name="name.model")
  }else{
    variables <- name
  }
  title <- "Likelihood ratio test\n"
  topnote <- paste("Model ", format(1:nmodels),": ", variables, sep="", collapse="\n")

  structure(as.data.frame(rval), heading = c(title, topnote),
            class = c("anova", "data.frame"))
}

# ?Methods_for_Nongenerics: Recommends to define both methods: The S3 method and also
# supply the identical function as the definition of the S4 method.
# Re-define same as S3 generic `lmtest::lrtest <- function(object, ...){ UseMethod("lrtest") }`
#' Likelihood Ratio Test of Nested Models
#'
#' @description
#' \code{lrtest} carrys out likelihood ratio tests to compare nested CLV models.
#'
#' The method consecutively compares the first model given in \code{object} with all the
#' other models passed in \code{...}. An asymptotic likelihood ratio test is carried out:
#' Twice the difference in log-likelihoods is compared with a Chi-squared distribution.
#'
#' @param object An fitted model object inheriting from \code{clv.fitted}.
#' @param ... Other models objects fitted on the same transaction data
#'
#' @returns A \code{data.frame} of class "anova" which contains the log-likelihood,
#' degrees of freedom, the difference in degrees of freedom, likelihood ratio
#' Chi-squared statistic and corresponding p-value.
#'
#'
#' @exportMethod lrtest
setGeneric(name = "lrtest", def=function(object, ...)
  standardGeneric("lrtest"))


#' @rdname lrtest
#'
#' @param name A character vector of names to use for the models in the resulting output.
#' If given, a name has to be provided for \code{object} and each model in \code{...}.
#' If not given, the default model names are used.
#'
#' @include all_generics.R
#' @exportMethod lrtest
setMethod("lrtest", signature(object="clv.fitted"), definition = lrtest.clv.fitted)
