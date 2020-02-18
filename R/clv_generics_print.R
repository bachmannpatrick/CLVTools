
#' @export
#' @include all_generics.R class_clv_fitted.R
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


#'@export
#' @include all_generics.R class_clv_fitted_static_cov.R
print.clv.fitted.static.cov <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # print standard parts
  NextMethod()
  # also print basic cov information
  # cat("\nLifetime covariates: ",    paste(clv.data.get.names.cov.life(x@clv.data, sep=","), "\n"))
  # cat("Transaction covariates: ", paste(clv.data.get.names.cov.trans(x@clv.data), sep=","))

  # options used
  cat("Constraints:    ", x@estimation.used.constraints,    "\n")
  cat("Regularization: ", x@estimation.used.regularization, "\n")

  invisible(x)
}


#' @include all_generics.R class_clv_fitted.R
#' @importFrom methods show
#' @export
setMethod(f = "show", signature = signature(object="clv.fitted"), definition = function(object){
  print(x=object)})

#' @include all_generics.R class_clv_fitted_static_cov.R
#' @importFrom methods show
#' @export
setMethod(f = "show", signature = signature(object="clv.fitted.static.cov"), definition = function(object){
  print(x=object)})
