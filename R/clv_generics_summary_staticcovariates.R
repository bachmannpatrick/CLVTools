#' @rdname summary.clv.fitted
#' @include class_clv_data_static_covariates.R
#' @export
#' @keywords internal
summary.clv.fitted.static.cov <- function(object, ...){

  # Get basic structure from nocov
  res <- NextMethod()
  class(res) <- c("summary.clv.fitted.static.cov", class(res))

  # Add static covariate stuff ontop
  # res$names.cov.data.trans <- clv.data.get.names.cov.trans(object@clv.data)
  # res$names.cov.data.life  <- clv.data.get.names.cov.life(object@clv.data)

  # Further optimization options ---------------------------------------------------
  #   Regularization
  #     lambdas
  #   Constraint covs
  #     which
  res$additional.options <- c(res$additional.options,
                              "Regularization"=object@estimation.used.regularization)
  if(object@estimation.used.regularization){
    res$additional.options <- c(res$additional.options,
                                "   lambda.life"  = object@reg.lambda.life,
                                "   lambda.trans" = object@reg.lambda.trans)
  }

  res$additional.options <- c(res$additional.options,
                              "Constraint covs" = object@estimation.used.constraints)
  if(object@estimation.used.constraints){
    res$additional.options <- c(res$additional.options,
                                "   Constraint params" = paste(object@names.original.params.constr, collapse = ", "))
  }

  return(res)
}
