#' @export
#' @include class_clv_fitted_transactions_staticcov.R
print.clv.fitted.transactions.static.cov <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # print standard parts
  NextMethod()

  # options used
  cat("Constraints:    ", x@estimation.used.constraints,    "\n")
  cat("Regularization: ", x@estimation.used.regularization, "\n")

  invisible(x)
}

#' @rdname summary.clv.fitted
#' @order 2
#' @include class_clv_data_staticcovariates.R
#' @export
summary.clv.fitted.transactions.static.cov <- function(object, ...){

  # Get basic structure from nocov
  res <- NextMethod()
  class(res) <- c("summary.clv.fitted.transactions.static.cov", class(res))

  # Add further optimization options -----------------------------------------------
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


#' @importFrom stats coef na.omit setNames
#' @importFrom optimx coef<-
#' @importFrom utils tail
#' @include class_clv_fitted_transactions_staticcov.R
#' @export
coef.clv.fitted.transactions.static.cov <- function(object, ...){

  # Covariates params -----------------------------------------------------------------------------------

  # Is estimated check and backtransformed coefs from model only
  original.scale.coef.model <- NextMethod()

  last.row.optimx.coef <- tail(coef(object@optimx.estimation.output),n=1)

  # Covariate params
  # Do NOT duplicate constrained params because output of coef() has to match vcov!
  if(object@estimation.used.constraints){
    names.prefixed.covs <- union(object@names.prefixed.params.constr,
                                 union(object@names.prefixed.params.free.life,
                                       object@names.prefixed.params.free.trans))
  }else{
    names.prefixed.covs <- union(object@names.prefixed.params.free.life,
                                 object@names.prefixed.params.free.trans)
  }

  # if for whatever reason there is still a NA leftover from previous design of constrained param names
  names.prefixed.covs <- na.omit(names.prefixed.covs)

  # let model backtransform from optimizer to original scale
  prefixed.params.cov       <- last.row.optimx.coef[1, names.prefixed.covs, drop=TRUE]
  prefixed.params.cov       <- setNames(prefixed.params.cov, names.prefixed.covs) # Names are lost if only 1 cov (single constraint)
  original.scale.params.cov <- clv.model.backtransform.estimated.params.cov(clv.model = object@clv.model,
                                                                            prefixed.params.cov = prefixed.params.cov)

  # There are no display/original scale names to set for covariates. They need the prefix to stay
  # distinguishable between processes
  original.scale.params.cov <- setNames(original.scale.params.cov[names.prefixed.covs],
                                        names.prefixed.covs)



  # Put together in correct order ------------------------------------------------------------------------------------------
  #   relevant order is as in optimx so that coef output is in the same order as vcov() / hessian
  #   it should also be possible through input structure to optimx (ie c(model, cov)) but guarantee
  #     by explicitely setting the same order as in optimx. This is greatly complicated by differing/prefixed names
  #
  #   covariates params keep prefix to stay distinguishable

  # mapping for original to prefixed
  #   content: original names for model and correlation, prefixed names for cov params
  #   names:   prefixed names

  # Content + transformed names for model and cor
  #   append correlation param, if exists and should be returned. If not included here, it is removed from nocov param vec
  names.original.named.prefixed.all <- names(original.scale.coef.model)

  if(clv.model.estimation.used.correlation(clv.model = object@clv.model)){
    names(names.original.named.prefixed.all) <- c(object@clv.model@names.prefixed.params.model,
                                                  object@clv.model@name.prefixed.cor.param.m)
  }else{
    names(names.original.named.prefixed.all) <- object@clv.model@names.prefixed.params.model
  }

  # Content + prefixed names for covs
  names.original.named.prefixed.all <- c(names.original.named.prefixed.all,
                                         setNames(names(original.scale.params.cov),
                                                  names(original.scale.params.cov)))

  # bring into same order as in optimx
  #   read definitive order from optimx through prefixed names
  names.optimx.coefs <- colnames(last.row.optimx.coef)

  # bring original scale names into order of optimx (prefixed) names
  names.original.named.prefixed.all <- names.original.named.prefixed.all[names.optimx.coefs]

  # put together return values in original scale
  params.all <- c(original.scale.coef.model, original.scale.params.cov)

  # return in correct order
  return(params.all[names.original.named.prefixed.all])
}


#' @include all_generics.R class_clv_fitted_transactions_staticcov.R
#' @importFrom methods show
#' @export
#' @rdname clv.fitted.transactions.static.cov-class
setMethod(f = "show", signature = signature(object="clv.fitted.transactions.static.cov"), definition = function(object){
  print(x=object)})
