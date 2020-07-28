#' Fitted Transaction Model with Static covariates
#'
#' Extends the class \code{clv.fitted.transactions} with slots to accommodate the various additional
#' optimization options that can be used for covariates models.
#' Also used to perform steps during the estimation process that are specific to static covariates models.
#'
#' @slot estimation.used.constraints Single boolean whether the estimation constraint any covariate parameters to be the same for both processes.
#' @slot names.original.params.constr Character vector with the original names of the constraint covariate parameters. Length zero if none are constraint.
#' @slot names.original.params.free.life Character vector with the original names of the not constraint lifetime covariate parameters. Length zero if none are free.
#' @slot names.original.params.free.trans Character vector with the original names of the not constraint transaction covariate parameters. Length zero if none are free.
#' @slot names.prefixed.params.constr Character vector with the prefixed names of the constraint covariate parameters during optimization. Length zero if none are constraint.
#' @slot names.prefixed.params.free.life Character vector with the prefixed names of the not constraint lifetime covariate parameters during optimization. Length zero if none are free.
#' @slot names.prefixed.params.free.trans Character vector with the prefixed names of the not constraint transaction covariate parameters during optimization. Length zero if none are free.
#' @slot names.prefixed.params.after.constr.life Character vector containing the names of all constraint and free lifetime covariates parameters with lifetime prefixes only. Needed after reduplicating the constraint parameters.
#' @slot names.prefixed.params.after.constr.trans Character vector containing the names of all constraint and free transaction covariates parameters with transaction prefixes only. Needed after reduplicating the constraint parameters.
#' @slot estimation.used.regularization Single boolean whether the estimation used regularization.
#' @slot reg.lambda.life Single numeric with the lambda used for regularizing the lifetime covariate parameters. Length zero if regularization is not used.
#' @slot reg.lambda.trans Single numeric with the lambda used for regularizing the transaction covariate parameters. Length zero if regularization is not used.
#' @slot prediction.params.life Numeric vector of the lifetime covariate parameters, set and used solely when predicting. Named after lifetime covariates and derived from \code{coef()}.
#' @slot prediction.params.trans Numeric vector of the transaction covariate parameters, set and used solely when predicting. Named after transaction covariates and derived from \code{coef()}.
#'
#' @seealso \linkS4class{clv.fitted}, \linkS4class{clv.fitted.transactions}, \linkS4class{clv.fitted.transactions.dynamic.cov}
#'
#' @importFrom methods setClass
#' @keywords internal
#' @include class_clv_fitted_transactions.R
setClass(Class = "clv.fitted.transactions.static.cov", contains = "clv.fitted.transactions",
         slots = c(
                   estimation.used.constraints = "logical",

                   # Needs original names per type to map back prefixed names
                   #  to original names (creating prediction.params)
                   names.original.params.constr      = "character",
                   names.original.params.free.life   = "character",
                   names.original.params.free.trans  = "character",

                   names.prefixed.params.constr     = "character",
                   names.prefixed.params.free.life  = "character",
                   names.prefixed.params.free.trans = "character",

                   # cannot use prefixed in interlayers after constraint interlayer (before LL & in reg),
                   #   it needs all life and trans params, not split in free/constraint
                   names.prefixed.params.after.constr.life  = "character",
                   names.prefixed.params.after.constr.trans = "character",

                   # Regularization parameters
                   estimation.used.regularization = "logical",
                   reg.lambda.life  = "numeric",
                   reg.lambda.trans = "numeric",

                   # Params from constraint and unconstraint coefs
                   prediction.params.life  = "numeric",
                   prediction.params.trans = "numeric"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           estimation.used.constraints      = logical(0),

           names.original.params.free.life  = character(0),
           names.original.params.free.trans = character(0),
           names.original.params.constr     = character(0),

           names.prefixed.params.free.life   = character(0),
           names.prefixed.params.free.trans  = character(0),
           names.prefixed.params.constr = character(0),

           names.prefixed.params.after.constr.trans = character(0),
           names.prefixed.params.after.constr.life  = character(0),

           estimation.used.regularization = logical(0),
           reg.lambda.life  = numeric(0),
           reg.lambda.trans = numeric(0),

           prediction.params.life  = numeric(0),
           prediction.params.trans = numeric(0)))



#' @importFrom methods new
clv.fitted.transactions.static.cov <- function(cl, clv.model, clv.data){
  return(new("clv.fitted.transactions.static.cov",
             clv.fitted.transactions(cl = cl, clv.model = clv.model, clv.data = clv.data)))
}



