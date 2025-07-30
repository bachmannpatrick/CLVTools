#' @param start.params.life Named start parameters containing the optimization start parameters for all lifetime covariates.
#' @param start.params.trans Named start parameters containing the optimization start parameters for all transaction covariates.
#' @param names.cov.life Which of the set Lifetime covariates should be used. Missing parameter indicates all covariates shall be used.
#' @param names.cov.trans Which of the set Transaction covariates should be used. Missing parameter indicates all covariates shall be used.
#' @param names.cov.constr Which covariates should be forced to use the same parameters for the lifetime and transaction process. The covariates need to be present as both, lifetime and transaction covariates.
#' @param start.params.constr Named start parameters containing the optimization start parameters for the constraint covariates.
#' @param reg.weights Named lambda parameters used for the L2 regularization of the lifetime and the transaction covariate parameters. Lambdas have to be >= 0.
