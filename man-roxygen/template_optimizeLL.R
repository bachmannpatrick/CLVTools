#' @param obj The model object to be estimated.
#' @param start.parameters Named start parameters for the optimization.
#' @param optimx.args Additional control arguments to use during optimization.
#' @param names.covariates.life Which of the set lifetime covariates should be used.
#' @param names.covariates.trans Which of the set transaction covariates should be used.
#'
#' @details
#'
#' \code{start.parameters} A named vector containing the start parameters for all model parameters.
#' \code{optimx.args} List of arguments to be directly forwarded to the opimtx function of package optimx.
#' \code{names.covariates.life} Only use the lifetime covariates given in this vector. Can only be used with covariate models.
#' \code{names.covariates.trans} Only use the transaction covariates given in this vector. Can only be used with covariate models.
#'
