#' @details
#' \code{r} r: TODO
#' \code{alpha} alpha: TODO
#' \code{a} a: TODO
#' \code{b} b: TODO
#'
#' \code{vLogparams} is vector with the BG/NBD model parameters at log scale,
#' followed by the parameters for the lifetime covariate at original scale and then
#' followed by the parameters for the transaction covariate at original scale
#'
#' \code{mCov_trans} is a matrix containing the covariates data of
#' the time-invariant covariates that affect the transaction process.
#' Each column represents a different covariate. For every column a gamma parameter
#' needs to added to \code{vCovParams_trans} at the respective position.
#'
#' \code{mCov_life} is a matrix containing the covariates data of
#' the time-invariant covariates that affect the lifetime process.
#' Each column represents a different covariate. For every column a gamma parameter
#' needs to added to \code{vCovParams_life} at the respective position.
