#include <RcppArmadillo.h>
#include "ggomnbd_staticcov_LL_ind.h"

//' @name ggomnbd_staticcov_LL_sum
//' @title GGompertz/NBD: LogLikelihood with static covariates
//'
//' @description
//'
//' The function \code{ggomnbd_staticcov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{ggomnbd_staticcov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vParams vector with the parameters for the GGompertz/NBD model and for the static
//' covariates. See Details.
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details
//'
//' \code{vParams} is vector with the GGompertz/NBD model parameters at log scale
//' (\code{r, alpha_0, b, s, beta_0}), followed by the parameters for the lifetime
//' covariate at original scale (\code{mCov_life}) and then followed by the parameters
//' for the transaction covariate at original scale \code{mCov_trans}.
//' \code{r, alpha_0, b, s, beta_0} are the log()-ed model parameters used for
//' estimation, in this order.\cr
//' \code{s}: shape parameter of the Gamma distribution for the lifetime process.
//' The smaller \code{s}, the stronger the heterogeneity of customer lifetimes.\cr
//' \code{beta}: scale parameter for the Gamma distribution for the lifetime process.\cr
//' \code{b:} scale parameter of the Gompertz distribution (constant across customers).\cr
//' \code{r:} shape parameter of the Gamma distribution of the purchase process.
//' The smaller \code{r}, the stronger the heterogeneity of the pruchase process.\cr
//' \code{alpha}: scale parameter of the Gamma distribution of the purchase process.\cr
//' \code{mCov_life}: parameters for the covariates affecting the lifetime process.\cr
//' \code{mCov_trans}: parameters for the covariates affecting the transaction process.

//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//'@return
//'  Returns the respective LogLikelihood value for the GGompertz/NBD model with static covariates.
//'
//'@template template_rcpp_ggomnbd_reference
//'
// [[Rcpp::export]]
double ggomnbd_staticcov_LL_sum(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){
  // vParams has to be single vector because used by optimizer

  // arma::vec ggomnbd_staticcov_LL_ind(const arma::vec& vParams,
  //                                 const arma::vec& vX,
  //                                 const arma::vec& vT_x,
  //                                 const arma::vec& vT_cal,
  //                                 const arma::mat& mCov_life,
  //                                 const arma::mat& mCov_trans);
  const arma::vec vLL = ggomnbd_staticcov_LL_ind(vParams,vX,vT_x,vT_cal,mCov_life,mCov_trans);

  return(-arma::sum(vLL));
}
