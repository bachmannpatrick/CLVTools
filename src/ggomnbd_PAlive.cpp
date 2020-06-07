#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL.h"

arma::vec ggomnbd_PAlive(const double r,
                         const double b,
                         const double s,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal,
                         const arma::vec& vAlpha_i,
                         const arma::vec& vBeta_i){

  const unsigned int n = vX.n_elem;

  // Individual LL values -------------------------------------------------
  arma::vec vLL = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  arma::vec vP1(n), vP2(n), vP3(n);

  vP1 = arma::lgamma(r + vX) - lgamma(r);
  vP2 = r * arma::log(vAlpha_i/(vAlpha_i + vT_cal)) + vX % arma::log(1/(vAlpha_i + vT_cal)) + s * arma::log(vBeta_i/(vBeta_i - 1 + exp(b * vT_cal)));
  vP3 = vLL;

  return arma::round(10000*arma::exp(vP1 + vP2 - vP3))/10000;

}


//' @title GGompertz/NBD: PAlive with Static Covariates
//'
//' @description
//' Calculates the probability of a customer being alive (PAlive) at the
//' end of the calibration period.
//'
//' @template template_params_rcpp_ggomnbd_estimatedparams
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @details
//' \code{vEstimated_params} is vector with the five estimated model parameters of the
//' GGompertz/NBD model parameters at original scale (\code{r, alpha_0, b, s, beta_0}),
//' followed by the parameters for the lifetime covariate at original scale (\code{mCov_life})
//' and then followed by the parameters for the transaction covariate at original scale
//' \code{mCov_trans}.\cr
//' \code{r, alpha_0, b, s, beta_0} are the log()-ed model parameters
//' used for estimation, in this order.\cr
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
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_trans} at the respective position.
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_life} at the respective position.
//'
//' @return
//' Returns a vector containing the PAlive for each customer.
//'
//' @template template_references_ggomnbd
//'
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_PAlive(const double r,
                                   const double alpha_0,
                                   const double b,
                                   const double s,
                                   const double beta_0,
                                   const arma::vec& vX,
                                   const arma::vec& vT_x,
                                   const arma::vec& vT_cal,
                                   const arma::vec& vCovParams_trans,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life,
                                   const arma::mat& mCov_trans){

  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

  // Calculate PAlive ------------------------------------------------
  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}


//' GGompertz/NBD: PAlive without Covariates
//'
//' Calculates the probability of a customer being alive (PAlive) at the
//' end of the calibration period in the GGompertz/NBD model
//'
//' @template template_params_rcpp_ggomnbd_estimatedparams
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r, alpha_0, b, s, beta_0} are the log()-ed model parameters used for
//' estimation, in this order.\cr
//' \code{s}: shape parameter of the Gamma distribution for the lifetime process. The smaller \code{s}, the stronger the heterogeneity of customer lifetimes.\cr
//' \code{beta}: scale parameter for the Gamma distribution for the lifetime process.\cr
//' \code{b:} scale parameter of the Gompertz distribution (constant across customers).\cr
//' \code{r:} shape parameter of the Gamma distribution of the purchase process. The smaller \code{r}, the stronger the heterogeneity of the pruchase process.\cr
//' \code{alpha}: scale parameter of the Gamma distribution of the purchase process.\cr
//'
//' @return
//' Returns a vector containing the PAlive for each customer.
//'
//' @template template_references_ggomnbd
//'
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_PAlive(const double r,
                               const double alpha_0,
                               const double b,
                               const double s,
                               const double beta_0,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}
