#include <RcppArmadillo.h>
#include "ggomnbd_PAlive.h"

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
//' @template template_rcpp_ggomnbd_reference
//'
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_PAlive(const arma::vec& vEstimated_params,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double b       = vEstimated_params(2);
  const double s       = vEstimated_params(3);
  const double beta_0  = vEstimated_params(4);
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  // arma::vec ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
  return ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}
