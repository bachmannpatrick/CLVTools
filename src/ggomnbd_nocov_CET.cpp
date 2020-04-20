#include <RcppArmadillo.h>
#include "ggomnbd_PAlive.h"
#include "ggomnbd_CET.h"

//' GGompertz/NBD: Conditional Expected Transactions without Covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customers past transaction behavior and the GGompertz/NBD model parameters.
//'
//' @template template_params_rcpp_ggomnbd_estimatedparams
//' @template template_params_rcppxtxtcal
//' @param dPrediction_period time prediction time frame
//'
//' @details
//'
//' \code{vEstimated_params}
//' s: shape parameter of the Gamma distribution for the lifetime process.\cr
//' The smaller s, the stronger the heterogeneity of customer lifetimes. \cr
//' beta: scale parameter for the Gamma distribution for the lifetime process. \cr
//' b: scale parameter of the Gompertz distribution (constant across customers). \cr
//' r: shape parameter of the Gamma distribution of the purchase process.
//' The smaller r, the stronger the heterogeneity of the pruchase process.\cr
//' alpha: scale parameter of the Gamma distribution of the purchase process.
//'
//' \code{dPrediction_period} is the duration over which the prediction is made.
//'  Usually this is the duration of the holdout period.
//'
//'@return
//' Returns a vector with the conditional expected transactions for the existing
//' customers in the GGompertz/NBD model.
//'
//' @template template_rcpp_ggomnbd_reference
//'
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_CET(const arma::vec& vEstimated_params,
                         const double dPrediction_period,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  // c("log.r","log.alpha", "log.b", "log.s", "log.beta")
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
  // ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
  const arma::vec vPAlive = ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);


  // Calculate CET ----------------------------------------------------------------
  // ggomnbd_CET(r,b,s,dPrediction_period,vX,vT_x,vT_cal,vAlpha_i, vBeta_i,vPAlive);
  return(ggomnbd_CET(r,b,s,dPrediction_period,vX,vT_x,vT_cal,vAlpha_i, vBeta_i,vPAlive));
}
