#include <RcppArmadillo.h>
#include "ggomnbd_PAlive.h"
#include "ggomnbd_CET.h"

// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_CET(const arma::vec& vEstimated_params,
                             const double dPrediction_period,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::vec& vCovParams_trans,
                             const arma::vec& vCovParams_life,
                             const arma::mat& mCov_life,
                             const arma::mat& mCov_trans){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  // c("log.r","log.alpha", "log.b", "log.s", "log.beta")
  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double b       = vEstimated_params(2);
  const double s       = vEstimated_params(3);
  const double beta_0  = vEstimated_params(4);


  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

  // Calculate PAlive -------------------------------------------------------------
  // ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
  const arma::vec vPAlive = ggomnbd_PAlive(r,s,b,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);

  // Calculate CET -----------------------------------------------------------------
  // ggomnbd_CET(r,b,s,dPrediction_period,vX,vT_x,vT_cal,vAlpha_i, vBeta_i,vPAlive);
  return(ggomnbd_CET(r,b,s,dPrediction_period,vX,vT_x,vT_cal,vAlpha_i, vBeta_i, vPAlive));
}
