#include <RcppArmadillo.h>
#include "pnbd_DERT_ind.h"

// [[Rcpp::export]]
arma::vec pnbd_staticcov_DERT(const arma::vec& vEstimated_params,
                              const double continuous_discount_factor,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans,
                              const arma::vec& vCovParams_life,
                              const arma::vec& vCovParams_trans){

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer

  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));


  // Calculate DERT --------------------------------------------------
  // pnbd_DERT_ind( r,s,
  //                vAlpha_i,vBeta_i,
  //                vX,vT_x,vT_cal,
  //                continuous_discount_factor)

    return pnbd_DERT_ind(r, s,
                         vAlpha_i, vBeta_i,
                         vX, vT_x, vT_cal,
                         continuous_discount_factor);
}


