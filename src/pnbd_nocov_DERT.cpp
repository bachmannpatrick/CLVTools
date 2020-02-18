#include <RcppArmadillo.h>
#include "pnbd_DERT_ind.h"

// [[Rcpp::export]]
arma::vec pnbd_nocov_DERT( const arma::vec& vEstimated_params,
                           const double continuous_discount_factor,
                           const arma::vec& vX,
                           const arma::vec& vT_x,
                           const arma::vec& vT_cal){


  // n = number of elements / customers
  const double n = vX.n_elem;

  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);


  // Build alpha and beta
  //    No covariates: Same alphas, betas for every customer
  // ----------------------------------------------------------------
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);

  // Calculate DERT
  // pnbd_DERT_ind( r,s,
  //                vAlpha_i,vBeta_i,
  //                vX,vT_x,vT_cal,
  //                continuous_discount_factor)
  // ----------------------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}


