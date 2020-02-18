#include <RcppArmadillo.h>
#include "pnbd_LL_ind.h"


//' @importFrom Rcpp evalCpp
//' @rdname pnbd_nocov_LL_sum
// [[Rcpp::export]]
arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  // vLogparams have to be single vector because used by optimizer

  //only first
  const double r       = exp(vLogparams(0));
  const double alpha_0 = exp(vLogparams(1));
  const double s       = exp(vLogparams(2));
  const double beta_0  = exp(vLogparams(3));

  // n = number of elements / customers
  const double n = vX.n_elem;



  // Build alpha and beta
  //    No covariates: Same alphas, betas for every customer
  // ----------------------------------------------------------------
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);


  // Calculate LL
  //    Calculate value for every customer
  //    Sum of all customers' LL value
  //
  //    arma::vec pnbd_LL_ind(r,s, vAlpha_i,vBeta_i, vX, vT_x, vT_cal)
  // ----------------------------------------------------------------
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
}
