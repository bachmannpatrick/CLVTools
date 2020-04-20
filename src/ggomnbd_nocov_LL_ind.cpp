#include <RcppArmadillo.h>
#include "ggomnbd_LL_ind.h"


//' @rdname ggomnbd_nocov_LL_sum
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  // c("log.r","log.alpha", "log.b", "log.s", "log.beta")
  const double r       = exp(vLogparams(0));
  const double alpha_0 = exp(vLogparams(1));
  const double b       = exp(vLogparams(2));
  const double s       = exp(vLogparams(3));
  const double beta_0  = exp(vLogparams(4));

  // n = number of elements / customers
  const double n = vX.n_elem;

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);

  // Calculate LL ---------------------------------------------------
  //    Calculate value for every customer
  //    Sum of all customers' LL value
  //
  // arma::vec ggomnbd_LL_ind(r,b,s,vAlpha_i,vBeta_i,vX,vT_x, vT_cal);

  arma::vec vLL = ggomnbd_LL_ind(r, b, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
}
