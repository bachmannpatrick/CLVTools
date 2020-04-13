#include <RcppArmadillo.h>
#include <math.h>
#include "bgbb_LL_ind.h"
#include "clv_vectorized.h"

// Code adapted from BTYD BGBB implementation https://github.com/cran/BTYD/blob/master/R/bgbb.R
arma::vec bgbb_DERT_ind(const double alpha,
                        const double beta,
                        const double gamma,
                        const double delta,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const arma::vec& vN_cal,
                        const double continuous_discount_factor){

  const unsigned int n = vX.n_elem;

  arma::vec vPart1(n), vPart2(n), vPart3(n), vPart4(n);

  vPart1 = arma::exp(lbeta(alpha + vX + 1, beta + vN_cal - vX) - R::lbeta(alpha, beta));
  vPart2 = arma::exp(lbeta(gamma, delta + vN_cal + 1) - R::lbeta(gamma, delta))/ (1 + continuous_discount_factor);
  vPart3 = R::Re(clv::vec_hyp2F1(1, delta + vN_cal + 1, gamma + delta + vN_cal + 1, 1 / (1 + continuous_discount_factor)));
  vPart4 = arma::exp(bgbb_LL_ind(alpha, beta, gamma, delta, vX, vT_x, vT_cal, vN_cal));

  return vPart1 % vPart2 % (vPart3 / vPart4);
}




// [[Rcpp::export]]
arma::vec bgbb_nocov_DERT(const arma::vec& vEstimated_params,
                          const double continuous_discount_factor,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal,
                          const arma::vec& vN_cal){

  const double alpha  = vEstimated_params(0);
  const double beta   = vEstimated_params(1);
  const double gamma  = vEstimated_params(2);
  const double delta  = vEstimated_params(3);


  // Calculate DERT -------------------------------------------------
  return bgbb_DERT_ind(alpha, beta,
                       gamma, delta,
                       vX, vT_x, vT_cal, vN_cal,
                       continuous_discount_factor);
}

