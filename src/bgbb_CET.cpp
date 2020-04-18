#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "bgbb_LL_ind.h"

// Code adapted from BTYD BGBB implementation https://github.com/cran/BTYD/blob/master/R/bgbb.R
arma::vec bgbb_CET(const double alpha,
                   const double beta,
                   const double gamma,
                   const double delta,
                   const double nPeriods,
                   const arma::vec& vX,
                   const arma::vec& vT_x,
                   const arma::vec& vT_cal,
                   const arma::vec& vN_cal,
                   const arma::vec& vN_star){

  const unsigned int n = vX.n_elem;

  arma::vec vPart1(n), vPart2(n), vPart3(n), vPart4(n), vPart5(n), vPart6(n), vPart7(n), vPart8(n);

  vPart1 = 1 / arma::exp(bgbb_LL_ind(alpha, beta, gamma, delta, vX, vT_x, vT_cal, vN_cal));
  vPart2 = arma::exp(clv::lbeta((alpha + vX + 1), (beta + vN_cal - vX))) - R::lbeta(alpha, beta); // TODO: find a vectorized lbeta
  vPart3 = delta / (gamma - 1);
  vPart4 = std::exp(std::lgamma(gamma + delta) - std::lgamma(1 + delta));
  vPart5 = arma::exp(arma::lgamma(1 + delta + vN_cal) - arma::lgamma(gamma + delta + vN_cal));
  vPart6 = arma::exp(arma::lgamma(1 + delta + vN_cal + vN_star) - arma::lgamma(gamma + delta + vN_cal + vN_star));

  return vPart1 % vPart2 % vPart3 % vPart4 % (vPart5 - vPart6);
}

//' @title BG/BB: Conditional Expected Transactions without covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the BG/NBD model parameters.
//'
//' @param alpha TODO: describe
//' @param beta TODO: describe
//' @param gamma TODO: describe
//' @param delta TODO: describe
//' @param nPeriods time prediction time frame
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{alpha} \code{beta} \code{gamma} \code{delta} estimated parameters in original scale
//' for the BG/BB model, namely (alpha, beta, gamma, delta).
//'
//'
//'@return
//' Returns a vector containing the conditional expected transactions for the existing
//' customers in the BG/BB model.
//'
//' @name bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_nocov_CET(const double alpha,
                          const double beta,
                          const double gamma,
                          const double delta,
                          const double nPeriods,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal,
                          const arma::vec& vN_cal,
                          const arma::vec& vN_star){
  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alpha, beta, gamma, delta for every customer

  return bgbb_CET(alpha, beta, gamma, delta, nPeriods, vX, vT_x, vT_cal, vN_cal, vN_star);
}
