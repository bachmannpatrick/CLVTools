#include <RcppArmadillo.h>
#include <math.h>
#include "bgbb_LL_ind.h"
#include "clv_vectorized.h"

// Code adapted from BTYD BGBB implementation https://github.com/cran/BTYD/blob/master/R/bgbb.R
arma::vec bgbb_PAlive(const double alpha,
                      const double beta,
                      const double gamma,
                      const double delta,
                      const arma::vec& vX,
                      const arma::vec& vT_x,
                      const arma::vec& vT_cal,
                      const arma::vec& vN_cal){
  const unsigned int n = vX.n_elem;

  arma::vec vPart1(n), vPart2(n);

  arma::vec vAlphaVx = (alpha + vX);
  arma::vec vBetaVnCalVx = (beta + vN_cal + vX);
  arma::vec vGamma(1);
  vGamma.fill(gamma);
  arma::vec vDeltaVnCal1 = (delta + vN_cal + 1);

  vPart1 = arma::exp(clv::lbeta(vAlphaVx, vBetaVnCalVx) - R::lbeta(alpha, beta) + clv::lbeta(vGamma, vDeltaVnCal1) - R::lbeta(gamma, delta));
  vPart2 = 1 / arma::exp(bgbb_LL_ind(alpha, beta, gamma, delta, vX, vT_x, vT_cal, vN_cal));

  return vPart1 % vPart2;
}

//' @title BG/BB: PAlive without covariates
//'
//' @description
//' BG/BB without Covariates: Calculates the probability of a customer being alive
//' at the end of the calibration period.
//'
//' @param alpha TODO: describe
//' @param beta TODO: describe
//' @param gamma TODO: describe
//' @param delta TODO: describe
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{alpha} \code{beta} \code{gamma} \code{delta} estimated parameters in original scale
//' for the BG/BB model, namely (alpha, beta, gamma, delta).
//'
//'
//'@return
//'Returns a vector with the PAlive for each customer.
//'
//' @name bgbb_nocov_PAlive
// [[Rcpp::export]]
arma::vec bgbb_nocov_PAlive(const double alpha,
                             const double beta,
                             const double gamma,
                             const double delta,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::vec& vN_cal){

  return bgbb_PAlive(alpha,
                      beta,
                      gamma,
                      delta,
                      vX,
                      vT_x,
                      vT_cal,
                      vN_cal);
}
