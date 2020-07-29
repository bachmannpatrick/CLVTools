#include <RcppArmadillo.h>
#include <math.h>
#include "bgnbd_expectation.h"
#include "clv_vectorized.h"

arma::vec bgnbd_expectation(const double r,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vA_i,
                            const arma::vec& vB_i,
                            const arma::vec& vT_i){
  arma::vec vR(vAlpha_i.n_elem);
  vR.fill(r);

  arma::vec term1 = (vA_i + vB_i - 1)/(vA_i - 1);
  arma::vec term2 = arma::pow((vAlpha_i / (vAlpha_i + vT_i)),r);
  arma::vec term3 = clv::vec_hyp2F1(vR, vB_i, vA_i+vB_i-1, vT_i/(vAlpha_i+vT_i));

  return term1 % (1 - term2 % term3);
}

// [[Rcpp::export]]
arma::vec bgnbd_nocov_expectation(const double r,
                                 const double alpha_0,
                                 const double a_0,
                                 const double b_0,
                                 const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const double n = vT_i.n_elem;
  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha_0);
  vA_i.fill(a_0);
  vB_i.fill(b_0);

  return bgnbd_expectation(r,
                          vAlpha_i,
                          vA_i,
                          vB_i,
                          vT_i);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_expectation(const double r,
                                     const double alpha_0,
                                     const double a_0,
                                     const double b_0,
                                     const arma::vec& vT_i,
                                     const arma::vec& vCovParams_trans,
                                     const arma::vec& vCovParams_life,
                                     const arma::mat& mCov_life,
                                     const arma::mat& mCov_trans){

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vA_i  = a_0 * arma::exp(mCov_life * vCovParams_life);
  const arma::vec vB_i  = b_0 * arma::exp(mCov_life * vCovParams_life);

  return(bgnbd_expectation(r,
                          vAlpha_i,
                          vA_i,
                          vB_i,
                          vT_i));
}
