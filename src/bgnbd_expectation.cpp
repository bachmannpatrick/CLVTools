#include <RcppArmadillo.h>
#include <math.h>
#include "bgnbd_expectation.hpp"
#include "clv_vectorized.hpp"

//' @name bgnbd_expectation
//' @title BG/NBD: Unconditional Expectation
//'
//' @description Computes the expected number of repeat transactions in the interval (0, vT_i]
//' for a randomly selected customer, where 0 is defined as the point when the customer came alive.
//'
//' @template template_params_bgnbd
//' @param vT_i Number of periods since the customer came alive
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
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

//' @rdname bgnbd_expectation
// [[Rcpp::export]]
arma::vec bgnbd_nocov_expectation(const double r,
                                 const double alpha,
                                 const double a,
                                 const double b,
                                 const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const double n = vT_i.n_elem;
  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha);
  vA_i.fill(a);
  vB_i.fill(b);

  return bgnbd_expectation(r,
                          vAlpha_i,
                          vA_i,
                          vB_i,
                          vT_i);
}

//' @rdname bgnbd_expectation
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_expectation(const double r,
                                     const double alpha,
                                     const double a,
                                     const double b,
                                     const arma::vec& vT_i,
                                     const arma::vec& vCovParams_trans,
                                     const arma::vec& vCovParams_life,
                                     const arma::mat& mCov_life,
                                     const arma::mat& mCov_trans){

  const arma::vec vAlpha_i = alpha * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vA_i  = a * arma::exp(mCov_life * vCovParams_life);
  const arma::vec vB_i  = b * arma::exp(mCov_life * vCovParams_life);

  return(bgnbd_expectation(r,
                          vAlpha_i,
                          vA_i,
                          vB_i,
                          vT_i));
}
