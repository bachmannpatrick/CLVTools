#include <RcppArmadillo.h>
#include <math.h>
#include "bgnbd_expectation.h"
#include "clv_vectorized.h"
#include "bgnbd_LL.h"

//' @name bgnbd_expectation
//' @title BG/NBD: Unconditional Expectation
//'
//' @template template_expectation_description
//'
//' @template template_params_bgnbd
//' @template template_expectation_params
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
//' @template template_expectation_return
//'
arma::vec bgnbd_expectation(const double r,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vA_i,
                            const arma::vec& vB_i,
                            const arma::vec& vT_i){
  arma::vec vR = clv::vec_fill(r, vAlpha_i.n_elem);

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

  vA_i = bgnbd_nocov_a_i(a, n);
  vB_i = bgnbd_nocov_b_i(b, n);
  vAlpha_i = bgnbd_nocov_alpha_i(alpha, n);

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
  const double n = vT_i.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = bgnbd_staticcov_alpha_i(alpha,
                                     vCovParams_trans,
                                     vCovParams_life,
                                     mCov_life,
                                     mCov_trans);

  vA_i  = bgnbd_staticcov_a_i(a,
                              vCovParams_life,
                              mCov_life);

  vB_i  = bgnbd_staticcov_b_i(b,
                              vCovParams_life,
                              mCov_life);

  return(bgnbd_expectation(r,
                          vAlpha_i,
                          vA_i,
                          vB_i,
                          vT_i));
}
