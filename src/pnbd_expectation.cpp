#include <RcppArmadillo.h>
#include <math.h>
#include "pnbd_expectation.h"
#include "clv_vectorized.h"
#include "pnbd_LL_ind.h"

//' @name pnbd_expectation
//' @title Pareto/NBD: Unconditional Expectation
//'
//' @template template_expectation_description
//'
//' @template template_params_pnbd
//' @template template_expectation_params
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_pnbd
//'
//' @template template_expectation_return
//'
arma::vec pnbd_expectation(const double r,
                           const double s,
                           const arma::vec& vAlpha_i,
                           const arma::vec& vBeta_i,
                           const arma::vec& vT_i){
  return (r * vBeta_i) / (vAlpha_i * (s - 1)) % (1 -arma::pow((vBeta_i/(vBeta_i + vT_i)), (s - 1)));
}

//' @rdname pnbd_expectation
// [[Rcpp::export]]
arma::vec pnbd_nocov_expectation(const double r,
                                    const double s,
                                    const double alpha_0,
                                    const double beta_0,
                                    const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const double n = vT_i.n_elem;

  const arma::vec vAlpha_i = pnbd_nocov_alpha_i(alpha_0, n);
  const arma::vec vBeta_i = pnbd_nocov_beta_i(beta_0, n);

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}

//' @rdname pnbd_expectation
// [[Rcpp::export]]
arma::vec pnbd_staticcov_expectation(const double r,
                                        const double s,
                                        const double alpha_0,
                                        const double beta_0,
                                        const arma::vec& vT_i,
                                        const arma::vec& vCovParams_trans,
                                        const arma::vec& vCovParams_life,
                                        const arma::mat& mCov_life,
                                        const arma::mat& mCov_trans){

  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer

  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}
