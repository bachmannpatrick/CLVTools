#include <RcppArmadillo.h>
#include <math.h>
#include "pnbd_expectation.h"
#include "clv_vectorized.h"

arma::vec pnbd_expectation(const double r,
                           const double s,
                           const arma::vec& vAlpha_i,
                           const arma::vec& vBeta_i,
                           const arma::vec& vT_i){
  return (r * vBeta_i) / (vAlpha_i * (s - 1)) % (1 -arma::pow((vBeta_i/(vBeta_i + vT_i)), (s - 1)));
}

// [[Rcpp::export]]
arma::vec pnbd_nocov_expectation(const double r,
                                    const double s,
                                    const double alpha_0,
                                    const double beta_0,
                                    const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const double n = vT_i.n_elem;
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}

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
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)
  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));
  arma::vec vR(vAlpha_i.n_elem);
  vR.fill(r);

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}
