#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL.h"


// integrand <- function(tau)  {tau * exp(b*tau)  *((beta + exp(b*tau) - 1)^(-(s+1)))}
double ggomnbd_expectation_integrand(double tau, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;

  const double b = (params -> b);
  const double s = (params -> s);
  const double beta_i = (params -> beta_i);

  return tau * std::exp(b * tau) * std::pow( beta_i + std::exp(b * tau) - 1.0, -(s+1.0) );
}

//' @name ggomnbd_expectation
//' @title GGompertz/NBD: Unconditional Expectation
//'
//' @description Computes the expected number of repeat transactions in the interval (0, vT_i]
//' for a randomly selected customer, where 0 is defined as the point when the customer came alive.
//'
//' @template template_params_ggomnbd
//' @param vT_i Number of periods since the customer came alive
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_expectation(const double b,
                              const double s,
                              // Pass r as vector because needed for CET
                              const arma::vec& vR,
                              const arma::vec& vAlpha_i,
                              const arma::vec& vBeta_i,
                              const arma::vec& vT_i){

  const arma::vec vF1 = (vR / vAlpha_i);
  const arma::vec vF2 = arma::pow(vBeta_i / (vBeta_i + arma::exp(b * vT_i)-1), s) % (vT_i);
  const arma::vec vF3 = b * s * arma::pow(vBeta_i, s);


  // r and vX are needed in LL but not in the expectation integral.
  //  Pass zeros (= vLower)
  const arma::vec vLower(vBeta_i.n_elem, arma::fill::zeros);
  const arma::vec vF4 = ggomnbd_integrate(0, b, s, vAlpha_i, vBeta_i,
                                          vLower, //instead of vX, only zeros
                                          &ggomnbd_expectation_integrand,
                                          vLower,
                                          vT_i);

  return(vF1 % (vF2 + (vF3 % vF4)));
}

//' @rdname ggomnbd_expectation
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_expectation(const double r,
                                    const double alpha_0,
                                    const double b,
                                    const double s,
                                    const double beta_0,
                                    const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const double n = vT_i.n_elem;
  arma::vec vAlpha_i(n), vBeta_i(n), vR(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);
  vR.fill(r);

  return(ggomnbd_expectation(b,
                             s,
                             vR,
                             vAlpha_i,
                             vBeta_i,
                             vT_i));
}

//' @rdname ggomnbd_expectation
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_expectation(const double r,
                                        const double alpha_0,
                                        const double b,
                                        const double s,
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

  return(ggomnbd_expectation(b,
                             s,
                             vR,
                             vAlpha_i,
                             vBeta_i,
                             vT_i));
}



