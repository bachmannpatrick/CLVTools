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
//' @template template_expectation_description
//'
//' @template template_params_ggomnbd
//' @template template_expectation_params
//' @param vAlpha_i Vector of individual parameters alpha
//' @param vBeta_i Vector of individual parameters beta
//'
//' @template template_references_ggomnbd
//'
//' @template template_expectation_return
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

  const arma::vec vAlpha_i = ggomnbd_nocov_alpha_i(alpha_0, n);
  const arma::vec vBeta_i = ggomnbd_nocov_beta_i(beta_0, n);
  const arma::vec vR = ggomnbd_nocov_r(r, n);

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
                                        const double b,
                                        const double s,
                                        const arma::vec& vAlpha_i,
                                        const arma::vec& vBeta_i,
                                        const arma::vec& vT_i){

  const double n = vT_i.n_elem;
  const arma::vec vR = ggomnbd_staticcov_r(r, n);

  return(ggomnbd_expectation(b,
                             s,
                             vR,
                             vAlpha_i,
                             vBeta_i,
                             vT_i));
}



