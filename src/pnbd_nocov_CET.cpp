#include <RcppArmadillo.h>
#include "pnbd_PAlive.h"
#include "pnbd_CET.h"

//' @title Pareto/NBD: Conditional Expected Transactions without covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the Pareto/NBD model parameters.
//'
//' @template template_params_rcppestimatedparams
//' @param dPrediction_period time prediction time frame
//' @template template_params_rcppxtxtcal
//'
//'
//' @details
//' \code{vEstimated_params} vector with the estimated parameters in original scale
//' for the Pareto/NBD model, namely (r, alpha, s, beta).
//' r and alpha: unobserved parameters that describe the NBD transaction process.
//' s and beta: unobserved parameters that describe the Pareto
//' (exponential gamma) dropout process.
//'
//'
//'@return
//' Returns a vector containing the conditional expected transactions for the existing
//' customers in the Pareto/NBD model.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @name pnbd_nocov_CET
//' @rdname pnbd_nocov_CET
// [[Rcpp::export]]
arma::vec pnbd_nocov_CET(const arma::vec& vEstimated_params,
                         const double dPrediction_period,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;
  // const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  // const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  // pnbd_PAlive(vEstimated_model_params, vX,
  //              vT_x, vT_cal,
  //              vAlpha_i, vBeta_i);

  const arma::vec vPAlive = pnbd_PAlive(vEstimated_params, vX,
                                        vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  // pnbd_CET(vEstimated_model_params,
  //          dPrediction_period,
  //          vX, vT_cal,
  //          vAlpha_i, vBeta_i,
  //          vPAlive)
  return(pnbd_CET(vEstimated_params,
                     dPrediction_period,
                     vX, vT_cal,
                     vAlpha_i, vBeta_i,
                     vPAlive));
}
