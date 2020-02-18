#include <RcppArmadillo.h>
#include "pnbd_PAlive.h"

//' @title Pareto/NBD: PAlive without covariates
//'
//' @description
//' Pareto/NBD without Covariates: Calculates the probability of a customer being alive
//' at the end of the calibration period.
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{vEstimated_params} vector with the estimated parameters in original scale
//' for the Pareto/NBD model, namely (r, alpha, s, beta).
//' r and alpha: unobserved parameters that describe the NBD transaction process.
//' s and beta: unobserved parameters that describe the pareto
//' (exponential gamma) dropout process.
//'
//'
//'@return
//'Returns a vector with the PAlive for each customer.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @name pnbd_nocov_PAlive
//' @rdname pnbd_nocov_PAlive
// [[Rcpp::export]]
arma::vec pnbd_nocov_PAlive(const arma::vec& vEstimated_params,
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

  return pnbd_PAlive(vEstimated_params,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}
