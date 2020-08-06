#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL.h"

//' @name ggomnbd_PAlive
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_ggomnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_PAlive(const double r,
                         const double b,
                         const double s,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal,
                         const arma::vec& vAlpha_i,
                         const arma::vec& vBeta_i){

  arma::vec vLL = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  const arma::vec vP1 = arma::lgamma(r + vX) - lgamma(r);
  const arma::vec vP2 = r * arma::log(vAlpha_i/(vAlpha_i + vT_cal)) + vX % arma::log(1/(vAlpha_i + vT_cal)) + s * arma::log(vBeta_i/(vBeta_i - 1 + exp(b * vT_cal)));
  const arma::vec vP3 = vLL;

  return arma::exp(vP1 + vP2 - vP3);

}


//' @rdname ggomnbd_PAlive
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_PAlive(const double r,
                                   const double alpha_0,
                                   const double b,
                                   const double s,
                                   const double beta_0,
                                   const arma::vec& vX,
                                   const arma::vec& vT_x,
                                   const arma::vec& vT_cal,
                                   const arma::vec& vCovParams_trans,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life,
                                   const arma::mat& mCov_trans){

  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  // Calculate PAlive ------------------------------------------------
  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}


//' @rdname ggomnbd_PAlive
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_PAlive(const double r,
                               const double alpha_0,
                               const double b,
                               const double s,
                               const double beta_0,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = ggomnbd_nocov_alpha_i(alpha_0, n);
  vBeta_i = ggomnbd_nocov_beta_i(beta_0, n);


  // Calculate PAlive -------------------------------------------------------------
  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}
