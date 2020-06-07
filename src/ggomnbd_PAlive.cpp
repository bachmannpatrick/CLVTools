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

  const unsigned int n = vX.n_elem;

  // Individual LL values -------------------------------------------------
  arma::vec vLL = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  arma::vec vP1(n), vP2(n), vP3(n);

  vP1 = arma::lgamma(r + vX) - lgamma(r);
  vP2 = r * arma::log(vAlpha_i/(vAlpha_i + vT_cal)) + vX % arma::log(1/(vAlpha_i + vT_cal)) + s * arma::log(vBeta_i/(vBeta_i - 1 + exp(b * vT_cal)));
  vP3 = vLL;

  return arma::round(10000*arma::exp(vP1 + vP2 - vP3))/10000;

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

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

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

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}
