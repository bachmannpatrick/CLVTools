#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "bgnbd_PAlive.h"
#include "bgnbd_LL.h"

//' @name bgnbd_PAlive
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_bgnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_PAlive(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){
  arma::vec n_term1 = (vA_i/(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r+vX));

  return (1 / (1 + (vX > 0) % n_term1));
}

//' @rdname bgnbd_PAlive
// [[Rcpp::export]]
arma::vec bgnbd_nocov_PAlive(const double r,
                       const double alpha,
                       const double a,
                       const double b,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){

  // Build alpha, a and b --------------------------------------------------------
  //    No covariates: Same alpha, a and b for every customer

  const double n = vX.n_elem;
  const arma::vec vA_i = bgnbd_nocov_a_i(a, n);
  const arma::vec vB_i = bgnbd_nocov_b_i(b, n);
  const arma::vec vAlpha_i = bgnbd_nocov_alpha_i(alpha, n);

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}

//' @rdname bgnbd_PAlive
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_PAlive(const double r,
                                 const double alpha,
                                 const double a,
                                 const double b,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::vec& vCovParams_trans,
                                 const arma::vec& vCovParams_life,
                                 const arma::mat& mCov_trans,
                                 const arma::mat& mCov_life){

  // Build alpha a and b --------------------------------------------
  //  Static covariates: Different alpha, a and b for every customer

  const arma::vec vAlpha_i = bgnbd_staticcov_alpha_i(alpha,
                                     vCovParams_trans,
                                     mCov_trans);

  const arma::vec vA_i  = bgnbd_staticcov_a_i(a,
                              vCovParams_life,
                              mCov_life);

  const arma::vec vB_i  = bgnbd_staticcov_b_i(b,
                              vCovParams_life,
                              mCov_life);

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}
