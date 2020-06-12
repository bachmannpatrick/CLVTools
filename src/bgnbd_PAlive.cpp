#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

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

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha);
  vA_i.fill(a);
  vB_i.fill(b);

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
  if(vCovParams_trans.n_elem != mCov_trans.n_cols)
    throw std::out_of_range("Vector of transaction parameters need to have same length as number of columns in transaction covariates!");

  if(vCovParams_life.n_elem != mCov_life.n_cols)
    throw std::out_of_range("Vector of lifetime parameters need to have same length as number of columns in lifetime covariates!");

  if((vX.n_elem != mCov_trans.n_rows) ||
     (vX.n_elem != mCov_life.n_rows))
    throw std::out_of_range("There need to be as many covariate rows as customers!");


  // Build alpha a and b --------------------------------------------
  //  Static covariates: Different alpha, a and b for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = alpha * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vA_i     = a     * arma::exp((mCov_life           * vCovParams_life));
  vB_i     = b     * arma::exp((mCov_life           * vCovParams_life));

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}
