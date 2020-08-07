#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "bgnbd_LL.h"

//' @name bgnbd_LL
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @templateVar model_params_ordered r, alpha_0, a, b
//' @template template_titleparamsdescriptionreturndetails_LL
//'
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @templateVar name_params_cov_life vLogparams
//' @templateVar name_params_cov_trans vLogparams
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_LL_ind(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){

  const arma::vec vPart1 = r * arma::log(vAlpha_i) + arma::lgamma(r + vX) - std::lgamma(r) - (r + vX) % arma::log(vAlpha_i + vT_x);

  const arma::vec vPart2 = beta_ratio(vA_i, (vB_i+vX), vA_i, vB_i) % clv::vec_pow((vAlpha_i + vT_x)/(vAlpha_i + vT_cal), (r + vX)) + ((vX > 0)) % beta_ratio(vA_i + 1 , (vB_i + vX - 1), vA_i, vB_i);

  arma::vec vLL = vPart1 + arma::log(vPart2);

  return(vLL);
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
arma::vec bgnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  const double r         = exp(vLogparams(0));
  const double alpha_0   = exp(vLogparams(1));
  const double a_0       = exp(vLogparams(2));
  const double b_0       = exp(vLogparams(3));

  const unsigned int n = vX.n_elem;

  const arma::vec vA_i = bgnbd_nocov_a_i(a_0, n);
  const arma::vec vB_i = bgnbd_nocov_b_i(b_0, n);
  const arma::vec vAlpha_i = bgnbd_nocov_alpha_i(alpha_0, n);

  arma::vec vLL = bgnbd_LL_ind(r, vAlpha_i, vA_i, vB_i, vX, vT_x, vT_cal);

  return(vLL);
}


//' @rdname bgnbd_LL
// [[Rcpp::export]]
double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal){

  arma::vec vLL = bgnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  return(-arma::sum(vLL));
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_LL_ind(const arma::vec& vParams,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::mat& mCov_life,
                                 const arma::mat& mCov_trans){
  const double no_cov_life  = mCov_life.n_cols;
  const double no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vParams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vParams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vParams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = exp(vModel_log_params(0));
  const double alpha_0  = exp(vModel_log_params(1));
  const double a_0      = exp(vModel_log_params(2));
  const double b_0      = exp(vModel_log_params(3));


  // Build alpha, a and b --------------------------------------------
  //    With static covariates: alpha, a and b different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    a_i:  a0 * exp(cov.life * cov.param.life)
  //    b_i:  b0 * exp(cov.life * cov.param.life)

  const arma::vec vAlpha_i = bgnbd_staticcov_alpha_i(alpha_0,
                                     vTrans_params,
                                     mCov_trans);

  const arma::vec vA_i  = bgnbd_staticcov_a_i(a_0,
                              vLife_params,
                              mCov_life);

  const arma::vec vB_i  = bgnbd_staticcov_b_i(b_0,
                              vLife_params,
                              mCov_life);

  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer
  arma::vec vLL = bgnbd_LL_ind(r, vAlpha_i, vA_i, vB_i, vX, vT_x, vT_cal);

  return(vLL);
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
double bgnbd_staticcov_LL_sum(const arma::vec& vParams,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans){
  arma::vec vLL = bgnbd_staticcov_LL_ind(vParams,
                                         vX,
                                         vT_x,
                                         vT_cal,
                                         mCov_life,
                                         mCov_trans);

  return(-arma::sum(vLL));
}

arma::vec beta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y){
  return(arma::exp(arma::lgamma(a) + arma::lgamma(b) - arma::lgamma(a + b) - arma::lgamma(x) - arma::lgamma(y) + arma::lgamma(x+y)));
}

arma::vec bgnbd_nocov_alpha_i(const double alpha, const int n){
  return clv::vec_fill(alpha, n);
}

arma::vec bgnbd_nocov_a_i(const double a, const int n){
  return clv::vec_fill(a, n);
}

arma::vec bgnbd_nocov_b_i(const double b, const int n){
  return clv::vec_fill(b, n);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_alpha_i(const double alpha_0,
                                  const arma::vec& vCovParams_trans,
                                  const arma::mat& mCov_trans){
  return alpha_0 * arma::exp((mCov_trans * (-1)) * vCovParams_trans);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_a_i(const double a_0,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life){
  return a_0 * arma::exp(mCov_life * vCovParams_life);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_b_i(const double b_0,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life){
  return b_0 * arma::exp(mCov_life * vCovParams_life);
}
