#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

arma::vec beta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y);

//' @name bgnbd_LL
//' @title BG/NBD: Log-Likelihood
//'
//' @description
//' Calculates the Log-likelihood for the BG/NBD model with and without static covariates.
//'
//' The function \code{bgnbd_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{bgnbd_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' The function \code{bgnbd_staticcov_LL_ind} calculates the individual Log-Likelihood
//' values for each customer for the given parameters and covariates.
//'
//' The function \code{bgnbd_staticcov_LL_sum} calculates the individual Log-Likelihood values summed
//' across customers.
//'
//' @param vLogparams vector with the BG/NBD model parameters log scaled
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details
//' \code{vLogparams} is vector with the BG/NBD model parameters at log scale,
//' followed by the parameters for the lifetime covariate at original scale and then
//' followed by the parameters for the transaction covariate at original scale
//'
//' @template template_details_paramsbgnbd
//' @templateVar name_params_cov_life vLogparams
//' @templateVar name_params_cov_trans vLogparams
//' @template template_details_rcppcovmatrix
//'
//' @return
//'  Returns the respective LogLikelihood value for the BG/NBD model without covariates.
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
  const unsigned int n = vX.n_elem;

  arma::vec vA(n), vB(n), vBetaRatio(n);

  vA = r * arma::log(vAlpha_i) + arma::lgamma(r + vX) - std::lgamma(r) - (r + vX) % arma::log(vAlpha_i + vT_x);

  vB = beta_ratio(vA_i, (vB_i+vX), vA_i, vB_i) % clv::vec_pow((vAlpha_i + vT_x)/(vAlpha_i + vT_cal), (r + vX)) + ((vX > 0)) % beta_ratio(vA_i + 1 , (vB_i + vX - 1), vA_i, vB_i);

  arma::vec vLL = vA + arma::log(vB);

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

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha_0);
  vA_i.fill(a_0);
  vB_i.fill(b_0);

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
arma::vec bgnbd_staticcov_LL_ind(const arma::vec& vLogparams,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::mat& mCov_life,
                                 const arma::mat& mCov_trans){
  const double no_cov_life  = mCov_life.n_cols;
  const double no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vLogparams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vLogparams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vLogparams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = exp(vModel_log_params(0));
  const double alpha_0  = exp(vModel_log_params(1));
  const double a_0        = exp(vModel_log_params(2));
  const double b_0   = exp(vModel_log_params(3));

  const double n = vX.n_elem;



  // Build alpha, a and b --------------------------------------------
  //    With static covariates: alpha, a and b different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    a_i:  a0 * exp(cov.life * cov.param.life)
  //    b_i:  b0 * exp(cov.life * cov.param.life)
  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  vA_i  = a_0  * arma::exp(((mCov_life * vLife_params)));
  vB_i  = b_0  * arma::exp(((mCov_life * vLife_params)));

  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer
  arma::vec vLL = bgnbd_LL_ind(r, vAlpha_i, vA_i, vB_i, vX, vT_x, vT_cal);

  return(vLL);
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
double bgnbd_staticcov_LL_sum(const arma::vec& vLogparams,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans){
  arma::vec vLL = bgnbd_staticcov_LL_ind(vLogparams,
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


