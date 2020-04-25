#include <RcppArmadillo.h>
#include "ggomnbd_LL_ind.h"


//' @title GGompertz/NBD: LogLikelihood with static covariates
//'
//' @description
//' GGompertz/NBD with Static Covariates:
//'
//' The function \code{ggomnbd_staticcov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters and covariates.
//'
//' The function \code{ggomnbd_staticcov_LL_sum} calculates the individual LogLikelihood values summed
//' across customers.
//'
//' @param vParams vector with the parameters for the GGompertz/NBD model and the static covariates. See Details.
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details
//' \code{vParams} is vector with the GGompertz/NBD model parameters at log scale,
//' followed by the parameters for the lifetime covariate at original scale and then
//' followed by the parameters for the transaction covariate at original scale
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//'
//'@return
//'  Returns the respective LogLikelihood value for the GGompertz/NBD model with static covariates.
//'
//'@references
//' TODO
//'
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_LL_ind(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  // Read out parameters from vParams
  //
  //    Contains model and covariate parameters
  //      Model:              first 5
  //      Life + Trans cov    after model params
  //                          depends on num of cols in cov data
  // vParams have to be single vector because used by optimizer
  const double r       = exp(vParams(0));
  const double alpha_0 = exp(vParams(1));
  const double b       = exp(vParams(2));
  const double s       = exp(vParams(3));
  const double beta_0  = exp(vParams(4));

  const int no_model_params = 5;
  const double num_cov_life  = mCov_life.n_cols;
  const double num_cov_trans = mCov_trans.n_cols;

  const arma::vec vLife_params      = vParams.subvec(no_model_params              ,  no_model_params+num_cov_life                 - 1);
  const arma::vec vTrans_params     = vParams.subvec(no_model_params + num_cov_life, no_model_params+num_cov_life + num_cov_trans - 1);



  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vLife_params));


  // Calculate LL --------------------------------------------------
  //    Calculate value for every customer
  //    Sum of all customers' LL value
  // arma::vec ggomnbd_LL_ind(const double r,
  //                       const double b,
  //                       const double s,
  //                       const arma::vec & vAlpha_i,
  //                       const arma::vec & vBeta_i,
  //                       const arma::vec & vX,
  //                       const arma::vec & vT_x,
  //                       const arma::vec & vT_cal);
  return(ggomnbd_LL_ind(r,b,s,vAlpha_i,vBeta_i,vX,vT_x,vT_cal));
}
