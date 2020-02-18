#include <RcppArmadillo.h>
#include "pnbd_LL_ind.h"

//' @title Pareto/NBD: LogLikelihood with static covariates
//'
//' @description
//' Pareto/NBD with Static Covariates:
//'
//' The function \code{pnbd_staticcov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters and covariates.
//'
//' The function \code{pnbd_staticcov_LL_sum} calculates the individual LogLikelihood values summed
//' across customers.
//'
//' @param vParams vector with the parameters for the Pareto/NBD model and the static covariates. See Details.
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details
//' \code{vParams} is vector with the Pareto/NBD model parameters at log scale,
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
//'  Returns the respective LogLikelihood value for the Pareto/NBD model with static covariates.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
arma::vec pnbd_staticcov_LL_ind(const arma::vec& vParams,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::mat& mCov_life,
                                 const arma::mat& mCov_trans){

  // vParams have to be single vector because used by optimizer


  const double no_cov_life  = mCov_life.n_cols;
  const double no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vParams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vParams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vParams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = exp(vModel_log_params(0));
  const double alpha_0  = exp(vModel_log_params(1));
  const double s        = exp(vModel_log_params(2));
  const double beta_0   = exp(vModel_log_params(3));

  // n = number of elements / customers
  const double n = vX.n_elem;



  // Build alpha and beta
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)
  // ----------------------------------------------------------------
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vLife_params));

  // Calculate LL
  //    Calculate value for every customer
  //    Sum of all customers' LL value
  //
  //    arma::vec pnbd_LL_ind(r,s, vAlpha_i,vBeta_i, vX, vT_x, vT_cal)
  // ----------------------------------------------------------------
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  return(vLL);
}

