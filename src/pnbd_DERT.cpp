#include <RcppArmadillo.h>
#include <math.h>
#include "pnbd_LL_ind.h"
#include "clv_vectorized.h"

//' @name pnbd_DERT
//'
//' @title Pareto/NBD: Discounted Expected Residual Transactions
//'
//' @description
//' Calculates the discounted expected residual transactions.
//'
//' \itemize{
//' \item{\code{pnbd_nocov_DERT}}{ DERT for the Pareto/NBD model without covariates}
//' \item{\code{pnbd_staticcov_DERT}}{ DERT for the Pareto/NBD model with static covariates}
//' }
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//' @param continuous_discount_factor continuous discount factor to use
//'
//' @details
//'
//' \code{vEstimated_params} vector with the estimated parameters in original scale
//' for the Pareto/NBD model, namely (r, alpha, s, beta). \cr
//' r and alpha: unobserved parameters that describe the NBD transaction process. \cr
//' s and beta: unobserved parameters that describe the pareto
//' (exponential gamma) lifetime process.
//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_trans} at the respective position.
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_life} at the respective position.
//'
//' @return
//' Returns a vector with the DERT for each customer.
//'
//' @references
//' Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//' Pareto/NBD Model and Related Expressions.", Web.
//' \url{http://www.brucehardie.com/notes/008/}.
//'
//'
arma::vec pnbd_DERT_ind(const double r,
                        const double s,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const double continuous_discount_factor){


  // Calculate LL ----------------------------------------------------
  //  Calculate value for every customer
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  arma::vec vZ = continuous_discount_factor * (vBeta_i + vT_cal);

  arma::vec vPart1 = (arma::pow(vZ, 1-s) / (s-1))  % clv::vec_x_hyp1F1(1, 2-s, vZ);
  arma::vec vPart2 = std::tgamma(1-s) * clv::vec_x_hyp1F1(s, s, vZ);
  //
  arma::vec vTerm = vPart1 + vPart2;

  arma::vec vDERT = arma::exp(
    r * arma::log(vAlpha_i)
    + s * arma::log(vBeta_i)
    + (s-1) * log(continuous_discount_factor)
    + arma::lgamma(r + vX + 1)
    + arma::log(vTerm)
    - std::lgamma(r)
    - (r + vX + 1) % arma::log(vAlpha_i + vT_cal)
    - vLL); // dont log as not exp()ed when receiving from pnbd_LL_ind!

    return vDERT;
}



//' @rdname pnbd_DERT
// [[Rcpp::export]]
arma::vec pnbd_nocov_DERT(const arma::vec& vEstimated_params,
                          const double continuous_discount_factor,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal){

  const double n = vX.n_elem;

  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);


  // Build alpha and beta -------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);

  // Calculate DERT -------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}



//' @rdname pnbd_DERT
// [[Rcpp::export]]
arma::vec pnbd_staticcov_DERT(const arma::vec& vEstimated_params,
                              const double continuous_discount_factor,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans,
                              const arma::vec& vCovParams_life,
                              const arma::vec& vCovParams_trans){

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer

  const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));


  // Calculate DERT --------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}

