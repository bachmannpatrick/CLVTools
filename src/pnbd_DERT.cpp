#include <RcppArmadillo.h>
#include <math.h>
#include "pnbd_LL_ind.h"
#include "clv_vectorized.h"
#include "pnbd_DERT.h"

//' @name pnbd_DERT
//'
//' @title Pareto/NBD: Discounted Expected Residual Transactions
//'
//' @description
//' Calculates the discounted expected residual transactions.
//'
//' \itemize{
//' \item{\code{pnbd_nocov_DERT}}{ Discounted expected residual transactions for the Pareto/NBD model without covariates}
//' \item{\code{pnbd_staticcov_DERT}}{ Discounted expected residual transactions for the Pareto/NBD model with static covariates}
//' }
//'
//' @template template_params_pnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//' @param continuous_discount_factor continuous discount factor to use
//'
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @return
//' Returns a vector with the DERT for each customer.
//'
//' @template template_references_pnbd
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
arma::vec pnbd_nocov_DERT(const double r,
                          const double alpha_0,
                          const double s,
                          const double beta_0,
                          const double continuous_discount_factor,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal){

  const double n = vX.n_elem;


  // Build alpha and beta -------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = pnbd_nocov_alpha_i(alpha_0, n);
  vBeta_i = pnbd_nocov_beta_i(beta_0, n);

  // Calculate DERT -------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}



//' @rdname pnbd_DERT
// [[Rcpp::export]]
arma::vec pnbd_staticcov_DERT(const double r,
                              const double alpha_0,
                              const double s,
                              const double beta_0,
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

  arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);


  // Calculate DERT --------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}

