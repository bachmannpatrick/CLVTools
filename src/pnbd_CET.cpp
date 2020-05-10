#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "pnbd_PAlive.h"

//' @name pnbd_CET
//' @keywords internal
//'
//' @title Pareto/NBD: Conditional Expected Transactions
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the Pareto/NBD model parameters.
//'
//' \itemize{
//' \item{\code{pnbd_nocov_CET}}{ Conditional Expected Transactions without covariates}
//' \item{\code{pnbd_staticcov_CET}}{ Conditional Expected Transactions with static covariates}
//' }
//'
//'
//' @template template_params_rcppestimatedparams
//' @param dPrediction_period time prediction time frame
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_pnbd vEstimated_params
//' @template template_details_paramspnbd
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//'
//'@return
//' Returns a vector containing the conditional expected transactions for the existing
//' customers in the Pareto/NBD model.
//'
//'@template template_references_pnbd
//'
arma::vec pnbd_CET(const arma::vec& vEstimated_model_params,
                   const double dPrediction_period,
                   const arma::vec& vX,
                   const arma::vec& vT_cal,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i,
                   const arma::vec& vPAlive){

  const double r = vEstimated_model_params(0);
  // const double alpha_0 = vEstimated_params(1);
  const double s = vEstimated_model_params(2);
  // const double beta_0 = vEstimated_params(3);

  arma::vec vP1, vP2, vP3;

  vP1 = (r + vX) % (vBeta_i + vT_cal) / ((vAlpha_i + vT_cal) * (s-1));
  vP2 = (1 - arma::pow((vBeta_i + vT_cal) / (vBeta_i + vT_cal + dPrediction_period), (s-1)));
  vP3 = vPAlive;

  // eval is needed as evaluation could be delayed!
  return (vP1 % vP2 % vP3).eval();
}



//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_nocov_CET(const arma::vec& vEstimated_params,
                         const double dPrediction_period,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;
  // const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  // const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(vEstimated_params, vX,
                                        vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(vEstimated_params,
                  dPrediction_period,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}





//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_staticcov_CET(const arma::vec& vEstimated_params,
                             const double dPrediction_period,
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


  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer

  const double n = vX.n_elem;

  // const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  // const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vBeta_i  = beta_0  * arma::exp(((mCov_life     * (-1)) * vCovParams_life));


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(vEstimated_params, vX,
                                        vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(vEstimated_params,
                  dPrediction_period,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}

