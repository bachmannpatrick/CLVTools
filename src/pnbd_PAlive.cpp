#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "clv_vectorized.h"
#include "pnbd_LL_ind.h"

//' @name pnbd_PAlive
//'
//' @title Pareto/NBD: Probability of Being Alive
//'
//' @description
//' Calculates the probability of a customer being alive at the end of the calibration period.
//'
//' \itemize{
//' \item{\code{pnbd_nocov_PAlive}}{ P(alive) for the Pareto/NBD model without covariates}
//' \item{\code{pnbd_staticcov_PAlive}}{ P(alive) for the Pareto/NBD model with static covariates}
//' }
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_pnbd vEstimated_params
//' @template template_details_paramspnbd
//'
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//'
//'
//'@return
//'Returns a vector with the PAlive for each customer.
//'
//'@template template_references_pnbd
//'
arma::vec pnbd_PAlive( const arma::vec& vEstimated_model_params,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vBeta_i)
{

 
  const double r       = vEstimated_model_params(0);
  // const double alpha_0 = vEstimated_model_params(1);
  const double s       = vEstimated_model_params(2);
  // const double beta_0  = vEstimated_model_params(3);


  const arma::vec vLL = pnbd_LL_ind(r,
                    s,
                    vAlpha_i,
                    vBeta_i,
                    vX,
                    vT_x,
                    vT_cal);


  const arma::vec vF1 = arma::lgamma(r+vX) - std::lgamma(r) + r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) +
  vX % (-arma::log(vAlpha_i + vT_cal)) + s*(arma::log(vBeta_i) - arma::log(vBeta_i+vT_cal+vT_x));

  const arma::vec vLogPAlive = vF1 - vLL;

  return(arma::exp(vLogPAlive));
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_nocov_PAlive(const arma::vec& vEstimated_params,
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
  return pnbd_PAlive(vEstimated_params,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_staticcov_PAlive(const arma::vec& vEstimated_params,
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
  vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

  // Calculate PAlive -------------------------------------------------
  return pnbd_PAlive(vEstimated_params,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}

