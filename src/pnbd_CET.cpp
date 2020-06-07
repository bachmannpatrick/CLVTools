#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "pnbd_PAlive.h"

//' @name pnbd_CET
//'
//' @templateVar name_model_full Pareto/NBD
//' @templateVar name_model_short pnbd
//' @template template_titledescriptionreturn_CET
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppperiods
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
//' @template template_references_pnbd
//'
arma::vec pnbd_CET(const double r,
                   const double s,
                   const double dPeriods,
                   const arma::vec& vX,
                   const arma::vec& vT_cal,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i,
                   const arma::vec& vPAlive){

  const arma::vec vP1 = (r + vX) % (vBeta_i + vT_cal) / ((vAlpha_i + vT_cal) * (s-1));
  const arma::vec vP2 = (1 - arma::pow((vBeta_i + vT_cal) / (vBeta_i + vT_cal + dPeriods), (s-1)));
  const arma::vec vP3 = vPAlive;

  // eval is needed as evaluation could be delayed!
  return (vP1 % vP2 % vP3).eval();
}



//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_nocov_CET(const double r,
                         const double alpha_0,
                         const double s,
                         const double beta_0,
                         const double dPeriods,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(r, s,
                                        vX, vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(r,
                  s,
                  dPeriods,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}





//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_staticcov_CET(const double r,
                             const double alpha_0,
                             const double s,
                             const double beta_0,
                             const double dPeriods,
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

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(r, s,
                                        vX, vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(r, s,
                  dPeriods,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}

