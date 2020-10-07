#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "clv_vectorized.h"
#include "pnbd_LL_ind.h"
#include "pnbd_PAlive.h"

//' @name pnbd_PAlive
//'
//' @templateVar name_model_full Pareto/NBD
//' @templateVar name_model_short pnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_pnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_pnbd
//'
arma::vec pnbd_PAlive( const double r,
                       const double s,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vBeta_i){

  const arma::vec vLL = pnbd_LL_ind(r,
                                    s,
                                    vAlpha_i,
                                    vBeta_i,
                                    vX,
                                    vT_x,
                                    vT_cal);

  const arma::vec vF1 = arma::lgamma(r+vX) - std::lgamma(r) + r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) +
    vX % (-arma::log(vAlpha_i + vT_cal)) + s*(arma::log(vBeta_i) - arma::log(vBeta_i+vT_cal));

  const arma::vec vLogPAlive = vF1 - vLL;

  return(arma::exp(vLogPAlive));
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_nocov_PAlive(const double r,
                            const double alpha_0,
                            const double s,
                            const double beta_0,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = pnbd_nocov_alpha_i(alpha_0, n);
  vBeta_i = pnbd_nocov_beta_i(beta_0, n);


  // Calculate PAlive -------------------------------------------------------------
  return pnbd_PAlive(r,
                     s,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_staticcov_PAlive(const double r,
                                const double alpha_0,
                                const double s,
                                const double beta_0,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_trans,
                                const arma::mat& mCov_life){


  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer

  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  // Calculate PAlive -------------------------------------------------
  return pnbd_PAlive(r,
                     s,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}

