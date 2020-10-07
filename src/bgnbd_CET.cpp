#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "bgnbd_CET.h"
#include "bgnbd_LL.h"

//' @name bgnbd_CET
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @template template_titledescriptionreturn_CET
//'
//' @template template_params_bgnbd
//' @template template_params_rcppperiods
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_CET(const double r,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const double dPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){
  arma::vec term1 = ((vA_i + vB_i + vX - 1) / (vA_i - 1));

  arma::vec term2 = 1 - clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_cal + dPeriods), (r + vX)) % clv::vec_hyp2F1((r + vX), (vB_i + vX), (vA_i + vB_i + vX - 1), dPeriods / (vAlpha_i + vT_cal + dPeriods));

  arma::vec term3 = 1 + (vX > 0) % (vA_i /(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r + vX));

  return term1 % term2 / term3;
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_nocov_CET(const double r,
                    const double alpha,
                    const double a,
                    const double b,
                    const double dPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vA_i = bgnbd_nocov_a_i(a, n);
  vB_i = bgnbd_nocov_b_i(b, n);
  vAlpha_i = bgnbd_nocov_alpha_i(alpha, n);

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, dPeriods, vX, vT_x, vT_cal);
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_CET(const double r,
                              const double alpha,
                              const double a,
                              const double b,
                              const double dPeriods,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::vec& vCovParams_trans,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_trans,
                              const arma::mat& mCov_life){


  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer

  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = alpha * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vA_i     = a     * arma::exp((mCov_life           * vCovParams_life));
  vB_i     = b     * arma::exp((mCov_life           * vCovParams_life));

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, dPeriods, vX, vT_x, vT_cal);
}
