#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @name bgnbd_CET
//' @title BG/NBD: Conditional Expected Transactions without covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the BG/NBD model parameters.
//'
//' @template template_params_bgnbd
//' @param nPeriods time prediction time frame
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @template template_details_paramsbgnbd
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @return
//' Returns a vector with the conditional expected transactions for the existing
//' customers in the BG/NBD model.
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_CET(const double r,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const double nPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){
  arma::vec term1 = ((vA_i + vB_i + vX - 1) / (vA_i - 1));

  arma::vec term2 = 1 - clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_cal + nPeriods), (r + vX)) % clv::vec_hyp2F1((r + vX), (vB_i + vX), (vA_i + vB_i + vX - 1), nPeriods / (vAlpha_i + vT_cal + nPeriods));

  arma::vec term3 = 1 + (vX > 0) % (vA_i /(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r + vX));

  return term1 % term2 / term3;
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_nocov_CET(const double r,
                    const double alpha,
                    const double a,
                    const double b,
                    const double nPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha);
  vA_i.fill(a);
  vB_i.fill(b);

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, nPeriods, vX, vT_x, vT_cal);
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_CET(const double r,
                              const double alpha,
                              const double a,
                              const double b,
                              const double nPeriods,
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

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = alpha * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vA_i  = a  * arma::exp(((mCov_life * vCovParams_life)));
  vB_i  = b  * arma::exp(((mCov_life * vCovParams_life)));

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, nPeriods, vX, vT_x, vT_cal);
}
