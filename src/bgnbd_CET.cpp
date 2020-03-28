#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @title BG/NBD: Conditional Expected Transactions without covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the BG/NBD model parameters.
//'
//' @template template_params_rcppestimatedparams
//' @param dPrediction_period time prediction time frame
//' @template template_params_rcppxtxtcal
//'
//'
//' @details
//' \code{vParams} vector with the estimated parameters in original scale
//' for the BG/NBD model, namely (r, alpha, a, b).
//' r and alpha: TODO: description.
//' a and b: TODO: description
//'
//'
//'@return
//' Returns a vector containing the conditional expected transactions for the existing
//' customers in the BG/NBD model.
//'
//' @name bgnbd_CET
//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_cet(const arma::vec& vParams,
                    const double nPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){

  const double r       = vParams(0);
  const double alpha   = vParams(1);
  const double a       = vParams(2);
  const double b       = vParams(3);

  arma::vec term1 = ((a + b + vX - 1) / (a - 1));

  arma::vec term2 = 1 - clv::vec_pow((alpha + vT_cal)/(alpha + vT_cal + nPeriods), (r + vX)) % clv::vec_hyp2F1((r + vX), (b + vX), (a + b + vX - 1), nPeriods / (alpha + vT_cal + nPeriods));

  arma::vec term3 = 1 + (vX > 0) % (a /(b + vX - 1)) % clv::vec_pow((alpha + vT_cal)/(alpha + vT_x), (r + vX));

  return term1 % term2 / term3;
}
