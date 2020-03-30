#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @title BG/NBD: Conditional Expected Transactions without covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customer's past transaction behavior and the BG/NBD model parameters.
//'
//' @param r TODO: describe
//' @param alpha TODO: describe
//' @param a TODO: describe
//' @param b TODO: describe
//' @param nPeriods time prediction time frame
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r} \code{alpha} \code{a} \code{b} estimated parameters in original scale
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
arma::vec bgnbd_cet(const double r,
                    const double alpha,
                    const double a,
                    const double b,
                    const double nPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){

  arma::vec term1 = ((a + b + vX - 1) / (a - 1));

  arma::vec term2 = 1 - clv::vec_pow((alpha + vT_cal)/(alpha + vT_cal + nPeriods), (r + vX)) % clv::vec_hyp2F1((r + vX), (b + vX), (a + b + vX - 1), nPeriods / (alpha + vT_cal + nPeriods));

  arma::vec term3 = 1 + (vX > 0) % (a /(b + vX - 1)) % clv::vec_pow((alpha + vT_cal)/(alpha + vT_x), (r + vX));

  return term1 % term2 / term3;
}
