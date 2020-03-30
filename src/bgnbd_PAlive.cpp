#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @title BG/NBD: PAlive without covariates
//'
//' @description
//' BG/NBD without Covariates: Calculates the probability of a customer being alive
//' at the end of the calibration period.
//'
//' @param r TODO: describe
//' @param alpha TODO: describe
//' @param a TODO: describe
//' @param b TODO: describe
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
//'Returns a vector with the PAlive for each customer.
//'
//' @name bgnbd_PAlive
//' @rdname bgnbd_PAlive
// [[Rcpp::export]]
arma::vec bgnbd_palive(const double r,
                       const double alpha,
                       const double a,
                       const double b,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){



  arma::vec n_term1 = (a/(b + vX - 1)) % clv::vec_pow((alpha + vT_cal)/(alpha + vT_x), (r+vX));

  return (1 / (1 + (vX > 0) % n_term1));
}
