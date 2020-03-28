#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @rdname bgnbd_palive
// [[Rcpp::export]]
arma::vec bgnbd_palive(const arma::vec& vParams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal){

  const double r       = vParams(0);
  const double alpha   = vParams(1);
  const double a       = vParams(2);
  const double b       = vParams(3);

  arma::vec n_term1 = (a/(b + vX - 1)) % clv::vec_pow((alpha + vT_cal)/(alpha + vT_x), (r+vX));

  return (1 / (1 + (vX > 0) % n_term1));
}
