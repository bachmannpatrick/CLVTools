#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

//' @rdname bgnbd_cet
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

  arma::vec term3 = 1 + clv::vec_as_numeric(vX) % (a /(b + vX - 1)) % clv::vec_pow((alpha + vT_cal)/(alpha + vT_x), (r + vX));

  return term1 % term2 / term3;
}
