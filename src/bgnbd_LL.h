#ifndef BGNBD_LL_HPP
#define BGNBD_LL_HPP
#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"


arma::vec bgnbd_nocov_LL_ind(const arma::vec& vLogparams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal);

double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal);

arma::vec beta_ratio(const double a, arma::vec& b, const double x, const double y);

#endif
