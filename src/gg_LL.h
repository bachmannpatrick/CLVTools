#ifndef GG_LL_HPP
#define GG_LL_HPP

#include <RcppArmadillo.h>
#include <cmath>
#include "clv_vectorized.h"

double gg_LL(const arma::vec& vLogparams,
             const arma::vec& vX,
             const arma::vec& vM_x,
             const arma::vec& vN);

#endif
