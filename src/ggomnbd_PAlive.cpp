#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL_ind.h"

arma::vec ggomnbd_PAlive(const double r,
                      const double s,
                      const double b,
                      const arma::vec& vX,
                      const arma::vec& vT_x,
                      const arma::vec& vT_cal,
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i){

  const unsigned int n = vX.n_elem;

  // Individual LL values -------------------------------------------------
  // ggomnbd_LL_ind(r,b,s,vAlpha_i,vBeta_i,vX,vT_x, vT_cal):
  arma::vec vLL = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  arma::vec vP1(n), vP2(n), vP3(n), vOnes(n);
  vOnes.fill(1);

  vP1 = arma::lgamma(r + vX) - lgamma(r);
  vP2 = r * arma::log(vAlpha_i/(vAlpha_i + vT_cal)) + vX % arma::log(1/(vAlpha_i + vT_cal)) + s * arma::log(vBeta_i/(vBeta_i - 1 + exp(b * vT_cal)));
  vP3 = vLL;

  return arma::exp(vP1 + vP2 - vP3);
}
