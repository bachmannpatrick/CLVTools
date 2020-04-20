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
  arma::vec vLL = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX,vT_x, vT_cal);

  //   P1 <- (gamma(r+cbs$x)/gamma(r))
  //   P2 <- (alpha_i/(alpha_i+cbs$T.cal))^r
  //   P3 <- (1/(alpha_i+cbs$T.cal)^cbs$x)*(beta_i/(beta_i-1+exp(b*cbs$T.cal)))^s/(ll)

  arma::vec vP1(n), vP2(n), vP3(n);

  for( unsigned int i=0; i<n; i++){
    vP1(i) = tgamma(r + vX(i)) / tgamma(r);
    vP2(i) = std::pow(vAlpha_i(i) / (vAlpha_i(i) + vT_cal(i) ), r );
    vP3(i) = (1.0 / std::pow(vAlpha_i(i) + vT_cal(i) , vX(i)) ) *
      std::pow( vBeta_i(i) / (vBeta_i(i) - 1.0 + std::exp(b * vT_cal(i) )) , s) /
      vLL(i);
  }

  // return(P1*P2*P3)
  //% := element wise multiplication
  return (vP1 % vP2 % vP3);

}
