#include <RcppArmadillo.h>
#include <math.h>
#include "pnbd_LL_ind.h"
#include "cephes_hypergeom1f1.h"
#include "clv_vectorized.h"

arma::vec pnbd_DERT_ind(const double r,
                        const double s,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const double continuous_discount_factor){


  // Calculate LL
  //    Calculate value for every customer
  //    arma::vec pnbd_LL_ind(r,s, vAlpha_i,vBeta_i, vX, vT_x, vT_cal)
  // ----------------------------------------------------------------
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  // // z <- d * (data$beta_i + data$T.cal)
  arma::vec vZ = continuous_discount_factor * (vBeta_i + vT_cal);
  //
  // // term.part.1 = ((z)^(1 - s))/(s - 1) * hyperg_1F1(1, 2 - s, z)
  // // term.part.2 = gamma(1 - s) * hyperg_1F1(s, s, z)
  // // term = term.part.1 + term.part.2
  //
  arma::vec vPart1 = (arma::pow(vZ, 1-s) / (s-1))  % clv::vec_x_hyp1F1(1, 2-s, vZ);
  arma::vec vPart2 = std::tgamma(1-s) * clv::vec_x_hyp1F1(s, s, vZ);
  //
  arma::vec vTerm = vPart1 + vPart2;


  // DERT <- exp(r * log(data$alpha_i) + s * log(data$beta) + (s - 1) *
  //   log(d) + lgamma(r + data$x + 1) + log(term) - lgamma(r) -
  //   (r + data$x + 1) * log(data$alpha_i + data$T.cal) - log(data$LL))

  arma::vec vDERT = arma::exp(
                          r * arma::log(vAlpha_i)
                        + s * arma::log(vBeta_i)
                        + (s-1) * log(continuous_discount_factor)
                        + arma::lgamma(r + vX + 1)
                        + arma::log(vTerm)
                        - std::lgamma(r)
                        - (r + vX + 1) % arma::log(vAlpha_i + vT_cal)
                        - vLL); // dont log as not exp()ed when receiving from pnbd_LL_ind !

  return vDERT;
}
