#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

// [[Rcpp::depends(RcppArmadillo)]]
arma::vec pnbd_CET( const arma::vec& vEstimated_model_params,
                    const double dPrediction_period,
                    const arma::vec& vX,
                    const arma::vec& vT_cal,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vBeta_i,
                    const arma::vec& vPAlive)
{

  const double r = vEstimated_model_params(0);
  // const double alpha_0 = vEstimated_params(1);
  const double s = vEstimated_model_params(2);
  // const double beta_0 = vEstimated_params(3);

  arma::vec vP1, vP2, vP3;

//  P1 <- (r + data$x) * (beta_i + data$T.cal)/((alpha_i + data$T.cal) * (s - 1))
//  P2 <- (1 - ( (beta_i + data$T.cal) / (beta_i + data$T.cal + prediction.period) )^(s - 1))
//  P3 <- pnbd_PAlive(params, data, covariates, dropout.cov, transaction.cov)

  // % is element wise muttiplication
  vP1 = (r + vX) % (vBeta_i + vT_cal) / ((vAlpha_i + vT_cal) * (s-1) );
  vP2 = (1 - arma::pow( ( vBeta_i + vT_cal) / (vBeta_i + vT_cal + dPrediction_period), (s-1)  )   );
  vP3 = vPAlive; //pnbd_PAlive(vEstimated_params, vX, vT_x, vT_cal, mCovariates_transaction, mCovariates_dropout);

  // % is element wise multiplication
  // eval is needed as evaluation could be delayed!
  return (vP1 % vP2 % vP3).eval();
}
