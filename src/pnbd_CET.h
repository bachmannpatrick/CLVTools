#ifndef PNBD_CET_HPP
#define PNBD_CET_HPP

arma::vec pnbd_CET( const arma::vec& vEstimated_model_params,
                    const double dPrediction_period,
                    const arma::vec& vX,
                    const arma::vec& vT_cal,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vBeta_i,
                    const arma::vec& vPAlive);

#endif
