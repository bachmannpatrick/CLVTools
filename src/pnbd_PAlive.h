#ifndef PNBD_PALIVE_HPP
#define PNBD_PALIVE_HPP

arma::vec pnbd_PAlive(  const arma::vec& vEstimated_model_params,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i);

#endif
