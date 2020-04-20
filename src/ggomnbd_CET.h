#ifndef GGOMNBD_CET_HPP
#define GGOMNBD_CET_HPP

arma::vec ggomnbd_CET(const double r,
                   const double b,
                   const double s,
                   const double dPrediction_period,
                   const arma::vec& vX,
                   const arma::vec& vT_x,
                   const arma::vec& vT_cal,
                   arma::vec vAlpha_i,
                   arma::vec vBeta_i,
                   const arma::vec& vPAlive);

#endif
