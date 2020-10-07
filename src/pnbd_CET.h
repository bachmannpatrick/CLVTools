#ifndef PNBD_CET_HPP
#define PNBD_CET_HPP

arma::vec pnbd_CET(const double r,
                   const double s,
                   const double dPeriods,
                   const arma::vec& vX,
                   const arma::vec& vT_cal,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i,
                   const arma::vec& vPAlive);

arma::vec pnbd_nocov_CET(const double r,
                         const double alpha_0,
                         const double s,
                         const double beta_0,
                         const double dPeriods,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal);

arma::vec pnbd_staticcov_CET(const double r,
                             const double alpha_0,
                             const double s,
                             const double beta_0,
                             const double dPeriods,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::vec& vCovParams_trans,
                             const arma::vec& vCovParams_life,
                             const arma::mat& mCov_trans,
                             const arma::mat& mCov_life);

#endif
