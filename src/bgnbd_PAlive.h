#ifndef BGNBD_PALIVE_HPP
#define BGNBD_PALIVE_HPP

arma::vec bgnbd_PAlive(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal);

arma::vec bgnbd_nocov_PAlive(const double r,
                             const double alpha,
                             const double a,
                             const double b,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal);

arma::vec bgnbd_staticcov_PAlive(const double r,
                                 const double alpha,
                                 const double a,
                                 const double b,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::vec& vCovParams_trans,
                                 const arma::vec& vCovParams_life,
                                 const arma::mat& mCov_trans,
                                 const arma::mat& mCov_life);



#endif
