#ifndef PNBD_PALIVE_HPP
#define PNBD_PALIVE_HPP

arma::vec pnbd_PAlive( const double r,
                       const double s,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vBeta_i);

arma::vec pnbd_nocov_PAlive(const double r,
                            const double alpha_0,
                            const double s,
                            const double beta_0,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal);

arma::vec pnbd_staticcov_PAlive(const double r,
                                const double alpha_0,
                                const double s,
                                const double beta_0,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_trans,
                                const arma::mat& mCov_life);

#endif
