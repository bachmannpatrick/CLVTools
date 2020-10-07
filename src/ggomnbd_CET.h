#ifndef GGOMNBD_CET_HPP
#define GGOMNBD_CET_HPP

double ggomnbd_CET_integrand(double omega, void * p_params);

arma::vec ggomnbd_CET(const double r,
                      const double b,
                      const double s,
                      const double dPeriods,
                      const arma::vec& vX,
                      const arma::vec& vT_x,
                      const arma::vec& vT_cal,
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i);

arma::vec ggomnbd_nocov_CET(const double r,
                            const double alpha_0,
                            const double b,
                            const double s,
                            const double beta_0,
                            const double dPeriods,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal);

arma::vec ggomnbd_staticcov_CET(const double r,
                                const double alpha_0,
                                const double b,
                                const double s,
                                const double beta_0,
                                const double dPeriods,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans);



#endif
