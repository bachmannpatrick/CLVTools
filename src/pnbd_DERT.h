#ifndef PNBD_DERT_HPP
#define PNBD_DERT_HPP

arma::vec pnbd_DERT_ind(const double r,
                        const double s,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const double continuous_discount_factor);

arma::vec pnbd_nocov_DERT(const double r,
                          const double alpha_0,
                          const double s,
                          const double beta_0,
                          const double continuous_discount_factor,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal);

arma::vec pnbd_staticcov_DERT(const double r,
                              const double alpha_0,
                              const double s,
                              const double beta_0,
                              const double continuous_discount_factor,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans,
                              const arma::vec& vCovParams_life,
                              const arma::vec& vCovParams_trans);



#endif
