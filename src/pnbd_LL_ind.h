#ifndef PNBD_LL_HPP
#define PNBD_LL_HPP

arma::vec pnbd_LL_ind( const double r,
                       const double s,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vBeta_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal);

arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal);

double pnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal);

arma::vec pnbd_staticcov_LL_ind(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans);

double pnbd_staticcov_LL_sum(const arma::vec& vParams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::mat& mCov_life,
                             const arma::mat& mCov_trans);

arma::vec pnbd_nocov_alpha_i(const double alpha_0, const double n);

arma::vec pnbd_nocov_beta_i(const double beta_0, const double n);

arma::vec pnbd_staticcov_alpha_i(const double alpha_0,
                                 const arma::vec& vCovParams_trans,
                                 const arma::mat& mCov_trans);

arma::vec pnbd_staticcov_beta_i(const double beta_0,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life);

#endif
