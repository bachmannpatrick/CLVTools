#ifndef PNBD_HPP
#define PNBD_HPP

#include <RcppArmadillo.h>
#include <cmath>

#include "clv_vectorized.h"

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



arma::vec pnbd_expectation(const double r,
                           const double s,
                           const arma::vec& vAlpha_i,
                           const arma::vec& vBeta_i,
                           const arma::vec& vT_i);



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
                         const arma::vec& vT_cal,
                         const arma::vec& vN);

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

arma::vec pnbd_staticcov_alpha_i(const double alpha_0,
                                 const arma::vec& vCovParams_trans,
                                 const arma::mat& mCov_trans);

arma::vec pnbd_staticcov_beta_i(const double beta_0,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life);



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


arma::vec pnbd_PMF(const double r,
                   const double s,
                   const unsigned int x,
                   const arma::vec& vT_i,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i);

arma::vec pnbd_nocov_PMF(const double r,
                         const double alpha_0,
                         const double s,
                         const double beta_0,
                         const unsigned int x,
                         const arma::vec& vT_i);

arma::vec pnbd_staticcov_PMF(const double r,
                             const double s,
                             const unsigned int x,
                             const arma::vec& vAlpha_i,
                             const arma::vec& vBeta_i,
                             const arma::vec& vT_i);


#endif
