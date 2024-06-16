#ifndef BGNBD_HPP
#define BGNBD_HPP

#include <RcppArmadillo.h>
#include <cmath>
#include "clv_vectorized.h"

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

arma::vec bgnbd_CET(const double r,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const double dPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal);

arma::vec bgnbd_nocov_CET(const double r,
                          const double alpha,
                          const double a,
                          const double b,
                          const double dPeriods,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal);

arma::vec bgnbd_staticcov_CET(const double r,
                              const double alpha,
                              const double a,
                              const double b,
                              const double dPeriods,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::vec& vCovParams_trans,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_trans,
                              const arma::mat& mCov_life);

arma::vec bgnbd_expectation(const double r,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vA_i,
                            const arma::vec& vB_i,
                            const arma::vec& vT_i);

arma::vec lbeta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y);
arma::vec beta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y);

arma::vec bgnbd_LL_ind(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal);

arma::vec bgnbd_nocov_LL_ind(const arma::vec& vLogparams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal);

double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal,
                          const arma::vec& vN);

arma::vec bgnbd_staticcov_LL_ind(const arma::vec& vParams,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::mat& mCov_life,
                                 const arma::mat& mCov_trans);

double bgnbd_staticcov_LL_sum(const arma::vec& vParams,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans);

arma::vec bgnbd_PMF(const double r,
                    const unsigned int x,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const arma::vec& vT_i);

arma::vec bgnbd_nocov_PMF(const double r,
                          const double alpha,
                          const double a,
                          const double b,
                          const arma::vec& vT_i);

arma::vec bgnbd_staticcov_PMF(const double r,
                              const arma::vec& vAlpha_i,
                              const arma::vec& vA_i,
                              const arma::vec& vB_i,
                              const arma::vec& vT_i);

arma::vec bgnbd_staticcov_alpha_i(const double alpha_0,
                                  const arma::vec& vCovParams_trans,
                                  const arma::mat& mCov_trans);

arma::vec bgnbd_staticcov_a_i(const double a,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life);

arma::vec bgnbd_staticcov_b_i(const double b,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life);


#endif
