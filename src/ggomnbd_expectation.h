#ifndef GGOMNBD_EXPECTATION_HPP
#define GGOMNBD_EXPECTATION_HPP
arma::vec ggomnbd_expectation(const double b,
                              const double s,
                              const arma::vec& vR,
                              const arma::vec& vAlpha_i,
                              const arma::vec& vBeta_i,
                              const arma::vec& vT_i);

arma::vec ggomnbd_nocov_expectation(const double r,
                                    const double alpha_0,
                                    const double b,
                                    const double s,
                                    const double beta_0,
                                    const arma::vec& vT_i);

arma::vec ggomnbd_staticcov_expectation(const double r,
                                        const double alpha_0,
                                        const double b,
                                        const double s,
                                        const double beta_0,
                                        const arma::vec& vT_i,
                                        const arma::vec& vCovParams_trans,
                                        const arma::vec& vCovParams_life,
                                        const arma::mat& mCov_life,
                                        const arma::mat& mCov_trans);

#endif
