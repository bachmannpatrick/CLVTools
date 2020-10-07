#ifndef GGOMNBD_LL_HPP
#define GGOMNBD_LL_HPP

arma::vec ggomnbd_LL_ind(const double r,
                         const double b,
                         const double s,
                         const arma::vec & vAlpha_i,
                         const arma::vec & vBeta_i,
                         const arma::vec & vX,
                         const arma::vec & vT_x,
                         const arma::vec & vT_cal);


arma::vec ggomnbd_integrate(const double r,
                            const double b,
                            const double s,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vBeta_i,
                            const arma::vec& vX,
                            double (*const p_integrationFunction)(double, void*),
                            const arma::vec& vLower,
                            const arma::vec& vUpper);

double ggomnbd_LL_integrand(double y, void * p_params);

arma::vec ggomnbd_nocov_LL_ind(const arma::vec& vLogparams,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal);

double ggomnbd_nocov_LL_sum(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal);

arma::vec ggomnbd_staticcov_LL_ind(const arma::vec& vParams,
                                   const arma::vec& vX,
                                   const arma::vec& vT_x,
                                   const arma::vec& vT_cal,
                                   const arma::mat& mCov_life,
                                   const arma::mat& mCov_trans);

double ggomnbd_staticcov_LL_sum(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans);

arma::vec ggomnbd_nocov_alpha_i(const double alpha_0, const double n);

arma::vec ggomnbd_nocov_beta_i(const double beta_0, const double n);

arma::vec ggomnbd_nocov_r(const double r, const double n);

arma::vec ggomnbd_staticcov_alpha_i(const double alpha_0,
                                    const arma::vec& vCovParams_trans,
                                    const arma::mat& mCov_trans);

arma::vec ggomnbd_staticcov_beta_i(const double beta_0,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life);

arma::vec ggomnbd_staticcov_r(const double r, const double n);


struct integration_params {
  double r;
  double alpha_i;
  double b;
  double s;
  double beta_i;
  double x_i;
};

#endif
