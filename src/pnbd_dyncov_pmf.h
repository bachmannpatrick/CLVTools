#ifndef PNBD_DYNCOV_PMF_H
#define PNBD_DYNCOV_PMF_H

#include <RcppArmadillo.h>
#include <gsl/gsl_sf_hyperg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_result.h>
#include "clv_vectorized.h"

arma::vec pnbd_PMF(const double r,
                   const double s,
                   const unsigned int x,
                   const arma::vec& vT_i,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i);

class DynamicCovariates {
public:
  const arma::vec& data;
  arma::vec cumsum_data;

  DynamicCovariates(const arma::vec& input_data);

  double at(arma::uword i) const;
  double sum_from_to(arma::uword from, arma::uword to) const;
  double sum_until(arma::uword i) const;
  arma::uword n_elem() const;
};

double pnbd_dyncov_pmf_per_customer(
    const arma::vec& cov_period_life_exp,
    const arma::vec& cov_period_trans_exp,
    const arma::vec& cov_sincealive_life_exp,
    const arma::vec& cov_sincealive_trans_exp,
    double r_param,
    double alpha_r_param,
    double s_param,
    double beta_s_param,
    double x_double,
    double t_r_param,
    double d1_param,
    double d_omega_param,
    double k0u_param,
    double ui_param
);

double pnbd_dyncov_pmf_hyp2f1_C(double a, double b, double c, double z);

double pnbd_dyncov_pmf_A_i_C(arma::uword i, const DynamicCovariates& dt_data_period_customer_trans);
double pnbd_dyncov_pmf_C_i_C(arma::uword i, const DynamicCovariates& dt_data_period_customer_life);

double pnbd_dyncov_pmf_Bbar_i_C(arma::uword i, const DynamicCovariates& dt_data_period_customer_trans, double d1, double ui);
double pnbd_dyncov_pmf_Dbar_i_C(arma::uword i, const DynamicCovariates& dt_data_period_customer_life, const DynamicCovariates& dt_data_since_alive_customer_life, double d_omega, double k0u_Dbar);

double pnbd_dyncov_pmf_bu_i_C(double ui, arma::uword i, double d1);

double pnbd_dyncov_pmf_S1_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    double x, double alpha_r, double beta_s, double r, double s, double t_r,
    double ui, double d1, double d_omega, double k0u_S1
);

double pnbd_dyncov_pmf_S2_1j_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword j, double x, double alpha_r, double beta_s, double r, double s,
    double ui, double d1, double d_omega, double k0u_S2_1j
);

double pnbd_dyncov_pmf_S2_ij_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword i_S2, arma::uword j_S2, double x, double alpha_r, double beta_s, double r, double s,
    double ui, double d1, double d_omega, double k0u_S2_ij
);

double pnbd_dyncov_pmf_S2_kutuj_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword j, double x, double r, double alpha_r, double s, double beta_s, double t_r,
    double ui, double d1, double d_omega, double k0u_S2_kutu
);


#endif
