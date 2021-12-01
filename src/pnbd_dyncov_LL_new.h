#ifndef PNBD_DYNCOV_NEW_HPP
#define PNBD_DYNCOV_NEW_HPP

#include <RcppArmadillo.h>
#include <cmath>
#include <list>

#include "clv_vectorized.h"

struct Walk {
  // Abstract away the (memory) representation of a walk
private:
  const arma::subview_col<double> walk_data;
public:
  Walk(const arma::vec&, const arma::uword, const arma::uword,
       const double, const double, const int);

  const double tjk;
  const double d;
  const int delta;

  double first() const;
  double last() const;
  arma::uword n_elem() const;
  double sum_middle_elems() const; //sum all elements which are not first or last. Requires at least 3 elements
};


double pnbd_dyncov_LL_i_hyp_alpha_ge_beta(const double r, const double s,
                                          const int x,
                                          const double alpha_1, const double beta_1,
                                          const double alpha_2, const double beta_2);


double pnbd_dyncov_LL_i_hyp_beta_g_alpha(const double r, const double s,
                                         const int x,
                                         const double alpha_1, const double beta_1,
                                         const double alpha_2, const double beta_2);



double pnbd_dyncov_LL_i_F2_1(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x, const double dT,
                             const double a1, const double b1,
                             const double A1T, const double C1T);



double pnbd_dyncov_LL_i_F2_2(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x,
                             const double akt, const double bkT,
                             const double aT, const double bT,
                             const double AkT, const double CkT);


double pnbd_dyncov_LL_i_F2(const int num_walks,
                           const double r, const double alpha_0, const double s, const double beta_0,
                           const int x, const double t_x, const double T_cal, const double dT,
                           const double Bjsum, const double B1,
                           const double D1,
                           const double BT, const double DT,
                           const double A1T, const double C1T,
                           const double AkT, const double CkT,
                           const double F2_3);

#endif

