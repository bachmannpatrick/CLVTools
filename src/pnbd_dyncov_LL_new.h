#ifndef PNBD_DYNCOV_NEW_HPP
#define PNBD_DYNCOV_NEW_HPP

#include <RcppArmadillo.h>
#include <cmath>
#include <vector>

#include "clv_vectorized.h"

struct Walk {
  // Abstract away the (memory) representation of a walk
// private:
  // arma::subview_col<double> walk_data;
  arma::vec walk_data;

  Walk():tjk(0), d(0), delta(0){
    this->walk_data = arma::zeros(0);
  }

 /*
  * MUST PASS DATA VEC BY REF TO CONSTRUCTORS
  *   because store a subview which would point to freed mem if passed by value
  */
  // Walk(const arma::vec& cov_data, const arma::uword, const arma::uword,
  //      const double, const double, const double, const bool);

  Walk(const arma::vec&, const arma::rowvec&);

  double tjk;
  double d;
  double delta; //can only be 0, 1 but store as double to avoid frequent casting and accidentially forgetting it
  bool is_aux_trans;

  double first() const;
  double last() const;
  double get_elem(arma::uword i) const;
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

