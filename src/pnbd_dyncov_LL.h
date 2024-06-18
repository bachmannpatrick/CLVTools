#ifndef PNBD_DYNCOV_NEW_HPP
#define PNBD_DYNCOV_NEW_HPP

#include <RcppArmadillo.h>
#include <cmath>
#include <vector>

#include <gsl/gsl_sf_hyperg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_result.h>

#include "clv_vectorized.h"

/*
 *  Abstract away the (memory) representation of a walk
 */
struct LifetimeWalk {

  // storing as arma::subview_col<double> walk_data leads to copy/move constructor issues
  arma::vec walk_data; // exp(gamma*X) of this walk

 /*
  * MUST PASS DATA VEC BY REF TO CONSTRUCTORS
  *   because store a subview which would point to freed mem if passed by value
  */
  LifetimeWalk(); // Default: leave in uninitialized state (all data is NaN)
  LifetimeWalk(const arma::vec&, const arma::rowvec&);

  arma::uword n_elem() const; // total number of elements in walk
  double first() const; // get first element in walk
  double last() const; //get last element in walk
  double get_elem(const arma::uword i) const; // get element at position i in data vector (Walk_(i+1))
  double sum_middle_elems() const; //sum all elements which are not first or last. Requires at least 3 elements
  double sum_from_to(const arma::uword from, const arma::uword to) const; //sum all elements which are not first or last. Requires at least 3 elements

protected:
  double val_sum_middle_elems;
};


struct EmptyLifetimeWalk : LifetimeWalk{
  EmptyLifetimeWalk();
  arma::uword n_elem() const;
};

struct TransactionWalk : LifetimeWalk{
  double d1; // d (number of periods to end of covariate period) of the first transcation from which the walk is built
  double tjk;
  TransactionWalk(); // used in vector<>
  TransactionWalk(const arma::vec&, const arma::rowvec&);
};


// Walk(const arma::vec& cov_data, const arma::uword from, const arma::uword to,
//      const double tjk, const double d);

// copy constructor
// Walk(const Walk& other) : tjk(other.tjk), d(other.d){
//   // this->walk_data = arma::vec(other.walk_data);
//
//   this->walk_data = arma::vec(other.walk_data.memptr(), other.walk_data.n_elem);
//   Rcpp::Rcout<<"Copy constructor called."<<std::endl;
// }

// // copy assignment operator
// Walk& operator=(const Walk& t)
// {
//   Rcpp::Rcout << "Assignment operator called " << std::endl;
//   return *this;
// }


// move constructor
//  removes copy assignment operator??
// Walk(Walk&& other) : tjk(other.tjk), d(other.d){
//   this->walk_data = arma::vec(other.walk_data.memptr(), other.walk_data.n_elem);
//   Rcpp::Rcout<<"Move constructor called."<<std::endl;
// }


struct Customer {
  // All members are public to facilitate access & readability

  const double x, t_x, T_cal;
  const double d_omega;

  std::vector<TransactionWalk> real_walks_trans;
  LifetimeWalk real_walk_life, aux_walk_life;
  TransactionWalk aux_walk_trans;


  /*
   * Constructors for customers with real trans walks and without (ie zero-repeaters)
   *  Define two separate constructors because required matrix walkinfo_real_trans
   *  cannot be created by subsetting full matrix with .rows(i,j) as (i,j) is NA / dont exists
   */
  // With real walks (ie not zero-repeaters)
  Customer(const double x, const double t_x, const double T_cal, const double d_omega,
           const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
           const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
           const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans,
           const arma::vec& adj_covdata_real_trans, const arma::mat& walkinfo_real_trans);

   // without real trans walks (ie zero-repeaters)
  Customer(const double x, const double t_x, const double T_cal, const double d_omega,
           const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
           const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
           const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans);

private:
  void set_real_walk_life(const arma::vec&, const arma::rowvec&);
};




double pnbd_dyncov_LL_i_hyp_alpha_ge_beta(const double r, const double s,
                                          const double x,
                                          const double alpha_1, const double beta_1,
                                          const double alpha_2, const double beta_2);


double pnbd_dyncov_LL_i_hyp_beta_g_alpha(const double r, const double s,
                                         const double x,
                                         const double alpha_1, const double beta_1,
                                         const double alpha_2, const double beta_2);

double pnbd_dyncov_LL_i_A1sum(const std::vector<TransactionWalk>& real_walks_trans);

double pnbd_dyncov_LL_i_BjSum(const std::vector<TransactionWalk>& real_walks);

double pnbd_dyncov_LL_i_BkSum(const double Bjsum, const TransactionWalk& aux_walk);

double pnbd_dyncov_LL_i_Bi(const arma::uword i, const double t_x, const TransactionWalk& aux_walk);

double pnbd_dyncov_LL_i_Di(const arma::uword i, const LifetimeWalk& real_walk_life,
                           const LifetimeWalk& aux_walk_life, const double d_omega);


double pnbd_dyncov_LL_i_F2_1(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x, const double dT,
                             const double a1, const double b1,
                             const double A1T, const double C1T);



double pnbd_dyncov_LL_i_F2_2(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x,
                             const double akt, const double bkT,
                             const double aT, const double bT,
                             const double AkT, const double CkT);


double pnbd_dyncov_LL_i_F2(const double r, const double alpha_0, const double s, const double beta_0,
                           const Customer& c,
                           const double B1, const double D1,
                           const double BT, const double DT,
                           const double A1T, const double C1T,
                           const double AkT, const double CkT,
                           const double Bjsum,
                           const bool return_intermediate_results,
                           arma::vec& intermediate_results);

Rcpp::NumericVector pnbd_dyncov_LL_i(const double r, const double alpha_0, const double s, const double beta_0,
                                     const Customer& c,
                                     const double DT,
                                     const double F2_3,
                                     const bool return_intermediate_results);


double pnbd_dyncov_LL_negsum(const arma::vec& params,
                             const arma::vec& X,
                             const arma::vec& t_x,
                             const arma::vec& T_cal,
                             const arma::vec& d_omega,
                             const Rcpp::NumericVector& vN,
                             const arma::vec& walkinfo_trans_from,
                             const arma::vec& walkinfo_trans_to,
                             const arma::vec& walkinfo_life_from,
                             const arma::vec& walkinfo_life_to,
                             const arma::mat& walk_info_life,
                             const arma::mat& walk_info_trans,
                             const arma::mat& cov_data_life,
                             const arma::mat& cov_data_trans);

Rcpp::NumericMatrix pnbd_dyncov_LL_ind(const arma::vec& params,
                                       const arma::vec& X,
                                       const arma::vec& t_x,
                                       const arma::vec& T_cal,
                                       const arma::vec& d_omega,

                                       const arma::mat& walkinfo_aux_life,
                                       const arma::mat& walkinfo_real_life,
                                       const arma::mat& walkinfo_aux_trans,
                                       const arma::mat& walkinfo_real_trans,

                                       const arma::vec& walkinfo_trans_real_from,
                                       const arma::vec& walkinfo_trans_real_to,

                                       const arma::mat& covdata_aux_life,
                                       const arma::mat& covdata_real_life,
                                       const arma::mat& covdata_aux_trans,
                                       const arma::mat& covdata_real_trans,

                                       const bool return_intermediate_results);

#endif

