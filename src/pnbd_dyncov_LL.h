#ifndef PNBD_DYNCOVLL_HPP
#define PNBD_DYNCOVLL_HPP

#include <RcppArmadillo.h>
#include <RcppGSL.h>
#include <gsl/gsl_sf_hyperg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_result.h>


arma::vec pnbd_dyncov_LL_Bi_cpp(const int i,
                                const arma::vec& t_x,
                                const arma::vec& d,
                                const arma::vec& delta,
                                const arma::ivec& n_walks,
                                const arma::vec& max_walks,
                                const arma::mat& walks);


arma::vec pnbd_dyncov_LL_Di1_cpp(const int i,
                                 const arma::vec& d,
                                 const arma::ivec& n_walks,
                                 const arma::vec& max_walks,
                                 const arma::vec& adj_walk1,
                                 const arma::mat& walks,
                                 const arma::ivec& kxT);

arma::vec pnbd_dyncov_LL_Di2_cpp(const int i,
                                   const arma::vec& d,
                                   const arma::vec& d_omega,
                                   const arma::ivec& n_walks,
                                   const arma::vec& max_walks,
                                   const arma::mat& walks,
                                   const arma::ivec& k0x);


arma::vec pnbd_dyncov_LL_Di_cpp(const int i,
                                const arma::vec& real_d,
                                const arma::vec& aux_d,
                                const arma::ivec& real_n_walks,
                                const arma::ivec& aux_n_walks,
                                const arma::vec& real_max_walks,
                                const arma::vec& aux_max_walks,
                                const arma::vec& real_adj_walk1,
                                const arma::mat& real_walks,
                                const arma::mat& aux_walks);


arma::vec hyp_beta_g_alpha_cpp(const arma::vec& alpha_1,
                               const arma::vec& beta_1,
                               const arma::vec& alpha_2,
                               const arma::vec& beta_2,
                               const arma::vec& x,
                               const double r,
                               const double s);

arma::vec hyp_alpha_ge_beta_cpp(const arma::vec& alpha_1,
                                const arma::vec& beta_1,
                                const arma::vec& alpha_2,
                                const arma::vec& beta_2,
                                const arma::vec& x,
                                const double r,
                                const double s);


#endif
