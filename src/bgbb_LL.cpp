#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "bgbb_LL_ind.h"
using namespace Rcpp;

// Code adapted from BTYD BGBB implementation https://github.com/cran/BTYD/blob/master/R/bgbb.R
// [[Rcpp::depends(RcppArmadillo)]]
//Individual bgbb LL.
arma::vec bgbb_LL_ind(const double alpha,
                       const double beta,
                       const double gamma,
                       const double delta,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal,
                       const arma::vec& vN_cal){
  const unsigned int n = vX.n_elem;

  //TODO: figure out how to build this
  arma::vec vLL;
  double denomAb = R::lbeta(alpha, beta);
  double denomGd = R::lbeta(gamma, delta);

  arma::vec ind_LL_sum = Rcpp::lbeta(alpha + vX, beta + vN_cal - vX) - denomAb + Rcpp::lbeta(gamma, delta + vN_cal) - denomGd;

  return(vLL);
}

//' @rdname bgbb_nocov_LL_sum
// [[Rcpp::export]]
arma::vec bgbb_nocov_LL_ind(const arma::vec& vLogparams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::vec& vN_cal){

  const double alpha = exp(vLogparams(0));
  const double beta  = exp(vLogparams(1));
  const double gamma = exp(vLogparams(2));
  const double delta = exp(vLogparams(3));

  arma::vec vLL = bgbb_LL_ind(alpha, beta, gamma, delta, vX, vT_x, vT_cal, vN_cal);

  return(vLL);
}

//' @title BG/BB: LogLikelihood without covariates
//'
//' @description
//' BG/BB without Covariates:
//'
//' The function \code{bgbb_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{bgbb_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vLogparams vector with the BG/BB model parameters log scaled
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{alpha, beta, gamma, delta} are the parameters used for estimation.\cr
//' TODO: add description of parameters
//'
//'@return
//'  Returns the respective LogLikelihood value for the BG/BB model without covariates.
//'
//'@references
//'
//'  \url{https://github.com/cran/BTYD/}.
//'
// [[Rcpp::export]]
double bgbb_nocov_LL_sum(const arma::vec& vLogparams,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal,
                          const arma::vec& vN_cal){

  arma::vec vLL = bgbb_nocov_LL_ind(vLogparams,
                                     vX,
                                     vT_x,
                                     vT_cal,
                                     vN_cal);

  return(arma::sum(vN_cal % vLL));
}


