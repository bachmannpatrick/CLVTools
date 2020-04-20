#include <RcppArmadillo.h>
#include "ggomnbd_nocov_LL_ind.h"

//' @title GGompertz/NBD: LogLikelihood without covariates
//'
//' @description
//'
//' The function \code{ggomnbd_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{ggomnbd_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vLogparams vector with the GGompertz/NBD model parameters at log scale
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r, alpha_0, b, s, beta_0} are the log()-ed model parameters used for
//' estimation, in this order.\cr
//' \code{s}: shape parameter of the Gamma distribution for the lifetime process.
//' The smaller \code{s}, the stronger the heterogeneity of customer lifetimes.\cr
//' \code{beta}: scale parameter for the Gamma distribution for the lifetime process.\cr
//' \code{b:} scale parameter of the Gompertz distribution (constant across customers).\cr
//' \code{r:} shape parameter of the Gamma distribution of the purchase process.
//' The smaller \code{r}, the stronger the heterogeneity of the pruchase process.\cr
//' \code{alpha}: scale parameter of the Gamma distribution of the purchase process.\cr
//'
//'
//' Ideally, the starting parameters for r and s represent your best guess
//' concerning the heterogeneity of customers in their buy and die rate.
//'
//'@return
//'  Returns the respective LogLikelihood value for the GGompertz/NBD Model without covariates.
//'
//'@template template_rcpp_ggomnbd_reference
//'
// [[Rcpp::export]]
double ggomnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // arma::vec ggomnbd_nocov_LL_ind(const arma::vec& vLogparams,
  //                             const arma::vec& vX,
  //                             const arma::vec& vT_x,
  //                             const arma::vec& vT_cal);
  arma::vec vLL = ggomnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  // accu sums all(!) element return
  return(-arma::sum(vLL));
}

