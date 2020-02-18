#include <RcppArmadillo.h>
#include "pnbd_nocov_LL_ind.h"

//' @title Pareto/NBD: LogLikelihood without covariates
//'
//' @description
//' Pareto/NBD without Covariates:
//'
//' The function \code{pnbd_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{pnbd_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vLogparams vector with the Pareto/NBD model parameters log scaled
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r, alpha_0, s, beta_0} are the parameters used for estimation.\cr
//' s: shape parameter of the Gamma distribution for the lifetime process.
//' The smaller s, the stronger the heterogeneity of customer lifetimes. \cr
//' beta: scale parameter for the Gamma distribution for the lifetime process. \cr
//' r: shape parameter of the Gamma distribution of the purchase process.
//' The smaller r, the stronger the heterogeneity of the purchase process.\cr
//' alpha: scale parameter of the Gamma distribution of the purchase process.
//'
//'@return
//'  Returns the respective LogLikelihood value for the Pareto/NBD model without covariates.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
double pnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // vLogparams have to be single vector because used by optimizer
  // Call and return summed values
  //

  // arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams,
  //                             const arma::vec& vX,
  //                             const arma::vec& vT_x,
  //                             const arma::vec& vT_cal)

  arma::vec vLL = pnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  // accu sums all(!) element return
  return(-arma::sum(vLL));
}
