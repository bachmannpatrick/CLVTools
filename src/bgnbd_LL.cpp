#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"

arma::vec beta_ratio(const double a, const arma::vec& b, const double x, const double y);


//' @rdname bgnbd_nocov_LL_sum
// [[Rcpp::export]]
arma::vec bgnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  const double r       = exp(vLogparams(0));
  const double alpha   = exp(vLogparams(1));
  const double a       = exp(vLogparams(2));
  const double b       = exp(vLogparams(3));

  const unsigned int n = vX.n_elem;

  arma::vec vA(n), vB(n), vBetaRatio(n);

  vA = r * log(alpha) + arma::lgamma(r + vX) - std::lgamma(r) - (r + vX) % arma::log(alpha + vT_x);

  vB = beta_ratio(a, (b+vX), a, b) % clv::vec_pow((alpha + vT_x)/(alpha + vT_cal), (r + vX)) + ((vX > 0)) % beta_ratio(a + 1 , (b + vX - 1), a, b);

  arma::vec vLL = vA + arma::log(vB);

  return(vLL);
}


//' @title BG/NBD: LogLikelihood without covariates
//'
//' @description
//' BG/NBD without Covariates:
//'
//' The function \code{bgnbd_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{bgnbd_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vLogparams vector with the Pareto/NBD model parameters log scaled
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r, alpha, a, b} are the parameters used for estimation.\cr
//' TODO: add description of parameters
//'
//'@return
//'  Returns the respective LogLikelihood value for the BG/NBD model without covariates.
//'
//'@references
//'
//'  \url{https://github.com/cran/BTYD/}.
//'
// [[Rcpp::export]]
double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  arma::vec vLL = bgnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  return(-arma::sum(vLL));
}

arma::vec beta_ratio(const double a, const arma::vec& b, const double x, const double y){
  return(arma::exp(std::lgamma(a) + arma::lgamma(b) - arma::lgamma(a + b) - std::lgamma(x) - std::lgamma(y) + std::lgamma(x+y)));
}


