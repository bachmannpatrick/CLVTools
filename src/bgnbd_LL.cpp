#include "bgnbd_LL.h"




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


  arma::uvec vX_filtered = find(vX > 0);

  arma::vec vA = r * log(alpha) + arma::lgamma(r + vX) - std::lgamma(r) - (r + vX)  * arma::log(alpha + vT_x);

  arma::vec vBX = (b+vX);
  arma::vec vBetaRatio = beta_ratio(a, vBX, a, b);

  arma::vec vAlphaTxByAlphaTcal = (alpha + vT_x)/(alpha / vT_cal);
  arma::vec vRX = (r + vX);
  arma::vec vBxFiltered = (b + (vX(vX_filtered)));
  arma::vec VBxFilteredMinus1 = (vBxFiltered - 1);
  arma::vec vB = vBetaRatio * clv::vec_pow(vAlphaTxByAlphaTcal, vRX) + vX(vX_filtered) * beta_ratio(a + 1 , VBxFilteredMinus1, a, b);

  arma::vec vLL = vA + arma::log(vB);

  return(vLL);
}


//' @title BG/NBD: LogLikelihood without covariates
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
// [[Rcpp::export]]
double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  arma::vec vLL = bgnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  // accu sums all elements, indifferent of axis
  return(-arma::sum(vLL));
}


arma::vec beta_ratio(const double a, arma::vec& b, const double x, const double y){
  return(arma::exp(std::lgamma(a) + arma::lgamma(b) - arma::lgamma(a + b) - std::lgamma(x) - std::lgamma(y) + std::lgamma(x+y)));
}

