#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// #include "CLVTool_math.h"
//lbeta:=lgamma(a) + lgamma(b) - lgamma(a+b)

double lbeta(double a, double b){
  return (lgamma(a) + lgamma(b) - lgamma(a+b));
}

//' @title Gamma-Gamma: Log-Likelihood Function
//'
//' @description
//' Calculates the Log-Likelihood value for the Gamma-Gamma model.
//'
//' @param vLogparams a vector containing the log of the parameters p, q, gamma
//' @param vX frequency vector of length n counting the numbers of purchases
//' @param vM_x the observed average spending for every customer during the calibration time.
//'
//' @details
//' \code{vLogparams} is a vector with the parameters for the Gamma-Gamma model.
//' It has three parameters (p, q, gamma). The scale parameter for each transaction
//' is distributed across customers according to a gamma distribution with
//' parameters q (shape) and gamma (scale).
//'
//'@return
//' Returns the Log-Likelihood value for the Gamma-Gamma model.
//'
//' @template template_rcpp_gg_reference
//'
//'
//' @name gg_LL
//' @rdname gg_LL
// [[Rcpp::export]]
double gg_LL( const arma::vec& vLogparams,
              const arma::vec& vX,
              const arma::vec& vM_x
              )
{

  const double p = std::exp(vLogparams(0));
  const double q = std::exp(vLogparams(1));
  const double gamma = std::exp(vLogparams(2));

  const unsigned int n = vX.n_elem;

  arma::vec vLL(n, arma::fill::zeros);

//   non.zero <- which(x > 0 & m.x > 0)
// #Calculate the likelood for all ≠0 values
//     LL[non.zero] <- (-lbeta(p * x[non.zero], q) + q * log(gamma) + (p * x[non.zero] - 1) * log(m.x[non.zero]) + (p * x[non.zero]) *
//       log(x[non.zero]) - (p * x[non.zero] + q) * log(gamma + m.x[non.zero] * x[non.zero]))

  arma::uvec vNonZero = find( (vX != 0.0) && (vM_x != 0.0));

  //lbeta cannot be vectorized. Everything else do vectorized, loop lbeta afterwards.
  //all the multiplications in R are elementwise
  // % := element wise multiplication
  vLL(vNonZero) = q * log(gamma)
              + ((p * vX(vNonZero) - 1 ) % arma::log(vM_x(vNonZero)))
              + ((p * vX(vNonZero) ) % arma::log(vX(vNonZero)))
              - (p * vX(vNonZero) + q) % arma::log(gamma + vM_x(vNonZero) % vX(vNonZero) );

  // -lbeta(p * x[non.zero], q)
  for( arma::uvec::iterator it = vNonZero.begin(); it != vNonZero.end(); it++)
  {
    vLL(*it) += -lbeta(p * vX(*it), q);
  }


// #Sum all LL-values and make them negative (we are maximizing)
//   LL <- -(1 * sum(LL))
  const double LL = -1 * arma::sum(vLL);

  return LL;

}


