#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"


arma::vec bgnbd_PAlive(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){
  arma::vec n_term1 = (vA_i/(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r+vX));

  return (1 / (1 + (vX > 0) % n_term1));
}

//' @title BG/NBD: PAlive without covariates
//'
//' @description
//' BG/NBD without Covariates: Calculates the probability of a customer being alive
//' at the end of the calibration period.
//'
//' @param r TODO: describe
//' @param alpha TODO: describe
//' @param a TODO: describe
//' @param b TODO: describe
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r} \code{alpha} \code{a} \code{b} estimated parameters in original scale
//' for the BG/NBD model, namely (r, alpha, a, b).
//' r and alpha: TODO: description.
//' a and b: TODO: description
//'
//'
//'@return
//'Returns a vector with the PAlive for each customer.
//'
//' @name bgnbd_nocov_PAlive
// [[Rcpp::export]]
arma::vec bgnbd_nocov_PAlive(const double r,
                       const double alpha,
                       const double a,
                       const double b,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){

  // Build alpha, a and b --------------------------------------------------------
  //    No covariates: Same alpha, a and b for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i.fill(alpha);
  vA_i.fill(a);
  vB_i.fill(b);

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}

//' @title BG/NBD: PAlive with static covariates
//'
//' @description
//' BG/NBD with Static Covariates: Calculates the probability of a customer
//' being alive (PAlive) at the end of the calibration period.
//'
//' @param r TODO: describe
//' @param alpha TODO: describe
//' @param a TODO: describe
//' @param b TODO: describe
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @details
//' \code{r} r: TODO
//'
//' \code{r} alpha: TODO
//'
//' \code{r} a: TODO
//'
//' \code{r} b: TODO
//'
//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_trans} at the respective position.
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_life} at the respective position.
//'
//'@return
//'Returns a vector containing the PAlive for each customer.
//'
//'@references
//'  TODO
//'
//' @name bgnbd_staticcov_PAlive
//' @rdname bgnbd_staticcov_PAlive
//'
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_PAlive(const double r,
                                 const double alpha,
                                 const double a,
                                 const double b,
                                 const arma::vec& vX,
                                 const arma::vec& vT_x,
                                 const arma::vec& vT_cal,
                                 const arma::vec& vCovParams_trans,
                                 const arma::vec& vCovParams_life,
                                 const arma::mat& mCov_trans,
                                 const arma::mat& mCov_life){
  if(vCovParams_trans.n_elem != mCov_trans.n_cols)
    throw std::out_of_range("Vector of transaction parameters need to have same length as number of columns in transaction covariates!");

  if(vCovParams_life.n_elem != mCov_life.n_cols)
    throw std::out_of_range("Vector of lifetime parameters need to have same length as number of columns in lifetime covariates!");

  if((vX.n_elem != mCov_trans.n_rows) ||
     (vX.n_elem != mCov_life.n_rows))
    throw std::out_of_range("There need to be as many covariate rows as customers!");


  // Build alpha a and b --------------------------------------------
  //  Static covariates: Different alpha, a and b for every customer
  const double n = vX.n_elem;

  arma::vec vAlpha_i(n), vA_i(n), vB_i(n);

  vAlpha_i = alpha * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vA_i  = a * arma::exp((mCov_life * vCovParams_life));
  vB_i  = b * arma::exp((mCov_life * vCovParams_life));

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}
