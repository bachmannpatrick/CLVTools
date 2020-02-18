#include <RcppArmadillo.h>
#include "pnbd_staticcov_LL_ind.h"

//' @rdname pnbd_staticcov_LL_ind
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
double pnbd_staticcov_LL_sum(const arma::vec& vParams,
                     const arma::vec& vX,
                     const arma::vec& vT_x,
                     const arma::vec& vT_cal,
                     const arma::mat& mCov_life,
                     const arma::mat& mCov_trans){

  // vParams have to be single vector because used by optimizer
  // Call and return summed values
  //
  // double pnbd_staticcov_LL_ind(const arma::vec& vParams,
  //                              const arma::vec& vX,
  //                              const arma::vec& vT_x,
  //                              const arma::vec& vT_cal,
  //                              const arma::mat& mCov_life,
  //                              const arma::mat& mCov_trans);
  // ----------------------------------------------------------------

  arma::vec vLL = pnbd_staticcov_LL_ind(vParams,
                                        vX,
                                        vT_x,
                                        vT_cal,
                                        mCov_life,
                                        mCov_trans);

  return(-arma::sum(vLL));
}
