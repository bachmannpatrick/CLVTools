#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"
#include "pnbd_LL_ind.h"


//' @name pnbd_LL
//'
//' @templateVar name_model_full Pareto/NBD
//' @templateVar name_model_short pnbd
//' @templateVar model_params_ordered r, alpha_0, s, beta_0
//' @template template_titleparamsdescriptionreturndetails_LL
//'
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @templateVar name_params_cov_life vParams
//' @templateVar name_params_cov_trans vParams
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_pnbd
//'
arma::vec pnbd_LL_ind(const double r,
                      const double s,
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i,
                      const arma::vec& vX,
                      const arma::vec& vT_x,
                      const arma::vec& vT_cal)
{

  const unsigned int n = vX.n_elem;


  // Param2: s+1 or r+x
  //  save indices as used again
  arma::uvec uvAlphaBetaFindRes = find(vAlpha_i < vBeta_i);
  arma::vec vParam2(n);
  vParam2.fill((s+1));
  vParam2(uvAlphaBetaFindRes) = (r + vX(uvAlphaBetaFindRes));
  // MaxAB
  arma::vec vMaxAB(vAlpha_i);
  vMaxAB(uvAlphaBetaFindRes) = vBeta_i(uvAlphaBetaFindRes);


  // Distinguish between case abs(alpha_i - beta_i) == 0 and != 0
  arma::vec vABabs = arma::abs((vAlpha_i - vBeta_i));
  arma::uvec uvLLFind1 = find(vABabs != 0.0) ;
  arma::uvec uvLLFind2 = find(vABabs == 0.0);

  arma::vec vF1(n), vF2(n), vPartF(n);

  // Calculate Part F for case vABabs != 0 --------------------------------------------------
  vF1(uvLLFind1) = clv::vec_hyp2F1(r + s + vX(uvLLFind1),
      vParam2(uvLLFind1),
      r + s + vX(uvLLFind1) + 1,
      vABabs(uvLLFind1) / (vMaxAB(uvLLFind1) + vT_x(uvLLFind1)));

  vF2(uvLLFind1) = clv::vec_hyp2F1(r + s + vX(uvLLFind1),
      vParam2(uvLLFind1),
      r + s + vX(uvLLFind1) + 1,
      vABabs(uvLLFind1)/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)));

  vF2(uvLLFind1) %= clv::vec_pow((vMaxAB(uvLLFind1) + vT_x(uvLLFind1))/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)),
      r + s + vX(uvLLFind1));

  vPartF(uvLLFind1) = -(r + s + vX(uvLLFind1)) % arma::log(vMaxAB(uvLLFind1) + vT_x(uvLLFind1)) + arma::log(vF1(uvLLFind1) - vF2(uvLLFind1));



  // Calculate Part F for case vABabs == 0 --------------------------------------------------
  vF1(uvLLFind2) = (-1 * (r + s + vX(uvLLFind2))) % arma::log(vMaxAB(uvLLFind2) + vT_x(uvLLFind2));

  vF2(uvLLFind2) = (vMaxAB(uvLLFind2) + vT_x(uvLLFind2)) / (vMaxAB(uvLLFind2) + vT_cal(uvLLFind2));
  vF2(uvLLFind2) %= clv::vec_pow(vF2(uvLLFind2), r + s + vX(uvLLFind2));
  vF2(uvLLFind2) = log(1 - vF2(uvLLFind2));

  vPartF(uvLLFind2) = vF1(uvLLFind2) + vF2(uvLLFind2);



  // Calculate LL ---------------------------------------------------------------------------
  // For numerical stability rewrite
  //  log(exp(a) + exp(b))
  //            as
  //  max(a,b) + log(exp(a-max(a,b)) + exp(b-max(a,b)))
  //
  // There still can be problems with vX as then vPart1 gets too large (lgamma(vX))

  arma::vec vPart1 = r * log(vAlpha_i) + s * log(vBeta_i) - std::lgamma(r) + arma::lgamma(r + vX);
  arma::vec vPart2 = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  arma::vec vPart3 = log(s) - arma::log(r + s + vX) + vPartF;

  arma::vec vMaxPart23 = arma::max(vPart2, vPart3);
  arma::vec vLL = vPart1 + (vMaxPart23 + arma::log(arma::exp(vPart2 - vMaxPart23) +
    arma::exp(vPart3 - vMaxPart23)));

  return (vLL);
}


//' @rdname pnbd_LL
// [[Rcpp::export]]
arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  const double r       = exp(vLogparams(0));
  const double alpha_0 = exp(vLogparams(1));
  const double s       = exp(vLogparams(2));
  const double beta_0  = exp(vLogparams(3));

  const double n = vX.n_elem;



  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = pnbd_nocov_alpha_i(alpha_0, n);
  const arma::vec vBeta_i = pnbd_nocov_beta_i(beta_0, n);


  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer

  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
}


//' @rdname pnbd_LL
// [[Rcpp::export]]
double pnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  arma::vec vLL = pnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  // accu sums all elements, indifferent of axis
  return(-arma::sum(vLL));
}


//' @rdname pnbd_LL
// [[Rcpp::export]]
arma::vec pnbd_staticcov_LL_ind(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  const double no_cov_life  = mCov_life.n_cols;
  const double no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vParams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vParams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vParams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = exp(vModel_log_params(0));
  const double alpha_0  = exp(vModel_log_params(1));
  const double s        = exp(vModel_log_params(2));
  const double beta_0   = exp(vModel_log_params(3));

  const double n = vX.n_elem;



  // Build alpha and beta --------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)
  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vTrans_params, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vLife_params, mCov_life);

  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  return(vLL);
}



//' @rdname pnbd_LL
// [[Rcpp::export]]
double pnbd_staticcov_LL_sum(const arma::vec& vParams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::mat& mCov_life,
                             const arma::mat& mCov_trans){


  // Call and return summed values ----------------------------
  arma::vec vLL = pnbd_staticcov_LL_ind(vParams,
                                        vX,
                                        vT_x,
                                        vT_cal,
                                        mCov_life,
                                        mCov_trans);

  return(-arma::sum(vLL));
}

arma::vec pnbd_nocov_alpha_i(const double alpha_0, const double n){
  return clv::vec_fill(alpha_0, n);
}

arma::vec pnbd_nocov_beta_i(const double beta_0, const double n){
  return clv::vec_fill(beta_0, n);
}

// [[Rcpp::export]]
arma::vec pnbd_staticcov_alpha_i(const double alpha_0,
                                 const arma::vec& vCovParams_trans,
                                 const arma::mat& mCov_trans){
  return alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
}

// [[Rcpp::export]]
arma::vec pnbd_staticcov_beta_i(const double beta_0,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life){
  return beta_0 * arma::exp(((mCov_life  * (-1)) * vCovParams_life));
}
