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

  // This is log() of Equation (18) in Fader & Hardie 2005 ("A Note on Deriving the Pareto/NBD Modeland Related Expressions")
  //
  // (18):  log(L) = log(x) + log({y + z})
  //
  // For numerical stability rewrite
  //  log({y + z}) as log({exp(log(y)) + exp(log(z))})
  //
  //  on which the Log-Sum-of-Exponents trick can be applied:
  //    log(exp(a) + exp(b))
  //              as
  //    max(a,b) + log(exp(a-max(a,b)) + exp(b-max(a,b)))
  //
  // log(y) is allowed because y>0 for all inputs
  //
  // However, log(z) where z = (s/(r+s+x) * A0) is illegal for A0=0.
  //  This is the case for Tcal=t.x.
  //    For this case, the LL simplifies to log(x)+log(y) because z = 0.
  //
  //  For large x, A0 as given in (19) and (20) will be (close to) 0
  //    Rewrite A0 as a1 * Atilde, where
  //      a1      = (max(alpha,beta) + tx)^(-(r+s+x))
  //      Atilde  = 2F1() - 2F1() * ((max(alpha,beta)+tx)/(max(alpha,beta)+Tcal))^(r+s+x)
  //    This allows to log() the ^x in a1 and leaves one 2F1 in Atilde free from any x:
  //      log(A0) = log(a1) + log(Atilde)
  //
  //    Additionally, log(Atilde) can be simplified for the case where alpha=beta:
  //      z=0 and both hyp2F1 = 1 which yields log(Atilde) = log(1 - (./.)^(r+s+x))
  //
  //
  // There still can be problems for large x in lgamma(x)

  const unsigned int n = vX.n_elem;

  // Log(A0) -------------------------------------------------------------------------------
  // log(A0) = log(a1) + log(Atilde)

  // . log(Atilde) -------------------------------------------------------------------------
  arma::vec vLog_Atilde(n);

  // alpha > beta or alpha < beta:
  // Instead of subsetting many vectors, fill vectors with required params and execute once
  //    2F1 2nd param b:
  //      alpha > beta: s + 1
  //      alpha < beta: r + x
  //    2F1 4th param z: Use larger, subtract smaller
  //      use Max(alpha, beta) and abs(alpha-beta)

  arma::vec vHyp2f1ParamB(n);
  vHyp2f1ParamB.fill(s + 1.0);
  arma::uvec uvAlphaSmallerBeta = find(vAlpha_i < vBeta_i);
  vHyp2f1ParamB(uvAlphaSmallerBeta) = (r + vX(uvAlphaSmallerBeta));

  arma::vec vABabs = arma::abs((vAlpha_i - vBeta_i));
  arma::vec vMaxAB(vAlpha_i);
  vMaxAB(uvAlphaSmallerBeta) = vBeta_i(uvAlphaSmallerBeta);


  // Distinguish between case alpha==beta and alpha != beta
  arma::uvec uvAlphaEqBeta = find(vAlpha_i == vBeta_i);
  arma::uvec uvAlphaNeqBeta = find(vAlpha_i != vBeta_i);

  // .. log(Atilde) for alpha != beta ------------------------------------------------------
  // log(Atilde) = log(2F1() - (2F1() * (./.)^(rsx)))
  vLog_Atilde(uvAlphaNeqBeta) = clv::vec_hyp2F1(r + s + vX(uvAlphaNeqBeta),
                                               vHyp2f1ParamB(uvAlphaNeqBeta),
                                               r + s + vX(uvAlphaNeqBeta) + 1.0,
                                               vABabs(uvAlphaNeqBeta) / (vMaxAB(uvAlphaNeqBeta) + vT_x(uvAlphaNeqBeta)));

  vLog_Atilde(uvAlphaNeqBeta) -= (clv::vec_hyp2F1(r + s + vX(uvAlphaNeqBeta),
                                                  vHyp2f1ParamB(uvAlphaNeqBeta),
                                                  r + s + vX(uvAlphaNeqBeta) + 1.0,
                                                  vABabs(uvAlphaNeqBeta) / (vMaxAB(uvAlphaNeqBeta) + vT_cal(uvAlphaNeqBeta)))
                              % clv::vec_pow((vMaxAB(uvAlphaNeqBeta) + vT_x(uvAlphaNeqBeta))/(vMaxAB(uvAlphaNeqBeta) + vT_cal(uvAlphaNeqBeta)),
                                              r + s + vX(uvAlphaNeqBeta)));

  vLog_Atilde(uvAlphaNeqBeta) = arma::log(vLog_Atilde(uvAlphaNeqBeta));


  // .. log(Atilde) for alpha == beta --------------------------------------------------------
  // log(Atilde) = log(1 - (./.)^(r+s+x))
  vLog_Atilde(uvAlphaEqBeta) = arma::log(1.0 - clv::vec_pow((vMaxAB(uvAlphaEqBeta) + vT_x(uvAlphaEqBeta)) / (vMaxAB(uvAlphaEqBeta) + vT_cal(uvAlphaEqBeta)),
                                                        r + s + vX(uvAlphaEqBeta)));


  // . log(a1) ------------------------------------------------------------------------------
  // log(a1) = log((max(alpha,beta) + tx)^(-(r+s+x))) = -(r+s+x) * log(max(alpha,beta) + tx)
  arma::vec vLog_a1 = (-1 * (r + s + vX)) % arma::log(vMaxAB + vT_x);

  // . log(A0) ------------------------------------------------------------------------------
  // log(A0) = log(a1) + log(Atilde)
  arma::vec vLog_A0 = vLog_a1 + vLog_Atilde;


  // Calculate LL ---------------------------------------------------------------------------
  // log(L) = log(x) + log({y + z})
  // => log(L) = log(x) + max(log(y), log(z)) + log(exp(log(y)-max(log(y),log(z))) + exp(log(z)-max(log(y),log(z))))
  //    For t.x=Tcal: log(L)  = log(x) + log(y)

  arma::vec vLog_x = r * log(vAlpha_i) + s * log(vBeta_i) - std::lgamma(r) + arma::lgamma(r + vX);
  arma::vec vLog_y = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  arma::vec vLog_z = log(s) - arma::log(r + s + vX) + vLog_A0;

  arma::vec vMaxLogYZ = arma::max(vLog_y, vLog_z);

  arma::vec vLL = vLog_x + (vMaxLogYZ + arma::log(arma::exp(vLog_y - vMaxLogYZ) + arma::exp(vLog_z - vMaxLogYZ)));

  // Special case: Tcal = t.x
  arma::uvec vTcalEqualTx = find(vT_cal == vT_x);
  vLL(vTcalEqualTx) = vLog_x(vTcalEqualTx) + vLog_y(vTcalEqualTx);

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

  // Build alpha and beta --------------------------------------------
  //    With static covariates: alpha and beta different per customer
  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vTrans_params, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vLife_params, mCov_life);

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
