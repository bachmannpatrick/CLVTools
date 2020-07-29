#include <RcppArmadillo.h>
#include <math.h>
#include "clv_vectorized.h"


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
  //  save indices because used again
  arma::uvec uvAlphaBetaFindRes = find(vAlpha_i < vBeta_i);
  arma::vec vParam2(n);
  vParam2.fill((s+1));
  vParam2(uvAlphaBetaFindRes) = (r + vX(uvAlphaBetaFindRes));

  // MaxAB
  arma::vec vMaxAB(vAlpha_i);
  vMaxAB(uvAlphaBetaFindRes) = vBeta_i(uvAlphaBetaFindRes);


  // Distinguish between case alpha==beta and alpha != beta <<==>> abs(alpha_i - beta_i) == 0 and != 0
  arma::vec vABabs = arma::abs((vAlpha_i - vBeta_i));
  arma::uvec uvLLFind1 = find(vABabs != 0.0) ;
  arma::uvec uvLLFind2 = find(vABabs == 0.0);

  // arma::vec vF1(n), vF2(n), vPartF(n);

  // Calculate Part F for case vABabs != 0 --------------------------------------------------
  // vF1(uvLLFind1) = clv::vec_hyp2F1(r + s + vX(uvLLFind1),
  //     vParam2(uvLLFind1),
  //     r + s + vX(uvLLFind1) + 1,
  //     vABabs(uvLLFind1) / (vMaxAB(uvLLFind1) + vT_x(uvLLFind1)));
  //
  // vF2(uvLLFind1) = clv::vec_hyp2F1(r + s + vX(uvLLFind1),
  //     vParam2(uvLLFind1),
  //     r + s + vX(uvLLFind1) + 1,
  //     vABabs(uvLLFind1)/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)));
  //
  // vF2(uvLLFind1) %= clv::vec_pow((vMaxAB(uvLLFind1) + vT_x(uvLLFind1))/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)),
  //     r + s + vX(uvLLFind1));
  //
  // vPartF(uvLLFind1) = -(r + s + vX(uvLLFind1)) % arma::log(vMaxAB(uvLLFind1) + vT_x(uvLLFind1)) + arma::log(vF1(uvLLFind1) - vF2(uvLLFind1));
  //
  //
  //
  // // Calculate Part F for case vABabs == 0 -----------------------------------------------------
  // vF1(uvLLFind2) = (-1 * (r + s + vX(uvLLFind2))) % arma::log(vMaxAB(uvLLFind2) + vT_x(uvLLFind2));
  //
  // vF2(uvLLFind2) = (vMaxAB(uvLLFind2) + vT_x(uvLLFind2)) / (vMaxAB(uvLLFind2) + vT_cal(uvLLFind2));
  // vF2(uvLLFind2) %= clv::vec_pow(vF2(uvLLFind2), r + s + vX(uvLLFind2));
  // vF2(uvLLFind2) = log(1 - vF2(uvLLFind2));
  //
  // vPartF(uvLLFind2) = vF1(uvLLFind2) + vF2(uvLLFind2);

  // Log(A0) -------------------------------------------------------------------------------
  // log(A0) = log(a1) + log(Atilde)

  // log(a1) = log((max(a,b) + tx)^(-(r+s+x))) = -(r+s+x) * log(max(a,b) + tx)
  arma::vec vLog_a1 = (-1 * (r + s + vX)) % arma::log(vMaxAB + vT_x);

  // log(Atilde) for alpha != beta
  // log(Atilde) = log(2F1() - (2F1() * (./.)^(rsx)))
  arma::vec vLog_Atilde(n);
  vLog_Atilde(uvLLFind1) = clv::vec_hyp2F1(r + s + vX(uvLLFind1),
                                           vParam2(uvLLFind1),
                                           r + s + vX(uvLLFind1) + 1.0,
                                           vABabs(uvLLFind1) / (vMaxAB(uvLLFind1) + vT_x(uvLLFind1)));

  vLog_Atilde(uvLLFind1) -= (clv::vec_hyp2F1(r + s + vX(uvLLFind1),
                                            vParam2(uvLLFind1),
                                            r + s + vX(uvLLFind1) + 1.0,
                                            vABabs(uvLLFind1)/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)))
                              % clv::vec_pow((vMaxAB(uvLLFind1) + vT_x(uvLLFind1))/(vMaxAB(uvLLFind1) + vT_cal(uvLLFind1)),
                                              r + s + vX(uvLLFind1)));

  vLog_Atilde(uvLLFind1) = arma::log(vLog_Atilde(uvLLFind1));


  // log(Atilde) for alpha == beta
  // log(Atilde) = log(1 - (./.)^(r+s+x))
  vLog_Atilde(uvLLFind2) = arma::log(1.0 - clv::vec_pow((vMaxAB(uvLLFind2) + vT_x(uvLLFind2)) / (vMaxAB(uvLLFind2) + vT_cal(uvLLFind2)),
                                                        r + s + vX(uvLLFind2)));

  arma::vec vLog_A0 = vLog_a1 + vLog_Atilde;

  // Calculate LL ---------------------------------------------------------------------------
  // This is log() of Equation (18) in Fader & Hardie 2005 (A note on deriving...)
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
  //  For large x, A0 as given in (19) and (20) will be (close to) 0.
  //    Rewrite A0 as a1 * Atilde, where
  //      a1      = (max(a,b) + tx)^(-(r+s+x)) and
  //      Atilde  = 2F1() - 2F1() * ((max(a,b)+tx)/(max(a,b)+Tcal))^(r+s+x)
  //    This allows to log() the ^x in a1 and leaves one 2F1 in Atilde free from any x:
  //      log(A0) = log(a1) + log(Atilde)
  //
  //    Additionally, log(Atilde) can be further simplified for the case where alpha=beta:
  //      z=0 and both hyp2F1 = 1 which yields log(Atilde) = log(1 - (./.)^(r+s+x))
  //
  //
  // There still can be problems with vX as then vPart1 gets too large (lgamma(vX))

  arma::vec vLog_x = r * log(vAlpha_i) + s * log(vBeta_i) - std::lgamma(r) + arma::lgamma(r + vX);
  arma::vec vLog_y = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  arma::vec vLog_z = log(s) - arma::log(r + s + vX) + vLog_A0;

  arma::vec vMaxLogYZ = arma::max(vLog_y, vLog_z);

  arma::vec vLL = vLog_x + (vMaxLogYZ + arma::log(arma::exp(vLog_y - vMaxLogYZ) + arma::exp(vLog_z - vMaxLogYZ)));

  // Special case: Tcal = t.x
  arma::uvec vTcalEqualTx = find(vT_cal == vT_x);
  vLL(find(vT_cal == vT_x)) = vLog_x(vTcalEqualTx) + vLog_y(vTcalEqualTx);

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
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);


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
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vLife_params));

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
