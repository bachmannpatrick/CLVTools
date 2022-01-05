#include "pnbd.h"


//' @name pnbd_CET
//'
//' @templateVar name_model_full Pareto/NBD
//' @templateVar name_model_short pnbd
//' @template template_titledescriptionreturn_CET
//'
//' @template template_params_pnbd
//' @template template_params_rcppperiods
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_pnbd
//'
arma::vec pnbd_CET(const double r,
                   const double s,
                   const double dPeriods,
                   const arma::vec& vX,
                   const arma::vec& vT_cal,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i,
                   const arma::vec& vPAlive){

  const arma::vec vP1 = (r + vX) % (vBeta_i + vT_cal) / ((vAlpha_i + vT_cal) * (s-1));
  const arma::vec vP2 = (1 - arma::pow((vBeta_i + vT_cal) / (vBeta_i + vT_cal + dPeriods), (s-1)));
  const arma::vec vP3 = vPAlive;

  return vP1 % vP2 % vP3;
}



//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_nocov_CET(const double r,
                         const double alpha_0,
                         const double s,
                         const double beta_0,
                         const double dPeriods,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vX.n_elem);
  const arma::vec vBeta_i = clv::vec_fill(beta_0, vX.n_elem);


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(r, s,
                                        vX, vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(r,
                  s,
                  dPeriods,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}





//' @rdname pnbd_CET
// [[Rcpp::export]]
arma::vec pnbd_staticcov_CET(const double r,
                             const double alpha_0,
                             const double s,
                             const double beta_0,
                             const double dPeriods,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::vec& vCovParams_trans,
                             const arma::vec& vCovParams_life,
                             const arma::mat& mCov_trans,
                             const arma::mat& mCov_life){

  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer
  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = pnbd_PAlive(r, s,
                                        vX, vT_x, vT_cal,
                                        vAlpha_i, vBeta_i);


  // Calculate CET -----------------------------------------------------------------
  return(pnbd_CET(r, s,
                  dPeriods,
                  vX, vT_cal,
                  vAlpha_i, vBeta_i,
                  vPAlive));
}




//' @name pnbd_DERT
//'
//' @title Pareto/NBD: Discounted Expected Residual Transactions
//'
//' @description
//' Calculates the discounted expected residual transactions.
//'
//' \itemize{
//' \item{\code{pnbd_nocov_DERT}}{ Discounted expected residual transactions for the Pareto/NBD model without covariates}
//' \item{\code{pnbd_staticcov_DERT}}{ Discounted expected residual transactions for the Pareto/NBD model with static covariates}
//' }
//'
//' @template template_params_pnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//' @param continuous_discount_factor continuous discount factor to use
//'
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @return
//' Returns a vector with the DERT for each customer.
//'
//' @template template_references_pnbd
//'
//'
arma::vec pnbd_DERT_ind(const double r,
                        const double s,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const double continuous_discount_factor){


  // Calculate LL ----------------------------------------------------
  //  Calculate value for every customer
  const arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  const arma::vec vZ = continuous_discount_factor * (vBeta_i + vT_cal);
  const arma::vec vTerm = clv::vec_x_kummerU(s, s, vZ);

  const arma::vec vDERT = arma::exp(
    r * arma::log(vAlpha_i)
    + s * arma::log(vBeta_i)
    + (s-1) * std::log(continuous_discount_factor)
    + arma::lgamma(r + vX + 1)
    + arma::log(vTerm)
    - std::lgamma(r)
    - (r + vX + 1) % arma::log(vAlpha_i + vT_cal)
    - vLL); // dont log as not exp()ed when receiving from pnbd_LL_ind!

    return vDERT;
}



//' @rdname pnbd_DERT
// [[Rcpp::export]]
arma::vec pnbd_nocov_DERT(const double r,
                          const double alpha_0,
                          const double s,
                          const double beta_0,
                          const double continuous_discount_factor,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal){

  // Build alpha and beta -------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vX.n_elem);
  const arma::vec vBeta_i = clv::vec_fill(beta_0, vX.n_elem);

  // Calculate DERT -------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}



//' @rdname pnbd_DERT
// [[Rcpp::export]]
arma::vec pnbd_staticcov_DERT(const double r,
                              const double alpha_0,
                              const double s,
                              const double beta_0,
                              const double continuous_discount_factor,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans,
                              const arma::vec& vCovParams_life,
                              const arma::vec& vCovParams_trans){

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer

  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);


  // Calculate DERT --------------------------------------------------
  return pnbd_DERT_ind(r, s,
                       vAlpha_i, vBeta_i,
                       vX, vT_x, vT_cal,
                       continuous_discount_factor);
}




//' @name pnbd_expectation
//' @title Pareto/NBD: Unconditional Expectation
//'
//' @template template_expectation_description
//'
//' @template template_params_pnbd
//' @template template_expectation_params
//' @param vAlpha_i Vector of individual parameters alpha
//' @param vBeta_i Vector of individual parameters beta
//'
//'
//' @template template_references_pnbd
//'
//' @template template_expectation_return
//'
arma::vec pnbd_expectation(const double r,
                           const double s,
                           const arma::vec& vAlpha_i,
                           const arma::vec& vBeta_i,
                           const arma::vec& vT_i){
  return (r * vBeta_i) / (vAlpha_i * (s - 1)) % (1 -arma::pow((vBeta_i/(vBeta_i + vT_i)), (s - 1)));
}

//' @rdname pnbd_expectation
// [[Rcpp::export]]
arma::vec pnbd_nocov_expectation(const double r,
                                 const double s,
                                 const double alpha_0,
                                 const double beta_0,
                                 const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vT_i.n_elem);
  const arma::vec vBeta_i = clv::vec_fill(beta_0, vT_i.n_elem);

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}

//' @rdname pnbd_expectation
// [[Rcpp::export]]
arma::vec pnbd_staticcov_expectation(const double r,
                                     const double s,
                                     const arma::vec& vAlpha_i,
                                     const arma::vec& vBeta_i,
                                     const arma::vec& vT_i){

  return(pnbd_expectation(r,
                          s,
                          vAlpha_i,
                          vBeta_i,
                          vT_i));
}




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

  const arma::uword n = vX.n_elem;

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

  arma::vec vHyp2f1ParamB = clv::vec_fill(s + 1.0, n);
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

  arma::vec vLog_x = r * arma::log(vAlpha_i) + s * arma::log(vBeta_i) - std::lgamma(r) + arma::lgamma(r + vX);
  arma::vec vLog_y = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  arma::vec vLog_z = std::log(s) - arma::log(r + s + vX) + vLog_A0;

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

  const double r       = std::exp(vLogparams(0));
  const double alpha_0 = std::exp(vLogparams(1));
  const double s       = std::exp(vLogparams(2));
  const double beta_0  = std::exp(vLogparams(3));

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vX.n_elem);
  const arma::vec vBeta_i = clv::vec_fill(beta_0, vX.n_elem);

  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
}


//' @rdname pnbd_LL
// [[Rcpp::export]]
double pnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  const arma::vec vLL = pnbd_nocov_LL_ind(vLogparams,
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

  const arma::uword no_cov_life  = mCov_life.n_cols;
  const arma::uword no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vParams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vParams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vParams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = std::exp(vModel_log_params(0));
  const double alpha_0  = std::exp(vModel_log_params(1));
  const double s        = std::exp(vModel_log_params(2));
  const double beta_0   = std::exp(vModel_log_params(3));

  // Build alpha and beta --------------------------------------------
  //    With static covariates: alpha and beta different per customer
  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vTrans_params, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vLife_params, mCov_life);

  const arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

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
  const arma::vec vLL = pnbd_staticcov_LL_ind(vParams,
                                              vX,
                                              vT_x,
                                              vT_cal,
                                              mCov_life,
                                              mCov_trans);

  return(-arma::sum(vLL));
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



//' @name pnbd_PAlive
//'
//' @templateVar name_model_full Pareto/NBD
//' @templateVar name_model_short pnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_pnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_pnbd
//'
arma::vec pnbd_PAlive( const double r,
                       const double s,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vBeta_i){

  const arma::vec vLL = pnbd_LL_ind(r,
                                    s,
                                    vAlpha_i,
                                    vBeta_i,
                                    vX,
                                    vT_x,
                                    vT_cal);

  const arma::vec vF1 = arma::lgamma(r+vX) - std::lgamma(r) + r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) +
    vX % (-arma::log(vAlpha_i + vT_cal)) + s*(arma::log(vBeta_i) - arma::log(vBeta_i+vT_cal));

  const arma::vec vLogPAlive = vF1 - vLL;

  return(arma::exp(vLogPAlive));
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_nocov_PAlive(const double r,
                            const double alpha_0,
                            const double s,
                            const double beta_0,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vX.n_elem);
  const arma::vec vBeta_i = clv::vec_fill(beta_0, vX.n_elem);


  // Calculate PAlive -------------------------------------------------------------
  return pnbd_PAlive(r,
                     s,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}



//' @rdname pnbd_PAlive
// [[Rcpp::export]]
arma::vec pnbd_staticcov_PAlive(const double r,
                                const double alpha_0,
                                const double s,
                                const double beta_0,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_trans,
                                const arma::mat& mCov_life){


  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer

  const arma::vec vAlpha_i = pnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = pnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  // Calculate PAlive -------------------------------------------------
  return pnbd_PAlive(r,
                     s,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}



//' @name pnbd_pmf
//' @templateVar name_model_full Pareto/NBD
//' @template template_pmf_titledescreturnpmfparams
//' @template template_params_pnbd
//' @param vAlpha_i Vector of individual parameters alpha.
//' @param vBeta_i Vector of individual parameters beta.
//' @template template_references_pnbd
//'
arma::vec pnbd_PMF(const double r,
                   const double s,
                   const unsigned int x,
                   const arma::vec& vT_i,
                   const arma::vec& vAlpha_i,
                   const arma::vec& vBeta_i){

  // replace log(factorial(n)) with lgamma(n+1)
  const double vlogPart1_1 = std::lgamma(r + x) - std::lgamma(r) - std::lgamma(x+1);
  const arma::vec vLogPart1_2 = r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_i));
  const arma::vec vLogPart1_3 = x * (arma::log(vT_i) - arma::log(vAlpha_i + vT_i));
  const arma::vec vLogPart1_4 = s * (arma::log(vBeta_i) - arma::log(vBeta_i + vT_i));
  const arma::vec vPart1 = arma::exp(vlogPart1_1 + vLogPart1_2 + vLogPart1_3 + vLogPart1_4);

  const arma::vec vLogPart2 = r*arma::log(vAlpha_i) + s*arma::log(vBeta_i) + clv::lbeta(r+x, s+1.0) - clv::lbeta(r,s);


  const arma::vec vAbsAB = arma::abs(vAlpha_i - vBeta_i);
  const arma::vec vMaxAB = arma::max(vAlpha_i, vBeta_i);
  const arma::vec vRS = clv::vec_fill(r+s, vAlpha_i.n_elem);
  const arma::vec vRSX1 = vRS + x + 1.0;

  arma::vec vHypArgB = clv::vec_fill(r + x, vAlpha_i.n_elem);
  vHypArgB(find(vAlpha_i >= vBeta_i)).fill(s + 1);

  const arma::vec vB1 = clv::vec_hyp2F1(vRS, vHypArgB, vRSX1, vAbsAB/vMaxAB) / arma::pow(vMaxAB, r+s);

  arma::vec vB2total(arma::size(vAlpha_i), arma::fill::zeros);
  arma::vec vRSI(arma::size(vAlpha_i), arma::fill::zeros);
  arma::vec vLogB2part;

  for(unsigned int i=0; i<=x; i++){
    // It is important to do B2 at log-scale because B2part gets very larger and B2i very small
    //  replace log(factorial(n)) with lgamma(n+1)
    vLogB2part = std::lgamma(r+s+i) + i*arma::log(vT_i) - std::lgamma(r+s) - std::lgamma(i+1);
    vRSI.fill(r+s+i);
    vB2total += arma::exp(vLogB2part + arma::log(clv::vec_hyp2F1(vRSI, vHypArgB, vRSX1, vAbsAB/(vMaxAB+vT_i))) - (r+s+i)*arma::log(vMaxAB+vT_i));
  }

  return(vPart1 + arma::exp(vLogPart2 + arma::log(vB1 - vB2total)));
}


//' @rdname pnbd_pmf
// [[Rcpp::export]]
arma::vec pnbd_nocov_PMF(const double r,
                         const double alpha_0,
                         const double s,
                         const double beta_0,
                         const unsigned int x,
                         const arma::vec& vT_i){

    const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vT_i.n_elem);
    const arma::vec vBeta_i = clv::vec_fill(beta_0, vT_i.n_elem);

    return(pnbd_PMF(r, s, x, vT_i, vAlpha_i, vBeta_i));
}


//' @rdname pnbd_pmf
// [[Rcpp::export]]
arma::vec pnbd_staticcov_PMF(const double r,
                             const double s,
                             const unsigned int x,
                             const arma::vec& vAlpha_i,
                             const arma::vec& vBeta_i,
                             const arma::vec& vT_i){

  return(pnbd_PMF(r, s, x, vT_i, vAlpha_i, vBeta_i));
}
