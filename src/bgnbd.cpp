#include "bgnbd.h"

//' @name bgnbd_CET
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @template template_titledescriptionreturn_CET
//'
//' @template template_params_bgnbd
//' @template template_params_rcppperiods
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_CET(const double r,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const double dPeriods,
                    const arma::vec& vX,
                    const arma::vec& vT_x,
                    const arma::vec& vT_cal){

  const arma::vec term1 = ((vA_i + vB_i + vX - 1) / (vA_i - 1));
  const arma::vec term2 = 1 - clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_cal + dPeriods), (r + vX)) % clv::vec_hyp2F1((r + vX), (vB_i + vX), (vA_i + vB_i + vX - 1), dPeriods / (vAlpha_i + vT_cal + dPeriods));
  const arma::vec term3 = 1 + (vX > 0) % (vA_i /(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r + vX));

  return term1 % term2 / term3;
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_nocov_CET(const double r,
                          const double alpha,
                          const double a,
                          const double b,
                          const double dPeriods,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal){

  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vA_i = clv::vec_fill(a, vX.n_elem);
  const arma::vec vB_i = clv::vec_fill(b, vX.n_elem);
  const arma::vec vAlpha_i = clv::vec_fill(alpha, vX.n_elem);

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, dPeriods, vX, vT_x, vT_cal);
}

//' @rdname bgnbd_CET
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_CET(const double r,
                              const double alpha,
                              const double a,
                              const double b,
                              const double dPeriods,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::vec& vCovParams_trans,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_trans,
                              const arma::mat& mCov_life){

  const arma::vec vAlpha_i = bgnbd_staticcov_alpha_i(alpha, vCovParams_trans, mCov_trans);
  const arma::vec vA_i  = bgnbd_staticcov_a_i(a, vCovParams_life, mCov_life);
  const arma::vec vB_i  = bgnbd_staticcov_b_i(b, vCovParams_life, mCov_life);

  return bgnbd_CET(r, vAlpha_i, vA_i, vB_i, dPeriods, vX, vT_x, vT_cal);
}




//' @name bgnbd_expectation
//' @title BG/NBD: Unconditional Expectation
//'
//' @template template_expectation_description
//'
//' @template template_params_bgnbd
//' @template template_expectation_params
//' @param vAlpha_i Vector of individual parameters alpha
//' @param vA_i Vector of individual parameters a
//' @param vB_i Vector of individual parameters b
//'
//' @template template_references_bgnbd
//'
//' @template template_expectation_return
//'
arma::vec bgnbd_expectation(const double r,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vA_i,
                            const arma::vec& vB_i,
                            const arma::vec& vT_i){
  const arma::vec vR = clv::vec_fill(r, vAlpha_i.n_elem);

  const arma::vec term1 = (vA_i + vB_i - 1)/(vA_i - 1);
  const arma::vec term2 = arma::pow((vAlpha_i / (vAlpha_i + vT_i)),r);
  const arma::vec term3 = clv::vec_hyp2F1(vR, vB_i, vA_i+vB_i-1, vT_i/(vAlpha_i+vT_i));

  return term1 % (1 - term2 % term3);
}

//' @rdname bgnbd_expectation
// [[Rcpp::export]]
arma::vec bgnbd_nocov_expectation(const double r,
                                  const double alpha,
                                  const double a,
                                  const double b,
                                  const arma::vec& vT_i){

  // Build alpha and beta --------------------------------------------------------
  const arma::vec vA_i = clv::vec_fill(a, vT_i.n_elem);
  const arma::vec vB_i = clv::vec_fill(b, vT_i.n_elem);
  const arma::vec vAlpha_i = clv::vec_fill(alpha, vT_i.n_elem);

  return bgnbd_expectation(r,
                           vAlpha_i,
                           vA_i,
                           vB_i,
                           vT_i);
}

//' @rdname bgnbd_expectation
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_expectation(const double r,
                                      const arma::vec& vAlpha_i,
                                      const arma::vec& vA_i,
                                      const arma::vec& vB_i,
                                      const arma::vec& vT_i){

  return(bgnbd_expectation(r,
                           vAlpha_i,
                           vA_i,
                           vB_i,
                           vT_i));
}





//' @name bgnbd_PAlive
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_bgnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_PAlive(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){
  const arma::vec n_term1 = (vA_i/(vB_i + vX - 1)) % clv::vec_pow((vAlpha_i + vT_cal)/(vAlpha_i + vT_x), (r+vX));

  return (1 / (1 + (vX > 0) % n_term1));
}

//' @rdname bgnbd_PAlive
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
  const arma::vec vA_i = clv::vec_fill(a, vX.n_elem);
  const arma::vec vB_i = clv::vec_fill(b, vX.n_elem);
  const arma::vec vAlpha_i = clv::vec_fill(alpha, vX.n_elem);

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}

//' @rdname bgnbd_PAlive
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

  const arma::vec vAlpha_i = bgnbd_staticcov_alpha_i(alpha, vCovParams_trans, mCov_trans);
  const arma::vec vA_i  = bgnbd_staticcov_a_i(a, vCovParams_life, mCov_life);
  const arma::vec vB_i  = bgnbd_staticcov_b_i(b, vCovParams_life, mCov_life);

  return bgnbd_PAlive(r,
                      vAlpha_i,
                      vA_i,
                      vB_i,
                      vX,
                      vT_x,
                      vT_cal);
}





//' @name bgnbd_LL
//'
//' @templateVar name_model_full BG/NBD
//' @templateVar name_model_short bgnbd
//' @templateVar model_params_ordered r, alpha_0, a, b
//' @template template_titleparamsdescriptionreturndetails_LL
//'
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @templateVar name_params_cov_life vLogparams
//' @templateVar name_params_cov_trans vLogparams
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_bgnbd
//'
arma::vec bgnbd_LL_ind(const double r,
                       const arma::vec& vAlpha_i,
                       const arma::vec& vA_i,
                       const arma::vec& vB_i,
                       const arma::vec& vX,
                       const arma::vec& vT_x,
                       const arma::vec& vT_cal){

  const arma::vec vPart1 = r * arma::log(vAlpha_i) + arma::lgamma(r + vX) - std::lgamma(r) - (r + vX) % arma::log(vAlpha_i + vT_x);

  const arma::vec vPart2 = beta_ratio(vA_i, (vB_i+vX), vA_i, vB_i) % clv::vec_pow((vAlpha_i + vT_x)/(vAlpha_i + vT_cal), (r + vX)) + ((vX > 0)) % beta_ratio(vA_i + 1 , (vB_i + vX - 1), vA_i, vB_i);

  const arma::vec vLL = vPart1 + arma::log(vPart2);

  return(vLL);
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
arma::vec bgnbd_nocov_LL_ind(const arma::vec& vLogparams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal){

  const double r         = std::exp(vLogparams(0));
  const double alpha_0   = std::exp(vLogparams(1));
  const double a_0       = std::exp(vLogparams(2));
  const double b_0       = std::exp(vLogparams(3));

  const arma::vec vA_i = clv::vec_fill(a_0, vX.n_elem);
  const arma::vec vB_i = clv::vec_fill(b_0, vX.n_elem);
  const arma::vec vAlpha_i = clv::vec_fill(alpha_0, vX.n_elem);

  arma::vec vLL = bgnbd_LL_ind(r, vAlpha_i, vA_i, vB_i, vX, vT_x, vT_cal);

  return(vLL);
}


//' @rdname bgnbd_LL
// [[Rcpp::export]]
double bgnbd_nocov_LL_sum(const arma::vec& vLogparams,
                          const arma::vec& vX,
                          const arma::vec& vT_x,
                          const arma::vec& vT_cal,
                          const arma::vec& vN){

  const arma::vec vLL = bgnbd_nocov_LL_ind(vLogparams,
                                           vX,
                                           vT_x,
                                           vT_cal);

  return(-arma::sum(vLL % vN));
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_LL_ind(const arma::vec& vParams,
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
  const double a_0      = std::exp(vModel_log_params(2));
  const double b_0      = std::exp(vModel_log_params(3));


  // Build alpha, a and b, per customer ------------------------------
  const arma::vec vAlpha_i = bgnbd_staticcov_alpha_i(alpha_0, vTrans_params, mCov_trans);
  const arma::vec vA_i  = bgnbd_staticcov_a_i(a_0, vLife_params, mCov_life);
  const arma::vec vB_i  = bgnbd_staticcov_b_i(b_0, vLife_params, mCov_life);


  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer
  const arma::vec vLL = bgnbd_LL_ind(r, vAlpha_i, vA_i, vB_i, vX, vT_x, vT_cal);

  return(vLL);
}

//' @rdname bgnbd_LL
// [[Rcpp::export]]
double bgnbd_staticcov_LL_sum(const arma::vec& vParams,
                              const arma::vec& vX,
                              const arma::vec& vT_x,
                              const arma::vec& vT_cal,
                              const arma::vec& vN,
                              const arma::mat& mCov_life,
                              const arma::mat& mCov_trans){
  const arma::vec vLL = bgnbd_staticcov_LL_ind(vParams,
                                               vX,
                                               vT_x,
                                               vT_cal,
                                               mCov_life,
                                               mCov_trans);

  return(-arma::sum(vLL % vN));
}



//' @name bgnbd_pmf
//' @templateVar name_model_full BG/NBD
//' @template template_pmf_titledescreturnpmfparams
//' @template template_params_bgnbd
//' @param vAlpha_i Vector of individual parameters alpha
//' @param vA_i Vector of individual parameters a
//' @param vB_i Vector of individual parameters b
//' @template template_references_bgnbd
//'
arma::vec bgnbd_PMF(const double r,
                    const unsigned int x,
                    const arma::vec& vAlpha_i,
                    const arma::vec& vA_i,
                    const arma::vec& vB_i,
                    const arma::vec& vT_i){
  const arma::vec vLogPart1 = lbeta_ratio(vA_i, vB_i+x, vA_i, vB_i) +
    std::lgamma(r + x) - std::lgamma(r) - std::lgamma(x+1) +
    r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_i)) +
    x * (arma::log(vT_i) - arma::log(vAlpha_i+vT_i));
  const arma::vec vPart1 = arma::exp(vLogPart1);

  if(x > 0){
    // Sum Ai
    arma::vec vAsum(size(vAlpha_i), arma::fill::zeros);
    // from 0 up to and including x-1
    for(arma::uword j=0; j<=x-1; j++){
      vAsum += arma::exp(std::lgamma(r + j) - std::lgamma(r) - std::lgamma(j+1) +
        j*(arma::log(vT_i)-arma::log(vAlpha_i + vT_i)));
    }

    const arma::vec vPart2 = beta_ratio(vA_i+1, vB_i+x-1, vA_i, vB_i) %
      (1.0 - arma::exp(

          r*(arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_i)) +
            arma::log(vAsum)));

    return(vPart1 + vPart2);
  }else{
    return(vPart1);
  }
}

//' @rdname bgnbd_pmf
// [[Rcpp::export]]
arma::vec bgnbd_nocov_PMF(const double r,
                          const double alpha,
                          const double a,
                          const double b,
                          const unsigned int x,
                          const arma::vec& vT_i){

  const arma::vec vA_i = clv::vec_fill(a, vT_i.n_elem);
  const arma::vec vB_i = clv::vec_fill(b, vT_i.n_elem);
  const arma::vec vAlpha_i = clv::vec_fill(alpha, vT_i.n_elem);

  return(bgnbd_PMF(r, x,
                   vAlpha_i,vA_i,vB_i,
                   vT_i));
}

//' @rdname bgnbd_pmf
// [[Rcpp::export]]
arma::vec bgnbd_staticcov_PMF(const double r,
                              const unsigned int x,
                              const arma::vec& vAlpha_i,
                              const arma::vec& vA_i,
                              const arma::vec& vB_i,
                              const arma::vec& vT_i){
  return(bgnbd_PMF(r, x,
                   vAlpha_i,vA_i,vB_i,
                   vT_i));
}

// Calculate log(Beta(a,b)/Beta(x,y))
arma::vec lbeta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y){
  return(arma::lgamma(a) + arma::lgamma(b) - arma::lgamma(a + b) - arma::lgamma(x) - arma::lgamma(y) + arma::lgamma(x+y));
}

// Calculate Beta(a,b)/Beta(x,y)
arma::vec beta_ratio(const arma::vec& a, const arma::vec& b, const arma::vec& x, const arma::vec& y){
  return(arma::exp(lbeta_ratio(a, b, x, y)));
}


// [[Rcpp::export]]
arma::vec bgnbd_staticcov_alpha_i(const double alpha_0,
                                  const arma::vec& vCovParams_trans,
                                  const arma::mat& mCov_trans){
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  return alpha_0 * arma::exp((mCov_trans * (-1)) * vCovParams_trans);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_a_i(const double a_0,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life){
  //    a_i:  a0 * exp(cov.life * cov.param.life)
  return a_0 * arma::exp(mCov_life * vCovParams_life);
}

// [[Rcpp::export]]
arma::vec bgnbd_staticcov_b_i(const double b_0,
                              const arma::vec& vCovParams_life,
                              const arma::mat& mCov_life){
  //    b_i:  b0 * exp(cov.life * cov.param.life)
  return b_0 * arma::exp(mCov_life * vCovParams_life);
}
