#include "ggomnbd.h"


double ggomnbd_CET_integrand(double tau, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;
  const double x = (params -> x_i);
  const double b = (params -> b);
  const double s = (params -> s);
  const double r = (params -> r);
  const double beta_i = (params -> beta_i);
  const double alpha_i = (params -> alpha_i);

  return(std::exp(b*tau) / (std::pow(std::exp(b*tau) + beta_i - 1.0,  s + 1.0) * std::pow(alpha_i + tau, r + x)));
}


double ggomnbd_CET_hyp2f1_integrand(double t, void * p_params){
  struct integration_params_CET_hyp21 * params = (struct integration_params_CET_hyp21*)p_params;
  const double s = (params -> s);
  const double z = (params -> z);

  return(std::pow(t,  s - 1.0)/(1.0 - z*t));
}



arma::vec ggomnbd_CET_hyp2f1_1_s_splus1_integrate(
    const double s,
    const arma::vec& vZ
){
  // Calculate Hyp2F1(1, s, s+1, z) using Jeff's integral representation:
  //        1/beta(1,s) * Integrate(f, lower=0, upper=1)
  //          where f(t) = (t^(s-1)) / (1-z*t)
  // Where the function is defined on the complex plane, it returns only the real part

  // Alternatively, it could the problematic hyp2f1 could also be rewritten as
  //      (1-z)^(-a) * 2F1(a, c-b, c, z/(z-1))
  //      (1-z)^(-b) * 2F1(c-a, b, c, z/(z-1))
  // However, it is unclear whether this can be calculated with the used Hyp2F1() for
  // all values of z or would need additional case analysis. Therefore simply use
  // numeric integration as this is not performance critical in CET.


  // Do not abort in case of error
  gsl_set_error_handler_off();

  gsl_integration_workspace * workspace = gsl_integration_workspace_alloc(1000);

  gsl_function integrand;
  integrand.function = &ggomnbd_CET_hyp2f1_integrand;

  const arma::uword n = vZ.n_elem;
  arma::vec vRes(n);
  double res, err;
  struct integration_params_CET_hyp21 params_i;
  for(arma::uword i = 0; i<n; i++){

    params_i.s = s;
    params_i.z = vZ(i);
    integrand.params = &params_i;

    gsl_integration_qags(&integrand, 0.0, 1.0, 1.0e-8, 1.0e-8, 1000, workspace, &res, &err);

    // beta(1,s) = 1/s, therefore 1/beta(1,s) = s
    vRes(i) = res * s;
  }

  gsl_integration_workspace_free(workspace);

  return vRes;
}



//' @name ggomnbd_CET
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @template template_titledescriptionreturn_CET
//'
//' @template template_params_ggomnbd
//' @template template_params_rcppperiods
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_CET(const double r,
                      const double b,
                      const double s,
                      const double dPeriods,
                      const arma::vec& vX,
                      const arma::vec& vT_x,
                      const arma::vec& vT_cal,
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i){

  // Errata by Adler (2022), https://pubsonline.informs.org/doi/10.1287/mnsc.2022.4422
  // See https://github.com/bachmannpatrick/CLVTools/issues/206

  const arma::vec vBetaIminus1 = vBeta_i - 1.0;
  const arma::vec vExpbTpart = arma::exp(b*vT_cal) + vBetaIminus1;
  const arma::vec vExpbTTstarpart = arma::exp(b*(vT_cal + dPeriods)) + vBetaIminus1;

  const arma::vec vHyp1 = ggomnbd_CET_hyp2f1_1_s_splus1_integrate(s, vBetaIminus1 / vExpbTpart);
  const arma::vec vHyp2 = ggomnbd_CET_hyp2f1_1_s_splus1_integrate(s, vBetaIminus1 / vExpbTTstarpart);
  const arma::vec vUpper = vHyp1 - arma::pow(vExpbTpart / vExpbTTstarpart, s) % vHyp2;

  const arma::vec vIntegral = ggomnbd_integrate(r, b, s, vAlpha_i, vBeta_i,vX,
                                                &ggomnbd_CET_integrand,
                                                vT_x, vT_cal);
  const arma::vec vLower = b * s * (1.0 + (b * s) * arma::pow(vAlpha_i + vT_cal, r + vX) % arma::pow(vExpbTpart, s) % vIntegral);
  const arma::vec vFront = (r + vX)/(vAlpha_i + vT_cal);

  return(vFront % vUpper / vLower);
}


//' @rdname ggomnbd_CET
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_CET(const double r,
                            const double alpha_0,
                            const double b,
                            const double s,
                            const double beta_0,
                            const double dPeriods,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  const arma::vec vAlpha_i = arma::vec(vX.n_elem, arma::fill::value(alpha_0));
  const arma::vec vBeta_i = arma::vec(vX.n_elem, arma::fill::value(beta_0));

  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i));
}


//' @rdname ggomnbd_CET
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_CET(const double r,
                                const double alpha_0,
                                const double b,
                                const double s,
                                const double beta_0,
                                const double dPeriods,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i));
}





// integrand <- function(tau)  {tau * exp(b*tau)  *((beta + exp(b*tau) - 1)^(-(s+1)))}
double ggomnbd_expectation_integrand(double tau, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;

  const double b = (params -> b);
  const double s = (params -> s);
  const double beta_i = (params -> beta_i);

  return tau * std::exp(b * tau) * std::pow( beta_i + std::exp(b * tau) - 1.0, -(s+1.0) );
}

//' @name ggomnbd_expectation
//' @title GGompertz/NBD: Unconditional Expectation
//'
//' @template template_expectation_description
//'
//' @template template_params_ggomnbd
//' @template template_expectation_params
//' @param vAlpha_i Vector of individual parameters alpha
//' @param vBeta_i Vector of individual parameters beta
//'
//' @template template_references_ggomnbd
//'
//' @template template_expectation_return
//'
arma::vec ggomnbd_expectation(const double b,
                              const double s,
                              // Pass r as vector because needed for CET
                              const arma::vec& vR,
                              const arma::vec& vAlpha_i,
                              const arma::vec& vBeta_i,
                              const arma::vec& vT_i){

  const arma::vec vF1 = (vR / vAlpha_i);
  const arma::vec vF2 = arma::pow(vBeta_i / (vBeta_i + arma::exp(b * vT_i)-1), s) % (vT_i);
  const arma::vec vF3 = b * s * arma::pow(vBeta_i, s);


  // r and vX are needed in LL but not in the expectation integral.
  //  Pass zeros (= vLower)
  const arma::vec vLower(vBeta_i.n_elem, arma::fill::zeros);
  const arma::vec vF4 = ggomnbd_integrate(0, b, s, vAlpha_i, vBeta_i,
                                          vLower, //instead of vX, only zeros
                                          &ggomnbd_expectation_integrand,
                                          vLower,
                                          vT_i);

  return(vF1 % (vF2 + (vF3 % vF4)));
}

//' @rdname ggomnbd_expectation
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_expectation(const double r,
                                    const double alpha_0,
                                    const double b,
                                    const double s,
                                    const double beta_0,
                                    const arma::vec& vT_i){

  const arma::vec vAlpha_i = arma::vec(vT_i.n_elem, arma::fill::value(alpha_0));
  const arma::vec vBeta_i = arma::vec(vT_i.n_elem, arma::fill::value(beta_0));
  const arma::vec vR = arma::vec(vT_i.n_elem, arma::fill::value(r));

  return(ggomnbd_expectation(b,
                             s,
                             vR,
                             vAlpha_i,
                             vBeta_i,
                             vT_i));
}

//' @rdname ggomnbd_expectation
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_expectation(const double r,
                                        const double b,
                                        const double s,
                                        const arma::vec& vAlpha_i,
                                        const arma::vec& vBeta_i,
                                        const arma::vec& vT_i){
  const arma::vec vR = arma::vec(vT_i.n_elem, arma::fill::value(r));

  return(ggomnbd_expectation(b,
                             s,
                             vR,
                             vAlpha_i,
                             vBeta_i,
                             vT_i));
}




arma::vec ggomnbd_integrate(const double r,
                            const double b,
                            const double s,
                            const arma::vec& vAlpha_i,
                            const arma::vec& vBeta_i,
                            const arma::vec& vX,
                            double (*const p_integrationFunction)(double, void*),
                            const arma::vec& vLower,
                            const arma::vec& vUpper){
  // Do not abort in case of error
  gsl_set_error_handler_off();

  gsl_integration_workspace *workspace
    = gsl_integration_workspace_alloc (1000);

  gsl_function integrand;
  integrand.function = p_integrationFunction;


  struct integration_params params_i;
  params_i.r = r;
  params_i.b = b;
  params_i.s = s;

  // Calculate integral for each customer
  double res, err;
  const arma::uword n = vAlpha_i.n_elem;
  arma::vec vRes(n);
  for(arma::uword i = 0; i<n; i++){
    // These differ per customer
    params_i.alpha_i = vAlpha_i(i);
    params_i.beta_i  = vBeta_i(i);
    params_i.x_i = vX(i);

    integrand.params = &params_i;

    gsl_integration_qags(&integrand, vLower(i), vUpper(i), 1.0e-8, 1.0e-8, 1000, workspace, &res, &err);
    vRes(i) = res;
  }

  gsl_integration_workspace_free(workspace);


  return(vRes);
}


double ggomnbd_LL_integrand(double y, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;

  const double r = (params -> r);
  const double b = (params -> b);
  const double s = (params -> s);
  const double beta_i = (params -> beta_i);
  const double alpha_i = (params -> alpha_i);
  const double x_i = (params -> x_i);

  // Rewrite integral as exp(log()) to improve numerical stability
  // Jeff:
  //    log(pow(y + alpha_i, -(r + x)) * pow(beta_i + exp(b * y) - 1 , -(s + 1)) * exp(b * y))
  //     = -(r+x)*log( y + alpha_i) - (s+1)*log( beta_i + exp( b * y) - 1) +b *y
  return std::exp(-(r + x_i) * std::log(y + alpha_i) - (s + 1.0) * std::log(beta_i + std::exp( b * y) - 1.0) + b * y);
}


//' @name ggomnbd_LL
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @templateVar model_params_ordered r, alpha_0, b, s, beta_0
//' @template template_titleparamsdescriptionreturndetails_LL
//'
//' @template template_params_rcppxtxtcal
//' @template template_param_rcppvn
//' @template template_params_rcppcovmatrix
//'
//' @templateVar name_params_cov_life vParams
//' @templateVar name_params_cov_trans vParams
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_LL_ind(const double r,
                         const double b,
                         const double s,
                         const arma::vec& vAlpha_i,
                         const arma::vec& vBeta_i,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  arma::vec vL1 = arma::lgamma(r + vX) - std::lgamma(r);
  arma::vec vL2 = arma::lgamma(r + vX) - std::lgamma(r);

  vL1 += r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) + vX % (0.0-arma::log(vAlpha_i + vT_cal)) + s * (arma::log(vBeta_i)-arma::log(vBeta_i-1.0 + arma::exp(b*vT_cal))) ;
  vL2 += std::log(b) + r *arma::log(vAlpha_i) + std::log(s) + s * arma::log(vBeta_i);

  const arma::vec vIntegrals = ggomnbd_integrate(r, b, s, vAlpha_i, vBeta_i,
                                                 vX,
                                                 &ggomnbd_LL_integrand,
                                                 vT_x, vT_cal);
  vL2 += arma::log(vIntegrals);

  // Calculate LL ---------------------------------------------------------------------------
  // arma::vec vLL = arma::log(arma::exp(vL1) + arma::exp(vL2));
  // For numerical stability rewrite
  //  log(exp(a) + exp(b))
  //            as
  //  max(a,b) + log(exp(a-max(a,b)) + exp(b-max(a,b)))

  const arma::vec vMaxPart = arma::max(vL1, vL2);
  const arma::vec vLL = vMaxPart + arma::log(arma::exp(vL1 - vMaxPart) + arma::exp(vL2 - vMaxPart));

  return(vLL);
}

//' @rdname ggomnbd_LL
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_LL_ind(const arma::vec& vLogparams,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal){

  const double r       = std::exp(vLogparams(0));
  const double alpha_0 = std::exp(vLogparams(1));
  const double b       = std::exp(vLogparams(2));
  const double s       = std::exp(vLogparams(3));
  const double beta_0  = std::exp(vLogparams(4));

  const arma::vec vAlpha_i = arma::vec(vX.n_elem, arma::fill::value(alpha_0));
  const arma::vec vBeta_i = arma::vec(vX.n_elem, arma::fill::value(beta_0));

  return(ggomnbd_LL_ind(r, b, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal));
}


//' @rdname ggomnbd_LL
// [[Rcpp::export]]
double ggomnbd_nocov_LL_sum(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal,
                            const arma::vec& vN){


  const arma::vec vLL = ggomnbd_nocov_LL_ind(vLogparams,
                                             vX,
                                             vT_x,
                                             vT_cal);

  // accu sums all elements
  return(-arma::sum(vLL % vN));
}




//' @rdname ggomnbd_LL
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_LL_ind(const arma::vec& vParams,
                                   const arma::vec& vX,
                                   const arma::vec& vT_x,
                                   const arma::vec& vT_cal,
                                   const arma::mat& mCov_life,
                                   const arma::mat& mCov_trans){

  // Read out parameters from vParams
  //
  //    Contains model and covariate parameters
  //      Model:              first 5
  //      Life + Trans cov    after model params
  //                          depends on num of cols in cov data
  // vParams have to be single vector because used by optimizer
  const double r       = std::exp(vParams(0));
  const double alpha_0 = std::exp(vParams(1));
  const double b       = std::exp(vParams(2));
  const double s       = std::exp(vParams(3));
  const double beta_0  = std::exp(vParams(4));

  const int num_model_params = 5;
  const arma::uword num_cov_life  = mCov_life.n_cols;
  const arma::uword num_cov_trans = mCov_trans.n_cols;

  const arma::vec vLife_params      = vParams.subvec(num_model_params              ,  num_model_params+num_cov_life                 - 1);
  const arma::vec vTrans_params     = vParams.subvec(num_model_params + num_cov_life, num_model_params+num_cov_life + num_cov_trans - 1);

  const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vTrans_params, mCov_trans);
  const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vLife_params, mCov_life);

  return(ggomnbd_LL_ind(r,b,s,vAlpha_i,vBeta_i,vX,vT_x,vT_cal));
}




//' @rdname ggomnbd_LL
// [[Rcpp::export]]
double ggomnbd_staticcov_LL_sum(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vN,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  // vParams has to be single vector because used by optimizer
  const arma::vec vLL = ggomnbd_staticcov_LL_ind(vParams,vX,vT_x,vT_cal,mCov_life,mCov_trans);

  return(-arma::sum(vLL % vN));
}


// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_alpha_i(const double alpha_0,
                                    const arma::vec& vCovParams_trans,
                                    const arma::mat& mCov_trans){

  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  return alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
}

// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_beta_i(const double beta_0,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life){
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)
  return beta_0 * arma::exp(((mCov_life  * (-1)) * vCovParams_life));
}



//' @name ggomnbd_PAlive
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @template template_titledescriptionreturn_palive
//'
//' @template template_params_ggomnbd
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_PAlive(const double r,
                         const double b,
                         const double s,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal,
                         const arma::vec& vAlpha_i,
                         const arma::vec& vBeta_i){

  const arma::vec vP1 = arma::lgamma(r + vX) - std::lgamma(r);
  const arma::vec vP2 = r * arma::log(vAlpha_i/(vAlpha_i + vT_cal)) + vX % arma::log(1/(vAlpha_i + vT_cal)) + s * arma::log(vBeta_i/(vBeta_i - 1 + arma::exp(b * vT_cal)));
  const arma::vec vP3 = ggomnbd_LL_ind(r, b ,s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  return arma::exp(vP1 + vP2 - vP3);

}


//' @rdname ggomnbd_PAlive
// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_PAlive(const double r,
                                   const double alpha_0,
                                   const double b,
                                   const double s,
                                   const double beta_0,
                                   const arma::vec& vX,
                                   const arma::vec& vT_x,
                                   const arma::vec& vT_cal,
                                   const arma::vec& vCovParams_trans,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life,
                                   const arma::mat& mCov_trans){

  const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
  const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}


//' @rdname ggomnbd_PAlive
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_PAlive(const double r,
                               const double alpha_0,
                               const double b,
                               const double s,
                               const double beta_0,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal){

  const arma::vec vAlpha_i = arma::vec(vX.n_elem, arma::fill::value(alpha_0));
  const arma::vec vBeta_i = arma::vec(vX.n_elem, arma::fill::value(beta_0));

  return ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);
}


double ggomnbd_PMF_integrand(double tau, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;

  const double r = (params -> r);
  const double b = (params -> b);
  const double s = (params -> s);
  const double beta_i = (params -> beta_i);
  const double alpha_i = (params -> alpha_i);
  const double x_i = (params -> x_i);

  return std::exp(
    x_i * std::log(tau) + b * tau - (r + x_i) * std::log(tau + alpha_i) - (s+1.0)*std::log(std::exp(b*tau) + beta_i - 1.0)
  );
}



//' @name ggomnbd_PMF
//' @templateVar name_model_full GGompertz/NBD
//' @template template_pmf_titledescreturnpmfparams
//'
//' @template template_params_ggomnbd
//' @template template_params_rcppvcovparams
//' @template template_params_rcppcovmatrix
//'
//' @templateVar name_params_cov_life vCovParams_life
//' @templateVar name_params_cov_trans vCovParams_trans
//' @template template_details_rcppcovmatrix
//'
//' @template template_references_ggomnbd
//'
arma::vec ggomnbd_PMF(const double r,
                      const double b,
                      const double s,
                      const unsigned int x,
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i,
                      const arma::vec& vT_i){

  // TODO: Type cast x to double

  // const double dx = static_cast<double>(a);
  // B(r, x+1) = G(r)*G(x+1)/G(r+x+1)
  // const double B = std::tgamma(r) * std::tgamma(x + 1.0) / std::tgamma(r + x + 1.0);
  // const arma::vec vFront = arma::pow(vAlpha_i, r) % arma::pow(vBeta_i, s) / ((r + x) * B);
  const double Blog = std::lgamma(r) + std::lgamma(x + 1.0) - std::lgamma(r + x + 1.0);
  const arma::vec vFront = arma::exp(
    r*arma::log(vAlpha_i) + s * arma::log(vBeta_i) - std::log(r+x) - Blog
  );

  // inner=exp(log(inner)) for numerical stability
  const arma::vec vInner = arma::exp(
    x * arma::log(vT_i) - (x+r) * arma::log(vT_i + vAlpha_i) - s*arma::log(arma::exp(b*vT_i) + vBeta_i - 1.0)
  );

  // Could write dedicated integration routine but not deemed performance critical.
  // Wastefully, two vectors are allocated for `x` and `0` when in fact these are
  // constant scalars. Also, the scalar `x` is transported to the integrand by
  // repurposing the `x_i` in struct `integration_params` which usually is used
  // for the number of tranasctions of an individual customer.
  // TODO: Uses wrong limit=0 in integration routine which is smaller than the
  // workspace. This is a strong case to write dedicated routine.
  const arma::vec vIntergral = ggomnbd_integrate(r, b, s, vAlpha_i, vBeta_i,
                                                 arma::vec(vT_i.n_elem, arma::fill::value(x)),
                                                 &ggomnbd_PMF_integrand,
                                                 arma::vec(vT_i.n_elem, arma::fill::zeros), vT_i);

  return vFront % (vInner + b * s * vIntergral);
}

//' @rdname ggomnbd_PMF
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_PMF(const double r,
                            const double alpha_0,
                            const double b,
                            const double s,
                            const double beta_0,
                            const unsigned int x,
                            const arma::vec& vT_i){

  const arma::vec vAlpha_i = arma::vec(vT_i.n_elem, arma::fill::value(alpha_0));
  const arma::vec vBeta_i = arma::vec(vT_i.n_elem, arma::fill::value(beta_0));

  return(ggomnbd_PMF(r,b,s,x,vAlpha_i,vBeta_i,vT_i));
}

//' @rdname ggomnbd_PMF
 // [[Rcpp::export]]
arma::vec ggomnbd_staticcov_PMF(const double r,
                                const double alpha_0,
                                const double b,
                                const double s,
                                const double beta_0,
                                const unsigned int x,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans,
                                const arma::vec& vT_i){

   const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);
   const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

   return(ggomnbd_PMF(r,b,s,x,vAlpha_i,vBeta_i,vT_i));
 }
