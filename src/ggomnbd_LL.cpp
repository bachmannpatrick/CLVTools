#include <RcppArmadillo.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_integration.h>
#include "ggomnbd_LL.h"
#include "clv_vectorized.h"

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

    gsl_integration_qags(&integrand, vLower(i), vUpper(i), 1.0e-8, 1.0e-8, 0, workspace, &res, &err);
    vRes(i) = res;
  }

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

  return  std::pow(y + alpha_i,  -(r + x_i))
  * std::pow(beta_i + std::exp( b * y) - 1.0 , -(s + 1.0))
  * std::exp(b * y);
}


//' @name ggomnbd_LL
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @templateVar model_params_ordered r, alpha_0, b, s, beta_0
//' @template template_titleparamsdescriptionreturndetails_LL
//'
//' @template template_params_rcppxtxtcal
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

  arma::vec vL1 = arma::lgamma(r + vX) - lgamma(r);
  arma::vec vL2 = arma::lgamma(r + vX) - lgamma(r);

  vL1 += r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) + vX % (0.0-arma::log(vAlpha_i + vT_cal)) + s * (arma::log(vBeta_i)-arma::log(vBeta_i-1.0 + arma::exp(b*vT_cal))) ;
  vL2 += std::log(b) + r *arma::log(vAlpha_i) + log(s) + s * arma::log(vBeta_i);

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
  const arma::vec vLL = vMaxPart + arma::log(arma::exp(vL1 - vMaxPart) + exp(vL2 - vMaxPart));

  return(vLL);
}

//' @rdname ggomnbd_LL
// [[Rcpp::export]]
arma::vec ggomnbd_nocov_LL_ind(const arma::vec& vLogparams,
                               const arma::vec& vX,
                               const arma::vec& vT_x,
                               const arma::vec& vT_cal){

  const double r       = exp(vLogparams(0));
  const double alpha_0 = exp(vLogparams(1));
  const double b       = exp(vLogparams(2));
  const double s       = exp(vLogparams(3));
  const double beta_0  = exp(vLogparams(4));

  // n = number of elements / customers
  const double n = vX.n_elem;

  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const arma::vec vAlpha_i = ggomnbd_nocov_alpha_i(alpha_0, n);
  const arma::vec vBeta_i = ggomnbd_nocov_beta_i(beta_0, n);

  return(ggomnbd_LL_ind(r, b, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal));
}


//' @rdname ggomnbd_LL
// [[Rcpp::export]]
double ggomnbd_nocov_LL_sum(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){


  arma::vec vLL = ggomnbd_nocov_LL_ind(vLogparams,
                                       vX,
                                       vT_x,
                                       vT_cal);

  // accu sums all elements
  return(-arma::sum(vLL));
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
  const double r       = exp(vParams(0));
  const double alpha_0 = exp(vParams(1));
  const double b       = exp(vParams(2));
  const double s       = exp(vParams(3));
  const double beta_0  = exp(vParams(4));

  const int num_model_params = 5;
  const double num_cov_life  = mCov_life.n_cols;
  const double num_cov_trans = mCov_trans.n_cols;

  const arma::vec vLife_params      = vParams.subvec(num_model_params              ,  num_model_params+num_cov_life                 - 1);
  const arma::vec vTrans_params     = vParams.subvec(num_model_params + num_cov_life, num_model_params+num_cov_life + num_cov_trans - 1);



  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

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
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  // vParams has to be single vector because used by optimizer
  const arma::vec vLL = ggomnbd_staticcov_LL_ind(vParams,vX,vT_x,vT_cal,mCov_life,mCov_trans);

  return(-arma::sum(vLL));
}

arma::vec ggomnbd_nocov_alpha_i(const double alpha_0, const double n){
  return clv::vec_fill(alpha_0, n);
}

arma::vec ggomnbd_nocov_beta_i(const double beta_0, const double n){
  return clv::vec_fill(beta_0, n);
}

arma::vec ggomnbd_nocov_r(const double r, const double n){
  return clv::vec_fill(r, n);
}

// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_alpha_i(const double alpha_0,
                                    const arma::vec& vCovParams_trans,
                                    const arma::mat& mCov_trans){
   return alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
}

// [[Rcpp::export]]
arma::vec ggomnbd_staticcov_beta_i(const double beta_0,
                                   const arma::vec& vCovParams_life,
                                   const arma::mat& mCov_life){

  return beta_0 * arma::exp(((mCov_life  * (-1)) * vCovParams_life));
}

arma::vec ggomnbd_staticcov_r(const double r, const double n){
  return ggomnbd_nocov_r(r, n);
}
