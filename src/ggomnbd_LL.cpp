#include <RcppArmadillo.h>
#include <gsl/gsl_integration.h>


// INTEGRATION WORKAROUND
// anonymous namespace to only make this variables availale in this translation unit
// the variables and functions defined here outside of the ggomnbd_PAlive function scope
// are needed during integration
namespace{

const arma::vec * gpvX=0, * gpvAlpha_i=0, * gpvBeta_i=0; //will point to vectors to avoid copying
  unsigned int globI=0; //to loop throught the vectors while integrating

  double r_glob=0, b_glob=0, s_glob=0;//parameters extracted from passed vector

  double integrationFunction (double x, void * params)
  {
    return  std::pow(x + (*gpvAlpha_i)(globI),  -(r_glob + (*gpvX)(globI)))
    * std::pow((*gpvBeta_i)(globI) + std::exp( b_glob * x) - 1.0 , -(s_glob + 1.0))
    * std::exp(b_glob * x);
  }
}




//' @name ggomnbd_LL
//'
//' @templateVar name_model_full GGompertz/NBD
//' @templateVar name_model_short ggomnbd
//' @template template_titledescriptionreturndetails_LL
//'
//' @param vLogparams vector with the GGompertz/NBD model parameters log scaled. See Details.
//' @param vParams vector with the parameters for the GGompertz/NBD model and the static covariates. See Details.
//'
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details \code{vLogparams} is a vector with model parameters
//' \code{r, alpha_0, b, s, beta_0} at log-scale, in this order.\cr
//'
//' @template template_details_paramsggomnbd
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
                         const arma::vec & vAlpha_i,
                         const arma::vec & vBeta_i,
                         const arma::vec & vX,
                         const arma::vec & vT_x,
                         const arma::vec & vT_cal){

  const unsigned int n = vX.n_elem;

  //set pointers to vecs for the integration workaround
  gpvX = &vX;
  gpvAlpha_i = &vAlpha_i;
  gpvBeta_i = &vBeta_i;

  //set the params for the integration workaround
  r_glob = r;
  b_glob = b;
  s_glob = s;


  const double below = pow(vT_x.max() + vAlpha_i.max(), -(r + vX.max()) ) * pow(vBeta_i.max() + exp(b*vT_x.max())-1.0, -(s+1.0))  * exp(b*vT_x.min()) ;
  const double above = pow(vT_x.min() + vAlpha_i.min(), -(r + vX.max()) ) * pow(vBeta_i.min() + exp(b*vT_x.min())-1.0, -(s+1.0))  * exp(b*vT_x.max()) ;


  //   //** TODO ** Zero or just very small??
  if( below == 0.0)//< 0.00001 )
    Rcpp::Rcout<<"Log of the integral might diverge; Lower Boundary = 0 "<<std::endl;

  if( above > pow(10,200) )
    Rcpp::Rcout<<"Log of the integral might diverge; Upper Boundary ="<<above<<std::endl;

  arma::vec vIntegrals(n);
  double res, err;

  gsl_integration_workspace *workspace
    = gsl_integration_workspace_alloc (1000);

  gsl_function integrand;
  integrand.function = &integrationFunction;
  integrand.params = NULL;

  for(globI = 0; globI<n; globI++){
    gsl_integration_qags(&integrand, vT_x(globI), vT_cal(globI), 1.0e-8, 1.0e-8, 0, workspace, &res, &err);
    vIntegrals(globI) = res;
  }

  arma::vec vL1(n), vL2(n);

  // **TODO: by now has arma::lgamma
  //calculate in 2 parts:
  // loop for gamma functions which are not in arma::
  // rest of calculation is in arma:: -> use for vectorized speed
  double tmp;
  const double r_lgamma = lgamma(r);
  for( unsigned int i=0; i<n; i++){
    tmp = lgamma(r + vX(i)) - r_lgamma;
    vL1(i) = tmp;
    vL2(i) = tmp;
  }


  vL1 += r * (arma::log(vAlpha_i) - arma::log(vAlpha_i + vT_cal)) + vX % (0.0-arma::log(vAlpha_i + vT_cal)) + s * (arma::log(vBeta_i)-arma::log(vBeta_i-1.0 + arma::exp(b*vT_cal))) ;
  vL2 += std::log(b) + r *arma::log(vAlpha_i) + log(s) + s * arma::log(vBeta_i) +arma::log(vIntegrals);

  //create result and store it in vector passed by ref
  arma::vec vLL = arma::log(arma::exp(vL1) + arma::exp(vL2));

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
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);

  // Calculate LL ---------------------------------------------------
  //    Calculate value for every customer
  //    Sum of all customers' LL value

  arma::vec vLL = ggomnbd_LL_ind(r, b, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
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

  const int no_model_params = 5;
  const double num_cov_life  = mCov_life.n_cols;
  const double num_cov_trans = mCov_trans.n_cols;

  const arma::vec vLife_params      = vParams.subvec(no_model_params              ,  no_model_params+num_cov_life                 - 1);
  const arma::vec vTrans_params     = vParams.subvec(no_model_params + num_cov_life, no_model_params+num_cov_life + num_cov_trans - 1);



  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vLife_params));


  // Calculate LL --------------------------------------------------
  //    Calculate value for every customer
  //    Sum of all customers' LL value
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
