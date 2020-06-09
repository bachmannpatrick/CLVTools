#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_PAlive.h"
#include <gsl/gsl_errno.h>
#include <gsl/gsl_integration.h>



//INTEGRATION WORKAROUND
//anonymous namespace to only make this variables availale in this translation unit
//the variables and functions defined here outside of the ggomnbd_PAlive function scope
//are needed during integration
namespace{

const arma::vec * gpvBeta_i=0; //will point to vectors to avoid copying
  unsigned int globI=0; //to loop throught the vectors while integrating

  double globB=0, globS=0;//parameters extracted from passed vector

  // integrand<-function(omega){omega*exp(b*omega)*(beta_i[i]+exp(b*omega)-1)^-(s+1)}
  double integrationFunction (double omega, void * params)
  {
    return omega * std::exp(globB * omega) * std::pow( (*gpvBeta_i)(globI) + std::exp(globB * omega) - 1.0, -(globS+1.0) );
  }
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
                      // Do not pass vAlpha and vBeta by ref because they will be modified
                      arma::vec vAlpha_i,
                      arma::vec vBeta_i,
                      const arma::vec& vPAlive){
  // Do not abort in case of error
  gsl_set_error_handler_off();

  const unsigned int n = vX.n_elem;

  // b, s are defined in the scope of this file
  globB = b;
  globS = s;
  gpvBeta_i = &vBeta_i;

  vAlpha_i += vX;
  vBeta_i += arma::exp(b * vT_cal) - 1.0;

  arma::vec vIntegrals(n);
  double res, err;

  gsl_integration_workspace *workspace
    = gsl_integration_workspace_alloc (1000);

  gsl_function integrand;
  integrand.function = &integrationFunction;
  integrand.params = NULL;

  for(globI = 0; globI<n; globI++){
    gsl_integration_qags(&integrand, 0, dPeriods, 1.0e-8, 1.0e-8, 0, workspace, &res, &err);
    vIntegrals(globI) = res;
  }

  arma::vec vP1 = vPAlive % ((r+vX) / (vAlpha_i));
  arma::vec vP2 = arma::pow( vBeta_i / (vBeta_i + std::exp(b* dPeriods) - 1.0 ), s );
  arma::vec vP3 = dPeriods + b * s * (arma::pow(vBeta_i, s) % vIntegrals);

  return( vP1 % vP2 % vP3);
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


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);


  // Calculate CET ----------------------------------------------------------------
  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i,vPAlive));
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

  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);

  // Calculate CET -----------------------------------------------------------------
  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i, vPAlive));
}
