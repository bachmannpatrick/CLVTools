#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_PAlive.h"
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



//' GGompertz/NBD: Conditional Expected Transactions without Covariates
//'
//' @description
//' Calculates the expected number of transactions in a given time period based
//' on a customers past transaction behavior and the GGompertz/NBD model parameters.
//'
//' @template template_params_rcpp_ggomnbd_estimatedparams
//' @template template_params_rcppxtxtcal
//' @param dPrediction_period time prediction time frame
//'
//' @details
//'
//' \code{vEstimated_params}
//' s: shape parameter of the Gamma distribution for the lifetime process.\cr
//' The smaller s, the stronger the heterogeneity of customer lifetimes. \cr
//' beta: scale parameter for the Gamma distribution for the lifetime process. \cr
//' b: scale parameter of the Gompertz distribution (constant across customers). \cr
//' r: shape parameter of the Gamma distribution of the purchase process.
//' The smaller r, the stronger the heterogeneity of the pruchase process.\cr
//' alpha: scale parameter of the Gamma distribution of the purchase process.
//'
//' \code{dPrediction_period} is the duration over which the prediction is made.
//'  Usually this is the duration of the holdout period.
//'
//'@return
//' Returns a vector with the conditional expected transactions for the existing
//' customers in the GGompertz/NBD model.
//'
//' @template template_references_ggomnbd
//'
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
