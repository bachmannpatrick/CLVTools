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
    return omega * std::exp(globB * omega) * std::pow( (*gpvBeta_i)(globI) + std::exp(globB * omega) - 1.0, -(globS+1.0) )
    ;
  }
}


arma::vec ggomnbd_CET(const double r,
                   const double b,
                   const double s,
                   const double dPrediction_period,
                   const arma::vec& vX,
                   const arma::vec& vT_x,
                   const arma::vec& vT_cal,
                   // Do not pass vAlpha and vBeta by ref as will be modified
                   arma::vec vAlpha_i,
                   arma::vec vBeta_i,
                   const arma::vec& vPAlive)
{
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
    gsl_integration_qags(&integrand, 0, dPrediction_period, 1.0e-8, 1.0e-8, 0, workspace, &res, &err);
    vIntegrals(globI) = res;
  }

  arma::vec vP1 = vPAlive % ((r+vX) / (vAlpha_i));
  arma::vec vP2 = arma::pow( vBeta_i / (vBeta_i + std::exp(b* dPrediction_period) - 1.0 ), s );
  arma::vec vP3 = dPrediction_period + b * s * (arma::pow(vBeta_i, s) % vIntegrals);

  return( vP1 % vP2 % vP3);
}
