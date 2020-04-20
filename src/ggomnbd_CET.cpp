
#include <RcppArmadillo.h>
#include <math.h>
#include "crbond_quadpack.h"
#include "ggomnbd_PAlive.h"




//INTEGRATION WORKAROUND
//anonymous namespace to only make this variables availale in this translation unit
//the variables and functions defined here outside of the ggomnbd_PAlive function scope
//are needed during integration
namespace{

const arma::vec * gpvBeta_i=0; //will point to vectors to avoid copying
  unsigned int globI=0; //to loop throught the vectors while integrating

  double globB=0, globS=0;//parameters extracted from passed vector

  // integrand<-function(omega){omega*exp(b*omega)*(beta_i[i]+exp(b*omega)-1)^-(s+1)}
  double integrationFunction (double omega)
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

  //   r=r+cbs$x
  //     alpha_i=alpha_i+cbs$x
  //     betas=beta_i+exp(b*cbs$T.cal)-1
  // *! r will be implemented directly in the code to avoid creating a n-vector *!
  vAlpha_i += vX;
  vBeta_i += arma::exp(b * vT_cal) - 1.0;

  //   int<-c()
  //     for (i in 1:n){
  //       integrand<-function(omega){omega*exp(b*omega)*(beta_i[i]+exp(b*omega)-1)^-(s+1)}
  //       int[i]<-integrate(integrand,0,prediction.period)$value
  //     }
  arma::vec vIntegrals(n);
  int err;
  for(globI = 0; globI<n; globI++){
    // integrand<-function(omega){omega*exp(b*omega)*(beta_i[i]+exp(b*omega)-1)^-(s+1)}
    vIntegrals(globI) = quadpack::integrate(&integrationFunction,
               0.0,  //lower bound
               dPrediction_period,//upper bound
               &err);
    // if(err==)
    // throw error ** TODO **
  }


  //   P1 <- ggomnbd_PAlive(params, cbs, covariates, dropout.cov.var, transaction.cov.var) * (r/alpha_i)
  //     P2 <- ((beta_i/(beta_i+exp(b*prediction.period)-1))^s)
  //     P3 <- prediction.period + b * s * beta_i^s * int

  // ** TODO:r has values added (+cbs$x) before this functions in R (??)
  arma::vec vP1 = vPAlive % ((r+vX) / (vAlpha_i));
  // /:= element wise division
  arma::vec vP2 = arma::pow( vBeta_i / (vBeta_i + std::exp(b* dPrediction_period) - 1.0 ), s );
  arma::vec vP3 = dPrediction_period + b * s * (arma::pow(vBeta_i, s) % vIntegrals);

  // return(P1 * P2 * P3)
  return( vP1 % vP2 % vP3);
}
