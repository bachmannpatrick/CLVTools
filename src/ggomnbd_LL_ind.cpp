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

  //integrand<-function(y){(y+alpha_i[i])^-(r+cbs$x[i])*(beta_i[i]+exp(b*y)-1)^-(s+1)*exp(b*y)}
  double integrationFunction (double x, void * params)
  {
    return  std::pow(x + (*gpvAlpha_i)(globI),  -(r_glob + (*gpvX)(globI)))
    * std::pow((*gpvBeta_i)(globI) + std::exp( b_glob * x) - 1.0 , -(s_glob + 1.0))
    * std::exp(b_glob * x);
  }
}



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


  //below<-(max(t.x)+max(alpha_i))^-(r+max(x))*(max(beta_i)+exp(b*max(t.x))-1)^-(s+1)*exp(b*min(t.x))
  const double below = pow(vT_x.max() + vAlpha_i.max(), -(r + vX.max()) ) * pow(vBeta_i.max() + exp(b*vT_x.max())-1.0, -(s+1.0))  * exp(b*vT_x.min()) ;
  // above<-(min(t.x)+min(alpha_i))^-(r+max(x))*(min(beta_i)+exp(b*min(t.x))-1)^-(s+1)*exp(b*max(t.x))
  const double above = pow(vT_x.min() + vAlpha_i.min(), -(r + vX.max()) ) * pow(vBeta_i.min() + exp(b*vT_x.min())-1.0, -(s+1.0))  * exp(b*vT_x.max()) ;


  //   //** TODO ** Zero or just very small??
  if( below == 0.0)//< 0.00001 )
    Rcpp::Rcout<<"Log of the integral might diverge; Lower Boundary = 0 "<<std::endl;

  if( above > pow(10,200) )
    Rcpp::Rcout<<"Log of the integral might diverge; Upper Boundary ="<<above<<std::endl;

  // # P(omega>T_i|r,alpha0,b,s,beta0,,x_i,t_i,T_i,gamma1,gamma2):
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
  // l1<-lgamma(r+x)-lgamma(r)+r*(log(alpha_i)-log(alpha_i+T.cal))+x*(0-log(alpha_i+T.cal))+s*(log(beta_i)-log(beta_i-1+exp(b*T.cal)))
  // l2<-lgamma(r+x)-lgamma(r)+log(b)+r*log(alpha_i)+log(s)+s*log(beta_i)+log(int)
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


  // ll<-exp(l1)+exp(l2)
  //create result and store it in vector passed by ref
  arma::vec vLL = arma::log(arma::exp(vL1) + arma::exp(vL2));

  return(vLL);
}
