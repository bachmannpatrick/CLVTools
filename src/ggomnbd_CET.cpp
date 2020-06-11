#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL.h"
#include "ggomnbd_PAlive.h"



// integrand<-function(omega){omega*exp(b*omega)*(beta_i[i]+exp(b*omega)-1)^-(s+1)}
double ggomnbd_CET_integrand(double omega, void * p_params){
  struct integration_params * params = (struct integration_params*)p_params;
  const double b = (params -> b);
  const double s = (params -> s);
  const double beta_i = (params -> beta_i);

  return(omega * std::exp(b * omega) * std::pow(beta_i + std::exp(b * omega) - 1.0, -(s+1.0) ) );
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
                      arma::vec vBeta_i){

  // Calculate PAlive -------------------------------------------------------------
  const arma::vec vPAlive = ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);

  // const unsigned int n = vAlpha_i.n_elem;
  // arma::vec vPeriods(n);
  // vPeriods.fill(dPeriods);

  // vAlpha_i += vX;
  // vBeta_i += arma::exp(b * vT_cal) - 1.0;
  //
  // arma::vec vExpectation(n);
  // arma::vec vAlpha_tmp(1), vBeta_tmp(1), vPeriods_tmp(1);
  // double r_star;
  // for(int i = 0; i<n; i++){
  //
  //   vAlpha_tmp(0) = vAlpha_i(i);
  //   vBeta_tmp(0) = vBeta_i(i);
  //   r_star = r + vX(i);
  //   vPeriods_tmp(0) = dPeriods;
  //   vExpectation(i) = ggomnbd_expectation(r_star, b, s, vAlpha_tmp, vBeta_tmp, vPeriods_tmp)(0);
  // }
  //
  // return(vPAlive % vExpectation);

  vAlpha_i += vX;
  vBeta_i += arma::exp(b * vT_cal) - 1.0;

  const arma::vec vLower(vBeta_i.n_elem, arma::fill::zeros);
  arma::vec vUpper(vBeta_i.n_elem);
  vUpper.fill(dPeriods);
  const arma::vec vIntegrals = ggomnbd_integrate(r, b, s, vAlpha_i, vBeta_i,
                                                 vX,
                                                 &ggomnbd_CET_integrand,
                                                 vLower,
                                                 vUpper);

  // From Matlab code:
  // gg_xt_cum_up(i)=p_i(i).*rstar./astar.*  (((betastar./(betastar+exp(bg*t)-1)).^sg).*t+bg.*sg.*betastar.^sg.*intgup_h(i));
  arma::vec vP1 = vPAlive % ((r+vX) / (vAlpha_i));
  arma::vec vP2 = arma::pow( vBeta_i / (vBeta_i + std::exp(b* dPeriods) - 1.0 ), s ) * dPeriods;
  arma::vec vP3 = b * s * arma::pow(vBeta_i, s) % vIntegrals;

  return( vP1 % (vP2 + vP3));
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

  // Build alpha and beta -------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)

  const arma::vec vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  const arma::vec vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vCovParams_life));

  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i));
}
