#include <RcppArmadillo.h>
#include <math.h>
#include "ggomnbd_LL.h"
#include "ggomnbd_PAlive.h"
#include "ggomnbd_expectation.h"
#include "ggomnbd_CET.h"



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
                      const arma::vec& vAlpha_i,
                      const arma::vec& vBeta_i){

  // Expectation is Formula 20: PAlive()*Expectation()
  //
  //  with
  //    r *    = r + x
  //    alpha* = alpha + Tcal
  //    beta*  = beta + exp(b*Tcal) - 1
  //    t_i    = dPeriods


  const arma::vec vPAlive = ggomnbd_PAlive(r,b,s,vX,vT_x,vT_cal,vAlpha_i,vBeta_i);

  const arma::vec vRStar     = r + vX;
  const arma::vec vAlphaStar = vAlpha_i + vX;
  const arma::vec vBetaStar  = vBeta_i + arma::exp(b * vT_cal) - 1.0;

  arma::vec vPeriods(vX.n_elem);
  vPeriods.fill(dPeriods);

  const arma::vec vExpectation = ggomnbd_expectation(b, s, vRStar, vAlphaStar, vBetaStar, vPeriods);


  return(vPAlive % vExpectation);
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

  vAlpha_i = ggomnbd_nocov_alpha_i(alpha_0, n);

  vBeta_i = ggomnbd_nocov_beta_i(beta_0, n);

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

  const arma::vec vAlpha_i = ggomnbd_staticcov_alpha_i(alpha_0, vCovParams_trans, mCov_trans);

  const arma::vec vBeta_i  = ggomnbd_staticcov_beta_i(beta_0, vCovParams_life, mCov_life);

  return(ggomnbd_CET(r,b,s,dPeriods,vX,vT_x,vT_cal,vAlpha_i, vBeta_i));
}
