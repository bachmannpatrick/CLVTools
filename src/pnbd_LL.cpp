#include <RcppArmadillo.h>
#include <math.h>
#include "cephes_hypergeom2f1.h"

//Individual pnbd LL. No cov and staticcov differ by the individual vAlpha_i and vBeta_i which are different
// for each customer depending on the covariate.
arma::vec pnbd_LL_ind(  const double r,
                        const double s,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal)
{

  const unsigned int n = vX.n_elem;

  arma::vec vABabs = arma::abs( (vAlpha_i - vBeta_i) );

  arma::vec vParam2(n);
  vParam2.fill((s+1));

  // save indices as used often
  arma::uvec uvAlphaBetaFindRes = find(vAlpha_i < vBeta_i);
  vParam2.elem( uvAlphaBetaFindRes ) = ( r + vX.elem(uvAlphaBetaFindRes) );

  arma::vec vMaxAB(vAlpha_i);
  vMaxAB.elem(uvAlphaBetaFindRes) = vBeta_i.elem(uvAlphaBetaFindRes);

  // Distinguish betwen case abs(alpha_i - beta_i) == 0 and != 0
  arma::uvec uvLLFind1 = find( vABabs != 0.0) ;
  arma::uvec uvLLFind2 = find( vABabs == 0.0);


  arma::vec vF1(n), vF2(n), vPartF(n);;
  arma::uvec::const_iterator it, itEnd;

  // Calculate Part F for case vABabs != 0
  //  loop because hypergeom2F1 is not vectorised
  itEnd = uvLLFind1.end();
  for(it =uvLLFind1.begin(); it!=itEnd; it++)
  {
    try {

      vF1(*it) = cephes::hypergeom2F1( r + s + vX(*it),
          vParam2(*it),
          r + s + vX(*it) + 1,
          vABabs(*it) / (vMaxAB(*it) + vT_x(*it))
      );

      vF2(*it) = cephes::hypergeom2F1( r + s + vX(*it),
          vParam2(*it),
          r + s + vX(*it) + 1,
          vABabs(*it)/(vMaxAB(*it) + vT_cal(*it))
      );

      vF2(*it) *= pow( (vMaxAB(*it) + vT_x(*it))/(vMaxAB(*it) + vT_cal(*it)) , r + s + vX(*it));

    }catch(std::exception &e)
    {
      // print error location and cause. Stop and return NA to optimization
      Rcpp::Rcout<<"Exception in pnbd_LL_ind: "<<e.what()<<std::endl;
      arma::vec ret(1);
      ret.fill(NA_REAL);
      return(ret);
    }
  }

  vPartF.elem(uvLLFind1) = -(r + s + vX.elem(uvLLFind1)) % arma::log(vMaxAB.elem(uvLLFind1) + vT_x.elem(uvLLFind1)) + arma::log(vF1.elem(uvLLFind1) - vF2.elem(uvLLFind1));


  // Calculate Part F for case vABabs == 0

  vF1.elem(uvLLFind2) = (-1 * (r + s + vX.elem(uvLLFind2))) % arma::log( vMaxAB.elem(uvLLFind2) + vT_x.elem(uvLLFind2) );

  //pow is not vectorised for two vecs, hence loop
  vF2.elem(uvLLFind2) = (vMaxAB.elem(uvLLFind2) + vT_x.elem(uvLLFind2)) / (vMaxAB.elem(uvLLFind2) + vT_cal.elem(uvLLFind2));

  for( it = uvLLFind2.begin(); it!=uvLLFind2.end(); it++)
    vF2(*it) = pow( vF2(*it), r + s + vX(*it) );

  vF2.elem(uvLLFind2) = log( 1 - vF2.elem(uvLLFind2));


  vPartF.elem(uvLLFind2) = vF1.elem(uvLLFind2) + vF2.elem(uvLLFind2);


  // Calculate LL
  arma::vec vPart1(n), vPart2(n), vPart3(n), vLL(n);

  // calc part1: lgamma is not vectorised, hence loop
  vPart1 = r * log(vAlpha_i) + s * log(vBeta_i);
  for( int i = 0; i<n; i++)
    vPart1(i) += -lgamma(r) + lgamma(r + vX(i));

  vPart2 = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  vPart3 = log(s) - arma::log(r + s + vX) + vPartF;

  // For numerical stability rewrite
  //  log(exp(a) + exp(b))
  //            as
  //  max(a,b) + log(exp(a-max(a,b)) + exp(b-max(a,b)))
  //
  // There still can be problems with vX as then vPart1 gets too large (lgamma(vX))
  //
  arma::vec vMaxPart23 = arma::max(vPart2, vPart3);
  vLL = vPart1 + (vMaxPart23 + arma::log( arma::exp(vPart2 - vMaxPart23) +
    arma::exp(vPart3 - vMaxPart23)));

  return (vLL);

}


//' @rdname pnbd_nocov_LL_sum
// [[Rcpp::export]]
arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){

  const double r       = exp(vLogparams(0));
  const double alpha_0 = exp(vLogparams(1));
  const double s       = exp(vLogparams(2));
  const double beta_0  = exp(vLogparams(3));

  const double n = vX.n_elem;



  // Build alpha and beta --------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill(beta_0);


  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer

  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);
  return(vLL);
}


//' @title Pareto/NBD: LogLikelihood without covariates
//'
//' @description
//' Pareto/NBD without Covariates:
//'
//' The function \code{pnbd_nocov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters.
//'
//' The function \code{pnbd_nocov_LL_sum} calculates the LogLikelihood value summed
//' across customers for the given parameters.
//'
//' @param vLogparams vector with the Pareto/NBD model parameters log scaled
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{r, alpha_0, s, beta_0} are the parameters used for estimation.\cr
//' s: shape parameter of the Gamma distribution for the lifetime process.
//' The smaller s, the stronger the heterogeneity of customer lifetimes. \cr
//' beta: scale parameter for the Gamma distribution for the lifetime process. \cr
//' r: shape parameter of the Gamma distribution of the purchase process.
//' The smaller r, the stronger the heterogeneity of the purchase process.\cr
//' alpha: scale parameter of the Gamma distribution of the purchase process.
//'
//'@return
//'  Returns the respective LogLikelihood value for the Pareto/NBD model without covariates.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
// [[Rcpp::export]]
double pnbd_nocov_LL_sum(const arma::vec& vLogparams,
                         const arma::vec& vX,
                         const arma::vec& vT_x,
                         const arma::vec& vT_cal){

  arma::vec vLL = pnbd_nocov_LL_ind(vLogparams,
                                    vX,
                                    vT_x,
                                    vT_cal);

  // accu sums all elements, indifferent of axis
  return(-arma::sum(vLL));
}


//' @title Pareto/NBD: LogLikelihood with static covariates
//'
//' @description
//' Pareto/NBD with Static Covariates:
//'
//' The function \code{pnbd_staticcov_LL_ind} calculates the individual LogLikelihood
//' values for each customer for the given parameters and covariates.
//'
//' The function \code{pnbd_staticcov_LL_sum} calculates the individual LogLikelihood values summed
//' across customers.
//'
//' @param vParams vector with the parameters for the Pareto/NBD model and the static covariates. See Details.
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//'
//' @details
//' \code{vParams} is vector with the Pareto/NBD model parameters at log scale,
//' followed by the parameters for the lifetime covariate at original scale and then
//' followed by the parameters for the transaction covariate at original scale
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column, a gamma parameter
//' needs to added to \code{vParams} at the respective position.
//'
//'
//'@return
//'  Returns the respective LogLikelihood value for the Pareto/NBD model with static covariates.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
// [[Rcpp::export]]
arma::vec pnbd_staticcov_LL_ind(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans){

  const double no_cov_life  = mCov_life.n_cols;
  const double no_cov_trans = mCov_trans.n_cols;

  const arma::vec vModel_log_params = vParams.subvec(0,3);  // elements 0,1,2,3 = 4 params
  const arma::vec vLife_params      = vParams.subvec(4              , 4+no_cov_life                - 1);
  const arma::vec vTrans_params     = vParams.subvec(4 + no_cov_life, 4+no_cov_life + no_cov_trans - 1);

  const double r        = exp(vModel_log_params(0));
  const double alpha_0  = exp(vModel_log_params(1));
  const double s        = exp(vModel_log_params(2));
  const double beta_0   = exp(vModel_log_params(3));

  const double n = vX.n_elem;



  // Build alpha and beta --------------------------------------------
  //    With static covariates: alpha and beta different per customer
  //
  //    alpha_i: alpha0 * exp(-cov.trans * cov.params.trans)
  //    beta_i:  beta0  * exp(-cov.life  * cov.parama.life)
  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vTrans_params));
  vBeta_i  = beta_0  * arma::exp(((mCov_life  * (-1)) * vLife_params));

  // Calculate LL ----------------------------------------------------
  //    Calculate value for every customer
  arma::vec vLL = pnbd_LL_ind(r, s, vAlpha_i, vBeta_i, vX, vT_x, vT_cal);

  return(vLL);
}



//' @rdname pnbd_staticcov_LL_ind
// [[Rcpp::export]]
double pnbd_staticcov_LL_sum(const arma::vec& vParams,
                             const arma::vec& vX,
                             const arma::vec& vT_x,
                             const arma::vec& vT_cal,
                             const arma::mat& mCov_life,
                             const arma::mat& mCov_trans){


  // Call and return summed values ----------------------------
  arma::vec vLL = pnbd_staticcov_LL_ind(vParams,
                                        vX,
                                        vT_x,
                                        vT_cal,
                                        mCov_life,
                                        mCov_trans);

  return(-arma::sum(vLL));
}
