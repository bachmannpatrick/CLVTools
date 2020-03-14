#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "cephes_hypergeom2f1.h"

// [[Rcpp::depends(RcppArmadillo)]]
arma::vec pnbd_PAlive(  const arma::vec& vEstimated_model_params,
                        const arma::vec& vX,
                        const arma::vec& vT_x,
                        const arma::vec& vT_cal,
                        const arma::vec& vAlpha_i,
                        const arma::vec& vBeta_i)
{


  const int n = vX.n_elem;

  const double r       = vEstimated_model_params(0);
  // const double alpha_0 = vEstimated_model_params(1);
  const double s       = vEstimated_model_params(2);
  // const double beta_0  = vEstimated_model_params(3);



  arma::vec  vPAlive(n);

  double tmpF1 = 0.0, tmpF2=0.0, tmpA0 = 0.0;
  unsigned int i, end = vX.n_elem;
  for( i=0; i<end; i++){

    //    if (nrow(data.tmp2) !=0) {
    //    #if (alpha >= beta) {
    //        data.tmp2$F1 <- hyperg_2F1(r + s + data.tmp2$x, s + 1, r + s + data.tmp2$x + 1, (data.tmp2$alpha_i -
    //            data.tmp2$beta_i)/(data.tmp2$alpha_i + data.tmp2$t.x))
    //        data.tmp2$F2 <- hyperg_2F1(r + s + data.tmp2$x, s + 1, r + s + data.tmp2$x + 1, (data.tmp2$alpha_i -
    //            data.tmp2$beta_i)/(data.tmp2$alpha_i + data.tmp2$T.cal))
    //        data$A0[data$Fselect == 2] <- data.tmp2$F1/((data.tmp2$alpha_i + data.tmp2$t.x)^(r + s + data.tmp2$x)) - data.tmp2$F2/((data.tmp2$alpha_i + data.tmp2$T.cal)^(r +
    //            s + data.tmp2$x))
    //    }

    if( vAlpha_i(i) >= vBeta_i(i) ){

      tmpF1 = cephes::hypergeom2F1(  r + s + vX(i),
                                     s + 1,
                                     r + s + vX(i) + 1,
                                     (vAlpha_i(i) - vBeta_i(i))/ (vAlpha_i(i) + vT_x(i))
      );

      tmpF2 = cephes::hypergeom2F1(  r + s + vX(i),
                                     s + 1,
                                     r + s + vX(i) + 1,
                                     (vAlpha_i(i) - vBeta_i(i)) / (vAlpha_i(i) + vT_cal(i))
      );

      tmpA0 = tmpF1 / pow(vAlpha_i(i) + vT_x(i), r + s + vX(i));
      tmpA0 -= tmpF2 / pow(vAlpha_i(i) + vT_cal(i) , r + s + vX(i));


    }else{
      //      vAlpha_i(i) < vBeta_i(i)

      //      if (nrow(data.tmp1) !=0) {
      //    #else {
      //        data.tmp1$F1 <- hyperg_2F1(r + s + data.tmp1$x, r + data.tmp1$x, r + s + data.tmp1$x + 1, (data.tmp1$beta_i -
      //            data.tmp1$alpha_i)/(data.tmp1$beta_i + data.tmp1$t.x))
      //        data.tmp1$F2 <- hyperg_2F1(r + s + data.tmp1$x, r + data.tmp1$x, r + s + data.tmp1$x + 1, (data.tmp1$beta_i -
      //            data.tmp1$alpha_i)/(data.tmp1$beta_i + data.tmp1$T.cal))
      //        data$A0[data$Fselect == 1] <- data.tmp1$F1/((data.tmp1$beta_i + data.tmp1$t.x)^(r + s + data.tmp1$x)) - data.tmp1$F2/((data.tmp1$beta_i + data.tmp1$T.cal)^(r +
      //            s + data.tmp1$x))
      //    }

      tmpF1 = cephes::hypergeom2F1(  r + s + vX(i),
                                     r + vX(i),
                                     r + s + vX(i) + 1,
                                     (vBeta_i(i) - vAlpha_i(i)) / (vBeta_i(i) + vT_x(i) ) );

      tmpF2 = cephes::hypergeom2F1(  r + s + vX(i),
                                     r + vX(i),
                                     r + s + vX(i) + 1,
                                     (vBeta_i(i) - vAlpha_i(i)) / (vBeta_i(i) + vT_cal(i))
      );

      tmpA0 = tmpF1 / pow(vBeta_i(i) + vT_x(i) , r + s + vX(i));
      tmpA0 -= tmpF2 / pow(vBeta_i(i) + vT_cal(i), r + s + vX(i));


    }//if

    //  data$PAlive <- ((1 + s/(r + s + data$x) * (data$alpha_i + data$T.cal)^(r + data$x) * (data$beta_i +
    //  data$T.cal)^s * data$A0)^(-1))
    vPAlive(i) = 1/( 1  + ( s / (r+s+vX(i))  * pow(vAlpha_i(i) + vT_cal(i), r + vX(i))  * pow( (vBeta_i(i) + vT_cal(i)), s) * tmpA0)   );

  }//for


  return vPAlive;


}//function


//' @title Pareto/NBD: PAlive without covariates
//'
//' @description
//' Pareto/NBD without Covariates: Calculates the probability of a customer being alive
//' at the end of the calibration period.
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppxtxtcal
//'
//' @details
//' \code{vEstimated_params} vector with the estimated parameters in original scale
//' for the Pareto/NBD model, namely (r, alpha, s, beta).
//' r and alpha: unobserved parameters that describe the NBD transaction process.
//' s and beta: unobserved parameters that describe the pareto
//' (exponential gamma) dropout process.
//'
//'
//'@return
//'Returns a vector with the PAlive for each customer.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @name pnbd_nocov_PAlive
//' @rdname pnbd_nocov_PAlive
// [[Rcpp::export]]
arma::vec pnbd_nocov_PAlive(const arma::vec& vEstimated_params,
                            const arma::vec& vX,
                            const arma::vec& vT_x,
                            const arma::vec& vT_cal){


  // Build alpha and beta --------------------------------------------------------
  //    No covariates: Same alphas, betas for every customer
  const double n = vX.n_elem;
  // const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  // const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i.fill(alpha_0);
  vBeta_i.fill( beta_0);


  // Calculate PAlive -------------------------------------------------------------
  // pnbd_PAlive(vEstimated_model_params, vX,
  //              vT_x, vT_cal,
  //              vAlpha_i, vBeta_i);

  return pnbd_PAlive(vEstimated_params,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}



//' @title Pareto/NBD: PAlive with static covariates
//'
//' @description
//' Pareto/NBD with Static Covariates: Calculates the probability of a customer
//' being alive (PAlive) at the end of the calibration period.
//'
//' @template template_params_rcppestimatedparams
//' @template template_params_rcppxtxtcal
//' @template template_params_rcppcovmatrix
//' @template template_params_rcppvcovparams
//'
//' @details
//' \code{vEstimated_params} vector with the estimated parameters in original scale
//' for the Pareto/NBD model, namely (r, alpha, s, beta).
//' r and alpha: unobserved parameters that describe the NBD transaction process.
//' s and beta: unobserved parameters that describe the pareto
//' (exponential gamma) lifetime process.
//'
//' \code{mCov_trans} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the transaction process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_trans} at the respective position.
//'
//' \code{mCov_life} is a matrix containing the covariates data of
//' the time-invariant covariates that affect the lifetime process.
//' Each column represents a different covariate. For every column a gamma parameter
//' needs to added to \code{vCovParams_life} at the respective position.
//'
//'@return
//'Returns a vector containing the PAlive for each customer.
//'
//'@references
//'  Fader, Peter S., and Bruce G.S. Hardie (2005). "A Note on Deriving the
//'  Pareto/NBD Model and Related Expressions.", Web.
//'  \url{http://www.brucehardie.com/notes/008/}.
//'
//' @name pnbd_staticcov_PAlive
//' @rdname pnbd_staticcov_PAlive
//'
// [[Rcpp::export]]
arma::vec pnbd_staticcov_PAlive(const arma::vec& vEstimated_params,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::vec& vCovParams_trans,
                                const arma::vec& vCovParams_life,
                                const arma::mat& mCov_trans,
                                const arma::mat& mCov_life){


  if(vCovParams_trans.n_elem != mCov_trans.n_cols)
    throw std::out_of_range("Vector of transaction parameters need to have same length as number of columns in transaction covariates!");

  if(vCovParams_life.n_elem != mCov_life.n_cols)
    throw std::out_of_range("Vector of lifetime parameters need to have same length as number of columns in lifetime covariates!");

  if(vX.n_elem != mCov_trans.n_rows |
     vX.n_elem != mCov_life.n_rows)
    throw std::out_of_range("There need to be as many covariate rows as customers!");


  // Build alpha and beta --------------------------------------------
  //  Static covariates: Different alpha/beta for every customer

  const double n = vX.n_elem;

  // const double r       = vEstimated_params(0);
  const double alpha_0 = vEstimated_params(1);
  // const double s       = vEstimated_params(2);
  const double beta_0  = vEstimated_params(3);

  arma::vec vAlpha_i(n), vBeta_i(n);

  vAlpha_i = alpha_0 * arma::exp(((mCov_trans * (-1)) * vCovParams_trans));
  vBeta_i  = beta_0  * arma::exp(((mCov_life     * (-1)) * vCovParams_life));

  // Calculate PAlive -------------------------------------------------------------
  // pnbd_PAlive(vEstimated_model_params, vX,
  //              vT_x, vT_cal,
  //              vAlpha_i, vBeta_i);

  return pnbd_PAlive(vEstimated_params,
                     vX,
                     vT_x,
                     vT_cal,
                     vAlpha_i,
                     vBeta_i);
}

