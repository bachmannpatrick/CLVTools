// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <math.h>
#include <vector>

#include "cephes_hypergeom2f1.h"

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





