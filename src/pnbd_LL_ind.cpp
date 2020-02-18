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

  //number of elements
  const unsigned int n = vX.n_elem;


  /*
  data$absab <- abs(data$alpha_i - data$beta_i)
  tryCatch(absab <- data[, "absab"])
  */
  arma::vec vABabs = arma::abs( (vAlpha_i - vBeta_i) );


  /*
  data$param2 <- (s + 1)
  data$param2[alpha_i < beta_i]  <- (r + data$x[alpha_i < beta_i])
  tryCatch(param2 <- data[, "param2"])
  */
  arma::vec vParam2(n);
  vParam2.fill((s+1));

  //save indices as used often
  arma::uvec uvAlphaBetaFindRes = find(vAlpha_i < vBeta_i);
  vParam2.elem( uvAlphaBetaFindRes ) = ( r + vX.elem(uvAlphaBetaFindRes) );

  /*
  data$maxab <- data$alpha_i
  data$maxab[data$alpha_i < data$beta_i] <- data$beta_i[data$alpha_i < data$beta_i]
  tryCatch(maxab <- data[, "maxab"])
  */
  arma::vec vMaxAB(vAlpha_i);
  vMaxAB.elem(uvAlphaBetaFindRes) = vBeta_i.elem(uvAlphaBetaFindRes);

  /*
    Distinguish betwen case abs(alpha_i - beta_i) == 0 and != 0

    data$LLselect  <- 1
    data$LLselect[abs(data$alpha_i - data$beta_i) == 0]  <- 2
    data.tmp1 <- subset(data, LLselect == 1)
    data.tmp2 <- subset(data, LLselect == 2)
  */
  arma::uvec uvLLFind1 = find( vABabs != 0.0) ;
  arma::uvec uvLLFind2 = find( vABabs == 0.0);


  arma::vec vF1(n), vF2(n), vPartF(n);;
  arma::uvec::const_iterator it, itEnd;

 /*
    Calculate Part F for case vABabs != 0

      data.tmp1$F1 <-  hyperg_2F1(r + s + data.tmp1$x, data.tmp1$param2, r + s + data.tmp1$x + 1, data.tmp1$absab/(data.tmp1$maxab + data.tmp1$t.x))
      data.tmp1$F2 <- hyperg_2F1(r + s + data.tmp1$x, data.tmp1$param2, r + s + data.tmp1$x + 1, data.tmp1$absab/(data.tmp1$maxab + data.tmp1$T.cal)) * ((data.tmp1$maxab + data.tmp1$t.x)/(data.tmp1$maxab + data.tmp1$T.cal))^(r + s + data.tmp1$x)
      data$partF[data$LLselect == 1] <- -(r + s + data.tmp1$x) * log(data.tmp1$maxab + data.tmp1$t.x) + log(data.tmp1$F1 - data.tmp1$F2)
  */

  //loop because hypergeom2F1 is not vectorised
  itEnd = uvLLFind1.end();
  for(it =uvLLFind1.begin(); it!=itEnd; it++)
  {
    try {

      vF1(*it) = cephes::hypergeom2F1( r + s + vX(*it),
                                      vParam2(*it),
                                      r + s + vX(*it) + 1,
                                      vABabs(*it) / (vMaxAB(*it) + vT_x(*it))
                                      );

      // Rcpp::Rcout<<"vABabs(*it): "<<vABabs(*it)<<std::endl;
      // Rcpp::Rcout<<"vMaxAB(*it): "<<vMaxAB(*it)<<std::endl;
      // Rcpp::Rcout<<"vABabs(*it) / (vMaxAB(*it) + vT_x(*it)): "<<vABabs(*it) / (vMaxAB(*it) + vT_x(*it))<<std::endl;
      // Rcpp::Rcout<<"-3: "<<vF1<<std::endl;
      vF2(*it) = cephes::hypergeom2F1( r + s + vX(*it),
                                      vParam2(*it),
                                      r + s + vX(*it) + 1,
                                      vABabs(*it)/(vMaxAB(*it) + vT_cal(*it))
                                      );

      // Rcpp::Rcout<<"vABabs(*it)/(vMaxAB(*it) + vT_cal(*it)): "<<vABabs(*it)/(vMaxAB(*it) + vT_cal(*it))<<std::endl;
      // Rcpp::Rcout<<"-2: "<<vF2<<std::endl;
      vF2(*it) *= pow( (vMaxAB(*it) + vT_x(*it))/(vMaxAB(*it) + vT_cal(*it)) , r + s + vX(*it));

      // Rcpp::Rcout<<"-1: "<<vF2<<std::endl;
    }catch(std::exception &e)
    {
      //print error location and cause. Stop and return NA to optimization
      Rcpp::Rcout<<"Exception in pnbd_LL_ind: "<<e.what()<<std::endl;
      //          ??
      arma::vec ret(1);
      ret.fill(NA_REAL);
      return(ret);
    }

  }//for
  // Rcpp::Rcout<<"1: "<<vF1<<std::endl;
  // Rcpp::Rcout<<"2: "<<vF2<<std::endl;
  vPartF.elem(uvLLFind1) = -(r + s + vX.elem(uvLLFind1)) % arma::log(vMaxAB.elem(uvLLFind1) + vT_x.elem(uvLLFind1)) + arma::log(vF1.elem(uvLLFind1) - vF2.elem(uvLLFind1));

  // Rcpp::Rcout<<"3: "<<vPartF<<std::endl;


   /*
    Calculate Part F for case vABabs == 0

      data.tmp2$F1 <- (-(r + s + data.tmp2$x) * log(data.tmp2$maxab + data.tmp2$t.x))
      data.tmp2$F2 <- log(1- ((data.tmp2$maxab + data.tmp2$t.x)/(data.tmp2$maxab + data.tmp2$T.cal))^(r + s + data.tmp2$x))
      data$partF[data$LLselect == 2] <- data.tmp2$F1 + data.tmp2$F2
  */

  // % is element wise mulitplication
  vF1.elem(uvLLFind2) = (-1 * (r + s + vX.elem(uvLLFind2))) % arma::log( vMaxAB.elem(uvLLFind2) + vT_x.elem(uvLLFind2) );
  // Rcpp::Rcout<<"4: "<<vF1<<std::endl;
  //pow is not vectorised for two vecs, hence loop
  vF2.elem(uvLLFind2) = (vMaxAB.elem(uvLLFind2) + vT_x.elem(uvLLFind2)) / (vMaxAB.elem(uvLLFind2) + vT_cal.elem(uvLLFind2));
  // Rcpp::Rcout<<"5: "<<vF2<<std::endl;
  for( it = uvLLFind2.begin(); it!=uvLLFind2.end(); it++)
      vF2(*it) = pow( vF2(*it), r + s + vX(*it) );
  // Rcpp::Rcout<<"6: "<<vF2<<std::endl;
  vF2.elem(uvLLFind2) = log( 1 - vF2.elem(uvLLFind2));
  // Rcpp::Rcout<<"7: "<<vF2<<std::endl;

  vPartF.elem(uvLLFind2) = vF1.elem(uvLLFind2) + vF2.elem(uvLLFind2);

  // Rcpp::Rcout<<"8: "<<vPartF<<std::endl;

  /*
    Calculate LL

      part1 <- r * log(alpha_i) + s * log(beta_i) - lgamma(r) + lgamma(r + x)
      part2 <- -(r + x) * log(alpha_i + T.cal) - s * log(beta_i + T.cal)
      part3 <- log(s) - log(r + s + data$x) + data$partF

      LL<- sum(part1 + log(exp(part2) + exp(part3)))
      return(-1 * LL)
  */

  arma::vec vPart1(n), vPart2(n), vPart3(n), vLL(n);

  // calc part1: lgamma is not vectorised, hence loop
  vPart1 = r * log(vAlpha_i) + s * log(vBeta_i);
  // Rcpp::Rcout<<"9: "<<vPart1<<std::endl;
  for( int i = 0; i<n; i++)
    vPart1(i) += -lgamma(r) + lgamma(r + vX(i));
  // Rcpp::Rcout<<"10: "<<vPart1<<std::endl;

  // % := element wise multiplication
  vPart2 = -(r + vX) % arma::log(vAlpha_i + vT_cal) - s * arma::log(vBeta_i + vT_cal);
  // Rcpp::Rcout<<"11: "<<vPart2<<std::endl;
  vPart3 = log(s) - arma::log(r + s + vX) + vPartF;
  // Rcpp::Rcout<<"12: "<<vPart3<<std::endl;

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


  // vLL = exp(vPart1) * arma::exp(vPart2) * arma::exp(vPart3) + 1;

  // exp(part1 ) * exp(log(exp(part2) + exp(part3)))
  // exp(part1 ) * (exp(part2) + exp(part3))


  // exp(part1 + exp(log(exp(part2) + exp(part3)) ))

  // Rcpp::Rcout<<"vLL: "<<vLL<<std::endl;
  // vLL = log(vLL);

  // Rcpp::Rcout<<"vPart1: "<<vPart1<<std::endl;
  // Rcpp::Rcout<<"vPart2: "<<vPart2<<std::endl;
  // Rcpp::Rcout<<"vPart3: "<<vPart3<<std::endl;
  // Rcpp::Rcout<<"arma::exp(vPart2): "<<arma::exp(vPart2)<<std::endl;
  // Rcpp::Rcout<<"arma::exp(vPart3): "<<arma::exp(vPart3)<<std::endl;
  // Rcpp::Rcout<<"arma::log(arma::exp(vPart2) + arma::exp(vPart3)): "<<arma::log(arma::exp(vPart2) + arma::exp(vPart3))<<std::endl;
  //
  // Rcpp::Rcout<<"end: "<<vLL<<std::endl;
  return (vLL);

}

