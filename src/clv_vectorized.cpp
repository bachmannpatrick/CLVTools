

#include <RcppArmadillo.h>
#include <RcppGSL.h>
#include <gsl/gsl_sf_hyperg.h>


namespace clv{


// vec_hyp2F1 --------------------------------------------------
//    All params as same-length vectors
//
arma::vec vec_hyp2F1(const arma::vec& vA, const arma::vec& vB, const arma::vec& vC, const arma::vec& vX){

  arma::vec vRes(vA);
  arma::uword n = vA.n_elem;

  for(arma::uword i = 0; i<n; i++)
    vRes(i) = gsl_sf_hyperg_2F1(vA(i), vB(i), vC(i), vX(i));

  return(vRes);
}



// vec_x_hyp1F1 ----------------------------------------------------
//    a, b:     scalars
//    X:        vector
//
//    hypergeom1F1(double a, double b, double x);
arma::vec vec_x_hyp1F1(const double a, const double b, const arma::vec& vX){

  arma::vec vRes(vX);

  arma::uword n = vX.n_elem;
  for(arma::uword i = 0; i<n; i++)
    vRes(i) = gsl_sf_hyperg_1F1(a, b, vX(i));

  return(vRes);
}


// vec_pow --------------------------------------------------------
//    element-by-element pow of the two given vectors
arma::vec vec_pow(const arma::vec& vA, const arma::vec& vP){
  arma::vec vRes(vA);
  arma::vec::const_iterator it_a = vA.begin(), it_p = vP.begin(), it_a_end = vA.end();
  arma::vec::iterator it_res = vRes.begin();

  while(it_a != it_a_end){
    (*it_res) = std::pow(*it_a, *it_p);
    it_a++;
    it_p++;
    it_res++;
  }

  return(vRes);
}

}
