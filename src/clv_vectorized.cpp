#include <RcppArmadillo.h>

#include "clv_vectorized.h"


//' @title GSL Hypergeom 2f0 for equal length vectors
//'
//' @param vA Vector of values for parameter a
//' @param vB Vector of values for parameter b
//' @param vZ Vector of values for parameter z
//'
//' @description Calculate the hypergeometric 2f0 using the GSL library (gsl_sf_hyperg_2F0_e)
//' @return List with vector of values and vector of gsl status codes
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List vec_gsl_hyp2f0_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vZ){

  if((vA->size != vB->size) || (vB->size != vZ->size))
    throw std::runtime_error(std::string("Not all vectors are of the same length!"));

  // Do not abort in case of error
  gsl_set_error_handler_off();

  const size_t n = vA->size;

  RcppGSL::Vector vRes(n);
  RcppGSL::IntVector vStatus(n);
  gsl_sf_result gsl_res;

  for(size_t i = 0; i<n; i++){
    vStatus[i] = gsl_sf_hyperg_2F0_e(vA[i], vB[i], vZ[i], &gsl_res);
    vRes[i] = gsl_res.val;
    // gsl_res.err
  }

  return Rcpp::List::create(Rcpp::Named("value") = Rcpp::wrap(vRes),
                            Rcpp::Named("status") = Rcpp::wrap(vStatus));
}

//' @title GSL Hypergeom 2f1 for equal length vectors
//'
//' @param vA Vector of values for parameter a
//' @param vB Vector of values for parameter b
//' @param vC Vector of values for parameter c
//' @param vZ Vector of values for parameter z
//'
//' @description Calculate the hypergeometric 2f1 using the GSL library (gsl_sf_hyperg_2F1_e)
//' @return List with vector of values and vector of gsl status codes
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List vec_gsl_hyp2f1_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vC, const RcppGSL::Vector& vZ){

  if((vA->size != vB->size) || (vB->size != vC->size) || (vC->size != vZ->size))
    throw std::runtime_error(std::string("Not all vectors are of the same length!"));

  // Do not abort in case of error
  gsl_set_error_handler_off();

  const size_t n = vA->size;

  RcppGSL::Vector vRes(n);
  RcppGSL::IntVector vStatus(n);
  gsl_sf_result gsl_res;

  for(size_t i = 0; i<n; i++){
    vStatus[i] = gsl_sf_hyperg_2F1_e(vA[i], vB[i], vC[i], vZ[i], &gsl_res);
    vRes[i] = gsl_res.val;
  }

  return Rcpp::List::create(Rcpp::Named("value") = Rcpp::wrap(vRes),
                            Rcpp::Named("status") = Rcpp::wrap(vStatus));
}


namespace clv{

// vec_hyp2F1 --------------------------------------------------
//    All params as same-length vectors

arma::vec vec_hyp2F1(const arma::vec& vA, const arma::vec& vB, const arma::vec& vC, const arma::vec& vX){

  // Do not abort in case of error
  gsl_set_error_handler_off();

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

  // Do not abort in case of error
  gsl_set_error_handler_off();

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

arma::vec vec_fill(const double number, const double repetitions){
  arma::vec vResult(repetitions);

  vResult.fill(number);

  return vResult;
}

}
