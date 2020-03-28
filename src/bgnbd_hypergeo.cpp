#include <RcppArmadillo.h>
#include <math.h>
#include <RcppGSL.h>
#include <gsl/gsl_sf_hyperg.h>

//' @title GSL Hypergeom 2f1
//'
//' @description Calculate the hypergeometric 2f1 using the GSL library (gsl_sf_hyperg_2F1_e)
//' @return Returns the value of the gsl_sf_hyperg_2F1 call.
//' @keywords internal
// [[Rcpp::export]]
double gsl_hyp_2F1(const double a,
                    const double b,
                    const double c,
                    const double x){

    return gsl_sf_hyperg_2F1(a, b, c, x);
}
