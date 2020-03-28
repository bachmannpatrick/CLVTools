#include <RcppArmadillo.h>
#include <math.h>
#include <RcppGSL.h>
#include <gsl/gsl_sf_hyperg.h>


//' @rdname gsl_hyp_2F1
// [[Rcpp::export]]
double gsl_hyp_2F1(const double a,
                    const double b,
                    const double c,
                    const double x){

    return gsl_sf_hyperg_2F1(a, b, c, x);
}
