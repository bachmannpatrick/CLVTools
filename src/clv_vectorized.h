#ifndef CLV_VEC_HPP
#define CLV_VEC_HPP
#include <RcppGSL.h>
#include <gsl/gsl_sf_hyperg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_result.h>

Rcpp::List vec_gsl_hyp2f0_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vZ);

Rcpp::List vec_gsl_hyp2f1_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vC, const RcppGSL::Vector& vZ);

namespace clv{
// vec_hyp2F1
//    all inputs as vectors
arma::vec vec_hyp2F1(const arma::vec& vA, const arma::vec& vB, const arma::vec& vC, const arma::vec& vX);

// vec_x_hyp1F1
//    X as vector, a, b as scalars
arma::vec vec_x_hyp1F1(double a, double b, const arma::vec& vX);

arma::vec vec_pow(const arma::vec& vA, const arma::vec& vP);

arma::vec vec_fill(const double number, const double repetitions);

}

#endif
