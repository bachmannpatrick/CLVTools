#ifndef CLV_VEC_HPP
#define CLV_VEC_HPP

namespace clv{
// vec_hyp2F1
//    all inputs as vectors
arma::vec vec_hyp2F1(const arma::vec& vA, const arma::vec& vB, const arma::vec& vC, const arma::vec& vX);

// vec_x_hyp1F1
//    X as vector, a, b as scalars
arma::vec vec_x_hyp1F1(double a, double b, const arma::vec& vX);

arma::vec vec_pow(const arma::vec& vA, const arma::vec& vP);

arma::vec lbeta(arma::vec& vA, arma::vec& vB);
}

#endif
