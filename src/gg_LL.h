#ifndef GG_LL_HPP
#define GG_LL_HPP

arma::vec lbeta(const arma::vec& a, const double b);

double gg_LL(const arma::vec& vLogparams,
             const arma::vec& vX,
             const arma::vec& vM_x);

#endif
