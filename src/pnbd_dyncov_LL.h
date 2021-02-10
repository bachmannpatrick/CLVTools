#ifndef PNBD_DYNCOVLL_HPP
#define PNBD_DYNCOVLL_HPP

#include <RcppArmadillo.h>


arma::vec pnbd_dyncov_LL_Bi_cpp(const int& i,
                                const arma::vec& t_x,
                                const arma::vec& d,
                                const arma::vec& delta,
                                const arma::ivec& n_walks,
                                const arma::vec& max_walks,
                                const arma::mat& walks);



#endif
