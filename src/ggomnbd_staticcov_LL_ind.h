#ifndef GGOMNBD_STATICCOV_LL_IND_HPP
#define GGOMNBD_STATICCOV_LL_IND_HPP

arma::vec ggomnbd_staticcov_LL_ind(const arma::vec& vParams,
                                const arma::vec& vX,
                                const arma::vec& vT_x,
                                const arma::vec& vT_cal,
                                const arma::mat& mCov_life,
                                const arma::mat& mCov_trans);

#endif
