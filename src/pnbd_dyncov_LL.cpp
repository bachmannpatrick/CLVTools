#include "pnbd_dyncov_LL.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppGSL)]]


// [[Rcpp::export]]
arma::vec pnbd_dyncov_LL_Bi_cpp(const int& i,
                                const arma::vec& t_x,
                                const arma::vec& d,
                                const arma::vec& delta,
                                const arma::ivec& n_walks,
                                const arma::vec& max_walks,
                                const arma::mat& walks)
{
  //C++ translation of CLVTools function .pnbd_dyncov_LL_Bi
  int n = t_x.n_elem;
  //just initialize with first walk
  //cuz we need it whether or not i %in% (1,2)
  //also just create single object
  //because Aji and Aki just get added together in the end
  arma::vec out = walks.col(0) % d;


  //Aji part
  if(i > 2){
    for(int j = 0; j < n; j++){
      for(int k = 1; k < i - 1; k++){
        //low-level equivalent of sum(..., na.rm = TRUE)
        if(arma::is_finite(walks(j,k))){
          out(j) += walks(j,k);
        }
      }
    }
  }

  //Aki part
  if(i == 1){
    out += walks.col(0) % (-t_x - d);
  }else{
    for(int j = 0; j < n; j++){
      if(n_walks(j) <= i){
        out(j) += max_walks(j) *
          (-t_x(j) - d(j) - (delta(j) * (n_walks(j) - 2)));
      }else{
        out(j) += walks(j, i - 1) *
          (-t_x(j) - d(j) - (delta(j) * (i - 2)));
      }
    }
  }

  return(out);

}
