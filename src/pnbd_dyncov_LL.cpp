#include "pnbd_dyncov_LL.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppGSL)]]


// [[Rcpp::export]]
arma::vec pnbd_dyncov_LL_Bi_cpp(const int i,
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




arma::vec pnbd_dyncov_LL_Di1_cpp(const int i,
                                 const arma::vec& d,
                                 const arma::ivec& n_walks,
                                 const arma::vec& max_walks,
                                 const arma::vec& adj_walk1,
                                 const arma::mat& walks,
                                 const arma::ivec& kxT)
{
  //C++ translation of CLVTools function .pnbd_dyncov_LL_Di1_gen

  int n = adj_walk1.n_elem;

  int most_walks = max(n_walks);
  arma::vec Di1 = d % adj_walk1;

  if(most_walks <= 2){
    if(i > 1){
      for(int j = 0; j < n; j++){
        if(arma::is_finite(max_walks(j))){
          Di1(j) += max_walks(j);
        }
      }
    }
  }else{
    for(int j = 0; j < n; j++){
      for(int k = 1; k < most_walks - 1; k++){
        if(arma::is_finite(walks(j,k))){
          Di1(j) += walks(j,k);
        }
      }
    }
    if(i > 1){
      for(int j = 0; j < n; j++){
        if(kxT(j) > 1 && arma::is_finite(max_walks(j))){
          Di1(j) += max_walks(j);
        }
      }
    }
  }

  return(Di1);

}


arma::vec pnbd_dyncov_LL_Di2_cpp(const int i,
                                 const arma::vec& d,
                                 const arma::vec& d_omega,
                                 const arma::ivec& n_walks,
                                 const arma::vec& max_walks,
                                 const arma::mat& walks,
                                 const arma::ivec& k0x)
{
  //C++ translation of CLVTools function .pnbd_dyncov_LL_Di2_gen

  int n = d.n_elem;
  arma::vec Di2(n);

  for(int j = 0; j < n; j++){
    double delta = 0;
    if((k0x(j) + i - 1) > 1){
      delta = 1;
    }
    if(n_walks(j) > i){
      Di2(j) = (-d_omega(j) - delta*(k0x(j) + i - 3)) *
        walks(j, i - 1);
    }else{
      Di2(j) = (-d_omega(j) - delta*(k0x(j) + n_walks(j) - 3)) *
        max_walks(j);
    }
    if(!arma::is_finite(Di2(j))){
      Di2(j) = 0;
    }
    if(i > 2){
      for(int k = 1; k < i - 1; k++){
        if(arma::is_finite(walks(j,k))){
          Di2(j) += walks(j,k);
        }
      }
    }
  }

  return(Di2);

}

// [[Rcpp::export]]
arma::vec pnbd_dyncov_LL_Di_cpp(const int i,
                                const arma::vec& real_d,
                                const arma::vec& aux_d,
                                const arma::ivec& real_n_walks,
                                const arma::ivec& aux_n_walks,
                                const arma::vec& real_max_walks,
                                const arma::vec& aux_max_walks,
                                const arma::vec& real_adj_walk1,
                                const arma::mat& real_walks,
                                const arma::mat& aux_walks)
{
  //C++ translation of CLVTools function .pnbd_dyncov_LL_Di
  arma::vec Di = pnbd_dyncov_LL_Di1_cpp(i, real_d, real_n_walks,
                                        real_max_walks, real_adj_walk1,
                                        real_walks, aux_n_walks) +
                                          pnbd_dyncov_LL_Di2_cpp(i, aux_d, real_d,
                                                           aux_n_walks, aux_max_walks,
                                                           aux_walks, real_n_walks);
  // just for debugging
  // mat Di(real_d.n_elem, 2);
  //
  // Di.col(0) = pnbd_dyncov_LL_Di1_cpp(i, real_d, real_n_walks,
  //        real_max_walks, real_adj_walk1,
  //        real_walks, aux_n_walks);
  // Di.col(1) =  pnbd_dyncov_LL_Di2_cpp(i, aux_d, real_d,
  //        aux_n_walks, aux_max_walks,
  //        aux_walks, real_n_walks);
  return(Di);
}





