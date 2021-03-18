// The code in this file has been kindly donated by Elliot Shin Oblander
// These functions serve as drop-in replacements for the previously existing R implementations

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

  int most_walks = n_walks.max();
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
  arma::vec Di2(n, arma::fill::zeros);

  for(int j = 0; j < n; j++){
    double delta = 0.0;
    if((k0x(j) + i - 1) > 1){
      delta = 1.0;
    }
    if(n_walks(j) > i){
      Di2(j) = (-d_omega(j) - delta*(((double)k0x(j)) + ((double)i) - 3.0)) * walks(j, i - 1);
    }else{
      Di2(j) = (-d_omega(j) - delta*(((double)k0x(j)) + ((double)n_walks(j)) - 3.0)) * max_walks(j);
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



// [[Rcpp::export]]
arma::vec hyp_alpha_ge_beta_cpp(const arma::vec& alpha_1,
                                const arma::vec& beta_1,
                                const arma::vec& alpha_2,
                                const arma::vec& beta_2,
                                const arma::vec& x,
                                const double r,
                                const double s)
{
  //C++ translation of .hyp.alpha.ge.beta
  int n = x.n_elem;
  arma::vec out(n);

  if(n > 0){
    arma::vec z1 = 1 - (beta_1/alpha_1);
    arma::vec z2 = 1 - (beta_2/alpha_2);
    arma::vec log_C = arma::lgamma(r + s + x + 1) +
      std::lgamma(s) +
      arma::lgamma(r + s + x) +
      std::lgamma(s + 1);

    for(int j = 0; j < n; j++){
      gsl_sf_result gsl_res;
      double hyp_z1;
      double hyp_z2;

      int status = gsl_sf_hyperg_2F1_e(r + s + x(j),
                                       s + 1,
                                       r + s + x(j) + 1,
                                       z1(j), &gsl_res);
      if(status == 11 || status == 1){
        hyp_z1 = std::pow(1 - z1(j), r + x(j)) *
          std::exp(log_C(j)) /
            std::pow(beta_1(j), r + s + x(j));
      }else{
        hyp_z1 = gsl_res.val /
          std::pow(alpha_1(j), r + s + x(j));
      }

      status = gsl_sf_hyperg_2F1_e(r + s + x(j),
                                   s + 1,
                                   r + s + x(j) + 1,
                                   z2(j), &gsl_res);
      if(status == 11 || status == 1){
        hyp_z2 = std::pow(1 - z2(j), r + x(j)) *
          std::exp(log_C(j)) /
            std::pow(beta_2(j), r + s + x(j));
      }else{
        hyp_z2 = gsl_res.val /
          std::pow(alpha_2(j), r + s + x(j));
      }

      out(j) = hyp_z1 - hyp_z2;

    }
  }

  return(out);

}

// [[Rcpp::export]]
arma::vec hyp_beta_g_alpha_cpp(const arma::vec& alpha_1,
                               const arma::vec& beta_1,
                               const arma::vec& alpha_2,
                               const arma::vec& beta_2,
                               const arma::vec& x,
                               const double r,
                               const double s)
{
  //C++ translation of .hyp.beta.e.alpha
  int n = x.n_elem;
  arma::vec out(n);

  if(n > 0){
    arma::vec z1 = 1 - (alpha_1/beta_1);
    arma::vec z2 = 1 - (alpha_2/beta_2);
    arma::vec log_C = arma::lgamma(r + s + x + 1) +
      arma::lgamma(r + x + 1) +
      arma::lgamma(r + s + x) +
      arma::lgamma(r + x);

    for(int j = 0; j < n; j++){
      gsl_sf_result gsl_res;
      double hyp_z1;
      double hyp_z2;

      int status = gsl_sf_hyperg_2F1_e(r + s + x(j),
                                       r + x(j),
                                       r + s + x(j) + 1,
                                       z1(j), &gsl_res);
      if(status == 11 || status == 1){
        hyp_z1 = std::pow(1 - z1(j), s + 1) *
          std::exp(log_C(j)) /
            std::pow(alpha_1(j), r + s + x(j));
      }else{
        hyp_z1 = gsl_res.val /
          std::pow(beta_1(j), r + s + x(j));
      }

      status = gsl_sf_hyperg_2F1_e(r + s + x(j),
                                   r + x(j),
                                   r + s + x(j) + 1,
                                   z2(j), &gsl_res);
      if(status == 11 || status == 1){
        hyp_z2 = std::pow(1 - z2(j), s + 1) *
          std::exp(log_C(j)) /
            std::pow(alpha_2(j), r + s + x(j));
      }else{
        hyp_z2 = gsl_res.val /
          std::pow(beta_2(j), r + s + x(j));
      }

      out(j) = hyp_z1 - hyp_z2;

    }
  }

  return(out);
}






// [[Rcpp::export]]
arma::vec F2_3_vecs_cpp(const arma::ivec& n_walks_cbs,
                        const arma::vec& dT_cbs,
                        const arma::vec& Bjsum_cbs,
                        const arma::vec& x_cbs,
                        const arma::vec& t_x_cbs,
                        const arma::ivec& n_walks_trans,
                        const arma::mat& walks_trans,
                        const arma::vec& d_trans,
                        const arma::vec& delta_trans,
                        const arma::vec& max_walks_trans,
                        const arma::ivec& n_walks_life_real,
                        const arma::vec& d_life_real,
                        const arma::vec& max_walks_life_real,
                        const arma::vec& adj_walk1_life_real,
                        const arma::mat& walks_life_real,
                        const arma::ivec& n_walks_life_aux,
                        const arma::vec& d_life_aux,
                        const arma::vec& max_walks_life_aux,
                        const arma::mat& walks_life_aux,
                        const double r,
                        const double alpha,
                        const double s,
                        const double beta)
{
  //C++ translation of foreach/%dopar% loop over walks
  //eventually should restructure so:
  //outer loop is customers, inner loop is periods/Num.Walks
  //will save a lot of memory by not making so many n-length temp objects

  int imax = n_walks_cbs.max() - 1;
  int n = n_walks_cbs.n_elem;
  arma::vec res(n, arma::fill::zeros);

  for(int i = 2; i <= imax; i++){
    arma::uvec cbs_i_inds = arma::find(n_walks_cbs - 1 >= i);
    arma::uvec trans_i_inds = arma::find(n_walks_trans - 1 >= i);
    arma::uvec life_i_inds = arma::find(n_walks_life_aux - 1 >= i);

    int n_trans = trans_i_inds.n_elem;
    int n_life = life_i_inds.n_elem;

    arma::vec Ai(n_trans, arma::fill::zeros);
    for(int j = 0; j < n_trans; j++){
      if(arma::is_finite(walks_trans(trans_i_inds(j), i - 1))){
        Ai(j) = walks_trans(trans_i_inds(j), i - 1);
      }
    }

    arma::vec Bi = pnbd_dyncov_LL_Bi_cpp(i,
                                         t_x_cbs.rows(cbs_i_inds),
                                         d_trans.rows(trans_i_inds),
                                         delta_trans.rows(trans_i_inds),
                                         n_walks_trans.rows(trans_i_inds),
                                         max_walks_trans.rows(trans_i_inds),
                                         walks_trans.rows(trans_i_inds));
    arma::vec ai = Bjsum_cbs.rows(cbs_i_inds) + Bi +
      Ai % (t_x_cbs.rows(cbs_i_inds) + dT_cbs.rows(cbs_i_inds) + i - 2);

    arma::vec Ci(n_life, arma::fill::zeros);
    for(int j = 0; j < n_life; j++){
      if(arma::is_finite(walks_life_aux(life_i_inds(j),i - 1))){
        Ci(j) = walks_life_aux(life_i_inds(j),i - 1);
      }
    }


    arma::vec Di = pnbd_dyncov_LL_Di_cpp(i,
                                         d_life_real.rows(life_i_inds),
                                         d_life_aux.rows(life_i_inds),
                                         n_walks_life_real.rows(life_i_inds),
                                         n_walks_life_aux.rows(life_i_inds),
                                         max_walks_life_real.rows(life_i_inds),
                                         max_walks_life_aux.rows(life_i_inds),
                                         adj_walk1_life_real.rows(life_i_inds),
                                         walks_life_real.rows(life_i_inds),
                                         walks_life_aux.rows(life_i_inds));

    arma::vec bi = Di + Ci % (t_x_cbs.rows(cbs_i_inds) +
      dT_cbs.rows(cbs_i_inds) + i - 2);

    arma::vec alpha_1 = alpha + ai;
    arma::vec beta_1 = (beta + bi) % Ai / Ci;
    arma::vec alpha_2 = alpha + ai + Ai;
    arma::vec beta_2 = (beta + bi + Ci) % Ai / Ci;

    arma::uvec a_ge_b_inds = arma::find(alpha_1 >= beta_1);
    arma::uvec b_g_a_inds = arma::find(beta_1 > alpha_1);

    arma::vec F2_3(n_life);
    arma::vec x_cbs_subset = x_cbs.rows(cbs_i_inds);

    if(a_ge_b_inds.n_elem > 0){
      F2_3.rows(a_ge_b_inds) = arma::pow(Ai.rows(a_ge_b_inds) / Ci.rows(a_ge_b_inds), s) %
          hyp_alpha_ge_beta_cpp(alpha_1.rows(a_ge_b_inds),
                                beta_1.rows(a_ge_b_inds),
                                alpha_2.rows(a_ge_b_inds),
                                beta_2.rows(a_ge_b_inds),
                                x_cbs_subset.rows(a_ge_b_inds),
                                r, s);
    }

    if(b_g_a_inds.n_elem > 0){
      F2_3.rows(b_g_a_inds) = arma::pow(Ai.rows(b_g_a_inds) / Ci.rows(b_g_a_inds), s) %
        hyp_beta_g_alpha_cpp(alpha_1.rows(b_g_a_inds),
                             beta_1.rows(b_g_a_inds),
                             alpha_2.rows(b_g_a_inds),
                             beta_2.rows(b_g_a_inds),
                             x_cbs_subset.rows(b_g_a_inds),
                             r, s);
    }

    res.rows(cbs_i_inds) += F2_3;

  }

  return(res);

}
