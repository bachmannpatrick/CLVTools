#include "pnbd_dyncov_LL_new.h"

// arma::vec pnbd_dyncov_LL_i_cov_exp(arma::vec params, arma::vec cov){
//   return();
// }

// struct CBS {
//   CBS(const int x, const double t_x, const double T_cal):x(x), t_x(t_x), T_cal(T_cal){};
//   const int x;
//   const double t_x;
//   const double T_cal;
// };

struct Customer {
  Walk aux_walk_life, aux_walk_trans;
  std::vector<Walk> real_walks_life, real_walks_trans;

  const double x, t_x, T_cal;
  // const double adj_transaction_cov_dyn, adj_lifetime_cov_dyn;

  Customer(const double x, const double t_x, const double T_cal,
           const arma::vec& adj_cov_data_life, const arma::mat& walks_info_life,
           const arma::vec& adj_cov_data_trans, const arma::mat& walks_info_trans);

  double adj_transaction_cov_dyn() const{
    return(this->aux_walk_trans.last());
  }

  double adj_lifetime_cov_dyn() const{
    return(this->aux_walk_life.last());
  }

private:
  void add_walks(const arma::vec& adj_cov_data, const arma::mat& walk_info,
                 std::vector<Walk>& real_walks, Walk& aux_walk);
};

Customer::Customer(const double x, const double t_x, const double T_cal,
                   const arma::vec& adj_cov_data_life, const arma::mat& walks_info_life,
                   const arma::vec& adj_cov_data_trans, const arma::mat& walks_info_trans)
  : x(x), t_x(t_x), T_cal(T_cal),
    real_walks_life(std::vector<Walk>(walks_info_life.n_rows - 1)),
    real_walks_trans(std::vector<Walk>(walks_info_trans.n_rows - 1)){

  this->add_walks(adj_cov_data_life, walks_info_life, this->real_walks_life, this->aux_walk_life);
  this->add_walks(adj_cov_data_trans, walks_info_trans, this->real_walks_trans, this->aux_walk_trans);
}

void Customer::add_walks(const arma::vec& adj_cov_data, const arma::mat& walk_info,
                    std::vector<Walk>& real_walks, Walk& aux_walk){
  // **TODO: Throw error if sum(auxcol) != 1

  for(arma::uword i = 0; i < walk_info.n_rows; i++){
    // Rcpp::Rcout<<"walk_info.row(i)"<<std::endl;
    // Rcpp::Rcout<<walk_info.row(i)<<std::endl;
    Walk w = Walk(adj_cov_data, walk_info.row(i));
    if(w.is_aux_trans){
      aux_walk = w;
    }else{
      real_walks[i] = w;
    }
  }
}

Walk::Walk(const arma::vec& cov_data, const arma::rowvec& walk_info)
  : walk_data(cov_data.subvec(
      static_cast<arma::uword>(walk_info(0))-1,
      static_cast<arma::uword>(walk_info(1))-1)),
    tjk{walk_info(2)}, d{walk_info(3)}, delta{walk_info(4)}
{
  // May not store refs/pointers to walk_info as will only receive subviews (mat.row())

  this->is_aux_trans = static_cast<bool>(walk_info(5));
}

// Walk::Walk(const arma::vec& cov_data, const arma::uword from, const arma::uword to,
//            const double tjk, const double d, const double delta, const bool is_aux_trans)
//   : walk_data(cov_data.subvec(from, to)), tjk(tjk), d(d), delta(delta), is_aux_trans(is_aux_trans) {
// }

double Walk::sum_middle_elems() const{
  // Rcpp::Rcout<<"walk_data in sum_middle_elems"<<std::endl;
  // Rcpp::Rcout<<this->walk_data<<std::endl;
  // Rcpp::Rcout<<this->walk_data.n_elem<<std::endl;
  // **TODO: Assert that only called if at least 3 elements
  return(arma::accu(this->walk_data.subvec(1, this->walk_data.n_elem-2)));
}

arma::uword Walk::n_elem() const{
  return(this->walk_data.n_elem);
}

double Walk::first() const{
  return(this->walk_data.front());
}

double Walk::last() const{
  return(this->walk_data.back());
}


double pnbd_dyncov_LL_i_hyp_alpha_ge_beta(const double r, const double s,
                                          const int x,
                                          const double alpha_1, const double beta_1,
                                          const double alpha_2, const double beta_2){
  const double z1 = 1 - (beta_1/alpha_1);
  const double z2 = 1 - (beta_2/alpha_2);
  const double log_C = std::lgamma(r + s + x + 1) +
    std::lgamma(s) +
    std::lgamma(r + s + x) +
    std::lgamma(s + 1);

  gsl_sf_result gsl_res;
  int status;
  double hyp_z1;
  double hyp_z2;

  status = gsl_sf_hyperg_2F1_e(r + s + x,
                               s + 1,
                               r + s + x + 1,
                               z1,
                               &gsl_res);
  if(status == 11 || status == 1){
    hyp_z1 = std::pow(1 - z1, r + x) * std::exp(log_C) / std::pow(beta_1, r + s + x);
  }else{
    hyp_z1 = gsl_res.val / std::pow(alpha_1, r + s + x);
  }


  status = gsl_sf_hyperg_2F1_e(r + s + x,
                               s + 1,
                               r + s + x + 1,
                               z2,
                               &gsl_res);
  if(status == 11 || status == 1){
    hyp_z2 = std::pow(1 - z2, r + x) * std::exp(log_C) / std::pow(beta_2, r + s + x);
  }else{
    hyp_z2 = gsl_res.val / std::pow(alpha_2, r + s + x);
  }

  return(hyp_z1 - hyp_z2);
}

double pnbd_dyncov_LL_i_hyp_beta_g_alpha(const double r, const double s,
                                         const int x,
                                         const double alpha_1, const double beta_1,
                                         const double alpha_2, const double beta_2){

  const double z1 = 1 - (beta_1/alpha_1);
  const double z2 = 1 - (beta_2/alpha_2);
  const double log_C = std::lgamma(r + s + x + 1) +
    std::lgamma(s) +
    std::lgamma(r + s + x) +
    std::lgamma(s + 1);

    gsl_sf_result gsl_res;
    double hyp_z1;
    double hyp_z2;

    int status = gsl_sf_hyperg_2F1_e(r + s + x,
                                     s + 1,
                                     r + s + x + 1,
                                     z1, &gsl_res);
    if(status == 11 || status == 1){
      hyp_z1 = std::pow(1 - z1, r + x) * std::exp(log_C) / std::pow(beta_1, r + s + x);
    }else{
      hyp_z1 = gsl_res.val / std::pow(alpha_1, r + s + x);
    }


    status = gsl_sf_hyperg_2F1_e(r + s + x,
                                 s + 1,
                                 r + s + x + 1,
                                 z2, &gsl_res);

    if(status == 11 || status == 1){
      hyp_z2 = std::pow(1 - z2, r + x) * std::exp(log_C) / std::pow(beta_2, r + s + x);
    }else{
      hyp_z2 = gsl_res.val / std::pow(alpha_2, r + s + x);
    }

    return(hyp_z1 - hyp_z2);
}

double pnbd_dyncov_LL_i_A1sum(const unsigned int x, const std::vector<Walk>& real_walks_trans){
  if(x == 0){
    return(0.0);
  }else{
    double A1sum = 0.0;
    // **TODO: What if no Walk? Is this x==0?
    for(Walk w : real_walks_trans){
      // log(adj.MaxWalk)
      A1sum += std::log(w.last());
    }
    return(A1sum);
  }
}

double pnbd_dyncov_LL_i_bksumbjsum_walk_i(const Walk& w){
  double n = static_cast<double>(w.n_elem());
  double last_mult = w.tjk - w.d - w.delta*(n - 2.0);

  if(w.n_elem() == 1){
    return(w.first()*w.d + w.last()*last_mult);
  }else{
    if(w.n_elem() == 2){
      // ** TODO: Same as == 1 ??
      return(w.first()*w.d + w.last()*last_mult);
    }else{
      // > {1,2}
      return(w.first()*w.d + w.sum_middle_elems() + w.last()*last_mult);
    }
  }
}

double pnbd_dyncov_LL_i_BjSum(const std::vector<Walk>& real_walks){
  if(real_walks.size() == 0){
    return 0.0;
  }else{
    double bjsum = 0.0;
    for(Walk w : real_walks){
      bjsum += pnbd_dyncov_LL_i_bksumbjsum_walk_i(w);
    }
    return(bjsum);
  }
}

double pnbd_dyncov_LL_i_BkSum(const std::vector<Walk>& real_walks, const Walk& aux_walk){
  return(pnbd_dyncov_LL_i_BjSum(real_walks) + pnbd_dyncov_LL_i_bksumbjsum_walk_i(aux_walk));
}

// double pnbd_dyncov_LL_i_Bi(const )


double pnbd_dyncov_LL_i_F2_1(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x, const double dT,
                             const double a1, const double b1,
                             const double A1T, const double C1T){

  const double alpha_1 = a1 + (1-dT)*A1T + alpha_0;
  const double beta_1 = (b1 + (1-dT)*C1T + beta_0) * A1T/C1T;

  const double alpha_2 = a1 + A1T + alpha_0;
  const double beta_2  = (b1 + C1T + beta_0)*A1T/C1T;

  double F2_1 = 0;
  if( alpha_1 >= beta_1){
    F2_1 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);

  }else{
    F2_1 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_beta_g_alpha(r, s, x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);
  }
  return(F2_1);
}

double pnbd_dyncov_LL_i_F2_2(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x,
                             const double akt, const double bkT,
                             const double aT, const double bT,
                             const double AkT, const double CkT){

  const double alpha_1 = akt + alpha_0;
  const double beta_1 =  (bkT + beta_0)*AkT/CkT;

  const double alpha_2 = (aT + alpha_0);
  const double beta_2 =  (bT + beta_0)*AkT/CkT;

  double F2_2 = 0;
  if(alpha_1 >= beta_1){
    F2_2 = std::pow(AkT/CkT, s) * pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);
  }else{
    F2_2 = std::pow(AkT/CkT, s) * pnbd_dyncov_LL_i_hyp_beta_g_alpha(r, s, x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);
  }
  return(F2_2);
}

// double pnbd_dyncov_LL_i_F2_3(){
//
// }

double pnbd_dyncov_LL_i_F2(const int num_walks,
                           const double r, const double alpha_0, const double s, const double beta_0,
                           const Customer& c,
                           const double B1,
                           const double D1,
                           const double BT, const double DT,
                           const double A1T, const double C1T,
                           const double AkT, const double CkT,
                           const double F2_3,
                           const bool return_intermediate_results,
                           arma::vec& intermediate_results){


  const double Bjsum = pnbd_dyncov_LL_i_BjSum(c.real_walks_trans);
  const double dT = c.aux_walk_trans.d;

  const double a1T = Bjsum + B1 + c.T_cal * A1T;
  const double b1T = D1 + c.T_cal * C1T;
  const double a1 = Bjsum + B1 + A1T * (c.t_x + dT - 1.0);
  const double b1  = D1 + C1T * (c.t_x + dT - 1.0);

  // **TODO: What is num_walks here? Length of 1 walk or all walks, incl aux or only real walks?**
  if(num_walks == 1){

    const double alpha_1 = a1 + (1.0-dT)*A1T + alpha_0;
    const double beta_1  = (b1 + (1.0-dT)*C1T + beta_0) * A1T/C1T;

    const double alpha_2 = a1T + alpha_0;
    const double beta_2  = (b1T  + beta_0)*A1T/C1T;

    // num_walk==1 has no F2.2 and F2.3
    double F2_1;
    if(alpha_1 >= beta_1){
      F2_1 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, c.x,
                      alpha_1, beta_1,
                      alpha_2, beta_2);
    }else{
      F2_1 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_beta_g_alpha(r, s, c.x,
                      alpha_1, beta_1,
                      alpha_2, beta_2);
    }

    if(return_intermediate_results){
      intermediate_results(0) = Bjsum;
      intermediate_results(1) = dT;
      intermediate_results(2) = arma::datum::nan;
      intermediate_results(3) = arma::datum::nan;
      intermediate_results(4) = arma::datum::nan;
      intermediate_results(5) = F2_1;
      intermediate_results(6) = 0.0;
      intermediate_results(7) = 0.0;
    }
    return(F2_1);

  }else{

    const double akt = Bjsum + BT + AkT * (c.t_x + dT + static_cast<double>(num_walks) - 2.0);
    const double bkT = DT + CkT * (c.t_x + dT + static_cast<double>(num_walks) - 2.0);
    const double aT = Bjsum + BT + (c.T_cal * AkT);
    const double bT  = DT + c.T_cal * CkT;

    const double F2_1 = pnbd_dyncov_LL_i_F2_1(r, alpha_0, s, beta_0,
                                              c.x, dT,
                                              a1, b1,
                                              A1T, C1T);
    const double F2_2 = pnbd_dyncov_LL_i_F2_2(r, alpha_0, s, beta_0,
                                              c.x,
                                              akt, bkT,
                                              aT, bT,
                                              AkT, CkT);

    // const double F2_3 = pnbd_dyncov_LL_i_F2_3();

    if(return_intermediate_results){
      intermediate_results(0) = Bjsum;
      intermediate_results(1) = dT;
      intermediate_results(2) = aT;
      intermediate_results(3) = bT;
      intermediate_results(4) = bkT;
      intermediate_results(5) = F2_1;
      intermediate_results(6) = F2_2;
      intermediate_results(7) = F2_3;
    }
    return(F2_1 + F2_2 + F2_3);
  }
}

/* [[Rcpp::export]]
 *
 */
Rcpp::NumericVector pnbd_dyncov_LL_i(const double r, const double alpha_0, const double s, const double beta_0,
                           const Customer& c,
                           const int num_walks,
                           const double B1, const double BT,
                           const double DT, const double D1,
                           const double F2_3,
                           const bool return_intermediate_results){

  // // #data.work.trans[AuxTrans==T, adj.transaction.cov.dyn]]
  // const double adj_transaction_cov_dyn,
  // // #cbs[, CkT:= data.work.life[AuxTrans==T, adj.lifetime.cov.dyn]]
  // const double adj_lifetime_cov_dyn,
  // // #cbs[, dT:= data.work.trans[AuxTrans==T, d]]
  // const double dT,
  // // #cbs[, A1T:= data.work.trans[AuxTrans==T, adj.Walk1]]
  // // #const arma::vec& cov_aux_trans,
  // const double A1T_R,
  // // #cbs[, C1T:= data.work.life[AuxTrans==T, adj.Walk1]]
  // // #const arma::vec& cov_aux_life,
  // const double C1T_R,

  // Transaction Process ---------------------------------------------------------


  const double A1T = c.aux_walk_trans.first();
  const double AkT = c.adj_transaction_cov_dyn();
  const double A1sum = pnbd_dyncov_LL_i_A1sum(static_cast<unsigned int>(c.x), c.real_walks_trans);


  // const double B1;
  // const double BT;

  const double Bksum = pnbd_dyncov_LL_i_BkSum(c.real_walks_trans, c.aux_walk_trans);


  // Lifetime Process ---------------------------------------------------

  const double C1T = c.aux_walk_life.first();
  const double CkT = c.adj_lifetime_cov_dyn();

  // const double DT;
  // const double D1;

  const double DkT = CkT * c.T_cal + DT;

  const double log_F0 = r*std::log(alpha_0) + s*std::log(beta_0) + lgamma(c.x+r) - lgamma(r) + A1sum;
  const double log_F1 = std::log(s) - std::log(r+s+c.x);

  arma::vec F2_intermediate_results = arma::vec(8);
  const double F2 = pnbd_dyncov_LL_i_F2(num_walks,
                                        r, alpha_0,  s,  beta_0,
                                        c,
                                        B1,
                                        D1, BT,  DT,
                                        A1T,  C1T, AkT,  CkT,
                                        F2_3,
                                        return_intermediate_results,
                                        F2_intermediate_results);

  const double log_F3 = -s * std::log(DkT + beta_0) - (c.x+r) * std::log(Bksum + alpha_0);

  // *** TODO: == 0?? 1e-16 xxx?
  double LL = 0;
  if(F2 < 0){
    LL = log_F0 + log_F3 + std::log1p(std::exp(log_F1-log_F3) * F2);
  }else{
    if(F2>0){
      double max_AB = std::fmax(log_F1 + std::log(F2), log_F3);
      LL = log_F0 + max_AB  + std::log(std::exp(log_F1 + std::log(F2) - max_AB) + std::exp(log_F3 - max_AB));
    }else{
      LL = log_F0 + log_F3;
    }
  }

  double Akprod = std::exp(A1sum);

  if(!return_intermediate_results){
    return(Rcpp::NumericVector::create(LL));
  }else{
    return(Rcpp::NumericVector::create(Rcpp::_["LL"]=LL,
                                       Rcpp::_["Akprod"]=Akprod,
                                       Rcpp::_["A1sum"]=A1sum,
                                       Rcpp::_["Bksum"]=Bksum,
                                       Rcpp::_["Bjsum"]= F2_intermediate_results(0),
                                       Rcpp::_["B1"]=B1,
                                       Rcpp::_["BT"]=BT,
                                       Rcpp::_["D1"]=D1,
                                       Rcpp::_["DT"]=DT,
                                       // Rcpp::_["DkT"]=DkT,
                                       Rcpp::_["log.F0"]=log_F0,
                                       Rcpp::_["log.F1"]=log_F1,
                                       Rcpp::_["log.F3"]=log_F3,
                                       Rcpp::_["F2"]=F2,
                                       Rcpp::_["dT"]=F2_intermediate_results(1),
                                       Rcpp::_["aT"]=F2_intermediate_results(2),
                                       Rcpp::_["bT"]=F2_intermediate_results(3),
                                       Rcpp::_["bkT"]=F2_intermediate_results(4),
                                       Rcpp::_["F2.1"]=F2_intermediate_results(5),
                                       Rcpp::_["F2.2"]=F2_intermediate_results(6),
                                       Rcpp::_["F2.3"]=F2_intermediate_results(7)));
  }
  // return(arma::vec = {LL, Akprod, F2});
  // return(LL);
}




// [[Rcpp::export]]
Rcpp::NumericVector LL_i_single_walk(const double r, const double alpha_0, const double s, const double beta_0,
                        const double x, const double t_x, const double T_cal,
                        const int num_walks,
                        const double B1, const double BT,
                        const double DT, const double D1,
                        const double F2_3,

                        const arma::vec& params_life,
                        const arma::vec& params_trans,
                        const arma::mat& cov_data_life,
                        const arma::mat& cov_data_trans,
                        const arma::mat& walk_info_life,
                        const arma::mat& walk_info_trans,
                        const bool return_intermediate_results){

  arma::vec adj_cov_data_life = arma::exp(cov_data_life * params_life);
  arma::vec adj_cov_data_trans = arma::exp(cov_data_trans * params_trans);

  // Rcpp::Rcout<<"adj_cov_data_life"<<std::endl;
  // Rcpp::Rcout<<adj_cov_data_life<<std::endl;
  // Rcpp::Rcout<<"n"<<std::endl;
  // Rcpp::Rcout<<adj_cov_data_life.n_elem<<std::endl;

  // Rcpp::Rcout<<"walk_info_life"<<std::endl;
  // Rcpp::Rcout<<walk_info_life<<std::endl;

  Customer c = Customer(x, t_x, T_cal,
                        adj_cov_data_life, walk_info_life,
                        adj_cov_data_trans, walk_info_trans);

  // Rcpp::NumericVector intermediate_results = Rcpp::NumericVector::create(5);

  return pnbd_dyncov_LL_i(r, alpha_0, s, beta_0,
                          c,
                          num_walks,
                          B1, BT,
                          DT, D1,
                          F2_3,
                          return_intermediate_results);
}

// [[Rcpp::export]]
double convert_walk(const arma::vec& params_life,
                    const arma::vec& params_trans,
                    const arma::mat& cov_data_life,
                    const arma::mat& cov_data_trans,
                    const arma::mat& walk_info_life,
                    const arma::mat& walk_info_trans){

  arma::vec adj_cov_data_life = arma::exp(cov_data_life * params_life);
  arma::vec adj_cov_data_trans = arma::exp(cov_data_trans * params_trans);
  // Rcpp::Rcout<<"adj_cov_data_life as in input"<<std::endl;
  // Rcpp::Rcout<<adj_cov_data_life<<std::endl;

  // arma::uword from = (arma::uword)walk_info(0)-1;
  // arma::uword to = (arma::uword)walk_info(1)-1;
  // arma::vec tmp = cov_data.col(0); if pass matrix
  // Walk w = Walk(tmp, from, to,
  //               walk_info(2), walk_info(3), walk_info(4));
  // Walk w = Walk(cov_data, walk_info.row(0));

  Customer c = Customer(0, 0, 0,
                        adj_cov_data_life, walk_info_life,
                        adj_cov_data_trans, walk_info_trans);

  double res = pnbd_dyncov_LL_i_BkSum(c.real_walks_trans, c.aux_walk_trans);

  return(res);
}
