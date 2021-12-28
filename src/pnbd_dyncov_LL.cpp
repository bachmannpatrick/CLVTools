#include "pnbd_dyncov_LL.h"

// void Customer::set_walks_life(const arma::vec& adj_covdata_full_life, const arma::rowvec& walkinfo_aux_life,
//                               const arma::uword customer_covdata_life_from){
//   // Create aux walk
//   this->aux_walk_life = LifetimeWalk(adj_covdata_full_life, walkinfo_aux_life);
//
//   // Create real walk from what is left in this customer's covdata before aux walk starts
//   const arma::uword aux_from = Walk::read_from(walkinfo_aux_life);
//   if(aux_from == customer_covdata_life_from){
//     // aux starts from first covariate of customer = no space for real walk
//     this->real_walk_life = EmptyWalk();
//   }else{
//     const arma::uword to = aux_from - 1
//     const arma::uword from = customer_covdata_life_from;
//     this->real_walk_life = RealWalkLife();
//   }
//
// }

void Customer::set_real_walk_life(const arma::vec& adj_covdata_real_life, const arma::rowvec& walkinfo_real_life){
  if(arma::is_finite(walkinfo_real_life(0)) && arma::is_finite(walkinfo_real_life(1))){
    this->real_walk_life = LifetimeWalk(adj_covdata_real_life, walkinfo_real_life);
  }else{
    this->real_walk_life = EmptyLifetimeWalk();
    Rcpp::Rcout<<"Creating empty walk"<<std::endl;
  }
}

// With real life walk
Customer::Customer(const double x, const double t_x, const double T_cal, const double d_omega,
                   const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
                   const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
                   const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans,
                   const arma::vec& adj_covdata_real_trans, const arma::mat& walkinfo_real_trans)
  :x(x), t_x(t_x), T_cal(T_cal), d_omega(d_omega),
   real_walks_trans(std::vector<TransactionWalk>(walkinfo_real_trans.n_rows)), // init vec with total capacity
   aux_walk_life(LifetimeWalk(adj_covdata_aux_life, walkinfo_aux_life)),
   aux_walk_trans(TransactionWalk(adj_covdata_aux_trans, walkinfo_aux_trans)){

  this->set_real_walk_life(adj_covdata_real_life, walkinfo_real_life);

  for(arma::uword i = 0; i < walkinfo_real_trans.n_rows; i++){
    this->real_walks_trans.at(i) = TransactionWalk(adj_covdata_real_trans, walkinfo_real_trans.row(i));
  }
  // **TODO: Throw error if aux walks are not the same length (n_elems())
  // assert(this->aux_walk_life.n_elems() == this->aux_walk_trans.n_elems())
}

// Without real trans walks (ie not zero-repeaters)
Customer::Customer(const double x, const double t_x, const double T_cal, const double d_omega,
                   const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
                   const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
                   const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans)
  : x(x), t_x(t_x), T_cal(T_cal), d_omega(d_omega),
    real_walks_trans(std::vector<TransactionWalk>(0)),
    aux_walk_life(LifetimeWalk(adj_covdata_aux_life, walkinfo_aux_life)),
    aux_walk_trans(TransactionWalk(adj_covdata_aux_trans, walkinfo_aux_trans)){

  this->set_real_walk_life(adj_covdata_real_life, walkinfo_real_life);
}




void Walk::set_walk_data(const arma::vec& cov_data, const arma::uword from, const arma::uword to){

  const arma::uword n_elems = to-from+1;
  double* ptr = const_cast<double*>(cov_data.memptr());
  this->walk_data = arma::vec(ptr+from, n_elems, false, true);

  // this->walk_data = arma::vec(cov_data.memptr()+from, view.n_elem);
  // const arma::subview_col<double> view = cov_data.subvec(from, to);
  // this->walk_data = arma::vec(view.colptr(0), view.n_elem);

  if(this->walk_data.n_elem >= 3){
    this->val_sum_middle_elems = arma::accu(this->walk_data.subvec(1, this->walk_data.n_elem-2));
  }else{
    // To propagate to optimizer if sum_middle_elems() called erroneously
    // **TODO: Or throw?
    this->val_sum_middle_elems = arma::datum::nan;
  }

}

Walk::Walk(const arma::vec& cov_data, const arma::rowvec& walk_info)
  : delta{walk_info(2)}
  // walk_data(cov_data.subvec(
  //     static_cast<arma::uword>(walk_info(0))-1,
  //     static_cast<arma::uword>(walk_info(1))-1)),
{
  /*
   * May not store refs/pointers to walk_info as will only receive subviews (mat.row())
   */

  arma::uword from = static_cast<arma::uword>(walk_info(0))-1;
  arma::uword to = static_cast<arma::uword>(walk_info(1))-1;
  this->set_walk_data(cov_data, from, to);
}

// Walk::Walk(const arma::vec& cov_data, const arma::uword from, const arma::uword to,
//            const double tjk, const double d, const double delta)
//   :  tjk(tjk), d(d), delta(delta){
//   this->set_walk_data(cov_data, from, to);
// }

TransactionWalk::TransactionWalk()
  :Walk(), d1{arma::datum::nan}, tjk{arma::datum::nan} {
}

TransactionWalk::TransactionWalk(const arma::vec& cov_data, const arma::rowvec& walk_info)
  : Walk(cov_data, walk_info), d1{walk_info(3)}, tjk{walk_info(4)}{
}

EmptyLifetimeWalk::EmptyLifetimeWalk()
  :LifetimeWalk(){
  this->walk_data.reset(); // no elements in walk_data vec
}

arma::uword EmptyLifetimeWalk::n_elem() const{
  return(0);
}


LifetimeWalk::LifetimeWalk()
  :Walk(){
}

LifetimeWalk::LifetimeWalk(const arma::vec& cov_data, const arma::rowvec& walk_info)
  : Walk(cov_data, walk_info){
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

double Walk::get_elem(const arma::uword i) const{
  return(this->walk_data(i));
}

double Walk::sum_middle_elems() const{
  return(this->val_sum_middle_elems);
  // **TODO: Assert that only called if at least 3 elements
}

double Walk::sum_from_to(const arma::uword from, const arma::uword to) const{
  return(arma::accu(this->walk_data.subvec(from, to)));
}


// RealWalkLife::RealWalkLife(const arma::vec& adj_covdata_full_life, const arma::rowvec& walkinfo_aux_life,
//                            const arma::uword customer_from){
//
//   const arma::uword aux_from = static_cast<arma::uword>(walkinfo_aux_life(0))-1;
//   const arma::uword customer_from_index = customer_from - 1; // adapt to vector position, like aux_from
//
//   // **TODO: Throw, may not happen!
//   // if(customer_from_index > aux_from)
//   //   throw
//
//   // Whether there is residual covdata to create a walk from
//   if(customer_from_index == aux_from){
//     // No space
//     this->walk_data.reset(); // Set size to zero
//     this->d = arma::datum::nan;
//     this->tjk = arma::datum::nan;
//     this->delta = arma::datum::nan;
//     this->val_sum_middle_elems = arma::datum::nan;
//   }else{
//     const arma::uword real_to = aux_from - 1;
//     this->set_walk_data(adj_covdata_full_life, customer_from_index, real_to);
//   }
// }

// FACTOR * (
//     hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_1-beta_1)/alpha_1) / (alpha_1^(r+s+x))
//   - hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_2-beta_2)/alpha_2) / (alpha_2^(r+s+x))
// )
double pnbd_dyncov_LL_i_hyp_alpha_ge_beta(const double r, const double s,
                                          const double x,
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


// FACTOR * (
//     hyp2F1(r+s+x,r+x,r+s+x+1,(beta_1-alpha_1)/beta_1) / (beta_1^(r+s+x))
//   - hyp2F1(r+s+x,r+x,r+s+x+1,(beta_2-alpha_2)/beta_2) / (beta_2^(r+s+x))
// )
double pnbd_dyncov_LL_i_hyp_beta_g_alpha(const double r, const double s,
                                         const double x,
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

/*
 * A1sum: Sum up covariates which were active during transaction
 *        **TODO: Verify last vs first are the relevant ones during transaction
 *        Name is choosen randomly
 */
double pnbd_dyncov_LL_i_A1sum(const arma::uword x, const std::vector<TransactionWalk>& real_walks_trans){
  // If x and t are in same interval (ie first and last transaction in 1 interval)

  if(real_walks_trans.size() == 0){
    return(0.0); // log(1)
  }else{
    double A1sum = 0.0;
    for(const Walk& w : real_walks_trans){
      A1sum += std::log(w.last());
    }
    return(A1sum);
  }
}

double pnbd_dyncov_LL_i_bksumbjsum_walk_i(const TransactionWalk& w){

  if(w.n_elem() == 1){
    // Transactions were in same cov period, delta = 0
    return(w.first() * (w.tjk));
    // return(w.first()*w.d + w.last()*last_mult);
  }else{
    if(w.n_elem() == 2){
      // because k-2 = 0
      return(w.first()*w.d1 + w.last()*(w.tjk - w.d1));
      // return(w.first()*w.d + w.last()*last_mult);
    }else{
      // >= 3
      double n = static_cast<double>(w.n_elem());
      double last_mult = w.tjk - w.d1 - w.delta*(n - 2.0);
      return(w.first()*w.d1 + w.sum_middle_elems() + w.last()*last_mult);
    }
  }
}

double pnbd_dyncov_LL_i_BjSum(const std::vector<TransactionWalk>& real_walks){
  if(real_walks.size() == 0){
    return 0.0;
  }else{
    double bjsum = 0.0;
    for(const TransactionWalk& w : real_walks){
      bjsum += pnbd_dyncov_LL_i_bksumbjsum_walk_i(w);
    }
    return(bjsum);
  }
}

double pnbd_dyncov_LL_i_BkSum(const double Bjsum, const TransactionWalk& aux_walk){
  return(Bjsum + pnbd_dyncov_LL_i_bksumbjsum_walk_i(aux_walk));
}

/*
 * Sum transaction aux walk up to (and incl) Walk_i
 */
double pnbd_dyncov_LL_i_Bi(const arma::uword i, const double t_x, const TransactionWalk& aux_walk){
  // **TODO: Make tests with i={1,2,3,10} and n_elems={1,2,3,10} (and i>n_elems?)

  if(i == 1){
    return(aux_walk.first() * (-t_x));
  }

  if(i == 2){
    return(aux_walk.first()*aux_walk.d1 + aux_walk.last() * (-t_x - aux_walk.d1));
  }

  // Sum elements up to Walk_i
  return(aux_walk.first()*aux_walk.d1 + aux_walk.sum_from_to(1, i-2) +
         aux_walk.get_elem(i-1) * (-t_x - aux_walk.d1 - aux_walk.delta*(static_cast<double>(i) - 2.0)));
}


// double pnbd_dyncov_LL_i_Di1(const arma::uword i, const arma::uword n_elem_aux_walk, const Walk& real_walk_life){
//   // D1
//   // max.walk=1 & max.walk=2:
//   //              i=1: Dk1, Dkn=0 -> Dk1
//   //              i>1: Dk1, Dkn
//   // max.walk>2:
//   //              i=1: Dk1, Dk2, ... Dk(max.walk-1), Dkn=0
//   //              i>1: Dk1, Dk2, ... Dk(max.walk-1), Dkn
//
//   // Additionally:
//   // data.work.life[Num.Walk==1 & AuxTrans==FALSE, Di.Max.Walk:=as.double(NA)]
//   // data.work.life[Num.Walk==1 & AuxTrans==TRUE, Di.adj.Walk1:=as.double(NA)]
//   // -> real_walk n_elem == 1: exclude last()
//   // -> aux_walk  n_elem == 1: exclude first() - is this n_elem_aux_walk? but now exclude last()..?
//
//   double Di1 = 0.0;
//
//   if(i == 1){
//     // do not include Dkn
//     if(real_walk_life.n_elem() == 1 | real_walk_life.n_elem() == 2){
//       // -> only Dk1
//       Di1 = real_walk_life.first()*real_walk_life.d;
//
//     }else{
//       // **These sum_middle are all independent from i? Why even have i..?
//       Di1 = real_walk_life.first()*real_walk_life.d + real_walk_life.sum_middle_elems();
//     }
//
//   }else{
//
//     if(real_walk_life.n_elem() == 1 | real_walk_life.n_elem() == 2){
//       Di1 = real_walk_life.first()*real_walk_life.d + real_walk_life.last();
//     }else{
//
//       // We must also ignore Dkn, in the case kxT == 1
//       if(n_elem_aux_walk == 1){
//         Di1 = real_walk_life.first()*real_walk_life.d + real_walk_life.sum_middle_elems();
//       }else{
//         Di1 = real_walk_life.first()*real_walk_life.d + real_walk_life.sum_middle_elems() + real_walk_life.last();
//       }
//     }
//   }
//   return(Di1);
// }
//
//
// double pnbd_dyncov_LL_i_Di2(const arma::uword i, const arma::uword n_elem_real_walk,
//                             const double d_omega, const Walk& aux_walk_life){
//
//   // i=1: 0, Cki
//   // i=2: 0, 0, Cki
//   // i>2: 0, Cj2, Cj3, .. Cj(i-1), Cki
//   // ** TODO: Is this simply sum(Walk_2...Walk_(i-1), Walk_i*(xxx))
//   // **TODO: Should throw if i>n_elems?
//
//   double Cji = 0.0;
//   if( i == 1 || i == 2){
//     // Di2 = sum(Cj1=0,Cj2=0, Cki), hence D2 = Cki
//     Cji = 0.0;
//   }else{
//     // **TODO: Does this not depend on Num.Walk/w.n_elems()??? What if i>=n_elems??
//     //          Does this always have to exclude MaxWalk/last()?
//     // Di2 = Cij  + Cki = sum(0,Cj2, Cj3, .., Cj(i-1)) + Cki -> Cji = sum(0,Cj2, Cj3, .., Cj(i-1))
//     // sum Cj2, .. Cj(i-1)
//     Cji = aux_walk_life.sum_from_to(1, i-2);
//   }
//
//   double Cki = 0.0;
//   double delta = static_cast<double>(n_elem_real_walk + i - 1 > 1);
//   if(aux_walk_life.n_elem() > i){
//     // **TODO: Do we really ever sum further than n_elem?
//     Cki = aux_walk_life.get_elem(i-1) * // Walk_i
//       (-d_omega - delta*(static_cast<double>(n_elem_real_walk) + static_cast<double>(i) - 3.0));
//   }else{
//     // **TODO: Does not matter if i==1 or i==2?
//     Cki = aux_walk_life.last() *
//       (-d_omega - delta*(static_cast<double>(n_elem_real_walk) + static_cast<double>(aux_walk_life.n_elem()) - 3.0));
//   }
//   return(Cji + Cki);
// }

/*
 * Sum up covs from
 * Do not sum up overlapping covs at end of real_walk and beginning of aux_walk (ie avoid real_walk.last() + aux_walk.first())
 * **Same as Bi(), if had one, continuous walk**
 *    Do NOT count covariate period of transaction x double if they overlap
 */
double pnbd_dyncov_LL_i_Di(const arma::uword i, const LifetimeWalk& real_walk_life,
                           const LifetimeWalk& aux_walk_life, const double d_omega){

  // Real and Aux walk are guaranteed to not overlap
  //  Cov where last transaction is, belongs only to aux walk

  // Cannot sum up real_walk_life first and then add aux_walk sum because
  //  for the case real_walk_life.n_elem() == 0, d_omega has to be multiplied with aux_walk.first() not to real_walk_life.first()

  if(real_walk_life.n_elem() == 0){
    // Directly sum up the aux walk until Walk_i, including first()*d
    if(i == 1){
      return(aux_walk_life.first()*d_omega);
    }else{
      double last_mult = (-d_omega - aux_walk_life.delta*(static_cast<double>(i) - 3.0));
      if(i == 2){
        return(aux_walk_life.first()*d_omega + aux_walk_life.last()*last_mult);
      }else{
        // i >= 3: Walk_1 + sum(Walk_2, Walk_i-1) + Walk_i
        return(aux_walk_life.first()*d_omega + aux_walk_life.sum_from_to(1, i-2) + aux_walk_life.get_elem(i-1)*last_mult);
      }
    }
  }else{

    // There are elements in the real walk that all need to be summed (independent of i)

    double sum_real_walk = 0.0;
    if(real_walk_life.n_elem() == 1){
      sum_real_walk = real_walk_life.first()*d_omega;
    }else{
      if(real_walk_life.n_elem() == 2){
        sum_real_walk = real_walk_life.first()*d_omega + real_walk_life.last();
      }else{
        // >= 3: everything in real walk
        sum_real_walk = real_walk_life.first()*d_omega + real_walk_life.sum_middle_elems() + real_walk_life.last();
      }
    }

    // Sum up aux walk until i
    double sum_aux_walk = 0.0;
    double last_mult = (-d_omega - aux_walk_life.delta*(static_cast<double>(i) - 3.0));
    if(i == 1){
      sum_aux_walk = aux_walk_life.first()*last_mult;
    }else{
      if(i == 2){
        sum_aux_walk = aux_walk_life.first() + aux_walk_life.get_elem(1)*last_mult;
      }else{
        // i >= 3
        sum_aux_walk = aux_walk_life.first() + aux_walk_life.sum_from_to(1,i-2) + aux_walk_life.get_elem(i-1)*last_mult;
      }
    }
    return(sum_real_walk + sum_aux_walk);
  }
}


double pnbd_dyncov_LL_i_F2_1(const double r, const double alpha_0, const double s, const double beta_0,
                             const int x, const double dT,
                             const double a1, const double b1,
                             const double A1T, const double C1T){

  const double alpha_1 = a1 + (1.0 - dT)*A1T + alpha_0;
  const double beta_1 = (b1 + (1.0 - dT)*C1T + beta_0) * A1T/C1T;

  const double alpha_2 =  a1 + A1T + alpha_0;
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

  double F2_2 = 0.0;
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

double pnbd_dyncov_LL_i_F2_3(const double r, const double alpha_0, const double s, const double beta_0,
                             const Customer& c,
                             const double Bjsum, const double dT){

  // No middle sum if only 2 elems in walk (only F2_1 and F2_2)
  if(c.aux_walk_trans.n_elem() <= 2){
    return(0.0);
  }

  // Loop counts Walks, not element access indices. ie Walk_2, Walk_3, ...
  //    Loop until and including Walk(n-1)
  double F2_3 = 0.0;
  const arma::uword i_end = c.aux_walk_trans.n_elem() - 1;
  for(arma::uword i = 2; i <= i_end; i++){

    // Transaction Process ------------------------------------------
    double Ai = c.aux_walk_trans.get_elem(i-1); // Walk_i
    double Bi = pnbd_dyncov_LL_i_Bi(i, c.t_x, c.aux_walk_trans);
    double ai = Bjsum + Bi + Ai * (c.t_x + dT + (static_cast<double>(i) - 2.0));

    // Lifetime Process ---------------------------------------------
    double Ci = c.aux_walk_life.get_elem(i-1); // Walk_i
    double Di = pnbd_dyncov_LL_i_Di(i, c.real_walk_life, c.aux_walk_life, c.d_omega);
    double bi = Di + Ci * (c.t_x + dT + (static_cast<double>(i) - 2.0));

    // Alpha & Beta ------------------------------------------------
    double alpha_1 = ai + alpha_0;
    double beta_1 = (bi + beta_0) * Ai/Ci;
    double alpha_2 = ai + Ai + alpha_0;
    double beta_2 = (bi + Ci + beta_0)*Ai/Ci;

    if(alpha_1 >= beta_1){
      F2_3 += std::pow(Ai/Ci, s) * pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, c.x,
                      alpha_1, beta_1,
                      alpha_2, beta_2);
    }else{
      F2_3 += std::pow(Ai/Ci, s) * pnbd_dyncov_LL_i_hyp_beta_g_alpha(r, s, c.x,
                       alpha_1, beta_1,
                       alpha_2, beta_2);
    }
  }

  return(F2_3);
}

double pnbd_dyncov_LL_i_F2(const double r, const double alpha_0, const double s, const double beta_0,
                           const Customer& c,
                           const double B1, const double D1,
                           const double BT, const double DT,
                           const double A1T, const double C1T,
                           const double AkT, const double CkT,
                           const double Bjsum,
                           const double F2_3_R,
                           const bool return_intermediate_results,
                           arma::vec& intermediate_results){

  // **TODO: Verify if this is calculated correctly
  //  (d1 of aux trans = time between T and interval(T) or tx and interval(tx) or what?)
  const double dT = c.aux_walk_trans.d1;

  const double a1T = Bjsum + B1 + c.T_cal * A1T;
  const double b1T = D1 + c.T_cal * C1T;
  const double a1 = Bjsum + B1 + A1T * (c.t_x + dT - 1.0);
  const double b1  = D1 + C1T * (c.t_x + dT - 1.0);

  if(c.aux_walk_life.n_elem() == 1){

    // Not exactly the same as F2_1

    const double alpha_1 =  a1 + (1.0-dT)*A1T + alpha_0;
    const double beta_1  = (b1 + (1.0-dT)*C1T + beta_0) * A1T/C1T;

    const double alpha_2 =  a1T + alpha_0;
    const double beta_2  = (b1T  + beta_0)*A1T/C1T;
    Rcpp::Rcout<<"r: "<<r<<"   s:"<<s<<"   c.x:"<< c.x << std::endl;
    Rcpp::Rcout<<"alpha_1: "<<alpha_1<< "  " <<"beta_1: "<<beta_1<<"  "<<"alpha_2: "<<alpha_2<<"  "<<"beta_2: "<<beta_2<<std::endl;
    pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, c.x,
                                       alpha_1, beta_1,
                                       alpha_2, beta_2);
    return 1.234;

    double F2 = 0.0;
    if(alpha_1 >= beta_1){
      F2 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_alpha_ge_beta(r, s, c.x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);
    }else{
      F2 = std::pow(A1T/C1T, s) * pnbd_dyncov_LL_i_hyp_beta_g_alpha(r, s, c.x,
                    alpha_1, beta_1,
                    alpha_2, beta_2);
    }

    if(return_intermediate_results){
      intermediate_results(0) = Bjsum;
      intermediate_results(1) = dT;
      intermediate_results(2) = arma::datum::nan;
      intermediate_results(3) = arma::datum::nan;
      intermediate_results(4) = arma::datum::nan;
      intermediate_results(5) = arma::datum::nan;
      intermediate_results(6) = arma::datum::nan;
      intermediate_results(7) = arma::datum::nan;
    }
    return(F2);

  }else{

    const double n_walks = static_cast<double>(c.aux_walk_life.n_elem());
    const double akt = Bjsum + BT + AkT * (c.t_x + dT + n_walks - 2.0);
    const double bkT = DT + CkT * (c.t_x + dT + n_walks - 2.0);
    const double aT = Bjsum + BT + (c.T_cal * AkT);
    const double bT  = DT + c.T_cal * CkT;
    return 1.234;
    // const double F2_1 = pnbd_dyncov_LL_i_F2_1(r, alpha_0, s, beta_0,
    //                                           c.x, dT,
    //                                           a1, b1,
    //                                           A1T, C1T);
    // const double F2_2 = pnbd_dyncov_LL_i_F2_2(r, alpha_0, s, beta_0,
    //                                           c.x,
    //                                           akt, bkT,
    //                                           aT, bT,
    //                                           AkT, CkT);

    // const double F2_3 = pnbd_dyncov_LL_i_F2_3(r, alpha_0, s, beta_0,
    //                                           c,
    //                                           Bjsum, dT);
    const double F2_1 = 0;
    const double F2_2 = 0;
    const double F2_3 = 0;

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


Rcpp::NumericVector pnbd_dyncov_LL_i(const double r, const double alpha_0, const double s, const double beta_0,
                                     const Customer& c,
                                     const double DT,
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

  // Transaction Process ------------------------------------------------
  const double A1T = c.aux_walk_trans.first();
  const double AkT = c.adj_transaction_cov_dyn();
  const double A1sum = pnbd_dyncov_LL_i_A1sum(static_cast<arma::uword>(c.x), c.real_walks_trans);
  const double B1 = pnbd_dyncov_LL_i_Bi(1, c.t_x, c.aux_walk_trans);

  const double BT = pnbd_dyncov_LL_i_Bi(c.aux_walk_trans.n_elem(), c.t_x, c.aux_walk_trans);
  const double Bjsum = pnbd_dyncov_LL_i_BjSum(c.real_walks_trans);
  const double Bksum = pnbd_dyncov_LL_i_BkSum(Bjsum, c.aux_walk_trans);


  // Lifetime Process ---------------------------------------------------
  const double C1T = c.aux_walk_life.first();
  const double CkT = c.adj_lifetime_cov_dyn();
  // const double D1 = 1.23;
  const double D1 = pnbd_dyncov_LL_i_Di(1, c.real_walk_life, c.aux_walk_life, c.d_omega);
  // const double DT = pnbd_dyncov_LL_i_Di(c.aux_walk_life.n_elem(), c.real_walk_life, c.aux_walk_life, c.d_omega);

  const double DkT = CkT * c.T_cal + DT;

  // LL ---------------------------------------------------------------------------
  //
  //   LL = log(F0)+log((F1 * F2) + F3)
  //
  //   We rely on various tricks to improve numerical stability
  //
  //   1. Improvement
  //   F0 quickly is too large to represent because of exp() and gamma(f(x))
  //   Because it is only used as log(F0) it can be directly rewritten:
  //
  //   F0      = ((alpha_0)^(r)*(beta_0)^(s) * (gamma(x+r)))/gamma(r) * exp(A1sum)
  //   log(F0) = r*log(alpha_0) + s*log(beta_0) + log(gamma(x+r)) - log(gamma(r)) + A1sum- (x+r)*log((Bksum + alpha_0))
  //
  //   and using the lgamma() function to calculate log(gamma())
  //
  //
  //   2. Improvement
  //   log((F1 * F2) + F3) can be to large to represent. It can be rewritten
  //   using the log trick:
  //   log(A + B) = log(max(A,B)) + log(1+(min(A,B)/max(A,B)))
  //
  //   where A = (F1*F2) and B = F3 in this case and using log1p(x) instead
  //   of log(1+x) for better log approximation in case of small x:
  //
  //     LL = log.F0 + log(A+B)    # where A=F1*F2, B=F3
  //     LL = log.F0 + log(max(A,                 B))  + log(1+(min(A,B)/max(A,B))) #as described on 290-292
  //     LL = log.F0 + max(log(A),            log(B))  + log(1+(min(A,B)/max(A,B)))
  //     LL = log.F0 + max(log(F1*F2),        log(B))  + log(1+(min(A,B)/max(A,B)))
  //     LL = log.F0 + max(log(F1) + log(F2), log(B))  + log(1+(min(A,B)/max(A,B)))
  //     Hence:
  //     LL = log.F0 + max(log(F1) + log(F2), log(F3))  + log(1+(min(F1*F2,F3)/max(F1*F2,F3)))
  //
  //     log(F1) and log(F3) can be simplified to logged sums as they are products. log(F2) cannot.
  //     F1 = s/(r+s+x)                                        =>   log.F1 = log(s) - log(r+s+x)
  //     F3 = 1 /((DkT  + beta_0)^(s)*(BkSum+alpha_0)^(x+1r))  =>   log.F3 = -s*log(DkT + beta_0) - (x+r)*log(Bksum+alpha_0)
  //
  //
  //     3. Improvement
  //     The F2 can be negative/zero for some observations and log(F2) cannot be calculated. Therefore, case
  //       differentiation is done for F2. In general, log((F1*F2) + F3) is because (F1*F2) + F3 > 0 as otherwise
  //       the whole likelihood does not make sense. Also we have that always F1 > 0 and B=F3 > 0 so A=F1*F2 <= 0 is
  //       possible but at the same time A+B > 0.
  //
  //       If F2 > 0: Same calculation as before.
  //
  //       If F2 < 0:  A=F1*F2 <= 0 and B=F3 > 0 but abs(F3) > abs(F1*F2)
  //         log(max(A,B))  + log(1+(min(A,B)/max(A,B)))
  //         log(B)         + log(1+A/B)                 with -1 < (A/B) < 0
  //         log(F3)        + log(1+(F1*F2/F3))
  //
  //       If F2 = 0: Based on the original LL
  //         LL = log.F0 + log((F1*F2) + F3)
  //         LL = log.F0 + log(0 + F3)
  //         LL = log.F0 + log.F3
  //
  //         4. Improvement
  //         For the case F2 < 0, the product F1*F2 in log(1+min()/max()) can still be to large to represent.
  //         They are elimenated by artificially exp() and then log components
  //
  //         log(F3)        + log(1 +         ( F1 * F2 / F3 ))
  //         log(F3)        + log(1 + exp( log(F1))* F2 / exp( log(F3)))
  //         log(F3)        + log(1 + exp( log.F1  - log.F3) * F2)
  //
  //
  //         For the case F2 > 0, the product F1*F2 in log((F1 * F2) + F3) can still be to large
  //         to represent. They are eliminated using the log-sum-of-exponents (LSE) trick.
  //         log(A + B)
  //         log(exp(log(A)) + exp(log(B)))
  //         -> LSE
  //         max(log(A),         log(B)) + log(exp(log(A          - max(log(A),         log(B)))) + exp(log(B- max(log(A),         log(B)))))
  //         max(log(F1*F2),     log(B)) + log(exp(log(F1*F2      - max(log(F1*F2),     log(B)))) + exp(log(B- max(log(F1*F2),     log(B)))))
  //         max(log.F1+log(F2), log.F3) + log(exp(log.F1+log(F2) - max(log.F1+log(F2), log.F3))) + exp(log(B- max(log.F1+log(F2), log.F3))))
  //
  //         Or alternative:
  //         max(log(F1) + log(F2), log(B))  + log(1+       (min(A,B)                  / max(A,B)))
  //         max(log(F1) + log(F2), log(B))  + log(1+exp(log(min(A,B)                  / max(A,B))))
  //         max(log(F1) + log(F2), log(B))  + log(1+exp(log(min(A,B))                 - log(max(A,B))))
  //         max(log(F1) + log(F2), log(B))  + log(1+exp(min(log(A),log(B))            - max(log(A),log(B))))
  //         max(log(F1) + log(F2), log(B))  + log(1+exp(min(log(F1*F2),log(F3))       - max(log(F1*F2),log(F3))))
  //         max(log(F1) + log(F2), log(B))  + log(1+exp(min(log.F1 + log(F2), log.F3) - max(log.F1 + log(F2),log.F3)))
  //         cbs[F2 >  0,  LL.other :=log.F0 +  pmax(log.F1 + log(F2), log.F3)  + log1p(exp(pmin(log.F1 + log(F2), log.F3) - pmax(log.F1 + log(F2),log.F3)))]


  const double log_F0 = r*std::log(alpha_0) + s*std::log(beta_0) + std::lgamma(c.x+r) - std::lgamma(r) + A1sum;
  const double log_F1 = std::log(s) - std::log(r+s+c.x);

  arma::vec F2_intermediate_results = arma::vec(8);
  double F2 = pnbd_dyncov_LL_i_F2(r, alpha_0,  s,  beta_0,
                                  c,
                                  B1, D1,
                                  BT,  DT,
                                  A1T,  C1T, AkT,  CkT,
                                  Bjsum,
                                  F2_3,
                                  return_intermediate_results,
                                  F2_intermediate_results);
  // double F2 = 0.0;

  const double log_F3 = -s * std::log(DkT + beta_0) - (c.x+r) * std::log(Bksum + alpha_0);

  // Branch by F2:
  //
  //  F2 may be non-finite (especially NaN):
  //    Propagate to LL
  //    Avoid that it falls into the default branch where it is omitted (LL = log_F0 + log_F3).
  //
  //  Floating point comparison, around 0:
  //    Like in R: Treat as equal if close enough, ie abs(diff) < sqrt(MachineEps)
  //    Set F2 to 0 if it is close enough to 0: abs(F2) < sqrt(MachineEps)
  double LL = 0;
  if(!arma::is_finite(F2)){
    LL = F2;
  }else{
    // Set F2 to exact 0 if it is reasonably small
    if(std::fabs(F2 - 0.0) < std::sqrt(arma::datum::eps)){
      F2 = 0.0;
    }

    if(F2 < 0.0){
      LL = log_F0 + log_F3 + std::log1p(std::exp(log_F1-log_F3) * F2);
    }else{

      if(F2 > 0.0){
        double max_AB = std::fmax(log_F1 + std::log(F2), log_F3);
        LL = log_F0 + max_AB  + std::log(std::exp(log_F1 + std::log(F2) - max_AB) + std::exp(log_F3 - max_AB));

      }else{
        // F2 == 0
        LL = log_F0 + log_F3;
      }
    }
  }



  double Akprod = std::exp(A1sum);

  if(!return_intermediate_results){
    return(Rcpp::NumericVector::create(LL));
  }else{
    // R: data.table(Id=cbs$Id,LL=cbs$LL, Akprod=exp(cbs$A1sum), Bksum=cbs$Bksum, DkT=cbs$DkT, Z=cbs$F2)
    // **TODO: Do Z=F2 in pnbd_dyncov_getLLdata()
    return(Rcpp::NumericVector::create(Rcpp::_["LL"]=LL,
                                       Rcpp::_["Akprod"]=Akprod,
                                       Rcpp::_["Bksum"]=Bksum,
                                       Rcpp::_["Bjsum"]= F2_intermediate_results(0),
                                       Rcpp::_["B1"]=B1,
                                       Rcpp::_["BT"]=BT,
                                       Rcpp::_["D1"]=D1,
                                       Rcpp::_["DT"]=DT,
                                       Rcpp::_["DkT"]=DkT,
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
}

// [[Rcpp::export]]
double pnbd_dyncov_LL_sum(const arma::vec& params,
                          const arma::vec& X,
                          const arma::vec& t_x,
                          const arma::vec& T_cal,
                          const arma::vec& d_omega,

                          const arma::mat& walkinfo_aux_life,
                          const arma::mat& walkinfo_real_life,
                          const arma::mat& walkinfo_aux_trans,
                          const arma::mat& walkinfo_real_trans,

                          const arma::vec& walkinfo_trans_real_from,
                          const arma::vec& walkinfo_trans_real_to,

                          const arma::mat& covdata_aux_life,
                          const arma::mat& covdata_real_life,
                          const arma::mat& covdata_aux_trans,
                          const arma::mat& covdata_real_trans){

  return(Rcpp::sum(pnbd_dyncov_LL_ind(params,
                                      X,
                                      t_x,
                                      T_cal,
                                      d_omega,

                                      walkinfo_aux_life,
                                      walkinfo_real_life,
                                      walkinfo_aux_trans,
                                      walkinfo_real_trans,

                                      walkinfo_trans_real_from,
                                      walkinfo_trans_real_to,

                                      covdata_aux_life,
                                      covdata_real_life,
                                      covdata_aux_trans,
                                      covdata_real_trans,

                                      false)));
}

// [[Rcpp::export]]
Rcpp::NumericMatrix pnbd_dyncov_LL_ind(const arma::vec& params,
                                       const arma::vec& X,
                                       const arma::vec& t_x,
                                       const arma::vec& T_cal,
                                       const arma::vec& d_omega,

                                       const arma::mat& walkinfo_aux_life,
                                       const arma::mat& walkinfo_real_life,
                                       const arma::mat& walkinfo_aux_trans,
                                       const arma::mat& walkinfo_real_trans,

                                       const arma::vec& walkinfo_trans_real_from,
                                       const arma::vec& walkinfo_trans_real_to,

                                       const arma::mat& covdata_aux_life,
                                       const arma::mat& covdata_real_life,
                                       const arma::mat& covdata_aux_trans,
                                       const arma::mat& covdata_real_trans,

                                       const bool return_intermediate_results=false){

  const arma::uword num_cov_life  = covdata_aux_life.n_cols;
  const arma::uword num_cov_trans = covdata_aux_trans.n_cols;

  const arma::vec model_log_params = params.subvec(0, 3);
  const arma::vec params_life      = params.subvec(4               , 4+num_cov_life                 - 1);
  const arma::vec params_trans     = params.subvec(4 + num_cov_life, 4+num_cov_life + num_cov_trans - 1);

  const double r        = std::exp(model_log_params(0));
  const double alpha_0  = std::exp(model_log_params(1));
  const double s        = std::exp(model_log_params(2));
  const double beta_0   = std::exp(model_log_params(3));

  const arma::vec DT(arma::size(X), arma::fill::zeros);
  const arma::vec F2_3(arma::size(X), arma::fill::zeros);


  // The only thing that changes between calls to the LL during optimization
  const arma::vec adj_covdata_aux_life   = arma::exp(covdata_aux_life   * params_life);
  const arma::vec adj_covdata_real_life  = arma::exp(covdata_real_life  * params_life);
  const arma::vec adj_covdata_aux_trans  = arma::exp(covdata_aux_trans  * params_trans);
  const arma::vec adj_covdata_real_trans = arma::exp(covdata_real_trans * params_trans);

  Rcpp::Rcout<<"multiplied cov data" <<std::endl;

  Rcpp::NumericMatrix res;
  if(return_intermediate_results){
    res = Rcpp::NumericMatrix(X.n_elem, 20);
  }else{
    res = Rcpp::NumericMatrix(X.n_elem, 1);
  }

  Rcpp::NumericVector res_i;
  for(arma::uword i = 0; i < X.n_elem; i++){

    Rcpp::Rcout<<"i: "<< i <<std::endl;

    // Check whether customer has real trans walks
    //   Could also check x==0, but saver to look at actual content
    if(arma::is_finite(walkinfo_trans_real_from(i))){

      // Repeat customer (with real trans walks)
      arma::uword wi_real_trans_from = static_cast<arma::uword>(walkinfo_trans_real_from(i)) - 1;
      arma::uword wi_real_trans_to = static_cast<arma::uword>(walkinfo_trans_real_to(i)) - 1;

      // Rcpp::Rcout<<LifetimeWalk(adj_covdata_aux_life, walkinfo_aux_life.row(i)).n_elem()<<std::endl;
      // Rcpp::Rcout<<TransactionWalk(adj_covdata_aux_trans, walkinfo_aux_trans.row(i)).n_elem()<<std::endl;
      // std::vector<TransactionWalk>(walkinfo_real_trans.rows(wi_real_trans_from, wi_real_trans_to).n_rows);

      Customer c = Customer(X(i), t_x(i), T_cal(i), d_omega(i),
                            adj_covdata_aux_life, walkinfo_aux_life.row(i),
                            adj_covdata_real_life, walkinfo_real_life.row(i),
                            adj_covdata_aux_trans, walkinfo_aux_trans.row(i),
                            adj_covdata_real_trans, walkinfo_real_trans.rows(wi_real_trans_from, wi_real_trans_to));

      // Rcpp::Rcout<<c.aux_walk_life.n_elem()<<std::endl;
      // Rcpp::Rcout<<c.aux_walk_trans.n_elem()<<std::endl;
      // Rcpp::Rcout<<c.real_walk_life.n_elem()<<std::endl;
      // Rcpp::Rcout<<c.real_walks_trans.size()<<std::endl;
      res_i = pnbd_dyncov_LL_i(r, alpha_0, s, beta_0,
                               Customer(X(i), t_x(i), T_cal(i), d_omega(i),
                                        adj_covdata_aux_life, walkinfo_aux_life.row(i),
                                        adj_covdata_real_life, walkinfo_real_life.row(i),
                                        adj_covdata_aux_trans, walkinfo_aux_trans.row(i),
                                        adj_covdata_real_trans, walkinfo_real_trans.rows(wi_real_trans_from, wi_real_trans_to)),
                                        DT(i), F2_3(i),
                               return_intermediate_results);
    }else{
      Rcpp::Rcout<<"Before creating customer"<<std::endl;

      // // Zero-repeater (no real trans walks)
      // Customer c = Customer(X(i), t_x(i), T_cal(i), d_omega(i),
      //                       adj_covdata_aux_life, walkinfo_aux_life.row(i),
      //                       adj_covdata_real_life, walkinfo_real_life.row(i),
      //                       adj_covdata_aux_trans, walkinfo_aux_trans.row(i));
      // Rcpp::Rcout<<"zero-repeater customer created"<<std::endl;
      // res_i = pnbd_dyncov_LL_i(r, alpha_0, s, beta_0,
      //                          c,
      //                          DT(i), F2_3(i),
      //                          return_intermediate_results);
    }

    res(i, Rcpp::_) = res_i;
  }

  // # Try cheating for stabilty -----------------------------------------------------
  // # Replace infinite LL values with the most extreme (finite) LL value
  // # If we have less than 5 % infinite values impute them with the max value we have in the likelihood
  //   most extreme value in the likelihood (without the infinity values)
  //     most.extreme.LL <- cbs[is.finite(LL), max(abs(LL))]
  //
  // # If the value we have is -infinity set the value to the largest negative value...
  //   cbs[is.infinite(LL) & sign(LL) == -1, LL := -abs(most.extreme.LL)]
  // # ...if +infinity set it to largest positive value.
  //   cbs[is.infinite(LL) & sign(LL) == 1,  LL :=  abs(most.extreme.LL)]
  // # Else, if > 5%, let it propagate


  Rcpp::CharacterVector c_names = {"LL"};
  if(return_intermediate_results){
    c_names = res_i.names();
  }
  Rcpp::colnames(res) = c_names;
  return(res);
}


