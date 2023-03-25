#include "pnbd_dyncov_LL.h"

void Customer::set_real_walk_life(const arma::vec& adj_covdata_real_life, const arma::rowvec& walkinfo_real_life){
  if(arma::is_finite(walkinfo_real_life(0)) && arma::is_finite(walkinfo_real_life(1))){
    this->real_walk_life = LifetimeWalk(adj_covdata_real_life, walkinfo_real_life);
  }else{
    this->real_walk_life = EmptyLifetimeWalk();
  }
}

// With real life walk (ie repeat buyer)
Customer::Customer(const double x, const double t_x, const double T_cal, const double d_omega,
                   const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
                   const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
                   const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans,
                   const arma::vec& adj_covdata_real_trans, const arma::mat& walkinfo_real_trans)
  :x(x),
   t_x(t_x),
   T_cal(T_cal),
   d_omega(d_omega),
   real_walks_trans(std::vector<TransactionWalk>(walkinfo_real_trans.n_rows)), // init vec with total capacity
   aux_walk_life(LifetimeWalk(adj_covdata_aux_life, walkinfo_aux_life)),
   aux_walk_trans(TransactionWalk(adj_covdata_aux_trans, walkinfo_aux_trans)){

  this->set_real_walk_life(adj_covdata_real_life, walkinfo_real_life);

  for(arma::uword i = 0; i < walkinfo_real_trans.n_rows; i++){
    this->real_walks_trans.at(i) = TransactionWalk(adj_covdata_real_trans, walkinfo_real_trans.row(i));
  }
}

// Without real trans walks (ie zero-repeater)
Customer::Customer(const double x, const double t_x, const double T_cal, const double d_omega,
                   const arma::vec& adj_covdata_aux_life,   const arma::rowvec& walkinfo_aux_life,
                   const arma::vec& adj_covdata_real_life,  const arma::rowvec& walkinfo_real_life,
                   const arma::vec& adj_covdata_aux_trans,  const arma::rowvec& walkinfo_aux_trans)
  : x(x),
    t_x(t_x),
    T_cal(T_cal),
    d_omega(d_omega),
    real_walks_trans(std::vector<TransactionWalk>(0)),
    aux_walk_life(LifetimeWalk(adj_covdata_aux_life, walkinfo_aux_life)),
    aux_walk_trans(TransactionWalk(adj_covdata_aux_trans, walkinfo_aux_trans)){

  this->set_real_walk_life(adj_covdata_real_life, walkinfo_real_life);
}

LifetimeWalk::LifetimeWalk(const arma::vec& cov_data, const arma::rowvec& walk_info){

   // May _not_ store refs/pointers to walk_info as will only receive subviews (mat.row()) which will vanish when function is done!
  arma::uword from = static_cast<arma::uword>(walk_info(0))-1;
  arma::uword to = static_cast<arma::uword>(walk_info(1))-1;

  // Set actual walk data
  // Re-use the memory of cov_data for this->walk_data rather than allocating own memory which would be much slower.
  // Use advanced constructor that uses auxiliary memory without copying
  //
  //
  // Can either
  // - arma::vec(ptr+from, n, copy, strict), requires to
  //    - cast pointer of cov_data to non-const pointer
  //    - calculating n=to-from+1
  //    - have to do raw pointer arithmetic with ptr+from
  //
  // - arma::vec(ptr+from, n)
  //    - no pointer casting
  //    - calculating n=to-from+1
  //    - have to do raw pointer arithmetic with ptr+from
  //
  // - use subview to get pointer position and number of elements to use in arma::vec(ptr+from, n)
  //    - no pointer casting
  //    - no pointer arithmetic
  //    - not calculating number of elements
  //    - slightly slower as requires subview
  //
  // Subview approach is deemed prefereable because no raw pointer arithmetic required.
  //  Was measured to be about 5% slower on small dataset (250 customers) but worth it because safer (reading from correct positions)

  const arma::subview_col<double> view = cov_data.subvec(from, to);
  this->walk_data = arma::vec(view.colptr(0), view.n_elem);


  // For completeness, the other discussed approaches
  //
  // const arma::uword n_elems = to-from+1;
  //
  // // using arma::vec(ptr+from, n, copy, strict)
  // double* ptr = const_cast<double*>(cov_data.memptr());
  // this->walk_data = arma::vec(ptr+from, n_elems, false, true);
  //
  // // using arma::vec(ptr+from, n)
  // this->walk_data = arma::vec(cov_data.memptr()+from, n_elems);


  if(this->walk_data.n_elem >= 3){
    this->val_sum_middle_elems = arma::accu(this->walk_data.subvec(1, this->walk_data.n_elem-2));
  }else{

    // Set to NA to mark as not calculated
    // This propagates NA to optimizer if sum_middle_elems() is called erroneously
    this->val_sum_middle_elems = arma::datum::nan;
  }
}


TransactionWalk::TransactionWalk()
  :LifetimeWalk(), d1{arma::datum::nan}, tjk{arma::datum::nan} {
}

TransactionWalk::TransactionWalk(const arma::vec& cov_data, const arma::rowvec& walk_info)
  : LifetimeWalk(cov_data, walk_info), d1{walk_info(2)}, tjk{walk_info(3)}{
}

EmptyLifetimeWalk::EmptyLifetimeWalk()
  :LifetimeWalk(){
  this->walk_data.reset(); // no elements in walk_data vec
}

arma::uword EmptyLifetimeWalk::n_elem() const{
  return(0);
}


LifetimeWalk::LifetimeWalk()
  : val_sum_middle_elems(arma::datum::nan){
  // Default ctor without walk data and walk info leaves Walk in uninitialized state
  //  (set all NaN to propagate)
  this->walk_data = arma::vec(1).fill(arma::datum::nan);
}


arma::uword LifetimeWalk::n_elem() const{
  return(this->walk_data.n_elem);
}

double LifetimeWalk::first() const{
  return(this->walk_data.front());
}

double LifetimeWalk::last() const{
  return(this->walk_data.back());
}

double LifetimeWalk::get_elem(const arma::uword i) const{
  return(this->walk_data(i));
}

double LifetimeWalk::sum_middle_elems() const{
  if(this-> n_elem() < 3){
    throw Rcpp::exception("sum_middle_elems() is CALLED erroneously with less than 3 elements!");
  }
  return(this->val_sum_middle_elems);
}

double LifetimeWalk::sum_from_to(const arma::uword from, const arma::uword to) const{
  return(arma::accu(this->walk_data.subvec(from, to)));
}


// FACTOR * (
//     hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_1-beta_1)/alpha_1) / (alpha_1^(r+s+x))
//   - hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_2-beta_2)/alpha_2) / (alpha_2^(r+s+x))
// )
double pnbd_dyncov_LL_i_hyp_alpha_ge_beta(const double r, const double s,
                                          const double x,
                                          const double alpha_1, const double beta_1,
                                          const double alpha_2, const double beta_2){
  // Do not abort in case of error in gsl functions (hypergeoms)
  gsl_set_error_handler_off();

  const double z1 = 1.0 - (beta_1/alpha_1);
  const double z2 = 1.0 - (beta_2/alpha_2);

  // c + <b-1> -a -b
  // cbs.z[,log.C :=  lgamma(r+s+x+1) + lgamma(s) - lgamma(r+s+x) - lgamma(s+1) ]
  const double log_C =
    lgamma(r + s + x + 1.0) +
    lgamma(s) -
    lgamma(r + s + x) -
    lgamma(s + 1.0);
  // const double log_C = std::lgamma(r + s + x + 1.0) +
  //   std::lgamma(s) +
  //   std::lgamma(r + s + x) +
  //   std::lgamma(s + 1.0);

  gsl_sf_result gsl_res;
  int status;

  // Z1
  double hyp_z1;
  // l.hyp.z1 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x, cbs.z$splus1, r+s+cbs.z$x+1, cbs.z$z.1)
  status = gsl_sf_hyperg_2F1_e(r + s + x,
                               s + 1.0,
                               r + s + x + 1.0,
                               z1,
                               &gsl_res);
  // status = gsl_sf_hyperg_2F1_e(r + s + x,
  //                              s + 1.0,
  //                              r + s + x + 1.0,
  //                              z1,
  //                              &gsl_res);

  if(status == GSL_EMAXITER || status == GSL_EDOM){
    // Rcpp::Rcout << "pnbd_dyncov_LL_i_hyp_alpha_ge_beta, z1, status: "<<status <<std::endl;
    // hyp.z1 := (1-z.1)^(r+x)*exp(log.C) / beta_1^(r+s+x)]
    hyp_z1 = std::pow(1.0 - z1, r + x) * std::exp(log_C) / std::pow(beta_1, r + s + x);
  }else{
    // cbs.z[, hyp.z1 := l.hyp.z1$value / (alpha_1^(r+s+x))]
    hyp_z1 = gsl_res.val / std::pow(alpha_1, r + s + x);
  }

  // Questions for JEFF:
  //
  // Stable:
  //  is /pow(beta_1, r+s+x): beta_1 oder alpha_1? Because alpha_1 in non-stable version but then changes
  //  is /pow(., r+s+x) = pow(., a) or fix pow(., r+s+x)
  //  can hyp_z all positive terms such that can do exp(log(hyp_z))?


  // Z2
  double hyp_z2;
  // l.hyp.z2 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x, cbs.z$splus1, r+s+cbs.z$x+1, cbs.z$z.2)
  status = gsl_sf_hyperg_2F1_e(r + s + x,
                               s + 1.0,
                               r + s + x + 1.0,
                               z2,
                               &gsl_res);
  // status = gsl_sf_hyperg_2F1_e(r + s + x,
  //                              s + 1.0,
  //                              r + s + x + 1.0,
  //                              z2,
  //                              &gsl_res);
  if(status == GSL_EMAXITER || status == GSL_EDOM){
    // hyp.z2 := (1-z.2)^(r+x)*exp(log.C) / beta_2^(r+s+x)]
    // Rcpp::Rcout << "pnbd_dyncov_LL_i_hyp_alpha_ge_beta, z2, status: "<<status <<std::endl;
    hyp_z2 = std::pow(1.0 - z2, r + x) * std::exp(log_C) / std::pow(beta_2, r + s + x);
  }else{
    // cbs.z[, hyp.z2 := l.hyp.z2$value / (alpha_2^(r+s+x))]
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

  // Do not abort in case of error in gsl functions (hypergeoms)
  gsl_set_error_handler_off();

  // cbs.z[,z.1 := (beta_1-alpha_1)/beta_1]
  // cbs.z[,z.2 := (beta_2-alpha_2)/beta_2]
  const double z1 = 1.0 - (alpha_1/beta_1);
  const double z2 = 1.0 - (alpha_2/beta_2);

  // c + <b-1> -a -b
  // cbs.z[,log.C :=  lgamma(r+s+x+1) + lgamma(r+x-1) - lgamma(r+s+x) - lgamma(r+x) ]
  const double log_C =
    lgamma(r + s + x + 1.0) +
    lgamma(r + x - 1.0) -
    lgamma(r + s + x) -
    lgamma(r + x);
  // const double log_C = std::lgamma(r + s + x + 1.0) +
  //   std::lgamma(s) +
  //   std::lgamma(r + s + x) +
  //   std::lgamma(s + 1.0);

    gsl_sf_result gsl_res;

    // Z1: l.hyp.z1 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x,r+cbs.z$x,r+s+cbs.z$x+1, cbs.z$z.1)
    double hyp_z1;
    int status = gsl_sf_hyperg_2F1_e(r + s + x,
                                     r + x,
                                     r + s + x + 1.0,
                                     z1,
                                     &gsl_res);
    // int status = gsl_sf_hyperg_2F1_e(r + s + x,
    //                                  s + 1.0,
    //                                  r + s + x + 1.0,
    //                                  z1, &gsl_res);
    if(status == GSL_EMAXITER || status == GSL_EDOM){
      // Rcpp::Rcout << "pnbd_dyncov_LL_i_hyp_beta_g_alpha, z1, status: "<<status <<std::endl;
      // hyp.z1 := (1-z.1)^(s+1)*exp(log.C) / (alpha_1)^(r+s+x)]
      hyp_z1 = std::pow(1.0 - z1, s + 1.0) * std::exp(log_C) / std::pow(alpha_1, r + s + x);
    }else{
      // cbs.z[, hyp.z1 := l.hyp.z1$value / (beta_1^(r+s+x))]
      hyp_z1 = gsl_res.val / std::pow(beta_1, r + s + x);
    }


    double hyp_z2;
    // l.hyp.z2 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x,r+cbs.z$x,r+s+cbs.z$x+1, cbs.z$z.2)
    status = gsl_sf_hyperg_2F1_e(r + s + x,
                                 r + x,
                                 r + s + x + 1.0,
                                 z2,
                                 &gsl_res);
    // status = gsl_sf_hyperg_2F1_e(r + s + x,
    //                              s + 1.0,
    //                              r + s + x + 1.0,
    //                              z2, &gsl_res);

    if(status == GSL_EMAXITER || status == GSL_EDOM){
      // Rcpp::Rcout << "pnbd_dyncov_LL_i_hyp_beta_g_alpha, z2, status: "<<status <<std::endl;
      // hyp.z2 := (1-z.2)^(s+1)*exp(log.C) / (alpha_2)^(r+s+x)]
      hyp_z2 = std::pow(1.0 - z2, s + 1.0) * std::exp(log_C) / std::pow(alpha_2, r + s + x);
    }else{
      // cbs.z[, hyp.z2 := l.hyp.z2$value / (beta_2^(r+s+x))]
      hyp_z2 = gsl_res.val / std::pow(beta_2, r + s + x);
    }

    return(hyp_z1 - hyp_z2);
}

/*
 * A1sum: Sum up covariates which were active during transaction
 *        Jeff/Patrik: Use the original covariate (log of adj cov) that is active active during transaction. This transaction is the upper end of the walk because the walk goes from [previous transaction, this transaction].
 *        Jeff: Name A1sum is chosen randomly
 */
double pnbd_dyncov_LL_i_A1sum(const std::vector<TransactionWalk>& real_walks_trans){

  // Zero-repeater do not have real trans walks
  //  Could also check x==0 but better look at actual content of vector
  if(real_walks_trans.size() == 0){
    return(0.0); // log(1)
  }else{
    double A1sum = 0.0;
    for(const TransactionWalk& w : real_walks_trans){
      // w.last() is covariate active when customer transacts as walk is [previous, this]
      A1sum += std::log(w.last());
    }
    return(A1sum);
  }
}

double pnbd_dyncov_LL_i_bksumbjsum_walk_i(const TransactionWalk& w){

  if(w.n_elem() == 1){
    // Transactions were in same cov period, delta = 0
    return(w.first() * (w.tjk));
  }else{
    if(w.n_elem() == 2){
      // because k-2 = 0
      return(w.first()*w.d1 + w.last()*(w.tjk - w.d1));
    }else{
      // >= 3
      //  delta = 1
      double n = static_cast<double>(w.n_elem());
      double last_mult = w.tjk - w.d1 - (n - 2.0);
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
 *  delta: depends on i, 0 if start and end in same period
 *
 */
double pnbd_dyncov_LL_i_Bi(const arma::uword i, const double t_x, const TransactionWalk& aux_walk){

  if(i == 1){
    // delta=0,
    // first is multiplied with and last term
    return(aux_walk.first() * (-t_x));
  }

  if(i == 2){
    // term delta*(i-2) disappears because (i-2)=0
    return(aux_walk.first()*aux_walk.d1 + aux_walk.get_elem(i-1) * (-t_x - aux_walk.d1));
  }

  // Sum elements up to Walk_i
  return(aux_walk.first()*aux_walk.d1 + aux_walk.sum_from_to(1, i-2) +
         aux_walk.get_elem(i-1) * (-t_x - aux_walk.d1 - (static_cast<double>(i) - 2.0)));
}


/*
 * Di()
 * Sum up all covs from coming alive until i periods after last transaction (Walk_i in aux walk)
 *  Treat as if real and aux walk were one continuous walk
 *    Real and aux walk may therefore not overlap
 *  k0x: the number of covariate periods (active covs) from 0 to x (all real walk elems + first aux elem)
 *  delta: whether the start (coming alive, 0) and the end of the sum (given by i) are in same covariate period.
 *         It depends on how long the walk from alive until i is (length of continuous walk until i)
 *  First element is *d_omega. First element is either from real walk or from aux walk (if no real walk)
 */
double pnbd_dyncov_LL_i_Di(const arma::uword i, const LifetimeWalk& real_walk,
                           const LifetimeWalk& aux_walk, const double d_omega){

  // Real and Aux walk are guaranteed to not overlap
  //  Cov where last transaction is in belongs only to aux walk

  // Cannot always sum up real_walk first and then add aux_walk sum because
  //  for the case real_walk.n_elem() == 0, d_omega has to be multiplied with aux_walk.first() not to real_walk.first()
  //  Therefore do real_walk.n_elem() == 0 separately
  if(real_walk.n_elem() == 0){
    // Delta = 1 for all other than i==1

    // Directly sum up the aux walk until Walk_i, including first()*d
    if(i == 1){
      // Cancels out because besides first() also last() (which is the same) is included for i=1!
      return(0.0);
    }else{
      if(i == 2){
        // k0x+i-3 = 1+2-3 = 0. Last multpart * delta disappears
        return(aux_walk.first()*d_omega + aux_walk.get_elem(1)*(-d_omega));
      }else{
        // i >= 3: Walk_1*domega + sum(Walk_2, Walk_i-1) + Walk_i*lastmult
        // lastmult=(-d_omega - delta*(k0x + static_cast<double>(i) - 3.0))
        //    where k0x=1, delta=1
        double last_mult = (-d_omega - (1.0 + static_cast<double>(i) - 3.0));
        return(aux_walk.first()*d_omega + aux_walk.sum_from_to(1, i-2) + aux_walk.get_elem(i-1)*last_mult);
      }
    }
  }else{

    // There are elements in the real walk that all need to be summed (independent of i)

    double sum_real_walk = 0.0;
    // branch because of summing middle elements
    if(real_walk.n_elem() == 1){
      sum_real_walk = real_walk.first()*d_omega;
    }else{
      if(real_walk.n_elem() == 2){
        sum_real_walk = real_walk.first()*d_omega + real_walk.last();
      }else{
        // >= 3: everything in real walk
        sum_real_walk = real_walk.first()*d_omega + real_walk.sum_middle_elems() + real_walk.last();
      }
    }

    // Sum up aux walk until i
    //   aux_walk.first()*d_omega is not required because done with real_walk.first()
    //   delta is always 1 here, because start period is in real walk and end period in auxwalk
    double k0x = static_cast<double>(real_walk.n_elem()) + 1.0; // +1 to also count period of x (first in aux walk)
    double last_mult = -d_omega - (k0x + static_cast<double>(i) - 3.0);

    double sum_aux_walk = 0.0;
    if(i == 1){
      sum_aux_walk = aux_walk.first()*last_mult;
    }else{
      if(i == 2){
        sum_aux_walk = aux_walk.first() + aux_walk.get_elem(1)*last_mult;
      }else{
        // i >= 3
        sum_aux_walk = aux_walk.first() + aux_walk.sum_from_to(1,i-2) + aux_walk.get_elem(i-1)*last_mult;
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

  // Term does not exist for n_elem <= 2
  //  No middle sum if only 2 elems in walk (only F2_1 and F2_2)
  if(c.aux_walk_trans.n_elem() <= 2){
    return(0.0);
  }

  // Loop counts Walks, not element access indices. ie Walk_2, Walk_3, ...
  //    Loop until and including Walk_(n-1)
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

    // abort immediately, do not waste more loops
    if(!arma::is_finite(F2_3)){
      return(F2_3);
    }
  }

  return(F2_3);
}

/*
 * Z = Y_1 + Y_kT + sum_{2}^{kT-1}{Y_i}
 *  Special case for k_T=1 (aux walk only has 1 element)
 */
double pnbd_dyncov_LL_i_F2(const double r, const double alpha_0, const double s, const double beta_0,
                           const Customer& c,
                           const double B1, const double D1,
                           const double BT, const double DT,
                           const double A1T, const double C1T,
                           const double AkT, const double CkT,
                           const double Bjsum,
                           const bool return_intermediate_results,
                           arma::vec& intermediate_results){

  // dT is d1 in the formula. It is d1 around tx, ie d1 of aux walk
  const double dT = c.aux_walk_trans.d1;

  const double a1T = Bjsum + B1 + c.T_cal * A1T;
  const double b1T = D1 + c.T_cal * C1T;
  const double a1 = Bjsum + B1 + A1T * (c.t_x + dT - 1.0);
  const double b1  = D1 + C1T * (c.t_x + dT - 1.0);

  if(c.aux_walk_life.n_elem() == 1){

    // Not the same as F2_1

    const double alpha_1 =  a1 + (1.0-dT)*A1T + alpha_0;
    const double beta_1  = (b1 + (1.0-dT)*C1T + beta_0) * A1T/C1T;

    const double alpha_2 =  a1T + alpha_0;
    const double beta_2  = (b1T  + beta_0)*A1T/C1T;

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
      intermediate_results(0) = dT;
      intermediate_results(1) = a1T;
      intermediate_results(2) = b1T;
      intermediate_results(3) = a1;
      intermediate_results(4) = b1;
      intermediate_results(5) = arma::datum::nan;
      intermediate_results(6) = arma::datum::nan;
      intermediate_results(7) = arma::datum::nan;
      intermediate_results(8) = arma::datum::nan;
      intermediate_results(9) = arma::datum::nan;
      intermediate_results(10) = arma::datum::nan;
      intermediate_results(11) = arma::datum::nan;
    }
    return(F2);

  }else{

    const double n_walks = static_cast<double>(c.aux_walk_life.n_elem());
    const double akt = Bjsum + BT + AkT * (c.t_x + dT + n_walks - 2.0);
    const double bkT = DT + CkT * (c.t_x + dT + n_walks - 2.0);
    const double aT = Bjsum + BT + (c.T_cal * AkT);
    const double bT  = DT + c.T_cal * CkT;

    if(return_intermediate_results){
      intermediate_results(0) = dT;
      intermediate_results(1) = a1T;
      intermediate_results(2) = b1T;
      intermediate_results(3) = a1;
      intermediate_results(4) = b1;

      intermediate_results(5) = akt;
      intermediate_results(6) = bkT;
      intermediate_results(7) = aT;
      intermediate_results(8) = bT;
      intermediate_results(9) = arma::datum::nan;
      intermediate_results(10) = arma::datum::nan;
      intermediate_results(11) = arma::datum::nan;
    }

    const double F2_1 = pnbd_dyncov_LL_i_F2_1(r, alpha_0, s, beta_0,
                                              c.x, dT,
                                              a1, b1,
                                              A1T, C1T);
    intermediate_results(9) = F2_1;
    if(!arma::is_finite(F2_1)){
      return(F2_1);
    }

    const double F2_2 = pnbd_dyncov_LL_i_F2_2(r, alpha_0, s, beta_0,
                                              c.x,
                                              akt, bkT,
                                              aT, bT,
                                              AkT, CkT);
    intermediate_results(10) = F2_2;
    if(!arma::is_finite(F2_2)){
      return(F2_2);
    }

    const double F2_3 = pnbd_dyncov_LL_i_F2_3(r, alpha_0, s, beta_0,
                                              c,
                                              Bjsum, dT);
    intermediate_results(11) = F2_3;

    return(F2_1 + F2_2 + F2_3);
  }
}


Rcpp::NumericVector pnbd_dyncov_LL_i(const double r, const double alpha_0, const double s, const double beta_0,
                                     const Customer& c,
                                     const bool return_intermediate_results){

  // Transaction Process ------------------------------------------------
  const double A1T = c.aux_walk_trans.first();
  const double AkT = c.aux_walk_trans.last(); // used to be adj_transaction_cov_dyn
  const double A1sum = pnbd_dyncov_LL_i_A1sum(c.real_walks_trans);

  const double B1 = pnbd_dyncov_LL_i_Bi(1, c.t_x, c.aux_walk_trans);
  const double BT = pnbd_dyncov_LL_i_Bi(c.aux_walk_trans.n_elem(), c.t_x, c.aux_walk_trans);
  const double Bjsum = pnbd_dyncov_LL_i_BjSum(c.real_walks_trans);
  const double Bksum = pnbd_dyncov_LL_i_BkSum(Bjsum, c.aux_walk_trans);


  // Lifetime Process ---------------------------------------------------
  const double C1T = c.aux_walk_life.first();
  const double CkT = c.aux_walk_life.last(); // used to be adj_lifetime_cov_dyn
  const double D1 = pnbd_dyncov_LL_i_Di(1, c.real_walk_life, c.aux_walk_life, c.d_omega);
  const double DT = pnbd_dyncov_LL_i_Di(c.aux_walk_life.n_elem(), c.real_walk_life, c.aux_walk_life, c.d_omega);

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

  arma::vec F2_intermediate_results = arma::vec(12);
  double F2 = pnbd_dyncov_LL_i_F2(r, alpha_0,  s,  beta_0,
                                  c,
                                  B1, D1,
                                  BT,  DT,
                                  A1T,  C1T, AkT,  CkT,
                                  Bjsum,
                                  return_intermediate_results,
                                  F2_intermediate_results);

  const double log_F3 = -s * std::log(DkT + beta_0) - (c.x+r) * std::log(Bksum + alpha_0);

  // Branch by F2:
  //
  //  F2 may be non-finite (especially NaN):
  //    Propagate to LL
  //    Avoid that it falls into the default branch where it is omitted (LL = log_F0 + log_F3)
  //
  //  Cannot set F2=0.0 when it is 'reasonably' small (<sqrt(machineeps)) in order to more often take
  //    advantage of the much simpler case F2==0 because also very small abs(F2) are really relevant for correct results!
  //
  double LL = 0;
  if(!arma::is_finite(F2)){
    LL = F2;
  }else{

    // Cannot: Set F2 to exact 0 if it is reasonably small. Wrong results :(
    // if(std::fabs(F2 - 0.0) < std::sqrt(arma::datum::eps)){
    //   F2 = 0.0;
    // }

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


  if(!return_intermediate_results){
    return(Rcpp::NumericVector::create(LL));
  }else{

    double Akprod = std::exp(A1sum);
    Rcpp::NumericVector res (30);

    Rcpp::CharacterVector res_names = {"LL",
                                       "A1T", "AkT", "A1sum", "B1", "BT", "Bjsum", "Bksum",
                                       "C1T", "CkT", "D1", "DT", "DkT",
                                       "log_F0", "log_F1", "F2", "log_F3",
                                       "Akprod",
                                       "dT", "a1T", "b1T", "a1", "b1",
                                       "akt", "bkT", "aT", "bT",
                                       "F2.1", "F2.2", "F2.3"};

    res[0]=LL;

    res[1]=A1T;
    res[2]=AkT;
    res[3]=A1sum;
    res[4]=B1;
    res[5]=BT;
    res[6]=Bjsum;
    res[7]=Bksum;

    res[8]=C1T;
    res[9]=CkT;
    res[10]=D1;
    res[11]=DT;
    res[12]=DkT;

    res[13]=log_F0;
    res[14]=log_F1;
    res[15]=F2;
    res[16]=log_F3;

    res[17]=Akprod;

    res[18]=F2_intermediate_results(0);
    res[19]=F2_intermediate_results(1);
    res[20]=F2_intermediate_results(2);
    res[21]=F2_intermediate_results(3);
    res[22]=F2_intermediate_results(4);
    res[23]=F2_intermediate_results(5);
    res[24]=F2_intermediate_results(6);
    res[25]=F2_intermediate_results(7);
    res[26]=F2_intermediate_results(8);
    res[27]=F2_intermediate_results(9);
    res[28]=F2_intermediate_results(10);
    res[29]=F2_intermediate_results(11);

    res.names() = res_names;
    return(res);

  }
}

// [[Rcpp::export]]
double pnbd_dyncov_LL_negsum(const arma::vec& params,
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

  return(-Rcpp::sum(pnbd_dyncov_LL_ind(params,
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


  // exp(gamma'*X)
  //  The only thing that changes between calls to the LL during optimization
  //  Has to be const because arma::vec in walk classes are reusing this memory
  const arma::vec adj_covdata_aux_life   = arma::exp(covdata_aux_life   * params_life);
  const arma::vec adj_covdata_real_life  = arma::exp(covdata_real_life  * params_life);
  const arma::vec adj_covdata_aux_trans  = arma::exp(covdata_aux_trans  * params_trans);
  const arma::vec adj_covdata_real_trans = arma::exp(covdata_real_trans * params_trans);

  Rcpp::NumericMatrix res;
  if(return_intermediate_results){
    res = Rcpp::NumericMatrix(X.n_elem, 30);
  }else{
    res = Rcpp::NumericMatrix(X.n_elem, 1);
  }

  Rcpp::NumericVector res_i;
  for(arma::uword i = 0; i < X.n_elem; i++){

    // Check whether customer has real trans walks
    //   Could also check x==0, but saver to look at actual content
    if(arma::is_finite(walkinfo_trans_real_from(i))){

      // Repeat customer (with real trans walks)
      arma::uword wi_real_trans_from = static_cast<arma::uword>(walkinfo_trans_real_from(i)) - 1;
      arma::uword wi_real_trans_to = static_cast<arma::uword>(walkinfo_trans_real_to(i)) - 1;

      res_i = pnbd_dyncov_LL_i(r, alpha_0, s, beta_0,
                               Customer(X(i), t_x(i), T_cal(i), d_omega(i),
                                        adj_covdata_aux_life, walkinfo_aux_life.row(i),
                                        adj_covdata_real_life, walkinfo_real_life.row(i),
                                        adj_covdata_aux_trans, walkinfo_aux_trans.row(i),
                                        adj_covdata_real_trans, walkinfo_real_trans.rows(wi_real_trans_from, wi_real_trans_to)),
                               return_intermediate_results);
    }else{
      // Zero-repeater (no real trans walks)
      res_i = pnbd_dyncov_LL_i(r, alpha_0, s, beta_0,
                               Customer(X(i), t_x(i), T_cal(i), d_omega(i),
                                        adj_covdata_aux_life, walkinfo_aux_life.row(i),
                                        adj_covdata_real_life, walkinfo_real_life.row(i),
                                        adj_covdata_aux_trans, walkinfo_aux_trans.row(i)),
                               return_intermediate_results);
    }

    res(i, Rcpp::_) = res_i;
  }

  Rcpp::CharacterVector c_names = {"LL"};
  if(return_intermediate_results){
    c_names = res_i.names();
  }
  Rcpp::colnames(res) = c_names;
  return(res);
}
