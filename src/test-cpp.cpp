/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */
#include "ggomnbd.h"
#include "pnbd_dyncov_LL.h"
#include <testthat.h>

bool equal(double x, double y, double maxdiff=0.00001){
  // Use fixed precision because results are from Excel (and therefore independent from machine eps)
  // return(std::fabs(x - y) < std::sqrt(arma::datum::eps));
  return(std::fabs(x - y) < maxdiff);
}
double t_x = 35.11;
double T = 57.28;
double d1 = 0.854;
double d_omega = 0.854;
double tjk = 23.87;
double x = 4;

// Bi() vs excel ------------------------------------------------------------------------------

context("Bi() vs excel") {
  // from and to are R indices (from 1 to n)
  const arma::vec walkdata = {0.123, 0.234, 0.345, 0.456, 0.567, 0.678};
  arma::rowvec walkinfo = {1, 6, d1, tjk}; //from, to, d1, tjk

  test_that("Bi() vs excel, walk length 6"){
    walkinfo(1) = 6;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(2, t_x, walk), -8.310534));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(3, t_x, walk), -12.413538));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(4, t_x, walk), -16.627542));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(5, t_x, walk), -20.952546));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(6, t_x, walk), -25.388550));
  }

  test_that("Bi() vs excel, walk length 5"){
    walkinfo(1) = 5;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(2, t_x, walk), -8.310534));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(3, t_x, walk), -12.413538));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(4, t_x, walk), -16.627542));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(5, t_x, walk), -20.952546));
  }

  test_that("Bi() vs excel, walk length 4"){
    walkinfo(1) = 4;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(2, t_x, walk), -8.310534));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(3, t_x, walk), -12.413538));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(4, t_x, walk), -16.627542));
  }

  test_that("Bi() vs excel, walk length 3"){
    walkinfo(1) = 3;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(2, t_x, walk), -8.310534));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(3, t_x, walk), -12.413538));
  }

  test_that("Bi() vs excel, walk length 2"){
    walkinfo(1) = 2;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
    expect_true(equal(pnbd_dyncov_LL_i_Bi(2, t_x, walk), -8.310534));
  }

  test_that("Bi() vs excel, walk length 1"){
    walkinfo(1) = 1;
    const auto walk = TransactionWalk(walkdata, walkinfo);
    expect_true(equal(pnbd_dyncov_LL_i_Bi(1, t_x, walk), -4.31853));
  }
}

// Di() vs excel ------------------------------------------------------------------------------

LifetimeWalk get_aux_lifetimewalk(arma::uword from){
  const arma::vec aux_walkdata = {0.123, 0.234, 0.345, 0.456};
  arma::rowvec aux_walkinfo = {1, 4}; // from and to are R indices (from 1 to n)
  aux_walkinfo(1) = from;
  return(LifetimeWalk(aux_walkdata, aux_walkinfo));
}

LifetimeWalk get_real_lifetimewalk(arma::uword from){
  if(from > 0){
    const arma::vec real_walkdata = {1.987, 1.876};
    arma::rowvec real_walkinfo = {1, 2};  // from and to are R indices (from 1 to n)
    real_walkinfo(1) = from;
    return(LifetimeWalk(real_walkdata, real_walkinfo));
  }else{
    return(EmptyLifetimeWalk());
  }
}

context("Di() vs excel") {

  test_that("Di() vs excel, real length 0, aux length 1"){
    auto real_walk = get_real_lifetimewalk(0);
    auto aux_walk = get_aux_lifetimewalk(1);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 0));
  }

  test_that("Di() vs excel, real length 0, aux length 2"){
    auto real_walk = get_real_lifetimewalk(0);
    auto aux_walk = get_aux_lifetimewalk(2);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 0));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), -0.094794));
  }

  test_that("Di() vs excel, real length 0, aux length 3"){
    auto real_walk = get_real_lifetimewalk(0);
    auto aux_walk = get_aux_lifetimewalk(3);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 0));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), -0.094794));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), -0.300588));
  }


  test_that("Di() vs excel, real length 0, aux length 4"){
    auto real_walk = get_real_lifetimewalk(0);
    auto aux_walk = get_aux_lifetimewalk(4);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 0));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), -0.094794));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), -0.300588));
    expect_true(equal(pnbd_dyncov_LL_i_Di(4, real_walk, aux_walk, d_omega), -0.617382));
  }

  test_that("Di() vs excel, real length 1, aux length 1"){
    auto real_walk = get_real_lifetimewalk(1);
    auto aux_walk = get_aux_lifetimewalk(1);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 1.591856));
  }

  test_that("Di() vs excel, real length 1, aux length 2"){
    auto real_walk = get_real_lifetimewalk(1);
    auto aux_walk = get_aux_lifetimewalk(2);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 1.591856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 1.386062));
  }

  test_that("Di() vs excel, real length 1, aux length 3"){
    auto real_walk = get_real_lifetimewalk(1);
    auto aux_walk = get_aux_lifetimewalk(3);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 1.591856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 1.386062));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), 1.069268));
  }


  test_that("Di() vs excel, real length 1, aux length 4"){
    auto real_walk = get_real_lifetimewalk(1);
    auto aux_walk = get_aux_lifetimewalk(4);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 1.591856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 1.386062));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), 1.069268));
    expect_true(equal(pnbd_dyncov_LL_i_Di(4, real_walk, aux_walk, d_omega), 0.641474));
  }

  test_that("Di() vs excel, real length 2, aux length 1"){
    auto real_walk = get_real_lifetimewalk(2);
    auto aux_walk = get_aux_lifetimewalk(1);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 3.344856));
  }

  test_that("Di() vs excel, real length 2, aux length 2"){
    auto real_walk = get_real_lifetimewalk(2);
    auto aux_walk = get_aux_lifetimewalk(2);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 3.344856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 3.028062));
  }


  test_that("Di() vs excel, real length 2, aux length 3"){
    auto real_walk = get_real_lifetimewalk(2);
    auto aux_walk = get_aux_lifetimewalk(3);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 3.344856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 3.028062));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), 2.600268));
  }


  test_that("Di() vs excel, real length 2, aux length 4"){
    auto real_walk = get_real_lifetimewalk(2);
    auto aux_walk = get_aux_lifetimewalk(4);

    expect_true(equal(pnbd_dyncov_LL_i_Di(1, real_walk, aux_walk, d_omega), 3.344856));
    expect_true(equal(pnbd_dyncov_LL_i_Di(2, real_walk, aux_walk, d_omega), 3.028062));
    expect_true(equal(pnbd_dyncov_LL_i_Di(3, real_walk, aux_walk, d_omega), 2.600268));
    expect_true(equal(pnbd_dyncov_LL_i_Di(4, real_walk, aux_walk, d_omega), 2.061474));
  }

}




context("GGomNBD CET hyp2F1 integral representation") {

  test_that("GGomnbd CET Hyp2F1 same as Mathematica"){


    // Check if hyp2F1 numeric integration used in GGomNBD CET produces
    // the same result as `Hypergeometric2F1[1, s, s+1, z]` on Mathematica

    const arma::vec vZ = {
      -997.0,
      // 999: cannot reproduce for s=0.1
      99.0,
      0.0,
      // 1.0,
      0.0001,
      0.9999,
      0.5,
    };

    // "correct"" results for s=0.1
    const arma::vec vMathematicaS01 = {
      0.509569,
      // 999: 0.4847490760, // -0.1574683 i, cannot reproduce
      0.61180298, //-0.1984204 i
      1.0,
      // arma::datum::inf,
      1.0,
      1.90569802,
      1.06398
    };
    auto res = ggomnbd_CET_hyp2f1_1_s_splus1_integrate(0.1, vZ);
    expect_true(equal(res(0), vMathematicaS01(0)));
    expect_true(equal(res(1), vMathematicaS01(1), 0.001));
    expect_true(equal(res(2), vMathematicaS01(2)));
    expect_true(equal(res(3), vMathematicaS01(3)));
    expect_true(equal(res(4), vMathematicaS01(4)));
    expect_true(equal(res(5), vMathematicaS01(5)));

    // "correct"" results for s=2.34
    const arma::vec vMathematicaS234 = {
      0.0017454,
      // 999: -0.00175452239, // -7.0369179 i
      -0.018251171, // -0.000157245 i
      1.0,
      // arma::datum::inf,
      1.00007,
      18.75187018,
      1.58087
    };
    auto res234 = ggomnbd_CET_hyp2f1_1_s_splus1_integrate(2.34, vZ);

    for(arma::uword i = 0; i<vZ.n_elem; i++){
      expect_true(equal(res234(i), vMathematicaS234(i)));
    }
  }
}


// hyp2F1 terms of the dyncov LL close to z = 1 ------------------------------------------------------

bool rel_equal(double x, double y, double maxreldiff=1e-8){
  return(std::fabs(x/y - 1.0) < maxreldiff);
}

context("PNBD dyncov hyp2F1 terms close to z = 1") {

  // hyp(z1) - hyp(z2) as returned by pnbd_dyncov_LL_i_hyp_beta_g_alpha() and pnbd_dyncov_LL_i_hyp_alpha_ge_beta(),
  // against reference values computed with 80 digits (mpmath).
  // gsl_sf_hyperg_2F1_e() returns GSL_EUNIMPL with value 0 for z >= 0.995 if r+s+x >= 10. This value used to
  // enter the likelihood as a valid 0, as did the quotient with beta^(r+s+x) once this power overflowed.

  test_that("beta > alpha"){
    // z1 just above 0.995, r+s+x >= 10 (GSL_EUNIMPL)
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(0.62, 7.9914, 9.0, 12.142857, 2428.8, 13.142857, 2429.8), 1.6499615942489781e-40));
    // same customer just below 0.995 (unchanged path)
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(0.62, 7.98, 9.0, 12.142857, 2425.3, 13.142857, 2426.3), 1.8256184985873479e-40));
    // r+s+x just above 10 at z1 > 0.995
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(0.62, 7.4, 2.0, 7.142857, 2244.57, 8.142857, 2245.57), 3.4625647929773733e-30));
    // integer r
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(1.0, 2.0, 3.0, 6.0, 3000.0, 7.0, 3001.0), 1.2691916318764055e-13));
    // many transactions (beta^(r+s+x) overflows)
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(1.5, 2.7, 150.0, 40.0, 3000.0, 41.0, 3001.0), 1.0576591590990417e-254));
    // z1 very close to 1
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha(0.62, 2.7, 1.0, 5.0, 2000000.0, 6.0, 2000001.0), 1.3330118544478058e-24));
  }

  test_that("alpha >= beta"){
    // z1 = 0.998, r+s+x >= 10
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_alpha_ge_beta(0.62, 2.7, 9.0, 3000.0, 6.0, 3001.0, 7.0), 4.3627788723595939e-36));
    // integer s
    expect_true(rel_equal(pnbd_dyncov_LL_i_hyp_alpha_ge_beta(0.8, 2.0, 12.0, 5000.0, 4.0, 5001.0, 5.0), 7.4834902869494363e-49));
  }

}


// hyp2F1 terms of the dyncov LL for customers with many transactions ---------------------------------

bool log_equal(const SignedLog& x, double log_y, double maxabsdiff=1e-9){
  return(x.sgn == 1.0 && std::fabs(x.lg - log_y) < maxabsdiff);
}

context("PNBD dyncov hyp2F1 terms smaller than the smallest double") {

  // log(hyp(z1) - hyp(z2)) against reference values computed with 120 digits (mpmath).
  // The terms are of the order alpha^-(r+s+x). On the linear scale they are below the smallest double for customers
  // with many transactions, lose precision in the subnormal range and then are 0. F2 used to be 0 for these customers
  // (as if they could not have churned) and the log-likelihood had small jumps in the parameters.

  test_that("beta > alpha"){
    // GSL value is finite, hyp ~ 1e-484
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha_log(0.9355, 0.3, 300.0, 40.0, 420.0, 41.0, 421.0), -1114.274929466984607));
    // 2F1 itself is larger than the largest double (GSL returns Inf): rescaled series
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha_log(0.9355, 0.3, 400.0, 10.0, 420.0, 11.0, 421.0), -928.73468391093306683));
    // z > 0.995: transformation z -> 1-z
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha_log(0.9355, 5.5, 250.0, 30.0, 7500.0, 31.0, 7501.0), -908.05195301670949973));
    // subnormal range on the linear scale (1e-317)
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha_log(0.9355, 0.3, 170.0, 70.0, 420.0, 71.0, 421.0), -729.90990909822542717));
    // linear scale: 0, as before
    expect_true(pnbd_dyncov_LL_i_hyp_beta_g_alpha(0.9355, 0.3, 300.0, 40.0, 420.0, 41.0, 421.0) == 0.0);
  }

  test_that("alpha >= beta"){
    // z > 0.995
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_alpha_ge_beta_log(0.62, 2.7, 400.0, 3000.0, 6.0, 3001.0, 7.0), -3208.4794221216461848));
    // z < 0.995
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_alpha_ge_beta_log(0.8, 0.5, 300.0, 60.0, 20.0, 61.0, 21.0), -1231.994482208007054));
  }

  test_that("previous values are unchanged"){
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_beta_g_alpha_log(0.62, 7.98, 9.0, 12.142857, 2425.3, 13.142857, 2426.3), std::log(1.8256184985873479e-40), 1e-8));
    expect_true(log_equal(pnbd_dyncov_LL_i_hyp_alpha_ge_beta_log(0.8, 2.0, 12.0, 5000.0, 4.0, 5001.0, 5.0), std::log(7.4834902869494363e-49), 1e-8));
  }

}
