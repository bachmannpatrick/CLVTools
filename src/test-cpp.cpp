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
