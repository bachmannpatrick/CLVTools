/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */
#include "pnbd_dyncov_LL.h"
#include <testthat.h>

bool equal(double x, double y){
  // Use fixed precision because results are from Excel (and therefore independent from machine eps)
  // return(std::fabs(x - y) < std::sqrt(arma::datum::eps));
  return(std::fabs(x - y) < 0.00001);
}
double t_x = 35.11;
double T = 57.28;
double delta = 1.0;
double d1 = 0.854;
double d_omega = 0.854;
double tjk = 23.87;
double x = 4;

// Bi() vs excel ------------------------------------------------------------------------------

context("Bi() vs excel") {
  // from and to are R indices (from 1 to n)
  const arma::vec walkdata = {0.123, 0.234, 0.345, 0.456, 0.567, 0.678};
  arma::rowvec walkinfo = {1, 6, delta, d1, tjk}; //from, to, delta, d1, tjk

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

