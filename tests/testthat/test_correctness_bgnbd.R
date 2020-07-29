data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

# Correct coefs are our estimates
fct.testthat.correctness.clvfittedtransactions(name.model = "BG/NBD", method=bgnbd, data.cdnow=cdnow,
                                               data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                                               correct.start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                               correct.params.nocov.coef = c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881),
                                               correct.LL.nocov = -9582.429,
                                               kkt2.true = TRUE)



context("Correctness - BG/NBD nocov - Recover parameters")
# As reported in Fader, Hardie, Lee (2005)
fct.testthat.correctness.clvfitted.correct.coefs(method = bgnbd,
                                                 cdnow = cdnow,
                                                 start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                                 params.nocov.coef = c(r = 0.243, alpha = 4.414, a = 0.793, b = 2.426),
                                                 LL.nocov = -9582.4)

context("Correctness - BG/NBD nocov - Expectation")

test_that("Expectation in Rcpp matches expectation in R (nocov)", {

  # No cov ---------------------------------------------------------------------------------------------------
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                     date.format = "ymd", time.unit = "W", estimation.split = 38,
                                     name.id = "Id", name.date = "Date", name.price = "Price"))


  expect_silent(obj.fitted <- do.call(bgnbd, list(clv.data = clv.cdnow, verbose = FALSE)))


  params_i <- obj.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers because there are no covariates
  params_i[, r := obj.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := obj.fitted@prediction.params.model[["alpha"]]]
  params_i[, a := obj.fitted@prediction.params.model[["a"]]]
  params_i[, b := obj.fitted@prediction.params.model[["b"]]]

  fct.expectation.R <- function(params_i.t){
    term1 <- params_i.t[,(a + b - 1)/(a - 1)]
    term2 <- params_i.t[,(alpha/(alpha + t_i))^r]
    term3 <- params_i.t[, vec_gsl_hyp2f1_e(r, b, a+b-1, t_i/(alpha+t_i) )$value]

    return(term1 * (1 - term2 * term3))
  }

  fct.expectation.Cpp <- function(params_i.t){return(params_i.t[, bgnbd_nocov_expectation(r = obj.fitted@prediction.params.model[["r"]],
                                                                                alpha = obj.fitted@prediction.params.model[["alpha"]],
                                                                                a = obj.fitted@prediction.params.model[["a"]],
                                                                                b = obj.fitted@prediction.params.model[["b"]],
                                                                                vT_i = t_i)])}

  dt.expectation.seq <- clv.time.expectation.periods(clv.time = obj.fitted@clv.data@clv.time,
                                                     user.tp.end = 38)

  result.R <- DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                            fct.expectation = fct.expectation.R, clv.time = obj.fitted@clv.data@clv.time)

  result.Cpp <- DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                              fct.expectation = fct.expectation.Cpp, clv.time = obj.fitted@clv.data@clv.time)

  expect_equal(result.R, result.Cpp)
})

context("Correctness - BG/NBD staticcov - Expectation")

test_that("Expectation in Rcpp matches expectation in R (staticcov)", {

  # Static cov ---------------------------------------------------------------------------------------------------
  expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans,
                                       date.format = "ymd", time.unit = "W", estimation.split = 38,
                                       name.id = "Id", name.date = "Date", name.price = "Price"))

  clv.apparel.static <- SetStaticCovariates(clv.data = clv.apparel,
                                            data.cov.life = apparelStaticCov,
                                            data.cov.trans = apparelStaticCov,
                                            names.cov.life = "Gender",
                                            names.cov.trans = "Gender",
                                            name.id = "Id")

  expect_silent(obj.fitted <- do.call(bgnbd, list(clv.data = clv.apparel.static, verbose = FALSE)))

  params_i <- obj.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=obj.fitted@clv.data, correct.row.names=params_i$Id,
                                                        correct.col.names=names(obj.fitted@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=obj.fitted@clv.data, correct.row.names=params_i$Id,
                                                         correct.col.names=names(obj.fitted@prediction.params.trans))

  # Alpha is for trans, a and b for live!
  params_i[, r       := obj.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := obj.fitted@prediction.params.model[["alpha"]] * exp( -m.cov.data.trans  %*% obj.fitted@prediction.params.trans)]
  params_i[, a_i     := obj.fitted@prediction.params.model[["a"]]     * exp(  m.cov.data.life   %*% obj.fitted@prediction.params.life)]
  params_i[, b_i     := obj.fitted@prediction.params.model[["b"]]     * exp(  m.cov.data.life   %*% obj.fitted@prediction.params.life)]

  fct.expectation.R <- function(params_i.t){
    term1 <- params_i.t[,(a_i + b_i - 1)/(a_i - 1)]
    term2 <- params_i.t[,(alpha_i/(alpha_i + t_i))^r]
    term3 <- params_i.t[, vec_gsl_hyp2f1_e(r, b_i, a_i+b_i-1, t_i/(alpha_i+t_i))$value]

    return(term1 * (1 - term2 * term3))
  }


  # To calculate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation.Cpp <- function(params_i.t){return(params_i.t[, bgnbd_staticcov_expectation(r = obj.fitted@prediction.params.model[["r"]],
                                                                                   alpha = obj.fitted@prediction.params.model[["alpha"]],
                                                                                   a = obj.fitted@prediction.params.model[["a"]],
                                                                                   b = obj.fitted@prediction.params.model[["b"]],
                                                                                   vT_i = t_i,
                                                                                   vCovParams_trans = obj.fitted@prediction.params.trans,
                                                                                   vCovParams_life = obj.fitted@prediction.params.life,
                                                                                   mCov_life = m.cov.data.life,
                                                                                   mCov_trans = m.cov.data.trans)])}

  dt.expectation.seq <- clv.time.expectation.periods(clv.time = obj.fitted@clv.data@clv.time,
                                                     user.tp.end = 38)

  result.R <- DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                            fct.expectation = fct.expectation.R, clv.time = obj.fitted@clv.data@clv.time)

  result.Cpp <- DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                              fct.expectation = fct.expectation.Cpp, clv.time = obj.fitted@clv.data@clv.time)

  expect_equal(result.R, result.Cpp)
})
