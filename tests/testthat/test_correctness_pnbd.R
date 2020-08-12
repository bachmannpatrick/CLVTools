data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# Correct coefs are our estimates
fct.testthat.correctness.clvfittedtransactions(name.model = "PNBD", method=pnbd, data.cdnow=cdnow,
                                               data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                                               correct.start.params.model = c(r=1, alpha = 1, s = 1, beta = 1),
                                               correct.params.nocov.coef = c(r=0.55315,   alpha=10.57633,  s=0.60625,   beta=11.67150),
                                               correct.LL.nocov = -9594.976,
                                               kkt2.true = TRUE)


# # Recover parameters ---------------------------------------------------------------------------------
context("Correctness - PNBD nocov - Recover parameters")

# As also reported to compare against bgnbd in Fader, Hardie, Lee (2005)
fct.testthat.correctness.clvfitted.correct.coefs(method = pnbd,
                                                 cdnow = cdnow,
                                                 start.params.model = c(r=1, alpha = 1, s = 1, beta = 1),
                                                 params.nocov.coef = c(r=0.553,   alpha=10.578,  s=0.606,   beta=11.669),
                                                 LL.nocov = -9595.0)
fct.testthat.correctness.clvfitted.nocov.correct.se(method = pnbd,
                                                    cdnow = cdnow,
                                                    start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                                                    params.nocov.se = c(r=0.0476264, alpha=0.8427222, s=0.1872594, beta=6.2105448))



context("Correctness - PNBD nocov - PAlive")

test_that("Can calculate numerically stable PAlive that produced NaNs in previous implementation", {

  vX     <- c(221,       254,      161,      204)
  vT_x   <- c(103.42857, 97.14286, 94.71429, 98.57143)
  vT_cal <- c(103.57143, 97.28571, 98.00000, 99.42857)

  expect_silent(palive <- pnbd_nocov_PAlive(r = 0.5143, alpha_0 = 2.8845, s = 0.2856, beta_0 = 14.1087,
                                            vX = vX, vT_x = vT_x, vT_cal = vT_cal))

  expect_false(any(!is.finite(palive)))
})



# Dyncov ---------------------------------------------------------------------------------------
fct.testthat.correctness.dyncov(data.apparelTrans=apparelTrans, data.apparelDynCov=apparelDynCov)

context("Correctness - PNBD nocov - Expectation")

test_that("Expectation in Rcpp matches expectation in R (nocov)", {

  skip_on_cran()
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                     date.format = "ymd", time.unit = "W", estimation.split = 38,
                                     name.id = "Id", name.date = "Date", name.price = "Price"))


  expect_silent(obj.fitted <- pnbd(clv.data = clv.cdnow, verbose = FALSE))

  params_i <- obj.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers because there are no covariates
  params_i[, r       := obj.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := obj.fitted@prediction.params.model[["alpha"]]]
  params_i[, s       := obj.fitted@prediction.params.model[["s"]]]
  params_i[, beta_i  := obj.fitted@prediction.params.model[["beta"]]]

  fct.expectation.R <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  fct.testthat.correctness.clvfittedtransactions.same.expectation.in.R.and.Cpp(fct.expectation.R = fct.expectation.R,
                                                                               params_i = params_i,
                                                                               obj.fitted = obj.fitted)

})

context("Correctness - PNBD staticcov - Expectation")

test_that("Expectation in Rcpp matches expectation in R (staticcov)", {

  skip_on_cran()
  # To test correctly, fake that some customers only come alive later
  apparelTrans.later <- copy(apparelTrans)
  apparelTrans.later[Id %in% c("1", "10", "100"), Date := Date + lubridate::weeks(10)]
  clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = apparelTrans.later,
                                                                    data.apparelStaticCov = apparelStaticCov,
                                                                    estimation.split = 38)

  expect_silent(obj.fitted <- pnbd(clv.data = clv.apparel.static, verbose = FALSE))

  params_i <- obj.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=obj.fitted@clv.data, correct.row.names=params_i$Id,
                                                        correct.col.names=names(obj.fitted@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=obj.fitted@clv.data, correct.row.names=params_i$Id,
                                                         correct.col.names=names(obj.fitted@prediction.params.trans))

  # all params exactly the same for all customers as there are no covariates
  params_i[, r       := obj.fitted@prediction.params.model[["r"]]]
  params_i[, s       := obj.fitted@prediction.params.model[["s"]]]

  # Alpha is for trans, beta for live!
  params_i[, alpha_i := obj.fitted@prediction.params.model[["alpha"]] * exp( -m.cov.data.trans %*% obj.fitted@prediction.params.trans)]
  params_i[, beta_i  := obj.fitted@prediction.params.model[["beta"]]  * exp( -m.cov.data.life  %*% obj.fitted@prediction.params.life)]

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation.R <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  fct.testthat.correctness.clvfittedtransactions.same.expectation.in.R.and.Cpp(fct.expectation.R = fct.expectation.R,
                                                                               params_i = params_i,
                                                                               obj.fitted = obj.fitted)
})
