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

if(requireNamespace("BTYD", quietly = TRUE)){
  fct.testthat.correctness.clvfittedtransactions.nocov.same.as.btyd(clvtools.method = pnbd,
                                                                    btyd.method = BTYD::pnbd.EstimateParameters,
                                                                    btyd.dert.method = BTYD::pnbd.DERT,
                                                                    btyd.cet.method = BTYD::pnbd.ConditionalExpectedTransactions,
                                                                    btyd.palive.method = BTYD::pnbd.PAlive,
                                                                    start.params.model = c(r=0.5, alpha = 6, s = 0.9, beta = 8),
                                                                    cdnow = cdnow)
}

context("Correctness - PNBD nocov - PAlive")

test_that("Can calculate numerically stable PAlive that produced NaNs in previous implementation and in BTYD", {

  vX     <- c(221,       254,      161,      204)
  vT_x   <- c(103.42857, 97.14286, 94.71429, 98.57143)
  vT_cal <- c(103.57143, 97.28571, 98.00000, 99.42857)

  expect_silent(palive <- pnbd_nocov_PAlive(r = 0.5143, alpha_0 = 2.8845, s = 0.2856, beta_0 = 14.1087,
                                            vX = vX, vT_x = vT_x, vT_cal = vT_cal))

  expect_false(any(!is.finite(palive)))
})



# Dyncov ---------------------------------------------------------------------------------------
fct.testthat.correctness.dyncov(data.apparelTrans=apparelTrans, data.apparelDynCov=apparelDynCov)

# ** Static cov??


