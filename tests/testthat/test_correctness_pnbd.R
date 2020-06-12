data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# Correct coefs are our estimates
fct.testthat.correctness(name.model = "PNBD", method=pnbd, data.cdnow=cdnow,
                         data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         correct.start.params.model = c(r=1, alpha = 1, s = 1, beta = 1),
                         correct.params.nocov.coef = c(r=0.55315,   alpha=10.57633,  s=0.60625,   beta=11.67150),
                         correct.LL.nocov = -9594.976)


# # Recover parameters ---------------------------------------------------------------------------------
context("Correctness - PNBD nocov - Recover parameters")

# As also reported to compare against bgnbd in Fader, Hardie, Lee (2005)
fct.testthat.correctness.nocov.correct.coefs(method = pnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r=1, alpha = 1, s = 1, beta = 1),
                                             params.nocov.coef = c(r=0.553,   alpha=10.578,  s=0.606,   beta=11.669),
                                             LL.nocov = -9595.0)
fct.testthat.correctness.nocov.correct.se(method = pnbd,
                                          cdnow = cdnow,
                                          start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                                          params.nocov.se = c(r=0.0476264, alpha=0.8427222, s=0.1872594, beta=6.2105448))

fct.testthat.correctness.nocov.same.as.btyd(clvtools.method = pnbd,
                                            btyd.method = BTYD::pnbd.EstimateParameters,
                                            btyd.dert.method = BTYD::pnbd.DERT,
                                            btyd.cet.method = BTYD::pnbd.ConditionalExpectedTransactions,
                                            btyd.palive.method = BTYD::pnbd.PAlive,
                                            start.params.model = c(r=0.5, alpha = 6, s = 0.9, beta = 8),
                                            cdnow = cdnow)



fct.testthat.correctness.dyncov(data.apparelTrans=apparelTrans, data.apparelDynCov=apparelDynCov)


# ** Static cov??

# *** Fit Dyncov (in helper function) + test that CET = 0 with prediction.end = 0 or prediction.end = holdout.start
# ** test with different prediction.ends that k is correctly caught
# document that prediction is from estimation.end until (incl) prediction.end
#


