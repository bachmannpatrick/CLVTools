data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

# Correct coefs are our estimates
fct.testthat.correctness.clvfittedtransactions(name.model = "BG/NBD", method=bgnbd, data.cdnow=cdnow,
                                               data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                                               correct.start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                               correct.params.nocov.coef = c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881),
                                               correct.LL.nocov = -9582.429)



context("Correctness - BG/NBD nocov - Recover parameters")
# As reported in Fader, Hardie, Lee (2005)
fct.testthat.correctness.clvfitted.correct.coefs(method = bgnbd,
                                                 cdnow = cdnow,
                                                 start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                                 params.nocov.coef = c(r = 0.243, alpha = 4.414, a = 0.793, b = 2.426),
                                                 LL.nocov = -9582.4)

fct.testthat.correctness.clvfittedtransactions.nocov.same.as.btyd(clvtools.method = bgnbd,
                                                                  btyd.method = BTYD::bgnbd.EstimateParameters,
                                                                  btyd.dert.method = NULL,
                                                                  btyd.cet.method = BTYD::bgnbd.ConditionalExpectedTransactions,
                                                                  btyd.palive.method = BTYD::bgnbd.PAlive,
                                                                  start.params.model = c(r = 1, alpha = 3, a = 1, b = 3),
                                                                  cdnow = cdnow,
                                                                  DERT.not.implemented = TRUE)
