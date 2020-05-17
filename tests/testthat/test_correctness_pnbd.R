# Recover parameters ---------------------------------------------------------------------------------
context("Correctness - PNBD nocov - Recover parameters")
data("cdnow")

# Is used at various places
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                   estimation.split = 38))

# Our estimates
fct.testthat.correctness.nocov.correct.coefs(method = pnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r=1, alpha = 1, s = 1, beta = 1),
                                             params.nocov.coef = c(r=0.55315,   alpha=10.57633,  s=0.60625,   beta=11.67150),
                                             LL.nocov = -9594.976)

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
                                            start.params.model = c(r=0.5, alpha = 6, s = 0.9, beta = 8), cdnow = cdnow)

# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - PNBD nocov - predict")
fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = pnbd(clv.cdnow),
                                                                clv.newdata = clv.cdnow)

fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = pnbd,
                                                                                 cdnow = cdnow,
                                                                                 clv.cdnow = clv.cdnow)

# Static cov: Data sorting ------------------------------------------------------------------------------
context("Correctness - PNBD static cov - Data sorting")

data("apparelTrans")
data("apparelStaticCov")

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                     estimation.split = 52))
# Standard cov data
expect_silent(clv.apparel.staticcov <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov))

expect_silent(p.static <- pnbd(clv.data=clv.apparel.staticcov, verbose=FALSE))

fct.testthat.correctness.staticcov.sorted.covariates(method = pnbd,
                                                     clv.apparel = clv.apparel,
                                                     apparelStaticCov = apparelStaticCov,
                                                     m.static = p.static)

# Static cov: predict ---------------------------------------------------------------------------------------
context("Correctness - PNBD static cov - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = p.static,
                                                                clv.newdata = clv.apparel.staticcov)

fct.testthat.correctness.staticcov.fitting.sample.predicting.full.data.equal(method = pnbd,
                                                                             apparelTrans = apparelTrans,
                                                                             apparelStaticCov = apparelStaticCov,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov)

fct.testthat.correctness.staticcov.regularization.lambda.0.no.regularization(method = pnbd,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov,
                                                                             m.static = p.static)
