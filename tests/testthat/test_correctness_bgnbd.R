# Load required data ---------------------------------------------------------------------------------
data("cdnow")

context("Correctness - BGNBD nocov - CBS comparison")
fct.testthat.correctness.nocov.compare.cbs(cdnow = cdnow)

context("Correctness - BGNBD nocov - Recover parameters")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                   date.format = "ymd",
                                   time.unit = "Week",
                                   estimation.split = "1997-09-30",
                                   name.id = "Id",
                                   name.date = "Date",
                                   name.price = "Price"))

# Our estimates
fct.testthat.correctness.nocov.correct.coefs(method = bgnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                             params.nocov.coef = c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881),
                                             LL.nocov = -9582.429)

# As reported in Fader, Hardie, Lee (2005)
fct.testthat.correctness.nocov.correct.coefs(method = bgnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r=1, alpha = 3, a = 1, b = 3),
                                             params.nocov.coef = c(r = 0.243, alpha = 4.414, a = 0.793, b = 2.426),
                                             LL.nocov = -9582.4)


fct.testthat.correctness.nocov.same.as.btyd(clvtools.method = bgnbd,
                                            btyd.method = BTYD::bgnbd.EstimateParameters,
                                            btyd.dert.method = NULL,
                                            btyd.cet.method = BTYD::bgnbd.ConditionalExpectedTransactions,
                                            btyd.palive.method = BTYD::bgnbd.PAlive,
                                            start.params.model = c(r = 1, alpha = 3, a = 1, b = 3),
                                            cdnow = cdnow,
                                            DERT.not.implemented = TRUE)

fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = bgnbd,
                                                                                 cdnow = cdnow,
                                                                                 clv.cdnow = clv.cdnow)

# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - BGNBD nocov - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = bgnbd(clv.cdnow),
                                                                clv.newdata = clv.cdnow)

fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = bgnbd,
                                                                                 cdnow = cdnow,
                                                                                 clv.cdnow = clv.cdnow)


# Static cov: Data sorting ------------------------------------------------------------------------------
context("Correctness - BGNBD static cov - Data sorting")

data("apparelTrans")
data("apparelStaticCov")

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                     estimation.split = 52))
# Standard cov data
expect_silent(clv.apparel.staticcov <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov))

expect_silent(b.static <- bgnbd(clv.data=clv.apparel.staticcov, verbose=FALSE))

fct.testthat.correctness.staticcov.sorted.covariates(method = bgnbd,
                                                     clv.apparel = clv.apparel,
                                                     apparelStaticCov = apparelStaticCov,
                                                     m.static = b.static)

# Static cov: predict ---------------------------------------------------------------------------------------
context("Correctness - BGNBD static cov - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = bgnbd(clv.cdnow),
                                                                clv.newdata = clv.cdnow)

fct.testthat.correctness.staticcov.fitting.sample.predicting.full.data.equal(method = bgnbd,
                                                                             apparelTrans = apparelTrans,
                                                                             apparelStaticCov = apparelStaticCov,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov)

fct.testthat.correctness.staticcov.regularization.lambda.0.no.regularization(method = bgnbd,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov,
                                                                             m.static = b.static)
