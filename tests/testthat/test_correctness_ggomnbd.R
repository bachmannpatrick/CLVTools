# Load required data ---------------------------------------------------------------------------------
data("cdnow")

set.seed(1000)

context("Correctness - GGompertz/NBD nocov - Recover parameters")


expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                   date.format = "ymd",
                                   time.unit = "W",
                                   estimation.split = 38,
                                   name.id = "Id",
                                   name.date = "Date",
                                   name.price = "Price"))

# ** CROSS check with paper!
fct.testthat.correctness.nocov.correct.coefs(method = ggomnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                             params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                                             LL.nocov = -9377.94)

# No reliable data available for comparison. Paper information is not sufficient
#fct.testthat.correctness.nocov.correct.se(method = ggomnbd,
#                                          cdnow = cdnow,
#                                          start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
#                                          params.nocov.se = c(r = 0.049, alpha = 0.949, b = 0.0000, s = 0.039, beta = 0.0004))

# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - GGompertz/NBD nocov - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = ggomnbd(clv.data = clv.cdnow),
                                                                clv.newdata = clv.cdnow)

fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = ggomnbd,
                                                                                 cdnow = cdnow,
                                                                                 clv.cdnow = clv.cdnow)


# Static cov: Data sorting ------------------------------------------------------------------------------
context("Correctness - GGompertz/NBD static cov - Data sorting")

data("apparelTrans")
data("apparelStaticCov")

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                     estimation.split = 52))
# Standard cov data
expect_silent(clv.apparel.staticcov <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov))

expect_silent(g.static <- ggomnbd(clv.data=clv.apparel.staticcov, verbose=FALSE))

fct.testthat.correctness.staticcov.sorted.covariates(method = ggomnbd,
                                                     clv.apparel = clv.apparel,
                                                     apparelStaticCov = apparelStaticCov,
                                                     m.static = g.static)

# Static cov: predict ---------------------------------------------------------------------------------------

context("Correctness - GGompertz/NBD static cov - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = ggomnbd(clv.cdnow),
                                                                clv.newdata = clv.cdnow)

fct.testthat.correctness.staticcov.fitting.sample.predicting.full.data.equal(method = ggomnbd,
                                                                             apparelTrans = apparelTrans,
                                                                             apparelStaticCov = apparelStaticCov,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov)

fct.testthat.correctness.staticcov.regularization.lambda.0.no.regularization(method = ggomnbd,
                                                                             clv.apparel.staticcov = clv.apparel.staticcov,
                                                                             m.static = g.static)
