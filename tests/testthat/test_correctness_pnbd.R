# Load required data ---------------------------------------------------------------------------------
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

apparel.nocov.coef <- c(r= 0.705, alpha = 14.402, s = 0.182, beta = 2.901)
apparel.nocov.se <- c(r= 0.064, alpha = 1.087, s = 0.026, beta = 2.901)
apparel.staticcov.coef <- c(r = 0.721, alpha = 22.410,s = 0.195,beta = 7.177,
                            life.Gender = 0.934, trans.Gender = 0.552)
apparel.staticcov.se <- c(r =0.066, alpha =1.709,s = 0.029,beta = 3.174,
                          life.Gender = 0.159,trans.Gender = 0.040)
cdnow.nocov.coef <- c(r=0.5437, alpha = 10.3242,s = 0.7072, beta = 14.1526)
cdnow.nocov.se <- c(r =0.0469, alpha = 0.8281, s = 0.2511, beta = 8.1587)


context("Correctness - PNBD nocov - Recover parameters")
expect_silent(pnbd.apparel.obj <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                          estimation.split = 52))
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                                  estimation.split = 38))


test_that("Apparel nocov correct coefs and SE", {
  expect_silent(e.pnbd.apparel.nocov<-pnbd(clv.data=pnbd.apparel.obj, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), verbose=FALSE))

  # expect_equal(coef(e.pnbd.apparel.nocov), apparel.nocov.coef)
  # **TODO: check SE
  # expect_equal(sqrt(diag(vcov(e.pnbd.apparel.nocov))), apparel.nocov.se)
})

test_that("Cdnow nocov correct coefs and SE", {
  expect_silent(e.pnbd.cdnow.nocov<-pnbd(clv.data=clv.cdnow, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), verbose=FALSE))

  # expect_equal(coef(e.pnbd.cdnow.nocov), cdnow.nocov.coef)
  # **TODO: check SE
  # expect_equal(sqrt(diag(vcov(e.pnbd.cdnow.nocov))), cdnow.nocov.se)
})


context("Correctness - PNBD nocov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  expect_silent(pnbd.cdnow.fit <- pnbd(clv.cdnow, verbose = FALSE))
  expect_true(isTRUE(all.equal(predict(pnbd.cdnow.fit),
                               predict(pnbd.cdnow.fit, newdata = clv.cdnow))))
})

test_that("fitting sample and predicting full data yields same results as predicting sample only", {
  skip_on_cran()
  # Sample only
  cdnow.id.sample <- unique(cdnow$Id)[1:100]
  cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
  expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                            time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))
  # Fit on sample only
  expect_silent(fitted.pnbd.nocov.sample <- pnbd(clv.cdnow.sample, verbose = FALSE))

  # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
  stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)
  # Prediction.end
  # Predict sample only
  expect_silent(dt.predict.sample <- predict(fitted.pnbd.nocov.sample, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))
  # Predict on full
  expect_silent(dt.predict.full <- predict(fitted.pnbd.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))

  expect_true(nrow(dt.predict.sample) == 100)
  expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})



context("Correctness - PNBD static cov - Recover parameters")
test_that("Apparel static cov correct coefs and SE", {
  expect_silent(pnbd.apparel.staticcov <- SetStaticCovariates(pnbd.apparel.obj,
                                                              names.cov.life = "Gender", names.cov.trans = "Gender",name.id = "Id",
                                                              data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov))
  expect_silent(e.pnbd.apparel.staticcov<-pnbd(clv.data=pnbd.apparel.staticcov, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                                               start.params.life = c(Gender=1), start.params.trans = c(Gender=1),
                                               verbose=FALSE))

  # expect_equal(coef(e.pnbd.apparel.staticcov), apparel.staticcov.coef, tolera)
  # **TODO: check SE
  # expect_equal(sqrt(diag(vcov(e.pnbd.apparel.staticcov))), apparel.staticcov.se)
})


context("Correctness - PNBD static cov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  dt.cov.static <- data.table(Id = unique(cdnow$Id),
                              Gender = c("F", rep(c("M", "F"), length(unique(cdnow$Id))/2)))
  clv.cdnow.static <- SetStaticCovariates(clv.cdnow, data.cov.life = dt.cov.static, data.cov.trans = dt.cov.static,
                                          names.cov.life = "Gender", names.cov.trans = "Gender")
  expect_silent(pnbd.static.cdnow.fit <- pnbd(clv.cdnow.static, verbose = FALSE))
  expect_true(isTRUE(all.equal(predict(pnbd.static.cdnow.fit),
                               predict(pnbd.static.cdnow.fit, newdata = clv.cdnow.static))))
})


test_that("fitting sample and predicting full data yields same results as predicting sample only", {
  skip_on_cran()

  # Sample only
  cdnow.id.sample <- unique(cdnow$Id)[1:100]
  cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
  expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                            time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))
  # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
  stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)

  # Add static cov
  dt.cov.static <- data.table(Id = unique(cdnow$Id),
                              Gender = c("F", rep(c("M", "F"), length(unique(cdnow$Id))/2)))
  clv.cdnow.static.full <- SetStaticCovariates(clv.cdnow, data.cov.life = dt.cov.static, data.cov.trans = dt.cov.static,
                                          names.cov.life = "Gender", names.cov.trans = "Gender")
  clv.cdnow.static.sample <- SetStaticCovariates(clv.cdnow.sample,
                                                 data.cov.life = dt.cov.static[Id %in% cdnow.id.sample], data.cov.trans = dt.cov.static[Id%in%cdnow.id.sample],
                                               names.cov.life = "Gender", names.cov.trans = "Gender")

  # Fit on sample only
  expect_silent(fitted.pnbd.static.sample <- pnbd(clv.cdnow.static.sample, verbose = FALSE))

  # Prediction.end
  # Predict sample only
  expect_silent(dt.predict.sample <- predict(fitted.pnbd.static.sample, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))
  # Predict on full
  expect_silent(dt.predict.full <- predict(fitted.pnbd.static.sample, newdata = clv.cdnow.static.full, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))

  expect_true(nrow(dt.predict.sample) == 100)
  expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


# test_that("fitting sample and predicting full data yields same results as predicting sample only", {})

# **TODO: get static cov object
# test_that("Regularization with 0 lambda should have same effect as no regularization", {
#   skip_on_cran()
#   expect_silent(e.pnbd.no.reg <- pnbd(pnbd.static.cov.obj))
#   expect_silent(e.pnbd.0.reg  <- pnbd(pnbd.static.cov.obj, reg.lambdas = c(trans=0, life=0)))
#   expect_equal(coef(e.pnbd.0.reg), coef(e.pnbd.no.reg))
#   expect_equal(coef(summary(e.pnbd.0.reg)), coef(summary(e.pnbd.no.reg)))
# })

# **TODO: compare coefs
# test_that("Faster regularization = smaller coefs one than other", {
#   skip_on_cran()
#   expect_message(res.l.100.10 <- pnbd(pnbd.static.cov.obj,reg.lambdas = c(trans=100, life=10)))
#   expect_message(res.l.10.100 <- pnbd(pnbd.static.cov.obj,reg.lambdas = c(trans=10,  life=100)))
#   # expect_true(coef() < coef())
#
#   expect_message(res.l.0.100 <- pnbd(pnbd.static.cov.obj,reg.lambdas = c(trans=0,  life=100)))
#   expect_message(res.l.100.0 <- pnbd(pnbd.static.cov.obj,reg.lambdas = c(trans=100,  life=0)))
#   # expect_true(coef() < coef())
# })



# Same results with differently sorted transaction data" is part of correctness_predict


