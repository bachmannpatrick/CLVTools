# Load required data ---------------------------------------------------------------------------------
data("cdnow")



# Recover parameters ---------------------------------------------------------------------------------
context("Correctness - PNBD nocov - Recover parameters")

# Is used at various places
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                   estimation.split = 38))

# **TODO: Check vs PAPER!
test_that("Cdnow nocov correct coefs and SE", {
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))
  expect_silent(p.cdnow <- pnbd(clv.data=clv.cdnow, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), verbose=FALSE))


  # cdnow.nocov.coef <- c(r = 0.5437, alpha = 10.3242,s = 0.7072, beta = 14.1526)
  # cdnow.nocov.se   <- c(r =0.0469,  alpha = 0.8281, s = 0.2511, beta = 8.1587)

  # From previous fit
  cdnow.nocov.coef <- c(r=0.55315,   alpha=10.57633,  s=0.60625,   beta=11.67150)
  cdnow.nocov.se   <- c(r=0.0476264, alpha=0.8427222, s=0.1872594, beta=6.2105448)

  expect_equal(coef(p.cdnow), cdnow.nocov.coef, tolerance = 0.001)
  expect_equal(sqrt(diag(vcov(p.cdnow))), cdnow.nocov.se, tolerance = 0.001)

})


test_that("Same results as BTYD", {
  # Fitting
  # From ?BTYD::pnbd.cbs.LL()
  data(cdnowSummary, package = "BTYD")
  expect_silent(cal.cbs <- cdnowSummary$cbs)
  expect_silent(startingparams <- c(0.5, 6, 0.9, 8))
  expect_silent(est.params <- BTYD::pnbd.EstimateParameters(cal.cbs, startingparams))

  # CLVTools
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))
  expect_silent(p.cdnow <- pnbd(clv.data=clv.cdnow, start.params.model = c(r=0.5, alpha = 6, s = 0.9, beta = 8), verbose=FALSE))
  expect_equal(unname(coef(p.cdnow)), est.params)


  # Predicting
  expect_silent(btyd.dert <- BTYD::pnbd.DERT(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"], d=0.15))
  expect_silent(btyd.cet  <- BTYD::pnbd.ConditionalExpectedTransactions(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"],
                                                          T.star = 10))
  expect_silent(btyd.palive <- BTYD::pnbd.PAlive(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"]))

  # CLVTools
  expect_silent(dt.pred <- predict(p.cdnow, prediction.end = 10, continuous.discount.factor = 0.15, verbose = FALSE))

  expect_equivalent(btyd.dert[dt.pred$Id],   dt.pred$DERT)
  expect_equivalent(btyd.cet[dt.pred$Id],    dt.pred$CET)
  expect_equivalent(btyd.palive[dt.pred$Id], dt.pred$PAlive)

  # Expectation: Cannot compare 1vs1 from fitted() output
  # expect_silent(btyd.expect <- BTYD::pnbd.Expectation(est.params, t=2))
})


# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - PNBD nocov - predict")

test_that("Same when predicting as with fitting data", {
  expect_silent(pnbd.cdnow.fit <- pnbd(clv.cdnow, verbose = FALSE))

  expect_true(isTRUE(all.equal(predict(pnbd.cdnow.fit),
                               predict(pnbd.cdnow.fit, newdata = clv.cdnow))))
})

test_that("Fitting sample but predicting with full data yields same results as predicting sample only", {
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

# context("Correctness - PNBD static cov - Recover parameters")



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


test_that("Same result for differently sorted covariates", {
  skip_on_cran()



  # shuffle
  expect_silent(apparelStaticCov.shuffle <- apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE), ])
  expect_silent(clv.apparel.shuffle <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov.shuffle, data.cov.trans = apparelStaticCov.shuffle))
  expect_silent(p.static.shuffle <- pnbd(clv.data=clv.apparel.shuffle, verbose=FALSE))


  # All should be exactly the same, except the call and optimx time
  #   replace these
  expect_silent(p.static.shuffle@call                           <- p.static@call)
  expect_silent(p.static.shuffle@optimx.estimation.output$xtime <- p.static@optimx.estimation.output$xtime)
  expect_true(isTRUE(all.equal(p.static.shuffle, p.static)))
})


# Static cov: predict ---------------------------------------------------------------------------------------
context("Correctness - PNBD static cov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  expect_true(isTRUE(all.equal(predict(p.static, verbose=FALSE),
                               predict(p.static, newdata = clv.apparel.staticcov, verbose=FALSE))))
})


test_that("Fitting with sample but predicting full data yields same results as predicting sample only", {
  skip_on_cran()

  # Sample only
  apparel.id.sample    <- unique(apparelTrans$Id)[1:100]
  apparelTrans.sample  <- apparelTrans[Id %in% apparel.id.sample]
  expect_silent(clv.apparel.sample <- clvdata(data.transactions = apparelTrans.sample, date.format = "ymd",
                                              time.unit = "w", estimation.split = clv.apparel.staticcov@clv.time@timepoint.estimation.end))
  clv.apparel.static.sample <- SetStaticCovariates(clv.apparel.sample,
                                                   data.cov.life = apparelStaticCov[Id %in% apparel.id.sample], data.cov.trans = apparelStaticCov[Id%in%apparel.id.sample],
                                                   names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"))

  # Fit on sample only
  expect_silent(p.sample <- pnbd(clv.apparel.static.sample, verbose = FALSE))

  # Predictions
  expect_silent(dt.predict.sample <- predict(p.sample,                                      verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))
  expect_silent(dt.predict.full   <- predict(p.sample, newdata = clv.apparel.static.sample, verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


test_that("Regularization with 0 lambda has the same effect as no regularization", {
  skip_on_cran()
  expect_silent(p.0.reg <- pnbd(clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE))

  expect_equal(coef(p.0.reg),          coef(p.static))
  expect_equal(coef(summary(p.0.reg)), coef(summary(p.static)))
})


# test_that("Faster regularization = smaller coefs one than other", {
#   skip_on_cran()
#   expect_message(res.l.100.10 <- pnbd(clv.apparel,reg.lambdas = c(trans=100, life=10)))
#   expect_message(res.l.10.100 <- pnbd(clv.apparel,reg.lambdas = c(trans=10,  life=100)))
#   # expect_true(coef() < coef())
#
#   expect_message(res.l.0.100 <- pnbd(clv.apparel,reg.lambdas = c(trans=0,  life=100)))
#   expect_message(res.l.100.0 <- pnbd(clv.apparel,reg.lambdas = c(trans=100,  life=0)))
#   # expect_true(coef() < coef())
# })



# Same results with differently sorted transaction data" is part of correctness_predict


