# Load required data ---------------------------------------------------------------------------------
data("cdnow")

# Is used at various places
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                   estimation.split = 38))

context("Correctness - BGNBD nocov - Recover parameters")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                   date.format = "ymd",
                                   time.unit = "Week",
                                   estimation.split = "1997-09-30",
                                   name.id = "Id",
                                   name.date = "Date",
                                   name.price = "Price"))

test_that("cdnow nocov correct coefs and SE", {
  expect_silent(e.bgnbd.cdnow.nocov<-bgnbd(clv.data=clv.cdnow, start.params.model = c(r=1, alpha = 3, a = 1, b = 3), verbose=FALSE))
  expect_equal(coef(e.bgnbd.cdnow.nocov), c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881))
})

test_that("Same results as BTYD", {
  # Fitting
  # From ?BTYD::bgnbd.cbs.LL()
  data(cdnowSummary, package = "BTYD")
  expect_silent(cal.cbs <- cdnowSummary$cbs)
  expect_silent(startingparams <- c(1, 3, 1, 3))
  expect_silent(est.params <- BTYD::bgnbd.EstimateParameters(cal.cbs, startingparams))

  # CLVTools
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))
  expect_silent(p.cdnow <- bgnbd(clv.data=clv.cdnow, start.params.model = c(r=1, alpha = 3, a = 1, b = 3), verbose=FALSE))
  expect_equal(unname(coef(p.cdnow)), est.params, tolerance = 0.0001)


  # Predicting
  expect_silent(btyd.cet  <- BTYD::bgnbd.ConditionalExpectedTransactions(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"],
                                                                        T.star = 10))
  expect_silent(btyd.palive <- BTYD::bgnbd.PAlive(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"]))

  # CLVTools
  expect_silent(dt.pred <- predict(p.cdnow, prediction.end = 10, continuous.discount.factor = 0.15, verbose = FALSE))

  expect_equivalent(btyd.cet[dt.pred$Id],    dt.pred$CET, tolerance = 0.0001)
  expect_equivalent(btyd.palive[dt.pred$Id], dt.pred$PAlive, tolerance = 0.0001)
})



context("Correctness - BGNBD nocov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  expect_silent(bgnbd.cdnow.fit <- bgnbd(clv.cdnow, verbose = FALSE))
  expect_true(isTRUE(all.equal(predict(bgnbd.cdnow.fit),
                               predict(bgnbd.cdnow.fit, newdata = clv.cdnow))))
})

test_that("fitting sample and predicting full data yields same results as predicting sample only", {
  skip_on_cran()
  # Sample only
  cdnow.id.sample <- unique(cdnow$Id)[1:100]
  cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
  expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                            time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))
  # Fit on sample only
  expect_silent(fitted.bgnbd.nocov.sample <- bgnbd(clv.cdnow.sample, verbose = FALSE))

  # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
  stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)
  # Prediction.end
  # Predict sample only
  expect_silent(dt.predict.sample <- predict(fitted.bgnbd.nocov.sample, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))
  # Predict on full
  expect_silent(dt.predict.full <- predict(fitted.bgnbd.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))

  expect_true(nrow(dt.predict.sample) == 100)
  expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


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

expect_silent(p.static <- bgnbd(clv.data=clv.apparel.staticcov, verbose=FALSE))


test_that("Same result for differently sorted covariates", {
  skip_on_cran()



  # shuffle
  expect_silent(apparelStaticCov.shuffle <- apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE), ])
  expect_silent(clv.apparel.shuffle <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov.shuffle, data.cov.trans = apparelStaticCov.shuffle))
  expect_silent(p.static.shuffle <- bgnbd(clv.data=clv.apparel.shuffle, verbose=FALSE))


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
  expect_silent(p.sample <- bgnbd(clv.apparel.static.sample, verbose = FALSE))

  # Predictions
  expect_silent(dt.predict.sample <- predict(p.sample,                                      verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))
  expect_silent(dt.predict.full   <- predict(p.sample, newdata = clv.apparel.static.sample, verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


test_that("Regularization with 0 lambda has the same effect as no regularization", {
  skip_on_cran()
  expect_silent(p.0.reg <- bgnbd(clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE))

  expect_equal(coef(p.0.reg),          coef(p.static))
  expect_equal(coef(summary(p.0.reg)), coef(summary(p.static)))
})
