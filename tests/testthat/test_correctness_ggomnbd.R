# Load required data ---------------------------------------------------------------------------------
data("cdnow")

context("Correctness - GGompertz / NBD nocov - Recover parameters")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                   date.format = "ymd",
                                   time.unit = "Week",
                                   estimation.split = "1997-09-30",
                                   name.id = "Id",
                                   name.date = "Date",
                                   name.price = "Price"))

test_that("cdnow nocov correct coefs and SE", {
  expect_silent(e.ggomnbd.cdnow.nocov<-ggomnbd(clv.data=clv.cdnow, start.params.model = c(r = 1, alpha = 1, beta = 1, b = 1, s = 1), verbose = FALSE))
#TODO define  expect_equal(coef(e.ggomnbd.cdnow.nocov), c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881))
})

# Is used at various places
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                   estimation.split = 38))

context("Correctness - GGompertz / NBD nocov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  expect_silent(ggomnbd.cdnow.fit <- ggomnbd(clv.cdnow, verbose = FALSE))
  expect_true(isTRUE(all.equal(predict(ggomnbd.cdnow.fit),
                               predict(ggomnbd.cdnow.fit, newdata = clv.cdnow))))
})

test_that("fitting sample and predicting full data yields same results as predicting sample only", {
  skip_on_cran()
  # Sample only
  cdnow.id.sample <- unique(cdnow$Id)[1:100]
  cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
  expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                            time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))
  # Fit on sample only
  expect_silent(fitted.ggomnbd.nocov.sample <- ggomnbd(clv.cdnow.sample, verbose = FALSE))

  # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
  stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)
  # Prediction.end
  # Predict sample only
  expect_silent(dt.predict.sample <- predict(fitted.ggomnbd.nocov.sample, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))
  # Predict on full
  expect_silent(dt.predict.full <- predict(fitted.ggomnbd.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))

  expect_true(nrow(dt.predict.sample) == 100)
  expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


# Static cov: Data sorting ------------------------------------------------------------------------------
context("Correctness - GGompertz / NBD static cov - Data sorting")

data("apparelTrans")
data("apparelStaticCov")

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                     estimation.split = 52))
# Standard cov data
expect_silent(clv.apparel.staticcov <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov))

expect_silent(p.static <- ggomnbd(clv.data=clv.apparel.staticcov, verbose=FALSE))


test_that("Same result for differently sorted covariates", {
  skip_on_cran()



  # shuffle
  expect_silent(apparelStaticCov.shuffle <- apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE), ])
  expect_silent(clv.apparel.shuffle <- SetStaticCovariates(clv.apparel,
                                                           names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                           data.cov.life = apparelStaticCov.shuffle, data.cov.trans = apparelStaticCov.shuffle))
  expect_silent(p.static.shuffle <- ggomnbd(clv.data=clv.apparel.shuffle, verbose=FALSE))


  # All should be exactly the same, except the call and optimx time
  #   replace these
  expect_silent(p.static.shuffle@call                           <- p.static@call)
  expect_silent(p.static.shuffle@optimx.estimation.output$xtime <- p.static@optimx.estimation.output$xtime)
  expect_true(isTRUE(all.equal(p.static.shuffle, p.static)))
})

# Static cov: predict ---------------------------------------------------------------------------------------
context("Correctness - GGompertz / NBD static cov - predict")

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
  expect_silent(p.sample <- ggomnbd(clv.apparel.static.sample, verbose = FALSE))

  # Predictions
  expect_silent(dt.predict.sample <- predict(p.sample,                                      verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))
  expect_silent(dt.predict.full   <- predict(p.sample, newdata = clv.apparel.static.sample, verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})


test_that("Regularization with 0 lambda has the same effect as no regularization", {
  skip_on_cran()
  expect_silent(p.0.reg <- ggomnbd(clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE))

  expect_equal(coef(p.0.reg),          coef(p.static))
  expect_equal(coef(summary(p.0.reg)), coef(summary(p.static)))
})
