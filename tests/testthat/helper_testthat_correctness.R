fct.testthat.correctness.correct.coefs.se <- function(method, cdnow, start.params.model, params.nocov.coef, params.nocov.se)

# **TODO: Check vs PAPER!
test_that("Cdnow nocov correct coefs and SE", {
  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))

  l.args <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
  expect_silent(p.cdnow <- do.call(what = method, args = l.args))

  # cdnow.nocov.coef <- c(r = 0.5437, alpha = 10.3242,s = 0.7072, beta = 14.1526)
  # cdnow.nocov.se   <- c(r =0.0469,  alpha = 0.8281, s = 0.2511, beta = 8.1587)

  # From previous fit
  expect_equal(coef(p.cdnow), params.nocov.coef, tolerance = 0.001)
  expect_equal(sqrt(diag(vcov(p.cdnow))), params.nocov.se, tolerance = 0.001)
})

fct.testthat.correctness.same.as.btyd <- function(clvtools.method, btyd.method, btyd.dert.method, btyd.cet.method, btyd.palive.method, start.params.model, cdnow){
  test_that("Same results as BTYD", {
    # Fitting
    # From ?BTYD::<model>.cbs.LL()
    data(cdnowSummary, package = "BTYD")
    expect_silent(cal.cbs <- cdnowSummary$cbs)
    expect_silent(startingparams <- unname(start.params.model))
    l.args.btyd <- list(cal.cbs, startingparams)
    expect_silent(est.params <- do.call(what = btyd.method, args = l.args.btyd))

    # CLVTools
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))
    l.args.clvtools <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = clvtools.method, args = l.args.clvtools))
    expect_equal(unname(coef(p.cdnow)), est.params)


    # Predicting
    l.args.btyd.dert <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"], d=0.15)
    expect_silent(btyd.dert <- do.call(what = btyd.dert.method, args = l.args.btyd.dert))
    l.args.btyd.cet <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"], T.star = 10)
    expect_silent(btyd.cet  <- do.call(what = btyd.cet.method, args = l.args.btyd.cet))
    l.args.btyd.palive <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"])
    expect_silent(btyd.palive <- do.call(what = btyd.palive.method, args = l.args.btyd.palive))

    # CLVTools
    expect_silent(dt.pred <- predict(p.cdnow, prediction.end = 10, continuous.discount.factor = 0.15, verbose = FALSE))

    expect_equivalent(btyd.dert[dt.pred$Id],   dt.pred$DERT)
    expect_equivalent(btyd.cet[dt.pred$Id],    dt.pred$CET)
    expect_equivalent(btyd.palive[dt.pred$Id], dt.pred$PAlive)

    # Expectation: Cannot compare 1vs1 from fitted() output
    # expect_silent(btyd.expect <- BTYD::<model>.Expectation(est.params, t=2))
  })
}

fct.testthat.correctness.same.predicting.fitting <- function(method, clv.cdnow){
  test_that("Same when predicting as with fitting data", {
    skip_on_cran()
    l.args <- list(clv.data = clv.cdnow, verbose = FALSE)
    expect_silent(cdnow.fit <- do.call(what = method, args = l.args))

    expect_true(isTRUE(all.equal(predict(cdnow.fit),
                                 predict(cdnow.fit, newdata = clv.cdnow))))
  })
}

fct.testthat.correctness.fitting.sample.predicting.full.data.equal <- function(method, cdnow, clv.cdnow){
  test_that("Fitting sample but predicting with full data yields same results as predicting sample only", {
    skip_on_cran()
    # Sample only
    cdnow.id.sample <- unique(cdnow$Id)[1:100]
    cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
    expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                              time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))

    l.args <- list(clv.data = clv.cdnow.sample, verbose = FALSE)
    # Fit on sample only
    expect_silent(fitted.nocov.sample <- do.call(what = method, args = l.args))

    # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
    stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)
    # Prediction.end
    # Predict sample only
    expect_silent(dt.predict.sample <- predict(fitted.nocov.sample, verbose=FALSE,
                                               predict.spending=FALSE, prediction.end="1998-07-06"))
    # Predict on full
    expect_silent(dt.predict.full <- predict(fitted.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))

    expect_true(nrow(dt.predict.sample) == 100)
    expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

    # The sample ones should be the exact same ones in the full
    expect_true(isTRUE(all.equal(dt.predict.sample,
                                 dt.predict.full[Id %in% dt.predict.sample$Id])))

  })
}

fct.testthat.correctness.sorted.covariates <- function(method, clv.apparel, apparelStaticCov, p.static = p.static){
  test_that("Same result for differently sorted covariates", {
    skip_on_cran()

    # shuffle
    expect_silent(apparelStaticCov.shuffle <- apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE), ])
    expect_silent(clv.apparel.shuffle <- SetStaticCovariates(clv.apparel,
                                                             names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                             data.cov.life = apparelStaticCov.shuffle, data.cov.trans = apparelStaticCov.shuffle))

    l.args <- list(clv.data=clv.apparel.shuffle, verbose=FALSE)
    expect_silent(p.static.shuffle <- do.call(what = method, args = l.args))


    # All should be exactly the same, except the call and optimx time
    #   replace these
    expect_silent(p.static.shuffle@call                           <- p.static@call)
    expect_silent(p.static.shuffle@optimx.estimation.output$xtime <- p.static@optimx.estimation.output$xtime)
    expect_true(isTRUE(all.equal(p.static.shuffle, p.static)))
  })
}
fct.testthat.correctness.staticcov.predicting.fitting <- function(p.static, clv.apparel.staticcov){
  test_that("Same when predicting as with fitting data", {
    skip_on_cran()
    expect_true(isTRUE(all.equal(predict(p.static, verbose=FALSE),
                                 predict(p.static, newdata = clv.apparel.staticcov, verbose=FALSE))))
  })
}

fct.testthat.correctness.staticcov.fitting.sample.predicting.full.data.equal <- function(method, apparelTrans, apparelStaticCov, clv.apparel.staticcov){
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
    l.args <- list(clv.data = clv.apparel.static.sample, verbose = FALSE)
    expect_silent(p.sample <- do.call(what = method, args = l.args))

    # Predictions
    expect_silent(dt.predict.sample <- predict(p.sample,                                      verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))
    expect_silent(dt.predict.full   <- predict(p.sample, newdata = clv.apparel.static.sample, verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))

    # The sample ones should be the exact same ones in the full
    expect_true(isTRUE(all.equal(dt.predict.sample,
                                 dt.predict.full[Id %in% dt.predict.sample$Id])))

  })
}

fct.testthat.correctness.regularization.lambda.0.no.regularization <- function(method, clv.apparel.staticcov, p.static){
  test_that("Regularization with 0 lambda has the same effect as no regularization", {
    skip_on_cran()
    l.args <- list(clv.data = clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE)
    expect_silent(p.0.reg <- do.call(what = method, args = l.args))

    expect_equal(coef(p.0.reg),          coef(p.static))
    expect_equal(coef(summary(p.0.reg)), coef(summary(p.static)))
  })
}
