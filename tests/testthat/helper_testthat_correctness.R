fct.testthat.correctness.nocov.correct.se <- function(method, cdnow, start.params.model, params.nocov.se){
  test_that("Cdnow nocov correct SE", {
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))

    l.args <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = method, args = l.args))

    # From previous fit
    expect_equal(sqrt(diag(vcov(p.cdnow))), params.nocov.se, tolerance = 0.001)
  })
}

fct.testthat.correctness.nocov.correct.coefs <- function(method, cdnow, start.params.model, params.nocov.coef, LL.nocov){
  test_that("Cdnow nocov correct coefs", {
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))

    l.args <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = method, args = l.args))

    # From previous fit
    expect_equal(coef(p.cdnow), params.nocov.coef, tolerance = 0.001)
    expect_equal(as.numeric(logLik(p.cdnow)), LL.nocov, tolerance = 0.001)
  })
}



fct.testthat.correctness.nocov.same.as.btyd <- function(clvtools.method, btyd.method, btyd.dert.method, btyd.cet.method, btyd.palive.method, start.params.model, cdnow, DERT.not.implemented = FALSE){
  test_that("Same results as BTYD", {
    # Fitting
    # From ?BTYD::<model>.cbs.LL()
    data("cdnowSummary", package = "BTYD", envir = environment())
    expect_silent(cal.cbs <- cdnowSummary$cbs)
    expect_silent(startingparams <- unname(start.params.model))
    l.args.btyd <- list(cal.cbs, startingparams)
    expect_silent(est.params <- do.call(what = btyd.method, args = l.args.btyd))

    # CLVTools
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))
    l.args.clvtools <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = clvtools.method, args = l.args.clvtools))
    expect_equal(unname(coef(p.cdnow)), est.params, tolerance = 0.0001)


    # Predicting
    if(!DERT.not.implemented){
      l.args.btyd.dert <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"], d=0.15)
      expect_silent(btyd.dert <- do.call(what = btyd.dert.method, args = l.args.btyd.dert))
    }

    l.args.btyd.cet <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"], T.star = 10)
    expect_silent(btyd.cet  <- do.call(what = btyd.cet.method, args = l.args.btyd.cet))
    l.args.btyd.palive <- list(est.params, x=cal.cbs[,"x"], t.x = cal.cbs[,"t.x"], T.cal = cal.cbs[,"T.cal"])
    expect_silent(btyd.palive <- do.call(what = btyd.palive.method, args = l.args.btyd.palive))

    # CLVTools
    expect_silent(dt.pred <- predict(p.cdnow, prediction.end = 10, continuous.discount.factor = 0.15, verbose = FALSE))

    if(!DERT.not.implemented){
      expect_equivalent(btyd.dert[dt.pred$Id],   dt.pred$DERT, tolerance = 0.0001)
    }

    expect_equivalent(btyd.cet[dt.pred$Id],    dt.pred$CET, tolerance = 0.0001)
    expect_equivalent(btyd.palive[dt.pred$Id], dt.pred$PAlive, tolerance = 0.0001)

    # Expectation: Cannot compare 1vs1 from fitted() output
    # expect_silent(btyd.expect <- BTYD::<model>.Expectation(est.params, t=2))
  })
}

fct.testthat.correctness.nocov.compare.cbs.vs.btyd <- function(method, cdnow){
  # cdnow has to be fit exactly as in BTYD example
  test_that("CBS is the same as BTYD", { # are the same - PNBD vs. BGNBD vs. BTYD", {

    data(cdnowSummary, package = "BTYD", envir = environment())
    expect_silent(cal.cbs <- cdnowSummary$cbs)
    expect_silent(btyd.cbs <- as.data.table(cal.cbs))
    expect_silent(btyd.cbs <- btyd.cbs[order("x", "t.x", "T.cal"),c("x", "t.x", "T.cal")])

    expect_silent(clv.data <- clvdata(data.transactions = cdnow,
                                      date.format="ymd",
                                      time.unit = "week",
                                      estimation.split = "1997-09-30",
                                      name.id = "Id",
                                      name.date = "Date",
                                      name.price = "Price"))

    expect_silent(cbs.method  <- method(clv.data = clv.data, verbose = FALSE)@cbs[order("x", "t.x", "T.cal"),c("x", "t.x", "T.cal")])

    expect_equivalent(cbs.method, btyd.cbs)
  })
}


fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal <- function(method, cdnow, clv.cdnow){
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

fct.testthat.correctness.staticcov.covariate.row.sorting <- function(method, clv.apparel, apparelStaticCov, m.fitted.static){
  test_that("Same result for differently sorted covariate data (row)", {
    skip_on_cran()

    # shuffle
    expect_silent(apparelStaticCov.shuffle <- apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE), ])
    expect_silent(clv.apparel.shuffle <- SetStaticCovariates(clv.apparel,
                                                             names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                             data.cov.life = apparelStaticCov.shuffle, data.cov.trans = apparelStaticCov.shuffle))

    l.args <- list(clv.data=clv.apparel.shuffle, verbose=FALSE)
    expect_silent(m.static.shuffle <- do.call(what = method, args = l.args))


    # All should be exactly the same, except the call and optimx time
    #   replace these
    expect_silent(m.static.shuffle@call                           <- m.fitted.static@call)
    expect_silent(m.static.shuffle@optimx.estimation.output$xtime <- m.fitted.static@optimx.estimation.output$xtime)
    expect_true(isTRUE(all.equal(m.static.shuffle, m.fitted.static)))

    expect_equal(predict(m.static.shuffle), predict(m.fitted.static))
    expect_equal(plot(m.static.shuffle),    plot(m.fitted.static))
    expect_equal(summary(m.static.shuffle), summary(m.fitted.static))
  })
}

fct.testthat.correctness.staticcov.covariate.column.sorting <- function(method, clv.apparel, apparelStaticCov, m.fitted.static){
  test_that("Same result for differently sorted covariate data (columns)", {
    skip_on_cran()

    # Sort columns the opposite way
    expect_silent(apparelStaticCov.reverse <- apparelStaticCov[, .SD, .SDcols = rev(colnames(apparelStaticCov))])
    names.cov <- colnames(apparelStaticCov) # keep in same order as data (ie reversed)
    names.cov <- names.cov[names.cov != "Id"]
    expect_silent(clv.apparel.reverse <- SetStaticCovariates(clv.apparel,
                                                             names.cov.life = names.cov, names.cov.trans = names.cov,
                                                             data.cov.life = apparelStaticCov.reverse, data.cov.trans = apparelStaticCov.reverse))

    l.args <- list(clv.data=clv.apparel.reverse, verbose=FALSE)
    expect_silent(m.static.reverse <- do.call(what = method, args = l.args))


    # All should be exactly the same, except the call and optimx time
    #   replace these
    expect_silent(m.static.reverse@call                           <- m.fitted.static@call)
    expect_silent(m.static.reverse@optimx.estimation.output$xtime <- m.fitted.static@optimx.estimation.output$xtime)
    expect_true(isTRUE(all.equal(m.static.reverse, m.fitted.static)))

    expect_equal(predict(m.static.reverse), predict(m.fitted.static))
    expect_equal(plot(m.static.reverse),    plot(m.fitted.static))
    expect_equal(summary(m.static.reverse), summary(m.fitted.static))
  })
}


fct.testthat.correctness.common.newdata.same.predicting.fitting <- function(clv.fitted, clv.newdata){
  test_that("Same when predicting as with fitting data", {
    skip_on_cran()
    expect_true(isTRUE(all.equal(predict(clv.fitted, verbose=FALSE),
                                 predict(clv.fitted, newdata = clv.newdata, verbose=FALSE))))
  })
}

fct.testthat.correctness.CET.0.for.no.prediction.period <- function(clv.fitted){
  test_that("CET = 0 for no prediction period", {
    skip_on_cran()
    expect_silent(dt.pred <- predict(clv.fitted, verbose = FALSE, prediction.end = 0))
    expect_true(dt.pred[, isTRUE(all.equal(CET, rep(0, .N)))])
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
    expect_silent(m.sample <- do.call(what = method, args = l.args))

    # Predictions
    expect_silent(dt.predict.sample <- predict(m.sample,                                      verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))
    expect_silent(dt.predict.full   <- predict(m.sample, newdata = clv.apparel.static.sample, verbose=FALSE, predict.spending=FALSE,  prediction.end="2007-01-01"))

    # The sample ones should be the exact same ones in the full
    expect_true(isTRUE(all.equal(dt.predict.sample,
                                 dt.predict.full[Id %in% dt.predict.sample$Id])))

  })
}

fct.testthat.correctness.staticcov.regularization.lambda.0.no.regularization <- function(method, clv.apparel.staticcov, m.fitted.static){
  test_that("Regularization with 0 lambda has the same effect as no regularization", {
    skip_on_cran()
    l.args <- list(clv.data = clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE)
    expect_silent(p.0.reg <- do.call(what = method, args = l.args))

    expect_equal(coef(p.0.reg),          coef(m.fitted.static))
    expect_equal(coef(summary(p.0.reg)), coef(summary(m.fitted.static)))
  })
}


fct.testthat.correctness <- function(name.model, method, data.cdnow, data.apparelTrans, data.apparelStaticCov,
                                     correct.start.params.model, correct.params.nocov.coef, correct.LL.nocov){

  # Create necessary object ----------------------------------------------------------------
  # Nocov
  expect_silent(clv.cdnow <- clvdata(data.transactions = data.cdnow,
                                     date.format = "ymd", time.unit = "W", estimation.split = 38,
                                     name.id = "Id", name.date = "Date", name.price = "Price"))
  expect_silent(obj.fitted <- do.call(method, list(clv.data = clv.cdnow, verbose = FALSE)))


  # Static cov
  expect_silent(clv.apparel <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd", time.unit = "W", estimation.split = 52))
  expect_silent(clv.apparel.staticcov <- SetStaticCovariates(clv.apparel,
                                                             names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                             data.cov.life = data.apparelStaticCov, data.cov.trans = data.apparelStaticCov))
  expect_silent(obj.fitted.static <- do.call(method, list(clv.data=clv.apparel.staticcov, verbose=FALSE)))




  context(paste0("Correctness - ",name.model," nocov - CBS comparison"))
  fct.testthat.correctness.nocov.compare.cbs.vs.btyd(method = method, cdnow = data.cdnow)

  context(paste0("Correctness - ",name.model," nocov - Recover parameters"))
  fct.testthat.correctness.nocov.correct.coefs(method = method, cdnow = data.cdnow, start.params.model = correct.start.params.model,
                                               params.nocov.coef = correct.params.nocov.coef, LL.nocov = correct.LL.nocov)

  context(paste0("Correctness - ",name.model," nocov - predict"))
  fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = obj.fitted, clv.newdata = clv.cdnow)
  fct.testthat.correctness.CET.0.for.no.prediction.period(clv.fitted = obj.fitted)

  fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = method, cdnow = data.cdnow, clv.cdnow = clv.cdnow)

  context(paste0("Correctness - ",name.model," static cov - Data sorting"))
  fct.testthat.correctness.staticcov.covariate.row.sorting(method = method, clv.apparel = clv.apparel,
                                                           apparelStaticCov = data.apparelStaticCov, m.fitted.static = obj.fitted.static)
  fct.testthat.correctness.staticcov.covariate.column.sorting(method = method, clv.apparel = clv.apparel,
                                                              apparelStaticCov = data.apparelStaticCov, m.fitted.static = obj.fitted.static)

  context(paste0("Correctness - ",name.model," static cov - predict"))
  fct.testthat.correctness.CET.0.for.no.prediction.period(clv.fitted = obj.fitted.static)
  fct.testthat.correctness.staticcov.fitting.sample.predicting.full.data.equal(method = method, apparelTrans = data.apparelTrans,
                                                                               clv.apparel.staticcov = clv.apparel.staticcov,
                                                                               apparelStaticCov = data.apparelStaticCov)

  context(paste0("Correctness - ",name.model," static cov - regularization"))
  fct.testthat.correctness.staticcov.regularization.lambda.0.no.regularization(method = method, clv.apparel.staticcov = clv.apparel.staticcov,
                                                                               m.fitted.static = obj.fitted.static)
}
