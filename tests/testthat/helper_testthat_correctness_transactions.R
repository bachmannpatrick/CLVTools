fct.testhat.correctness.clvfittedtransactions.same.spending.as.independent.spending.model <- function(method, clv.data){
  test_that("Predict spending in transaction prediction same as independent spending model", {
    skip_on_cran()
    expect_silent(fitted <- do.call(method, list(clv.data = clv.data, verbose = FALSE)))
    # with prediction.end to also predict if no holdout
    expect_silent(dt.pred <- predict(fitted, prediction.end = 10, verbose = FALSE))
    expect_silent(dt.pred.gg <- predict(gg(clv.data, verbose = FALSE)))
    expect_equal(dt.pred.gg[order(Id), "predicted.mean.spending"], dt.pred[order(Id), "predicted.mean.spending"])
  })
}





fct.testthat.correctness.clvfittedtransactions.nocov.newdata.fitting.sample.predicting.full.data.equal <- function(method, cdnow, clv.cdnow){
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

fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.row.sorting <- function(method, clv.apparel, apparelStaticCov, m.fitted.static){
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

fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.column.sorting <- function(method, clv.apparel, apparelStaticCov, m.fitted.static){
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

fct.testthat.correctness.clvfittedtransactions.predict.actual.x <- function(method, data.cdnow){
  test_that("actual.x are counted correctly", {
    skip_on_cran()

    expect_silent(fitted <- do.call(method, list(clvdata(data.cdnow, estimation.split = "1998-01-01", date.format = "ymd", time.unit = "w"), verbose=FALSE)))
    expect_silent(dt.pred <- predict(fitted, verbose = FALSE))
    # some known customers
    expect_true(dt.pred[Id == "1", actual.x] == 0)
    expect_true(dt.pred[Id == "1000", actual.x] == 3)
    expect_true(dt.pred[Id == "1056", actual.x] == 3) # has a transaction on 1998-01-01 which may not be counted in actual.x!
  })
}


fct.testthat.correctness.clvfitted.newdata.same.predicting.fitting <- function(clv.fitted){
  test_that("Same when predicting as with fitting data", {
    skip_on_cran()
    expect_true(isTRUE(all.equal(predict(clv.fitted, verbose=FALSE),
                                 predict(clv.fitted, newdata = clv.fitted@clv.data, verbose=FALSE))))
  })
}

fct.testthat.correctness.clvfittedtransactions.CET.0.for.no.prediction.period <- function(clv.fitted){
  test_that("CET = 0 for no prediction period", {
    skip_on_cran()
    expect_silent(dt.pred <- predict(clv.fitted, verbose = FALSE, prediction.end = 0))
    expect_true(dt.pred[, isTRUE(all.equal(CET, rep(0, .N)))])
  })
}


fct.testthat.correctness.clvfittedtransactions.staticcov.fitting.sample.predicting.full.data.equal <- function(method, apparelTrans, apparelStaticCov, clv.apparel.staticcov){
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

fct.testthat.correctness.clvfittedtransactions.staticcov.regularization.lambda.0.no.regularization <- function(method, clv.apparel.staticcov, m.fitted.static){
  test_that("Regularization with 0 lambda has the same effect as no regularization", {
    skip_on_cran()
    l.args <- list(clv.data = clv.apparel.staticcov, reg.lambdas = c(trans=0, life=0), verbose = FALSE)
    expect_silent(p.0.reg <- do.call(what = method, args = l.args))

    expect_equal(coef(p.0.reg),          coef(m.fitted.static))
    expect_equal(coef(summary(p.0.reg)), coef(summary(m.fitted.static)))
  })
}


fct.testthat.correctness.clvfittedtransactions <- function(name.model, method, data.cdnow, data.apparelTrans, data.apparelStaticCov,
                                                           correct.start.params.model, correct.params.nocov.coef, correct.LL.nocov,
                                                           kkt2.true){


  # No cov ---------------------------------------------------------------------------------------------------
  expect_silent(clv.cdnow <- clvdata(data.transactions = data.cdnow,
                                     date.format = "ymd", time.unit = "W", estimation.split = 38,
                                     name.id = "Id", name.date = "Date", name.price = "Price"))
  expect_silent(obj.fitted <- do.call(method, list(clv.data = clv.cdnow, verbose = FALSE)))

  context(paste0("Correctness - ",name.model," nocov - Recover parameters"))
  fct.testthat.correctness.clvfitted.correct.coefs(method = method, cdnow = data.cdnow, start.params.model = correct.start.params.model,
                                                   params.nocov.coef = correct.params.nocov.coef, LL.nocov = correct.LL.nocov)

  context(paste0("Correctness - ",name.model," nocov - Example data"))
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.cdnow, kkt2.true = kkt2.true)


  context(paste0("Correctness - ",name.model," nocov - predict"))
  fct.testthat.correctness.clvfittedtransactions.predict.actual.x(method = method, data.cdnow = data.cdnow)
  fct.testthat.correctness.clvfitted.newdata.same.predicting.fitting(clv.fitted = obj.fitted)
  fct.testthat.correctness.clvfittedtransactions.CET.0.for.no.prediction.period(clv.fitted = obj.fitted)

  fct.testthat.correctness.clvfittedtransactions.nocov.newdata.fitting.sample.predicting.full.data.equal(method = method, cdnow = data.cdnow, clv.cdnow = clv.cdnow)
  fct.testhat.correctness.clvfittedtransactions.same.spending.as.independent.spending.model(method = method, clv.data = clv.cdnow)


  # Static cov ------------------------------------------------------------------------------------------------
  clv.apparel.staticcov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                       estimation.split = 52)
  clv.apparel.nocov <- as(clv.apparel.staticcov, "clv.data")

  expect_silent(obj.fitted.static <- do.call(method, list(clv.data=clv.apparel.staticcov, verbose=FALSE)))


  context(paste0("Correctness - ",name.model," static cov - Data sorting"))
  fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.row.sorting(method = method, clv.apparel = clv.apparel.nocov,
                                                                                 apparelStaticCov = data.apparelStaticCov, m.fitted.static = obj.fitted.static)
  fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.column.sorting(method = method, clv.apparel = clv.apparel.nocov,
                                                                                    apparelStaticCov = data.apparelStaticCov, m.fitted.static = obj.fitted.static)


  context(paste0("Correctness - ",name.model," static cov - Example data"))
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.nocov, kkt2.true = kkt2.true)
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.staticcov, kkt2.true = kkt2.true)


  context(paste0("Correctness - ",name.model," static cov - predict"))
  fct.testthat.correctness.clvfittedtransactions.CET.0.for.no.prediction.period(clv.fitted = obj.fitted.static)
  fct.testthat.correctness.clvfittedtransactions.staticcov.fitting.sample.predicting.full.data.equal(method = method, apparelTrans = data.apparelTrans,
                                                                                                     clv.apparel.staticcov = clv.apparel.staticcov,
                                                                                                     apparelStaticCov = data.apparelStaticCov)
  fct.testhat.correctness.clvfittedtransactions.same.spending.as.independent.spending.model(method = method, clv.data = clv.apparel.staticcov)


  context(paste0("Correctness - ",name.model," static cov - regularization"))
  fct.testthat.correctness.clvfittedtransactions.staticcov.regularization.lambda.0.no.regularization(method = method, clv.apparel.staticcov = clv.apparel.staticcov,
                                                                                                     m.fitted.static = obj.fitted.static)
}

fct.testthat.correctness.clvfittedtransactions.same.expectation.in.R.and.Cpp <- function(fct.expectation.R, params_i, obj.fitted, tolerance=testthat_tolerance()){

  # dt.expectation.seq has to be copied for calling expectation as otherwise the same table is overwritten!
  dt.expectation.seq <- clv.time.expectation.periods(clv.time = obj.fitted@clv.data@clv.time, user.tp.end = 38)

  expect_silent(result.R <- DoExpectation(dt.expectation.seq = copy(dt.expectation.seq), params_i = params_i,
                                          fct.expectation = fct.expectation.R, clv.time = obj.fitted@clv.data@clv.time))

  expect_silent(result.Rcpp.model <- clv.model.expectation(clv.model = obj.fitted@clv.model, clv.fitted = obj.fitted,
                                                           dt.expectation.seq = copy(dt.expectation.seq), verbose = FALSE))

  expect_equal(result.R, result.Rcpp.model, tolerance = tolerance)
}
