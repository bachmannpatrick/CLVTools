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





fct.testthat.correctness.clvfittedtransactions.nocov.newdata.fitting.sample.predicting.full.data.equal <- function(method, clv.cdnow){
  test_that("Fitting sample but predicting with full data yields same results as predicting sample only", {
    skip_on_cran()
    cdnow <- fct.helper.load.cdnow()

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

fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.row.sorting <- function(method, m.fitted.static){
  test_that("Same result for differently sorted covariate data (row)", {
    skip_on_cran()

    apparelStaticCov <- fct.helper.load.apparelStaticCov()

    # shuffled
    clv.apparel.shuffle <- fct.helper.create.clvdata.apparel.staticcov(estimation.split=m.fitted.static@clv.data@clv.time@timepoint.estimation.end,
                                                                       data.apparelStaticCov=apparelStaticCov[sample.int(n = nrow(apparelStaticCov), replace = FALSE),])

    expect_silent(m.static.shuffle <- do.call(what = method, args = list(clv.data=clv.apparel.shuffle, verbose=FALSE)))


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

fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.column.sorting <- function(method, m.fitted.static){
  test_that("Same result for differently sorted covariate data (columns)", {
    skip_on_cran()
    data.apparelStaticCov <- fct.helper.load.apparelStaticCov()

    # Sort columns the opposite way
    clv.apparel.reverse <- fct.helper.create.clvdata.apparel.staticcov(data.apparelStaticCov=data.apparelStaticCov[, .SD, .SDcols = rev(colnames(data.apparelStaticCov))],
                                                                       estimation.split=m.fitted.static@clv.data@clv.time@timepoint.estimation.end)

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

fct.testthat.correctness.clvfittedtransactions.predict.actual.x <- function(method){
  test_that("actual.x are counted correctly", {
    skip_on_cran()

    fitted <- fit.cdnow(model = method, estimation.split = "1998-01-01")
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


fct.testthat.correctness.clvfittedtransactions.pmf.more.x.more.p <- function(clv.fitted){
  test_that("PMF - more x = more p",{
    skip_on_cran()
    expect_silent(dt.pmf <- pmf(clv.fitted, x=0:5))
    expect_silent(dt.pmf.more <- pmf(clv.fitted, x=0:6))
    expect_true(all(rowSums(dt.pmf.more[, !"Id"]) > rowSums(dt.pmf[, !"Id"])))
  })
}

fct.testthat.correctness.clvfittedtransactions.pmf.valid.values <- function(clv.fitted){
  test_that("PMF - only valid values",{
    skip_on_cran()
    expect_silent(dt.pmf <- pmf(clv.fitted, x=0:20))
    expect_true(all(rowSums(dt.pmf[, !"Id"]) <= 1))
    expect_true(all(dt.pmf[, !"Id"] >= 0))
    expect_true(all(dt.pmf[, !"Id"] <= 1))
    expect_false(anyNA(dt.pmf))
  })
}

fct.testthat.correctness.clvfittedtransactions.pmf.only.depends.on.Tcal <- function(clv.fitted){
  test_that("PMF - value only depend on T.cal", {
    skip_on_cran()
    expect_silent(dt.pmf <- pmf(clv.fitted, x=0:10))
    dt.pmf[clv.fitted@cbs, T.cal := T.cal, on="Id"]
    # varies by others (x, t.x) because they vary by T.cal
    #   by=Tcal already accounts for grouping additionally by x, t.x
    dt.pmf.melted <- melt(dt.pmf, id.vars="T.cal", measure.vars = paste0("pmf.x.", 0:10))
    # single value per Tcal (and pmf.x)
    expect_true(all(dt.pmf.melted[, .(num.vals = uniqueN(value)), by=c("T.cal", "variable")][,"num.vals"] == 1))
    # different value per Tcal (per pmf.x) <=> as many different pmf values as there are Tcal, per pmf.x
    expect_true(all(dt.pmf.melted[, .(num.vals = uniqueN(value)), by="variable"][,"num.vals"] == clv.fitted@cbs[, uniqueN(T.cal)]))
  })
}


fct.testthat.correctness.clvfittedtransactions.pmf.smaller.p.the.larger.Tcal.for.x0 <- function(clv.fitted){
  # For x=0, pmf has to get smaller the longer t <=> "The more opportunity to transact, the lower the probability to make 0 transaction"
  #  PNBD P(X=0)d/dt  = -(l/exp((l+m)t)) < 0, for all t>0
  #  BGNBD P(X=0)d/dt = -(m/exp(m*t)) < 0, for all t>0
  expect_silent(dt.pmf <- pmf(clv.fitted, x=0))
  dt.pmf[clv.fitted@cbs, T.cal := T.cal, on="Id"]
  # strictly decreasing p(X=0) with larger T.cal
  expect_false(unique(dt.pmf[, c("pmf.x.0", "T.cal")])[order(-T.cal),
                                                       is.unsorted(pmf.x.0, strictly = TRUE)])
}


fct.testthat.correctness.clvfittedtransactions.staticcov.fitting.sample.predicting.full.data.equal <- function(method, clv.apparel.staticcov){
  test_that("Fitting with sample but predicting full data yields same results as predicting sample only", {
    skip_on_cran()
    data.apparelTrans <- fct.helper.load.apparelTrans()
    data.apparelStaticCov <- fct.helper.load.apparelStaticCov()

    # Sample only
    id.sample <- unique(data.apparelTrans$Id)[1:300]
    clv.apparel.static.sample <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans[Id %in% id.sample],
                                                                             data.apparelStaticCov = data.apparelStaticCov[Id %in% id.sample],
                                                                             estimation.split = clv.apparel.staticcov@clv.time@timepoint.estimation.end)

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

fct.testthat.correctness.clvfittedtransactions.nocov.predict.newcustomer.0.for.num.periods.eq.0 <- function(clv.fitted){
  test_that("nocov: predict newcustomer 0 for t=0", {
    expect_silent(pred <- predict(clv.fitted, newdata=newcustomer(num.periods = 0)))
    expect_true(pred == 0)
  })
}

# predict(newdata=newcustomer): static cov
fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.independent.of.col.sorting <- function(m.fitted.static){
  test_that("staticcov: predict(newcustomer) results independent of cov data row sorting", {

    df.cov <- fct.helper.default.newcustomer.covdata.static()

    stopifnot(ncol(df.cov) > 1) # does not make sense otherwise
    df.cov.rev <- df.cov[, rev(colnames(df.cov))]

    expect_silent(pred <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 4.56,
        data.cov.life = df.cov,
        data.cov.trans = df.cov)))

    expect_silent(pred.rev <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 4.56,
        data.cov.life = df.cov.rev,
        data.cov.trans = df.cov.rev)))

    expect_true(pred == pred.rev)
  })
}

fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.different.result.for.different.covs <- function(m.fitted.static){
  test_that("staticcov: predict(newcustomer) different results for different cov data", {

    df.cov <- fct.helper.default.newcustomer.covdata.static()

    expect_silent(pred.original <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 4.56,
        data.cov.life = df.cov,
        data.cov.trans = df.cov)))

    expect_silent(pred.life <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 4.56,
        data.cov.life = df.cov*2,
        data.cov.trans = df.cov)))

    expect_silent(pred.trans <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 4.56,
        data.cov.life = df.cov,
        data.cov.trans = df.cov*2)))

    expect_true(pred.original != pred.life)
    expect_true(pred.original != pred.trans)
    expect_true(pred.trans != pred.life)
  })
}

fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.0.for.num.periods.eq.0 <- function(m.fitted.static){
  test_that("staticcov: predict(newcustomer) 0 for t=0", {
    df.cov <- fct.helper.default.newcustomer.covdata.static()
    expect_silent(pred <- predict(
      m.fitted.static,
      newdata=newcustomer.static(
        num.periods = 0,
        data.cov.life = df.cov,
        data.cov.trans = df.cov)))
    expect_true(pred == 0)
  })
}

fct.testthat.correctness.clvfittedtransactions <- function(name.model, method,
                                                           correct.start.params.model, correct.params.nocov.coef, correct.LL.nocov,
                                                           kkt2.true){


  # No cov ---------------------------------------------------------------------------------------------------
  clv.cdnow <- fct.helper.create.clvdata.cdnow()
  expect_silent(obj.fitted <- do.call(method, list(clv.data = clv.cdnow, verbose = FALSE)))

  fct.testthat.correctness.clvfitted.correct.coefs(method = method, start.params.model = correct.start.params.model,
                                                   params.nocov.coef = correct.params.nocov.coef, LL.nocov = correct.LL.nocov)

  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.cdnow, kkt2.true = kkt2.true)

  fct.testthat.correctness.clvfittedtransactions.predict.actual.x(method = method)
  fct.testthat.correctness.clvfitted.newdata.same.predicting.fitting(clv.fitted = obj.fitted)
  fct.testthat.correctness.clvfittedtransactions.CET.0.for.no.prediction.period(clv.fitted = obj.fitted)

  fct.testthat.correctness.clvfittedtransactions.nocov.newdata.fitting.sample.predicting.full.data.equal(method = method, clv.cdnow = clv.cdnow)
  fct.testhat.correctness.clvfittedtransactions.same.spending.as.independent.spending.model(method = method, clv.data = clv.cdnow)

  if(fct.helper.has.pmf(obj.fitted)){
    fct.testthat.correctness.clvfittedtransactions.pmf.more.x.more.p(clv.fitted = obj.fitted)
    fct.testthat.correctness.clvfittedtransactions.pmf.valid.values(clv.fitted = obj.fitted)
    fct.testthat.correctness.clvfittedtransactions.pmf.only.depends.on.Tcal(clv.fitted = obj.fitted)
    fct.testthat.correctness.clvfittedtransactions.pmf.smaller.p.the.larger.Tcal.for.x0(clv.fitted = obj.fitted)
  }

  # predict(newdata=newcustomer): no cov
  fct.testthat.correctness.clvfittedtransactions.nocov.predict.newcustomer.0.for.num.periods.eq.0(obj.fitted)

  # Static cov data --------------------------------------------------------------------------------------------
  # why 100 and not 104???????
  clv.apparel.staticcov <- fct.helper.create.clvdata.apparel.staticcov(estimation.split = 100)
  clv.apparel.nocov <- as(clv.apparel.staticcov, "clv.data")

  expect_silent(obj.fitted.static <- do.call(method, list(clv.data=clv.apparel.staticcov, verbose=FALSE)))


  fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.row.sorting(method = method,
                                                                                 m.fitted.static = obj.fitted.static)
  fct.testthat.correctness.clvfittedtransactions.staticcov.covariate.column.sorting(method = method,
                                                                                    m.fitted.static = obj.fitted.static)

  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.nocov, kkt2.true = kkt2.true)
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.staticcov, kkt2.true = kkt2.true)

  fct.testthat.correctness.clvfittedtransactions.CET.0.for.no.prediction.period(clv.fitted = obj.fitted.static)
  fct.testthat.correctness.clvfittedtransactions.staticcov.fitting.sample.predicting.full.data.equal(method = method,
                                                                                                     clv.apparel.staticcov = clv.apparel.staticcov)
  fct.testhat.correctness.clvfittedtransactions.same.spending.as.independent.spending.model(method = method, clv.data = clv.apparel.staticcov)

  fct.testthat.correctness.clvfittedtransactions.staticcov.regularization.lambda.0.no.regularization(method = method, clv.apparel.staticcov = clv.apparel.staticcov,
                                                                                                     m.fitted.static = obj.fitted.static)

  # predict(newdata=newcustomer): static cov
  fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.independent.of.col.sorting(obj.fitted.static)

  fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.0.for.num.periods.eq.0(obj.fitted.static)

  fct.testthat.correctness.clvfittedtransactions.staticcov.predict.newcustomer.different.result.for.different.covs(obj.fitted.static)



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
