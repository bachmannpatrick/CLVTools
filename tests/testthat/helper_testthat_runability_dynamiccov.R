fct.testthat.runability.dynamiccov.LL.is.correct <- function(clv.fitted){
  test_that("LL.data is correct",{
    expect_true(isTRUE(all.equal(as.numeric(logLik(clv.fitted)),
                                 clv.fitted@LL.data[, sum(LL)],
                                 tolerance = sqrt(.Machine$double.eps))))
  })
}

fct.testthat.runability.dynamiccov.plot.works <- function(clv.fitted){
  test_that("Plot works", {
    # Only check whether the expectation runs through,
    #   with as few as possible periods (from estimation.end)
    expect_warning(plot(clv.fitted, prediction.end = 5, verbose=FALSE),
                   regexp = "Not plotting full holdout period")
  })
}

fct.testthat.runability.dynamiccov.plot.has.0.repeat.transactions.expectations <- function(clv.fitted){
  test_that("Plot always has 0 on repeat transactions and expectations", {
    expect_warning(dt.plot <- plot(clv.fitted, prediction.end = 5, verbose=FALSE, plot=FALSE),
                   regexp = "Not plotting full holdout period")
    expect_true(isTRUE(all.equal(dt.plot[period.until == min(period.until), value], c(0,0))))
  })
}

fct.testthat.runability.dynamiccov.predict.works <- function(clv.fitted){
  test_that("Predict works", {
    # Works for partial prediction.end which only touch 1 cov period
    stopifnot(is(clv.fitted@clv.data@clv.time, "clv.time.weeks"))
    for(d.end in as.character(seq(from=clv.fitted@clv.data@clv.time@timepoint.estimation.end,
                                  length.out = 8, by="1 day"))){
      expect_silent(predict(clv.fitted, prediction.end = d.end, verbose=FALSE))
    }
  })
}

fct.testthat.runability.dynamiccov.predict.newdata.works <- function(clv.fitted){

  data.apparelTrans <- fct.helper.load.apparelTrans()
  data.apparelDynCov <- fct.helper.load.apparelDynCov()

  sample.ids <- unique(data.apparelTrans$Id)[101:200]

  clv.dyncov.sample <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelTrans=data.apparelTrans[Id %in% sample.ids],
    data.apparelDynCov=data.apparelDynCov[Id %in% sample.ids],
    names.cov.life=names(clv.fitted@prediction.params.life),
    names.cov.trans=names(clv.fitted@prediction.params.trans),
    # Estimation split exactly the same as the one for fitting
    estimation.split = clv.fitted@clv.data@clv.time@timepoint.estimation.end)

  test_that("Predict newdata works to predict orginal data", {
    expect_silent(dt.pred <- predict(clv.fitted, predict.spending=FALSE, verbose=FALSE))
    expect_silent(dt.pred.newdata <- predict(clv.fitted, newdata=clv.fitted@clv.data, predict.spending=FALSE, verbose=FALSE))
    expect_equal(dt.pred, dt.pred.newdata)
  })

  test_that("Predict newdata works by predicting on sample of orginal data", {
    # deep copy of whole object
    backup.fittted.dyncov <- data.table::copy(clv.fitted)

    expect_silent(predict(clv.fitted, newdata=clv.dyncov.sample, predict.spending=FALSE, verbose=FALSE))

    # Check that the fitted model is left unchanged
    expect_true(isTRUE(all.equal(backup.fittted.dyncov, clv.fitted)))
  })

  test_that("Predict with newdata and <=2 periods works (issue #128)", {
    expect_silent(predict(clv.fitted, newdata=clv.dyncov.sample, prediction.end=2, predict.spending=FALSE, verbose=FALSE))
    expect_silent(predict(clv.fitted, newdata=clv.dyncov.sample, prediction.end=1, predict.spending=FALSE, verbose=FALSE))
    expect_silent(predict(clv.fitted, newdata=clv.dyncov.sample, prediction.end=0, predict.spending=FALSE, verbose=FALSE))
  })
}

fct.testthat.runability.dynamiccov.predict.longer.with.newdata <- function(clv.fitted, clv.data.extra){
  test_that("Can predict longer with newdata than with the data used for fitting", {
    expect_error(predict(clv.fitted, prediction.end = "2018-01-01", predict.spending=FALSE),
                 regexp = "in the fitted model are not long enough")
    expect_silent(dt.predict <- predict(
      clv.fitted, newdata=clv.data.extra,
      predict.spending=FALSE,
      prediction.end = clv.fitted@clv.data@clv.time@timepoint.holdout.end + lubridate::weeks(13),
      verbose=FALSE))
    expect_true(dt.predict[, max(period.last)] > clv.fitted@clv.data@clv.time@timepoint.holdout.end)
  })

}

fct.testthat.runability.dynamiccov.plot.longer.with.newdata <- function(clv.fitted, clv.data.extra){
  test_that("Can plot longer with newdata than with the data used for fitting", {
    expect_error(plot(clv.fitted, prediction.end = "2018-01-01"),
                 regexp = "in the fitted model are not long enough")

    expect_silent(dt.plot <- plot(
      clv.fitted,
      newdata=clv.data.extra,
      plot=FALSE,
      prediction.end=clv.fitted@clv.data@clv.time@timepoint.holdout.end + lubridate::weeks(13),
      verbose=FALSE))

    expect_true(dt.plot[, max(period.until)] > clv.fitted@clv.data@clv.time@timepoint.holdout.end)
  })
}

fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout <- function(apparelDynCov.extra){

  test_that("Can predict/plot beyond holdout if there is more covs in the data than used for holdout",{

    fitted.dyncov <- fct.helper.dyncov.quickfit.apparel.data(data.apparelDynCov=apparelDynCov.extra)

    # Only predict & plots until transaction data end / holdout end....
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE, predict.spending=FALSE))
    expect_true(dt.plot[, max(period.until)] <= ceiling_date(fitted.dyncov@clv.data@clv.time@timepoint.holdout.end, unit="week"))
    expect_true(dt.predict[, max(period.last)] <= fitted.dyncov@clv.data@clv.time@timepoint.holdout.end)

    # ...but: Can also predict & plot further because there are more covariates present
    prediction.end.over <-  fitted.dyncov@clv.data@clv.time@timepoint.holdout.end + lubridate::period(10, "weeks")
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE, prediction.end = prediction.end.over))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE, prediction.end = prediction.end.over, predict.spending=FALSE))

    expect_true(dt.plot[, max(period.until)] > fitted.dyncov@clv.data@clv.time@timepoint.holdout.end)
    expect_true(dt.predict[, max(period.last)] > fitted.dyncov@clv.data@clv.time@timepoint.holdout.end)
  })

}



fct.helper.runability.dyncov.all.downstream <- function(fitted.dyncov, names.params){

  # Standard S3 tests ---------------------------------------------------------------

  # Run the standard S3 tests on the fitted model,
  #   but not plot() and predict() which takes too long.
  #   also plot() produces all NAs if quickfit is used
  .fct.helper.clvfitted.all.s3.except.plot.and.predict(
    clv.fitted=fitted.dyncov,
    full.names=names.params
  )

  # Cannot check if LLsum is correct because may be called with regularization
  # and then is not expected to be same as LLdata[, sum(LL)]


  # Plot ------------------------------------------------------------------
  fct.testthat.runability.dynamiccov.plot.works(clv.fitted = fitted.dyncov)

  fct.testthat.runability.dynamiccov.plot.has.0.repeat.transactions.expectations(clv.fitted = fitted.dyncov)


  # Predict ----------------------------------------------------------------
  fct.testthat.runability.dynamiccov.predict.works(clv.fitted = fitted.dyncov)

  fct.testthat.runability.dynamiccov.predict.newdata.works(clv.fitted = fitted.dyncov)


  # Newdata ----------------------------------------------------------------------------------------------------------
  apparelDynCov.extra <- fct.helper.dyncov.create.longer.dyncov.data(
    num.additional = 100,
    data.apparelDynCov = fct.helper.load.apparelDynCov()
  )
  clv.data.extra <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelDynCov=apparelDynCov.extra,
    names.cov.life=names(fitted.dyncov@prediction.params.life),
    names.cov.trans=names(fitted.dyncov@prediction.params.trans),
  )

  fct.testthat.runability.dynamiccov.predict.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.extra = clv.data.extra)

  fct.testthat.runability.dynamiccov.plot.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.extra = clv.data.extra)



  # Overlong data ------------------------------------------------------------------------------
  # Cannot do without holdout because takes too long to estimate
  fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout(apparelDynCov.extra=apparelDynCov.extra)
}

