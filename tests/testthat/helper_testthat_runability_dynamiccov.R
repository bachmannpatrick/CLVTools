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
    expect_true(isTRUE(all.equal(unlist(dt.plot[period.until == min(period.until), c(2,3)]),
                                 c(0,0), check.attributes = FALSE)))
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

fct.testthat.runability.dynamiccov.predict.newdata.works <- function(clv.fitted, data.apparelTrans, data.apparelDynCov){

  # test_that("Predict newdata works by predicting on orginal data", {
  #
  #   # Full fails for whatever reason
  #
  #   # expect_output(dt.pred.mini <- predict(clv.fitted))
  #   # expect_output(dt.pred.full <- predict(clv.fitted, newdata=clv.data.full.dyncov))
  #   #
  #   # expect_true(nrow(dt.pred.mini) == length(unique(mini.apparelTrans$Id)))
  #   # expect_true(nrow(dt.pred.full) == length(unique(apparelTrans$Id)))
  #   # expect_true(all.equal(dt.pred.mini,
  #   #                       dt.pred.full[Id %in% dt.pred.mini$Id]))
  # })

  test_that("Predict newdata works by predicting on sample of orginal data", {
    sample.ids <- unique(apparelTrans$Id)[101:200]

    clv.dyncov.sample <- fct.helper.create.clvdata.apparel.dyncov(
      data.apparelTrans=data.apparelTrans[Id %in% sample.ids],
      data.apparelDynCov=data.apparelDynCov[Id %in% sample.ids],
      # Estimation split exactly the same as the one for fitting
      estimation.end = clv.fitted@clv.data@clv.time@timepoint.estimation.end)

    # deep copy of whole object
    backup.fittted.dyncov <- data.table::copy(clv.fitted)

    expect_silent(predict(clv.fitted, newdata=clv.dyncov.sample, verbose=FALSE))

    # Check that the fitted model is left unchanged
    expect_true(isTRUE(all.equal(backup.fittted.dyncov, clv.fitted)))
  })
}

fct.testthat.runability.dynamiccov.predict.longer.with.newdata <- function(clv.fitted, clv.data.extra, clv.data.trans){
  test_that("Can predict longer with newdata than with the data used for fitting", {
    expect_error(predict(clv.fitted, prediction.end = "2018-01-01"),
                 regexp = "in the fitted model are not long enough")
    expect_silent(dt.predict <- predict(clv.fitted, newdata=clv.data.extra,
                                        prediction.end = "2006-07-26",verbose=FALSE))
    expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })

}

fct.testthat.runability.dynamiccov.plot.longer.with.newdata <- function(clv.fitted, clv.data.extra, clv.data.trans){
  test_that("Can plot longer with newdata than with the data used for fitting", {
    expect_error(plot(clv.fitted, prediction.end = "2018-01-01"),
                 regexp = "in the fitted model are not long enough")
    expect_silent(dt.plot <- plot(clv.fitted, newdata=clv.data.extra, plot=FALSE,
                                  prediction.end = "2006-07-26",verbose=FALSE))
    expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })
}

fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout <- function(data.apparelTrans, apparelDynCov.extra){

  test_that("Can predict/plot beyond holdout if there is more covs in the data than used for holdout",{

    fitted.dyncov <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans=data.apparelTrans,
                                                             data.apparelDynCov=apparelDynCov.extra,
                                                             hessian=FALSE)

    # Only predict & plots until transaction data end / holdout end....
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE))
    expect_true(dt.plot[, max(period.until)] <= ceiling_date(clv.data.trans@clv.time@timepoint.holdout.end, unit="week"))
    expect_true(dt.predict[, max(period.last)] <= clv.data.trans@clv.time@timepoint.holdout.end)

    # ...but: Can also predict & plot further because there are more covariates present
    prediction.end.over <-  clv.data.trans@clv.time@timepoint.holdout.end + lubridate::period(10, "weeks")
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE, prediction.end = prediction.end.over))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE, prediction.end = prediction.end.over))

    expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
    expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })

}

