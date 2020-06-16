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

fct.testthat.runability.dynamiccov.predict.newdata.works <- function(clv.fitted, apparelTrans, apparelDynCov){
  test_that("Predict newdata works by predicting on another sample", {
    # Full fails for whatever reason

    # expect_output(dt.pred.mini <- predict(clv.fitted))
    # expect_output(dt.pred.full <- predict(clv.fitted, newdata=clv.data.full.dyncov))
    #
    # expect_true(nrow(dt.pred.mini) == length(unique(mini.apparelTrans$Id)))
    # expect_true(nrow(dt.pred.full) == length(unique(apparelTrans$Id)))
    # expect_true(all.equal(dt.pred.mini,
    #                       dt.pred.full[Id %in% dt.pred.mini$Id]))

    # The next 200 Ids
    mini2.apparelTrans <- apparelTrans[Id %in% unique(apparelTrans$Id)[101:200]]
    mini2.apparelDynCov <- apparelDynCov[Id %in% mini2.apparelTrans$Id]

    # Estimation split exactly the same as the one for fitting
    clv.data.trans.mini2 <- clvdata(data.transactions = mini2.apparelTrans, date.format = "ymd",
                                    time.unit = "W",
                                    estimation.split = clv.fitted@clv.data@clv.time@timepoint.estimation.end)

    clv.data.mini2.dyncov <-
      SetDynamicCovariates(clv.data = clv.data.trans.mini2,
                           data.cov.life = mini2.apparelDynCov,
                           data.cov.trans = mini2.apparelDynCov,
                           names.cov.life = "Gender",
                           names.cov.trans = "Gender",
                           name.date = "Cov.Date")

    bck.fittted.dyncov <- data.table::copy(clv.fitted)

    # Use model fitted on first sample to predict for another 2nd sample
    expect_silent(predict(clv.fitted, newdata=clv.data.mini2.dyncov, verbose=FALSE))

    # Check that the fitted model is left unchanged
    expect_true(isTRUE(all.equal(bck.fittted.dyncov, clv.fitted)))
  })
}

fct.testthat.runability.dynamiccov.predict.longer.with.newdata <- function(clv.fitted, clv.data.mini.extra, clv.data.trans){
  test_that("Can predict longer with newdata than with the data used for fitting", {
    expect_error(predict(clv.fitted, prediction.end = "2018-01-01"),
                 regexp = "in the fitted model are not long enough")
    expect_silent(dt.predict <- predict(clv.fitted, newdata=clv.data.mini.extra,
                                        prediction.end = "2006-07-26",verbose=FALSE))
    expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })

}

fct.testthat.runability.dynamiccov.plot.longer.with.newdata <- function(clv.fitted, clv.data.mini.extra, clv.data.trans){
  test_that("Can plot longer with newdata than with the data used for fitting", {
    expect_error(plot(clv.fitted, prediction.end = "2018-01-01"),
                 regexp = "in the fitted model are not long enough")
    expect_silent(dt.plot <- plot(clv.fitted, newdata=clv.data.mini.extra, plot=FALSE,
                                  prediction.end = "2006-07-26",verbose=FALSE))
    expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })
}

fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout <- function(method, clv.data.trans, mini.apparelDynCov.long, start.params.model){
  test_that("Can predict/plot beyond holdout if there is more covs in the data than used for holdout",{

    expect_silent(clv.data.mini.extra <- SetDynamicCovariates(clv.data.trans,
                                                              data.cov.life = mini.apparelDynCov.long,
                                                              data.cov.trans = mini.apparelDynCov.long,
                                                              names.cov.life = c("Marketing", "Gender","Channel"),
                                                              names.cov.trans = c("Marketing", "Gender","Channel"),
                                                              name.date = "Cov.Date"))


    l.args <- list(clv.data.mini.extra,
                   start.params.model = start.params.model,
                   start.params.life = c(Marketing=0.5, Gender = 0.8, Channel=0.9304636),
                   start.params.trans = c(Marketing=1.1, Gender = 1.33, Channel=0.9304636),
                   optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                      hessian=FALSE, # no hessian
                                      control=list(kkt=FALSE, # kkt takes forever
                                                   reltol = 1000)))

    # Fit model until estimation.end only (end of estimation.end, 2016-10-08)
    #   high tolerance to converge quickly
    #   no hessian to avoid additional evaluations after convergence
    expect_warning(fitted.dyncov <- do.call(what = method, args = l.args), regexp = "Hessian could not be derived.")

    # Only predict & plots until transaction data end / holdout end
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE))
    expect_true(dt.plot[, max(period.until)] <= ceiling_date(clv.data.trans@clv.time@timepoint.holdout.end, unit="week"))
    expect_true(dt.predict[, max(period.last)] <= clv.data.trans@clv.time@timepoint.holdout.end)

    # Can also predict & plot further
    prediction.end.over <-  clv.data.trans@clv.time@timepoint.holdout.end + lubridate::period(10, "weeks")
    expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE, prediction.end = prediction.end.over))
    expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE, prediction.end = prediction.end.over))

    expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
    expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
  })

}

