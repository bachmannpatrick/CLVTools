fct.testthat.runability.clvfittedtransactions.predict <- function(fitted.transactions, clv.newdata.nohold, clv.newdata.withhold){

  # Only for models which were fit with holdout data
  if(clv.data.has.holdout(fitted.transactions@clv.data)){
    test_that("Works without parameters (has holdout)", {
      expect_silent(predict(fitted.transactions, verbose=FALSE))
    })
    test_that("Works with prediction end in holdout period", {
      skip_on_cran()
      expect_silent(dt.pred <- predict(fitted.transactions,prediction.end = as.character(fitted.transactions@clv.data@clv.time@timepoint.holdout.end - lubridate::days(1)),
                                       predict.spending=FALSE, verbose=FALSE))
      # then also has actuals
      expect_true(c("actual.x" %in% colnames(dt.pred)))

      if(clv.data.has.spending(fitted.transactions@clv.data)){
        expect_true(c("actual.total.spending" %in% colnames(dt.pred)))
      }else{
        expect_false(c("actual.total.spending" %in% colnames(dt.pred)))
      }
    })
  }

  # Test
  #   Sum of actual.total.spending same as sum based on data
  #   sum of actual.x same as sum based on data
  #   actual.total.spending all > 0
  #   actual.transactions all > 0
  # predicted CLV is = X*Y
  if(clv.data.has.spending(fitted.transactions@clv.data)){

    test_that("Predict works with logical for predict.spending", {
      skip_on_cran()
      skip_on_ci()
      expect_silent(pred <- predict(fitted.transactions, prediction.end=6, predict.spending = TRUE, verbose=FALSE))
      expect_true("predicted.mean.spending" %in% colnames(pred))
      expect_silent(pred <- predict(fitted.transactions, prediction.end=6, predict.spending = FALSE, verbose=FALSE))
      expect_false("predicted.mean.spending" %in% colnames(pred))
    })

    test_that("Predict works with fitted spending model for predict.spending (and newdata) ",{
      skip_on_cran()
      skip_on_ci()

      # ordinary
      expect_silent(pred <- predict(fitted.transactions, prediction.end=6,  verbose=FALSE,
                                    predict.spending = gg(fitted.transactions@clv.data, verbose=FALSE)))
      expect_true("predicted.mean.spending" %in% colnames(pred))

      # fitted on different clv.data object
      expect_silent(fitted.spending.different.data <- gg(clv.newdata.nohold, verbose=FALSE))
      expect_silent(pred <- predict(fitted.transactions, prediction.end=6,  verbose=FALSE,
                                    predict.spending = fitted.spending.different.data))
      expect_true("predicted.mean.spending" %in% colnames(pred))
      expect_equal(pred[order(Id), "Id"], fitted.transactions@cbs[order(Id), "Id"]) # all original Ids present
      expect_false(anyNA(pred[, c("Id", "predicted.mean.spending")])) # No Id w/o predicted spending

      # mix it all up: predict for clv.newdata.withhold using spending model fitted on clv.newdata.nohold
      #   but use newdata with different Ids to see a difference to the one used for spending model fitting
      subset.id  <- fitted.transactions@clv.data@data.transactions[, sample(x = unique(Id), size = uniqueN(Id)/2)]
      clv.subset <- clvdata(fitted.transactions@clv.data@data.transactions[Id %in% subset.id],
                            date.format=fitted.transactions@clv.data@clv.time@time.format,
                            time.unit=fitted.transactions@clv.data@clv.time@name.time.unit)
      if(is(fitted.transactions@clv.data, "clv.data.static.covariates")){
        clv.subset <- SetStaticCovariates(clv.subset,
                                          data.cov.life  = fitted.transactions@clv.data@data.cov.life[Id %in% subset.id],  names.cov.life = fitted.transactions@clv.data@names.cov.data.life,
                                          data.cov.trans = fitted.transactions@clv.data@data.cov.trans[Id %in% subset.id], names.cov.trans = fitted.transactions@clv.data@names.cov.data.trans)
      }
      expect_silent(pred <- predict(fitted.transactions,prediction.end=6, verbose=FALSE,
                                    newdata = clv.subset,
                                    predict.spending = fitted.spending.different.data))
      expect_true("predicted.mean.spending" %in% colnames(pred))
      expect_equal(pred[order(Id), "Id"], unique(clv.subset@data.transactions[order(Id), "Id"])) # all original Ids present
      expect_false(anyNA(pred[, c("Id", "predicted.mean.spending")])) # No Id w/o predicted spending
    })

    test_that("Predict works with CLVTools spending model method for predict.spending",{
      skip_on_cran()
      skip_on_ci()
      expect_silent(pred <- predict(fitted.transactions, predict.spending = gg, prediction.end=6,  verbose=FALSE))
      expect_true("predicted.mean.spending" %in% colnames(pred))
    })


    test_that("Predict forwards parameter verbose to fitting spending model", {
      skip_on_cran()
      expect_silent(predict(fitted.transactions, predict.spending = gg, prediction.end=6,  verbose=FALSE))
      expect_message(predict(fitted.transactions, predict.spending = gg, prediction.end=6,  verbose=TRUE),
                    regexp = "Estimation finished")
    })
  }

  test_that("Formal correct", {
    skip_on_cran()
    expect_silent(dt.pred <- predict(fitted.transactions, prediction.end = 6, predict.spending=FALSE, verbose = FALSE))
    expect_true(dt.pred[, data.table::uniqueN(Id)] == fitted.transactions@clv.data@data.transactions[, data.table::uniqueN(Id)])
    # all ids in predictions
    expect_true(nrow(data.table::fsetdiff(fitted.transactions@clv.data@data.transactions[, "Id"], dt.pred[, "Id"]))==0)
    # May fail because of numerical tolerance issues
    expect_true(nrow(dt.pred[PAlive < 0 - sqrt(.Machine$double.eps) | PAlive > 1 + sqrt(.Machine$double.eps)]) == 0)

    #   all columns > 0
    expect_true(dt.pred[, all(.SD >= (0 - sqrt(.Machine$double.eps)) | is.na(.SD))])
    expect_true(all(c("Id", "CET", "PAlive") %in% colnames(dt.pred)))
    if(fct.helper.has.DERT(fitted.transactions)){
      expect_true("DERT" %in% colnames(dt.pred) | "DECT" %in% colnames(dt.pred))
    }else{
      expect_false("DERT" %in% colnames(dt.pred) | "DECT" %in% colnames(dt.pred))
    }

    if(clv.data.has.holdout(fitted.transactions@clv.data)){
      # Has actuals if there is a holdout period
      expect_true("actual.x" %in% colnames(dt.pred))
      expect_true(dt.pred[, is.numeric(actual.x)])

      if(clv.data.has.spending(fitted.transactions@clv.data)){
        expect_true("actual.total.spending" %in% colnames(dt.pred))
        expect_true(dt.pred[, is.numeric(actual.total.spending)])
      }else{
        expect_false("actual.total.spending" %in% colnames(dt.pred))
      }
    }else{
      expect_false("actual.x" %in% colnames(dt.pred))
      expect_false("actual.total.spending" %in% colnames(dt.pred))
    }
  })


  if(fct.helper.has.DERT(fitted.transactions)){
    test_that("Works with discount factor", {
      skip_on_cran()
      expect_silent(dt.pred.1 <- predict(fitted.transactions, continuous.discount.factor = 0.001,prediction.end = 6, predict.spending=FALSE, verbose=FALSE))
      expect_silent(dt.pred.2 <- predict(fitted.transactions, continuous.discount.factor = 0.06, prediction.end = 6, predict.spending=FALSE, verbose=FALSE))
      expect_silent(dt.pred.3 <- predict(fitted.transactions, continuous.discount.factor = 0.99, prediction.end = 6, predict.spending=FALSE, verbose=FALSE))
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))
      expect_true(dt.pred.1[, all(is.finite(DERT))])
      expect_true(dt.pred.2[, all(is.finite(DERT))])
      expect_true(dt.pred.3[, all(is.finite(DERT))])
    })
  }

  test_that("Works with different newdata", {
    skip_on_cran()
    # **TODO: Often still has NA (because estimated params do not work with artificial newdata)
    # No holdout needs prediction.end
    expect_silent(dt.pred <- predict(fitted.transactions, newdata = clv.newdata.nohold, prediction.end = 25, predict.spending = FALSE, verbose=FALSE))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))

    # Holdout needs no prediction end, but do both
    expect_silent(dt.pred <- predict(fitted.transactions, newdata = clv.newdata.withhold, verbose=FALSE, predict.spending = FALSE))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))

    expect_silent(dt.pred <- predict(fitted.transactions, newdata = clv.newdata.withhold, prediction.end = 10, predict.spending = FALSE, verbose=FALSE))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))
  })

  test_that("Works with different types of prediction.end: number, date, posix, char (long)", {
    skip_on_cran()

    fct.predict <- function(end.1, end.2, end.3, ...){
      if(is(fitted.transactions@clv.data@clv.time, "clv.time.date") & (is.POSIXct(end.1) | lubridate::is.POSIXlt(end.1))){
        expect_message(dt.pred.1 <- predict(fitted.transactions,prediction.end = end.1, predict.spending=FALSE, verbose=FALSE), regexp = "cut off")
        expect_message(dt.pred.2 <- predict(fitted.transactions,prediction.end = end.2, predict.spending=FALSE, verbose=FALSE), regexp = "cut off")
        expect_message(dt.pred.3 <- predict(fitted.transactions,prediction.end = end.3, predict.spending=FALSE, verbose=FALSE), regexp = "cut off")
      }else{
        expect_silent(dt.pred.1 <- predict(fitted.transactions,prediction.end = end.1, predict.spending=FALSE, verbose=FALSE))
        expect_silent(dt.pred.2 <- predict(fitted.transactions,prediction.end = end.2, predict.spending=FALSE, verbose=FALSE))
        expect_silent(dt.pred.3 <- predict(fitted.transactions,prediction.end = end.3, predict.spending=FALSE, verbose=FALSE))
      }
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))
    }


    if(lubridate::is.POSIXct(fitted.transactions@clv.data@clv.time@timepoint.estimation.start)){
      # long enough to not such that as.Date() falls below estimation.split
      pred.end.char.1 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::hours(24))
      pred.end.char.2 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::hours(100))
      pred.end.char.3 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::hours(200))

      # sure different from each other and after estimation.end
      pred.end.date.1 <- as.Date(lubridate::ymd_hms(pred.end.char.1))
      pred.end.date.2 <- as.Date(lubridate::ymd_hms(pred.end.char.2))
      pred.end.date.3 <- as.Date(lubridate::ymd_hms(pred.end.char.3))

      pred.end.posixct.1 <- as.POSIXct(lubridate::ymd_hms(pred.end.char.1), tz="UTC")
      pred.end.posixct.2 <- as.POSIXct(lubridate::ymd_hms(pred.end.char.2), tz="UTC")
      pred.end.posixct.3 <- as.POSIXct(lubridate::ymd_hms(pred.end.char.3), tz="UTC")

      pred.end.posixlt.1 <- as.POSIXlt(lubridate::ymd_hms(pred.end.char.1), tz="UTC")
      pred.end.posixlt.2 <- as.POSIXlt(lubridate::ymd_hms(pred.end.char.2), tz="UTC")
      pred.end.posixlt.3 <- as.POSIXlt(lubridate::ymd_hms(pred.end.char.3), tz="UTC")

    }else{
      pred.end.char.1 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::days(3))
      pred.end.char.2 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::days(180))
      pred.end.char.3 <- as.character(fitted.transactions@clv.data@clv.time@timepoint.estimation.end+lubridate::days(365))

      pred.end.date.1 <- lubridate::ymd(pred.end.char.1)
      pred.end.date.2 <- lubridate::ymd(pred.end.char.2)
      pred.end.date.3 <- lubridate::ymd(pred.end.char.3)

      pred.end.posixct.1 <- as.POSIXct(lubridate::ymd(pred.end.char.1))
      pred.end.posixct.2 <- as.POSIXct(lubridate::ymd(pred.end.char.2))
      pred.end.posixct.3 <- as.POSIXct(lubridate::ymd(pred.end.char.3))

      pred.end.posixlt.1 <- as.POSIXlt(lubridate::ymd(pred.end.char.1))
      pred.end.posixlt.2 <- as.POSIXlt(lubridate::ymd(pred.end.char.2))
      pred.end.posixlt.3 <- as.POSIXlt(lubridate::ymd(pred.end.char.3))
    }

    fct.predict(4, 26, 52)
    fct.predict(pred.end.char.1, pred.end.char.2, pred.end.char.3)
    fct.predict(pred.end.date.1, pred.end.date.2, pred.end.date.3)
    fct.predict(pred.end.posixct.1, pred.end.posixct.2, pred.end.posixct.3)
    fct.predict(pred.end.posixlt.1, pred.end.posixlt.2, pred.end.posixlt.3)
  })

}

fct.testthat.runability.clvfittedspending.predict <- function(fitted.spending, clv.newdata.nohold, clv.newdata.withhold){
  # Whether predict works with and without holdout in fitted data is checked by fitting on different objects and passing here

  test_that("Predict works out of the box", {
    skip_on_cran()
    expect_silent(predict(fitted.spending))
  })

  test_that("Predict works with verbose = T and verbose = F", {
    skip_on_cran()
    # expect_message(predict(fitted.spending, verbose = TRUE))
    expect_silent(predict(fitted.spending, verbose = FALSE))
  })

  test_that("Predict works with newdata w/ and w/ holdout", {
    skip_on_cran()
    expect_silent(predict(fitted.spending, newdata = clv.newdata.nohold, verbose = FALSE))
    expect_silent(predict(fitted.spending, newdata = clv.newdata.withhold, verbose = FALSE))
  })

  test_that("Predict is formally correct", {
    skip_on_cran()
    expect_silent(dt.pred <- predict(fitted.spending, verbose = FALSE))
    # All Ids there
    expect_equal(dt.pred[order(Id),"Id"], fitted.spending@cbs[order(Id), "Id"])
    expect_true(fsetequal(dt.pred[,"Id"], unique(fitted.spending@clv.data@data.transactions[, "Id"])))
    # Spending cols
    expect_true("predicted.mean.spending" %in% colnames(dt.pred))
    expect_true(dt.pred[, is.numeric(predicted.mean.spending)])

    # Has actuals if there is a holdout period
    if(clv.data.has.holdout(fitted.spending@clv.data)){
      expect_true("actual.mean.spending" %in% colnames(dt.pred))
      expect_true(dt.pred[, is.numeric(actual.mean.spending)])
    }else{
      expect_false("actual.mean.spending" %in% colnames(dt.pred))
    }
  })
}
