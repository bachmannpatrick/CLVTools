.fct.helper.s3.fitted.predict <- function(clv.fitted, clv.newdata.nohold, clv.newdata.withhold,
                                          DERT.not.implemented){

  # Only for models which were fit with heldout data
  if(clv.fitted@clv.data@has.holdout){

    test_that("Works without parameters (has holdout)", {
      expect_silent(predict(clv.fitted, verbose=FALSE))
    })
    test_that("Works with prediction end in holdout period", {
      skip_on_cran()
      expect_silent(dt.pred <- predict(clv.fitted,prediction.end = as.character(clv.fitted@clv.data@clv.time@timepoint.holdout.end - lubridate::days(30)), verbose=FALSE))
      # then also has actuals
      expect_true(c("actual.x" %in% colnames(dt.pred)))

      if("Price" %in% colnames(clv.fitted@clv.data@data.transactions))
        expect_true(c("actual.spending" %in% colnames(dt.pred)))
    })
  }

  # Test
  #   Sum of actual.spending same as sum based on data
  #   sum of actual.x same as sum based on data
  #   actual.spending all > 0
  #   actual.transactions all > 0
  # predicted CLV is = X*Y

  if("Price" %in% colnames(clv.fitted@clv.data@data.transactions)){
    test_that("Works with and without spending", {
      skip_on_cran()
      expect_silent(pred <- predict(clv.fitted, prediction.end=6, predict.spending = TRUE, verbose=FALSE))
      expect_true("predicted.Spending" %in% colnames(pred))
      expect_silent(pred <- predict(clv.fitted, prediction.end=6, predict.spending = FALSE, verbose=FALSE))
      expect_false("predicted.Spending" %in% colnames(pred))
    })
  }

  test_that("Formal correct", {
    dt.pred <- predict(clv.fitted, prediction.end = 6)
    expect_true(dt.pred[, data.table::uniqueN(Id)] == clv.fitted@clv.data@data.transactions[, data.table::uniqueN(Id)])
    # all ids in predictions
    expect_true(nrow(data.table::fsetdiff(clv.fitted@clv.data@data.transactions[, "Id"], dt.pred[, "Id"]))==0)
    # May fail because of numerical tolerance issues
    expect_true(nrow(dt.pred[PAlive < 0 - sqrt(.Machine$double.eps) | PAlive > 1 + sqrt(.Machine$double.eps)]) == 0)

    #   all columns > 0
    expect_true(dt.pred[, all(.SD >= 0 | is.na(.SD))])
    expect_true(all(c("Id", "CET", "DERT", "PAlive") %in% colnames(dt.pred)))
  })


  if(!DERT.not.implemented){
    test_that("Works with discount factor", {
      skip_on_cran()
      expect_silent(dt.pred.1 <- predict(clv.fitted, continuous.discount.factor = 0,    prediction.end = 6, verbose=FALSE))
      expect_silent(dt.pred.2 <- predict(clv.fitted, continuous.discount.factor = 0.06, prediction.end = 6, verbose=FALSE))
      expect_silent(dt.pred.3 <- predict(clv.fitted, continuous.discount.factor = 0.99, prediction.end = 6, verbose=FALSE))
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))
    })
  }

  test_that("Works with different types of prediction.end: number, date, posix, char (short) ",{
    # not checking anything correctness on cran, just run
    expect_silent(predict(clv.fitted,prediction.end = 4, verbose=FALSE))
    pred.end.char <- as.character(as.Date(clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::days(30), tz=""))

    expect_silent(predict(clv.fitted,prediction.end = pred.end.char, verbose=FALSE))
    expect_silent(predict(clv.fitted,prediction.end = as.Date(lubridate::ymd(pred.end.char)), verbose=FALSE))
    if(lubridate::is.POSIXct(clv.fitted@clv.data@clv.time@timepoint.estimation.start)){
      expect_silent(predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char)), verbose=FALSE))
      expect_silent(predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char)), verbose=FALSE))
    }else{
      expect_message(predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char)), verbose=FALSE), regexp = "ignored")
      expect_message(predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char)), verbose=FALSE), regexp = "ignored")
    }
  })


  test_that("Works with different newdata", {
    skip_on_cran()
    # **TODO: Often still has NA (because estimated params do not work with artificial newdata)
    # No holdout needs prediction.end
    expect_silent(dt.pred <- predict(clv.fitted, newdata = clv.newdata.nohold, prediction.end = 25,
                                     predict.spending = clv.newdata.nohold@has.spending, verbose=FALSE))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))

    # Holdout needs no prediction end, but do both
    expect_silent(dt.pred <- predict(clv.fitted, newdata = clv.newdata.withhold, verbose=FALSE,
                                     predict.spending = clv.newdata.withhold@has.spending))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))

    expect_silent(dt.pred <- predict(clv.fitted, newdata = clv.newdata.withhold, prediction.end = 10,
                                     predict.spending = clv.newdata.withhold@has.spending, verbose=FALSE))
    # expect_false(anyNA(dt.pred))
    expect_true(all(unique(clv.newdata.nohold@data.transactions$Id) %in% dt.pred$Id))
  })

  # **TODO: Fix date converting
  test_that("Works with different types of prediction.end: number, date, posix, char (long)", {
    skip_on_cran()

    expect_silent(dt.pred.1 <- predict(clv.fitted,prediction.end = 4, verbose=FALSE))
    expect_silent(dt.pred.2 <- predict(clv.fitted,prediction.end = 26, verbose=FALSE))
    expect_silent(dt.pred.3 <- predict(clv.fitted,prediction.end = 104, verbose=FALSE))
    expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
    expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))

    pred.end.char.1 <- as.character(clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::days(30))
    pred.end.char.2 <- as.character(clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::days(180))
    pred.end.char.3 <- as.character(clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::years(1))

    expect_silent(dt.pred.1 <- predict(clv.fitted,prediction.end = pred.end.char.1, verbose=FALSE))
    expect_silent(dt.pred.2 <- predict(clv.fitted,prediction.end = pred.end.char.2, verbose=FALSE))
    expect_silent(dt.pred.3 <- predict(clv.fitted,prediction.end = pred.end.char.3, verbose=FALSE))
    expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
    expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))

    expect_silent(dt.pred.1 <- predict(clv.fitted,prediction.end = lubridate::ymd(pred.end.char.1), verbose=FALSE))
    expect_silent(dt.pred.2 <- predict(clv.fitted,prediction.end = lubridate::ymd(pred.end.char.2), verbose=FALSE))
    expect_silent(dt.pred.3 <- predict(clv.fitted,prediction.end = lubridate::ymd(pred.end.char.3), verbose=FALSE))
    expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
    expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))

    if(lubridate::is.POSIXct(clv.fitted@clv.data@clv.time@timepoint.estimation.start)){
      expect_silent(dt.pred.1 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.1)), verbose=FALSE))
      expect_silent(dt.pred.2 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.2)), verbose=FALSE))
      expect_silent(dt.pred.3 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.3)), verbose=FALSE))
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))

      expect_silent(dt.pred.1 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.1)), verbose=FALSE))
      expect_silent(dt.pred.2 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.2)), verbose=FALSE))
      expect_silent(dt.pred.3 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.3)), verbose=FALSE))
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))
    }else{
      expect_message(dt.pred.1 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.1)), verbose=FALSE), regexp = "ignored")
      expect_message(dt.pred.2 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.2)), verbose=FALSE), regexp = "ignored")
      expect_message(dt.pred.3 <- predict(clv.fitted,prediction.end = as.POSIXct(lubridate::ymd(pred.end.char.3)), verbose=FALSE), regexp = "ignored")
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))

      expect_message(dt.pred.1 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.1)), verbose=FALSE), regexp = "ignored")
      expect_message(dt.pred.2 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.2)), verbose=FALSE), regexp = "ignored")
      expect_message(dt.pred.3 <- predict(clv.fitted,prediction.end = as.POSIXlt(lubridate::ymd(pred.end.char.3)), verbose=FALSE), regexp = "ignored")
      expect_false(isTRUE(all.equal(dt.pred.1, dt.pred.2)))
      expect_false(isTRUE(all.equal(dt.pred.2, dt.pred.3)))
    }



  })

}
