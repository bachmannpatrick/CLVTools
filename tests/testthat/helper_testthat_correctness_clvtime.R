fct.helper.clv.time.create.test.objects <- function(with.holdout){
  expect_silent(clv.t.hours <- clv.time.hours(time.format="ymd HMS"))
  expect_silent(clv.t.days  <- clv.time.days( time.format="ymd"))
  expect_silent(clv.t.weeks <- clv.time.weeks(time.format="ymd"))
  expect_silent(clv.t.years <- clv.time.years(time.format="ymd"))

  pred.tp.first <- as.Date("2005-01-20")
  pred.tp.last  <- as.Date("2008-09-27")
  if(with.holdout){
    expect_silent(clv.t.hours <-clv.time.set.sample.periods(clv.t.hours, user.estimation.end = 37,
                                                            tp.first.transaction = lubridate::ymd("2005-01-20", tz="UTC"),
                                                            tp.last.transaction = lubridate::ymd("2008-09-27", tz="UTC")))
    expect_silent(clv.t.days <-clv.time.set.sample.periods(clv.t.days, user.estimation.end = 37,
                                                           tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
    expect_silent(clv.t.weeks <-clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = 37,
                                                            tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
    expect_silent(clv.t.years <-clv.time.set.sample.periods(clv.t.years, user.estimation.end = 1,
                                                            tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
  }else{
    expect_silent(clv.t.hours <-clv.time.set.sample.periods(clv.t.hours, user.estimation.end = NULL,
                                                            tp.first.transaction = lubridate::ymd("2005-01-20", tz="UTC"),
                                                            tp.last.transaction = lubridate::ymd("2008-09-27", tz="UTC")))
    expect_silent(clv.t.days <-clv.time.set.sample.periods(clv.t.days, user.estimation.end = NULL,
                                                           tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
    expect_silent(clv.t.weeks <-clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = NULL,
                                                            tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
    expect_silent(clv.t.years <-clv.time.set.sample.periods(clv.t.years, user.estimation.end = NULL,
                                                            tp.first.transaction = pred.tp.first, tp.last.transaction = pred.tp.last))
  }

  return(list(clv.t.hours = clv.t.hours, clv.t.days = clv.t.days, clv.t.weeks = clv.t.weeks, clv.t.years = clv.t.years))
}

# >0-length period:
#   - period.first = estimation.end + 1 epsilon (not holdout.start if no holdout period)
#   - period.last > period.first
#   - period.length = num periods that is [period.first, period.last], including boundaries
# Dont test for 0-length period because "different" valid table
fct.helper.test.valid.prediction.table <- function(dt.prediction.table, clv.t){
  expect_true(nrow(dt.prediction.table) == 1)
  expect_true(dt.prediction.table[, period.first] == clv.t@timepoint.estimation.end + clv.time.epsilon(clv.t))
  expect_true(dt.prediction.table[, period.last >= period.first])

  # +1epsilon to include boundaries, ie so that Monday-Sunday counts as a week (Mo-So is only 0.85 weeks)
  expect_true(dt.prediction.table[, period.length] ==
                clv.time.interval.in.number.tu(clv.time=clv.t,
                                               interv=interval(start = dt.prediction.table[, period.first],
                                                               end = dt.prediction.table[, period.last] +
                                                                 clv.time.epsilon(clv.t))))
}

fct.testthat.correctness.clvtime.prediction.table.valid.for.numeric.end <- function(clv.t){
  # Additionally: prediction.length is the same as prediction.end in numeric
  test_that("Valid prediction table for prediction.end as numeric", {
    for(numeric.prediction.end in c(1, 2, 5)){
      dt.pred <- clv.time.get.prediction.table(clv.time = clv.t,
                                               user.prediction.end = numeric.prediction.end)
      fct.helper.test.valid.prediction.table(dt.pred, clv.t = clv.t)
      expect_true(dt.pred[, period.length] == numeric.prediction.end)
    }
  })
}

fct.testthat.correctness.clvtime.prediction.table.valid.for.date.end <- function(clv.t){
  # Additionally: period.last = prediction.end in date
  test_that("valid prediction table for prediction.end as date", {
    for(date.prediction.end in c(as.character(clv.t@timepoint.estimation.end+lubridate::days(1)),
                                 as.character(clv.t@timepoint.estimation.end+lubridate::days(30)),
                                 as.character(clv.t@timepoint.estimation.end+lubridate::days(700)))){
      if(is(clv.t, "clv.time.date")){
        date.prediction.end <- lubridate::ymd(date.prediction.end)
      }else{
        date.prediction.end <- lubridate::parse_date_time(date.prediction.end,
                                                          orders = c("ymd", "ymd HMS"))
      }

      dt.pred <- clv.time.get.prediction.table(clv.time = clv.t,
                                               user.prediction.end = date.prediction.end)
      fct.helper.test.valid.prediction.table(dt.pred, clv.t = clv.t)
      expect_true(dt.pred[, period.last] == date.prediction.end)
    }
  })
}



fct.testthat.correctness.clvtime.prediction.table.correct.for.0.length.period <- function(clv.t){
  test_that("Correct table for no prediction length: period.first and period.last same date and length=0", {
    # 0-length period:
    #   - period.first = period.last = estimation.end
    #   - period.length = 0
    fct.correct.prediction.end.0 <- function(dt.pred.table){
      expect_true(dt.pred.table[, period.last] == clv.t@timepoint.estimation.end)
      expect_true(dt.pred.table[, period.last] == dt.pred.table[, period.first])
      expect_true(dt.pred.table[, period.length] == 0)
    }

    # Numeric: 0
    fct.correct.prediction.end.0(clv.time.get.prediction.table(clv.time = clv.t, user.prediction.end = 0))

    # Date: Same as holdout start
    fct.correct.prediction.end.0(clv.time.get.prediction.table(clv.time = clv.t,
                                                               user.prediction.end = clv.t@timepoint.estimation.end))
  })
}


fct.testthat.correctness.clvtime.prediction.table.stop.for.prediction.end.before.estimation.end <- function(clv.t){
  test_that("Stop for prediction.end before estimation end", {
    # Numeric
    expect_error(clv.time.get.prediction.table(clv.time = clv.t, user.prediction.end = -1),
                 regexp = "after the estimation period")
    expect_error(clv.time.get.prediction.table(clv.time = clv.t, user.prediction.end = -2),
                 regexp = "after the estimation period")

    # Date
    expect_error(clv.time.get.prediction.table(clv.time = clv.t,
                                               user.prediction.end = clv.t@timepoint.estimation.end-lubridate::days(1)),
                 regexp = "after the estimation period")
    expect_error(clv.time.get.prediction.table(clv.time = clv.t,
                                               user.prediction.end = clv.t@timepoint.estimation.end-lubridate::seconds(1)),
                 regexp = "after the estimation period")
    # holdout - 1 week is before estimation.end
    expect_error(clv.time.get.prediction.table(clv.time = clv.t,
                                               user.prediction.end = clv.t@timepoint.holdout.start-lubridate::weeks(1)),
                 regexp = "after the estimation period")
  })
}
