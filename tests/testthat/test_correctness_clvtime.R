# clv.time.epsilon --------------------------------------------------------------------------------
context("Correctness - clv.time - epsilon")

# names.clv.t.datetime <- "clv.t.hours"
# names.clv.t.date <- c("clv.t.days", "clv.t.weeks", "clv.t.months")

for(clv.t in c(fct.helper.clv.time.create.test.objects(with.holdout = FALSE),
               fct.helper.clv.time.create.test.objects(with.holdout = TRUE))){
  if(is(clv.t, "clv.time.date")){
    fct.testthat.correctness.clvtime.epsilon.correct.length.date(clv.t)
  }else{
    fct.testthat.correctness.clvtime.epsilon.correct.length.datetime(clv.t)
  }
}


# set.sample.periods --------------------------------------------------------------------------------
context("Correctness - clv.time - set.sample.periods")

for(clv.t in list(clv.time.hours(time.format="ymd HMS"),
                  clv.time.days( time.format="ymd"),
                  clv.time.weeks(time.format="ymd"),
                  clv.time.years(time.format="ymd"))){

  fct.testthat.correctness.clvtime.set.sample.periods.no.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.numeric.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.warn.partial.period(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.date.estimation.end(clv.t)
  # fct.testthat.correctness.clvtime.set.sample.periods.posixct.estimation.end(clv.t)
}
# **TODO: Test that estimation.start is correct

# test_that("floor date works when setting the estimation period",{
#   tp.first <- as.Date("2018-01-01")
#   tp.last  <- as.Date("2019-06-15")
#
#   expect_silent(t.days <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = NULL, tp.first.transaction =tp.first,
#                                                       tp.last.transaction = tp.last))
#   expect_equal(t.days@timepoint.estimation.start, tp.first)
#
#   expect_silent(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = NULL, tp.first.transaction =tp.first,
#                                                        tp.last.transaction = tp.last))
#   expect_equal(t.weeks@timepoint.estimation.start, tp.first)
#
#   expect_silent(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = NULL, tp.first.transaction =tp.first,
#                                                        tp.last.transaction = tp.last))
#   expect_equal(t.years@timepoint.estimation.start, tp.first)
# })


# test_that("** what if date on last transaction date??")
# parse chars, with / withouth time
# same results with transaction data posixct in different TZs
# same results with transaction data posixct in mixed TZs

# convert.user.input.to.timepoint --------------------------------------------------------------------------------
context("Correctness - clv.time - convert.user.input.to.timepoint")

for(clv.t in c(fct.helper.clv.time.create.test.objects(with.holdout = FALSE),
               fct.helper.clv.time.create.test.objects(with.holdout = TRUE))){
  if(is(clv.t, "clv.time.date")){
    fct.testthat.correctness.clvtime.convert.user.input.chars.to.date(clv.t)
    fct.testthat.correctness.clvtime.convert.user.input.posixct.to.date(clv.t)
    fct.testthat.correctness.clvtime.convert.user.input.date.to.date(clv.t)
  }else{
    fct.testthat.correctness.clvtime.convert.user.input.chars.to.posixct(clv.t)
    fct.testthat.correctness.clvtime.convert.user.input.date.to.posixct(clv.t)
    fct.testthat.correctness.clvtime.convert.user.input.posixct.to.posixct(clv.t)
  }
}



# number.timeunits.to.timeperiod ----------------------------------------------------------------------
context("Correctness - clv.time - number.timeunits.to.timeperiod")
for(clv.t in c(fct.helper.clv.time.create.test.objects(with.holdout = FALSE),
               fct.helper.clv.time.create.test.objects(with.holdout = TRUE))){
  fct.testthat.correctness.clvtime.number.to.time.periods(clv.t)
}




# floor.date --------------------------------------------------------------------------------
context("Correctness - clv.time - floor.date")

test_that("floor date rounds down", {
  # *** TODO: HOURS***
  # Dates
  # Days stay same
  expect_equal(clv.time.floor.date(clv.t.days, timepoint = lubridate::ymd("2019-01-01")),
               lubridate::ymd("2019-01-01")) # Mon
  expect_equal(clv.time.floor.date(clv.t.days, timepoint = lubridate::ymd("2019-01-03")),
               lubridate::ymd("2019-01-03")) # Thu
  expect_equal(clv.time.floor.date(clv.t.days, timepoint = lubridate::ymd("2019-01-06")),
               lubridate::ymd("2019-01-06")) #Sun

  # weeks go to start of week which depends on locale.
  # however wday() always has to be 1 because defined as first day of week
  expect_equal(clv.time.floor.date(clv.t.weeks, timepoint = lubridate::ymd("2019-01-03")),
               lubridate::floor_date(lubridate::ymd("2019-01-03"), "weeks"))
  expect_equal(lubridate::wday(clv.time.floor.date(clv.t.weeks, timepoint = lubridate::ymd("2019-01-05"))),
               1)

  # years go to 1.1
  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2019-01-03")),
               lubridate::ymd("2019-01-01"))
  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2019-06-06")),
               lubridate::ymd("2019-01-01"))
  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2019-12-31")),
               lubridate::ymd("2019-01-01"))
  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2018-10-28")),
               lubridate::ymd("2018-01-01"))
})

test_that("floor date stays when already correct", {

  expect_equal(clv.time.floor.date(clv.t.days, timepoint = lubridate::ymd("2018-01-01")),
               lubridate::ymd("2018-01-01"))
  expect_equal(clv.time.floor.date(clv.t.days, timepoint = lubridate::ymd("2019-06-13")),
               lubridate::ymd("2019-06-13"))

  # unclear which day of week is first (**maybe test 7 days?)

  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2018-01-01")),
               lubridate::ymd("2018-01-01"))
  expect_equal(clv.time.floor.date(clv.t.years, timepoint = lubridate::ymd("2019-01-01")),
               lubridate::ymd("2019-01-01"))
})





# sequence.of.covariate.timepoints --------------------------------------------------------------------------------
context("Correctness - clv.time - sequence.of.covariate.timepoints")

# expect_that difference to end is never >= 1 time.unit

test_that("Cov dates start and end correct for start off and end off period begin", {
  tp.cov.start <- lubridate::ymd("2018-01-11") #Thu
  tp.cov.end <- lubridate::ymd("2019-01-25") #Thu
  # days
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
                                                            tp.start = tp.cov.start,
                                                            tp.end = tp.cov.end)
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-11"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2019-01-25"))

  # weeks
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.weeks,
                                                            tp.start = tp.cov.start,
                                                            tp.end = tp.cov.end)
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = tp.cov.start))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = tp.cov.end))

  # years
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.years,
                                                            tp.start = lubridate::ymd("2012-06-06"),
                                                            tp.end = lubridate::ymd("2021-12-31"))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2012-01-01"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2021-01-01"))
})


test_that("Cov dates start and end correct for start on and end off period begin", {

  # days
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
                                                            tp.start = lubridate::ymd("2018-01-10"),
                                                            tp.end = lubridate::ymd("2018-01-26"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-10"),
                                                    to=lubridate::ymd("2018-01-26"),
                                                    by="1 day")))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-10"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2018-01-26"))

  # weeks
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.weeks,
                                                            tp.start = clv.time.floor.date(clv.time = clv.t.weeks,
                                                                                           timepoint = lubridate::ymd("2018-01-10")),
                                                            tp.end = lubridate::ymd("2018-02-01")) # Thu
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=clv.time.floor.date(clv.time = clv.t.weeks,
                                                                             timepoint = lubridate::ymd("2018-01-10")),
                                                    to=clv.time.floor.date(clv.time = clv.t.weeks,
                                                                           timepoint = lubridate::ymd("2018-02-01")), #Thu
                                                    by="1 week")))

  expect_true(dt.cov.seq.d[, min(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-01-10")))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-02-01")))

  # years
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.years,
                                                            tp.start = lubridate::ymd("2018-01-01"),
                                                            tp.end = lubridate::ymd("2020-06-06"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-01"),
                                                    to=lubridate::ymd("2020-01-01"),
                                                    by="1 years")))

  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-01"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2020-01-01"))
})

test_that("Cov dates start and end correct for start off and end on period begin", {

  # days
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
                                                            tp.start = lubridate::ymd("2018-01-10"),
                                                            tp.end = lubridate::ymd("2018-01-26"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-10"),
                                                    to=lubridate::ymd("2018-01-26"),
                                                    by="1 day")))

  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-10"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2018-01-26"))

  # weeks
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.weeks,
                                                            tp.start = lubridate::ymd("2018-01-04"),
                                                            tp.end = clv.time.floor.date(clv.time = clv.t.weeks,
                                                                                         timepoint = lubridate::ymd("2018-01-25")))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=clv.time.floor.date(clv.time = clv.t.weeks,
                                                                             timepoint = lubridate::ymd("2018-01-04")),
                                                    to = clv.time.floor.date(clv.time = clv.t.weeks,
                                                                             timepoint = lubridate::ymd("2018-01-25")),
                                                    by="1 week")))

  expect_true(dt.cov.seq.d[, min(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-01-04")))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-01-25")))

  # years
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.years,
                                                            tp.start = lubridate::ymd("2018-06-06"),
                                                            tp.end = lubridate::ymd("2020-01-01"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-01"),
                                                    to=lubridate::ymd("2020-01-01"),
                                                    by="1 years")))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-01"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2020-01-01"))

})

test_that("Cov dates start and end correct for start on and end on period begin", {

  # days
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
                                                            tp.start = lubridate::ymd("2018-01-10"),
                                                            tp.end = lubridate::ymd("2019-12-26"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-10"),
                                                    to=lubridate::ymd("2019-12-26"),
                                                    by="1 day")))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-10"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2019-12-26"))

  # weeks
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.weeks,
                                                            tp.start = lubridate::ymd("2018-01-11"),
                                                            tp.end = lubridate::ymd("2018-01-25"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=clv.time.floor.date(clv.time = clv.t.weeks,
                                                                             timepoint = lubridate::ymd("2018-01-11")), #Thu
                                                    to = clv.time.floor.date(clv.time = clv.t.weeks,
                                                                             timepoint = lubridate::ymd("2018-01-25")), #Thu
                                                    by="1 week")))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-01-11")))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == clv.time.floor.date(clv.time = clv.t.weeks,
                                                                   timepoint = lubridate::ymd("2018-01-25")))

  # years
  dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.years,
                                                            tp.start = lubridate::ymd("2018-01-01"),
                                                            tp.end = lubridate::ymd("2020-01-01"))
  expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-01"),
                                                    to=lubridate::ymd("2020-01-01"),
                                                    by="1 years")))
  expect_true(dt.cov.seq.d[, min(Cov.Date)] == lubridate::ymd("2018-01-01"))
  expect_true(dt.cov.seq.d[, max(Cov.Date)] == lubridate::ymd("2020-01-01"))

})


# test_that("Correctly works for single period", {
#   # days
#   dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
#                                                             tp.start = lubridate::ymd("2018-01-1"),
#                                                             tp.end = lubridate::ymd("2019-12-26"))
#   expect_true(all(dt.cov.seq.d$Cov.Date == seq.Date(from=lubridate::ymd("2018-01-10"),
#                                                      to=lubridate::ymd("2019-12-26"),
#                                                      by="1 day")))
# })



# clv.time.get.prediction.table --------------------------------------------------------------------------------
context("Correctness - clv.time - get.prediction.table")

# - check its usage in newdata
# - generally check that prediction.end=0 -> CET=0

for(clv.t in c(fct.helper.clv.time.create.test.objects(with.holdout = TRUE),
               fct.helper.clv.time.create.test.objects(with.holdout = FALSE))){
  fct.testthat.correctness.clvtime.prediction.table.valid.for.numeric.end(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.valid.for.date.end(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.correct.for.0.length.period(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.stop.for.prediction.end.before.estimation.end(clv.t)
}


