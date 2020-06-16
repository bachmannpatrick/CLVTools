# clv.time.epsilon --------------------------------------------------------------------------------
context("Correctness - clv.time - epsilon")


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
  fct.testthat.correctness.clvtime.set.sample.periods.estimation.start(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.no.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.numeric.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.date.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.posixct.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.char.estimation.end(clv.t)
  fct.testthat.correctness.clvtime.set.sample.periods.warn.partial.period(clv.t)
}
fct.testthat.correctness.clvtime.set.sample.periods.stop.estimation.period.less.1.period(clv.t.hours = clv.time.hours(time.format="ymd HMS") ,
                                                                                         clv.t.days  = clv.time.days(time.format="ymd"),
                                                                                         clv.t.weeks = clv.time.weeks(time.format="ymd"),
                                                                                         clv.t.years = clv.time.years(time.format="ymd"))

fct.testthat.correctness.clvtime.set.sample.periods.stop.holdout.length.less.2.period(clv.t.hours = clv.time.hours(time.format="ymd HMS") ,
                                                                                      clv.t.days  = clv.time.days(time.format="ymd"),
                                                                                      clv.t.weeks = clv.time.weeks(time.format="ymd"),
                                                                                      clv.t.years = clv.time.years(time.format="ymd"))

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
for(holdout in c(TRUE, FALSE)){
  l.clv.t <- fct.helper.clv.time.create.test.objects(with.holdout = holdout)
  fct.testthat.correctness.clvtime.floor.date.rounds.down(clv.t.hours = l.clv.t[["clv.t.hours"]],
                                                          clv.t.days  = l.clv.t[["clv.t.days"]],
                                                          clv.t.weeks = l.clv.t[["clv.t.weeks"]],
                                                          clv.t.years = l.clv.t[["clv.t.years"]])
  fct.testthat.correctness.clvtime.floor.date.stays.when.correct(clv.t.hours = l.clv.t[["clv.t.hours"]],
                                                                 clv.t.days  = l.clv.t[["clv.t.days"]],
                                                                 clv.t.years = l.clv.t[["clv.t.years"]])
}



# sequence.of.covariate.timepoints --------------------------------------------------------------------------------
context("Correctness - clv.time - sequence.of.covariate.timepoints")

# expect_that difference to end is never >= 1 time.unit
for(holdout in c(TRUE, FALSE)){
  l.clv.t <- fct.helper.clv.time.create.test.objects(with.holdout = holdout)
  clv.t.days  <- l.clv.t[["clv.t.days"]]
  clv.t.weeks <- l.clv.t[["clv.t.weeks"]]
  clv.t.years <- l.clv.t[["clv.t.years"]]
  fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.off.end.off.period(clv.t.days=clv.t.days, clv.t.weeks=clv.t.weeks, clv.t.years=clv.t.years)
  fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.on.end.off.period(clv.t.days=clv.t.days, clv.t.weeks=clv.t.weeks, clv.t.years=clv.t.years)
  fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.off.end.on.period(clv.t.days=clv.t.days, clv.t.weeks=clv.t.weeks, clv.t.years=clv.t.years)
  fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.on.end.on.period(clv.t.days=clv.t.days, clv.t.weeks=clv.t.weeks, clv.t.years=clv.t.years)
}

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

for(clv.t in c(fct.helper.clv.time.create.test.objects(with.holdout = TRUE),
               fct.helper.clv.time.create.test.objects(with.holdout = FALSE))){
  fct.testthat.correctness.clvtime.prediction.table.valid.for.numeric.end(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.valid.for.date.end(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.correct.for.0.length.period(clv.t)
  fct.testthat.correctness.clvtime.prediction.table.stop.for.prediction.end.before.estimation.end(clv.t)
}


