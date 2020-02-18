# set.sample.periods --------------------------------------------------------------------------------
context("Correctness - clv.time - set.sample.periods")

expect_silent(clv.t.hours <- clv.time.hours(time.format="ymd HMS"))
expect_silent(clv.t.days  <- clv.time.days( time.format="ymd"))
expect_silent(clv.t.weeks <- clv.time.weeks(time.format="ymd"))
expect_silent(clv.t.years <- clv.time.years(time.format="ymd"))

# **last transaction or time period where last transaction is inside?
test_that("No (NULL) estimation split results in last transaction = estimation end & no holdout", {
  tp.first <- as.Date("2018-01-01")
  tp.last  <- as.Date("2019-06-15")
  tp.first.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.first), tz = "UTC")
  tp.last.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.last), tz = "UTC")

  # Posixct
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = NULL, tp.first.transaction =tp.first.posix,
                                                       tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, tp.last.posix)
  expect_equal(t.hours@timepoint.holdout.start, tp.last.posix)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)
  expect_equal(t.hours@holdout.period.in.tu, 0)

  # Dates
  expect_silent(t.days <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.days@timepoint.estimation.end, tp.last)
  expect_equal(t.days@timepoint.holdout.start, tp.last)
  expect_equal(t.days@timepoint.holdout.end, tp.last)
  expect_equal(t.days@holdout.period.in.tu, 0)

  expect_silent(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.weeks@timepoint.estimation.end, tp.last)
  expect_equal(t.weeks@timepoint.holdout.start, tp.last)
  expect_equal(t.weeks@timepoint.holdout.end, tp.last)
  expect_equal(t.weeks@holdout.period.in.tu, 0)

  expect_silent(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
  expect_equal(t.years@timepoint.estimation.end, tp.last)
  expect_equal(t.years@timepoint.holdout.start, tp.last)
  expect_equal(t.years@timepoint.holdout.end, tp.last)
  expect_equal(t.weeks@holdout.period.in.tu, 0)
})

test_that("estimation split numeric results in estimation end = this many periods", {
  tp.first <- as.Date("2018-01-01")
  tp.last  <- as.Date("2025-06-15")
  tp.first.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.first), tz = "UTC")
  tp.last.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.last), tz = "UTC")

  # POSIX
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = 41*7*24, tp.first.transaction =tp.first.posix,
                                                      tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, tp.first.posix+lubridate::period(41*7*24, "hours"))
  expect_equal(t.hours@estimation.period.in.tu, 41*7*24)
  expect_equal(t.hours@timepoint.holdout.start, t.hours@timepoint.estimation.end+1L)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)

  # Dates
  expect_silent(t.days <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = 41*7, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.days@timepoint.estimation.end, tp.first+lubridate::period(41*7, "days"))
  expect_equal(t.days@estimation.period.in.tu, 41*7)
  expect_equal(t.days@timepoint.holdout.start, t.days@timepoint.estimation.end+1L)
  expect_equal(t.days@timepoint.holdout.end, tp.last)

  expect_silent(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = 41, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
  expect_equal(t.weeks@timepoint.estimation.end, tp.first+lubridate::period(41, "weeks"))
  expect_equal(t.weeks@estimation.period.in.tu, 41)
  expect_equal(t.weeks@timepoint.holdout.start, t.weeks@timepoint.estimation.end+1L)
  expect_equal(t.weeks@timepoint.holdout.end, tp.last)

  expect_silent(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = 1, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
  expect_equal(t.years@timepoint.estimation.end, tp.first+lubridate::period(1, "years"))
  expect_equal(t.years@estimation.period.in.tu, 1)
  expect_equal(t.years@timepoint.holdout.start, t.years@timepoint.estimation.end+1L)
  expect_equal(t.years@timepoint.holdout.end, tp.last)
})


test_that("Warn if numeric estimation implies partial periods", {
  tp.first <- as.Date("2018-01-01")
  tp.last  <- as.Date("2025-06-15")
  expect_warning(t.weeks <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = 287.75, tp.first.transaction =tp.first,
                                                        tp.last.transaction = tp.last),
                 regexp = "partial periods")
  expect_equal(t.weeks@estimation.period.in.tu, 287)

  expect_warning(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = 41.5, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last),
                 regexp = "partial periods")
  expect_equal(t.weeks@estimation.period.in.tu, 41)

  expect_warning(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = 1.5, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last),
                 regexp = "partial periods")
  expect_equal(t.years@estimation.period.in.tu, 1)
})

test_that("estimation split Date results in estimation end = on this date", {
  tp.first <- as.Date("2018-01-01")
  tp.split <- as.Date("2019-07-19")
  tp.last  <- as.Date("2025-06-15")
  tp.first.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.first), tz = "UTC")
  tp.last.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.last), tz = "UTC")
  tp.split.posix <- as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.split), tz = "UTC")



  # POSIX - but split with Date (ymd by user)
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = tp.split, tp.first.transaction = tp.first.posix,
                                                      tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, tp.split.posix) # Split same Date but as posix
  expect_equal(t.hours@estimation.period.in.tu, time_length(tp.split.posix-tp.first.posix, "hours"))
  expect_equal(t.hours@timepoint.holdout.start, t.hours@timepoint.estimation.end+1L)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)

  # Dates
  expect_silent(t.days <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = tp.split, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.days@timepoint.estimation.end, tp.split)
  expect_equal(t.days@estimation.period.in.tu, time_length(tp.split-tp.first, "days"))
  expect_equal(t.days@timepoint.holdout.start, t.days@timepoint.estimation.end+1L)
  expect_equal(t.days@timepoint.holdout.end, tp.last)

  expect_silent(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = tp.split, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.weeks@timepoint.estimation.end, tp.split)
  expect_equal(t.weeks@estimation.period.in.tu, time_length(tp.split-tp.first, "weeks"))
  expect_equal(t.weeks@timepoint.holdout.start, t.weeks@timepoint.estimation.end+1L)
  expect_equal(t.weeks@timepoint.holdout.end, tp.last)

  expect_silent(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = tp.split, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
  expect_equal(t.years@timepoint.estimation.end, tp.split)
  expect_equal(t.years@estimation.period.in.tu, time_length(tp.split-tp.first, "years"))
  expect_equal(t.years@timepoint.holdout.start, t.years@timepoint.estimation.end+1L)
  expect_equal(t.years@timepoint.holdout.end, tp.last)
})


test_that("estimation split POSIXct results in estimation end = on this date", {
  # Split on
  tp.first.posix <- lubridate::ymd("2018-01-01", tz="UTC")
  tp.last.posix  <- lubridate::ymd("2019-06-15", tz="UTC")

  # Midnight
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = lubridate::ymd_hms("2018-05-01 00:00:00"),
                                                       tp.first.transaction = tp.first.posix, tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, lubridate::ymd("2018-05-01", tz="UTC"))
  expect_equal(t.hours@estimation.period.in.tu, time_length(lubridate::ymd("2018-05-01", tz="UTC")-tp.first.posix, "hours"))
  expect_equal(t.hours@timepoint.holdout.start, t.hours@timepoint.estimation.end+1L)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)

  # with hour
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = lubridate::ymd_hms("2018-05-01 14:00:00"),
                                                       tp.first.transaction = tp.first.posix, tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, lubridate::ymd_hms("2018-05-01 14:00:00"))
  expect_equal(t.hours@estimation.period.in.tu, time_length(lubridate::ymd_hms("2018-05-01 14:00:00")-tp.first.posix, "hours"))
  expect_equal(t.hours@timepoint.holdout.start, t.hours@timepoint.estimation.end+1L)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)

  # halfhour
  expect_silent(t.hours <- clv.time.set.sample.periods(clv.t.hours, user.estimation.end = lubridate::ymd_hms("2018-05-01 14:35:43"),
                                                       tp.first.transaction = tp.first.posix, tp.last.transaction = tp.last.posix))
  expect_equal(t.hours@timepoint.estimation.end, lubridate::ymd_hms("2018-05-01 14:35:43"))
  expect_equal(t.hours@estimation.period.in.tu, time_length(lubridate::ymd_hms("2018-05-01 14:35:43")-tp.first.posix, "hours"))
  expect_equal(t.hours@timepoint.holdout.start, t.hours@timepoint.estimation.end+1L)
  expect_equal(t.hours@timepoint.holdout.end, tp.last.posix)

})

# test_that("** what if date on last transaction date??")
# parse chars, with / withouth time
# same results with transaction data posixct in different TZs
# same results with transaction data posixct in mixed TZs

# convert.user.input.to.timepoint --------------------------------------------------------------------------------
context("Correctness - clv.time - convert.user.input.to.timepoint")

test_that("Chars convert to correct POSIX", {
  # Midnight
  expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-01-01 00:00:00"))
  # partial hours
  expect_equal(lubridate::ymd_hms("2019-12-18 05:00:01", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 05:00:01"))
  expect_equal(lubridate::ymd_hms("2019-12-18 05:32:03", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 05:32:03"))
  expect_equal(lubridate::ymd_hms("2019-12-18 14:59:59", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 14:59:59"))
  expect_equal(lubridate::ymd_hms("2019-12-18 23:59:59", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 23:59:59"))
  # exact hours
  expect_equal(lubridate::ymd_hms("2019-12-18 11:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 11:00:00"))
  expect_equal(lubridate::ymd_hms("2019-12-18 22:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 22:00:00"))
  expect_equal(lubridate::ymd_hms("2019-12-18 23:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = "2019-12-18 23:00:00"))
})

test_that("Chars convert to correct Dates", {
  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = "2019-01-01"))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = "2019-12-18"))

  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = "2019-01-01"))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = "2019-12-18"))

  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = "2019-01-01"))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = "2019-12-18"))
})


test_that("Dates convert to correct POSIX", {
  expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd("2019-01-01")))
  expect_equal(lubridate::ymd_hms("2019-12-18 00:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd("2019-12-18")))
})

test_that("Dates convert to correct Dates", {
  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = lubridate::ymd("2019-01-01")))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = lubridate::ymd("2019-12-18")))

  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = lubridate::ymd("2019-01-01")))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = lubridate::ymd("2019-12-18")))

  expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = lubridate::ymd("2019-01-01")))
  expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = lubridate::ymd("2019-12-18")))
})

test_that("POSIXct convert to correct POSIXct", {
  # Midnight
  expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-01-01 00:00:00",tz="UTC")))
  # partial hours
  expect_equal(lubridate::ymd_hms("2019-12-18 05:00:01", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 05:00:01",tz="UTC")))
  expect_equal(lubridate::ymd_hms("2019-12-18 05:32:03", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 05:32:03",tz="UTC")))
  expect_equal(lubridate::ymd_hms("2019-12-18 14:59:59", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 14:59:59",tz="UTC")))
  expect_equal(lubridate::ymd_hms("2019-12-18 23:59:59", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 23:59:59",tz="UTC")))
  # exact hours
  expect_equal(lubridate::ymd_hms("2019-12-18 11:00:00", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 11:00:00",tz="UTC")))
  expect_equal(lubridate::ymd_hms("2019-12-18 22:00:00", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 22:00:00",tz="UTC")))
  expect_equal(lubridate::ymd_hms("2019-12-18 23:00:00", tz="UTC"),
               clv.time.convert.user.input.to.timepoint(clv.t.hours, user.timepoint = lubridate::ymd_hms("2019-12-18 23:00:00",tz="UTC")))
})

test_that("POSIXct convert to correct Dates", {
  # outputs cuttoff
  expect_message(d.1 <- clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = lubridate::ymd_hms("2019-01-01 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_message(d.2 <- clv.time.convert.user.input.to.timepoint(clv.t.days, user.timepoint = lubridate::ymd_hms("2019-12-18 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_equal(lubridate::ymd("2019-01-01"), d.1)
  expect_equal(lubridate::ymd("2019-12-18"), d.2)

  expect_message(w.1 <- clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = lubridate::ymd_hms("2019-01-01 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_message(w.2 <- clv.time.convert.user.input.to.timepoint(clv.t.weeks, user.timepoint = lubridate::ymd_hms("2019-12-18 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_equal(lubridate::ymd("2019-01-01"), w.1)
  expect_equal(lubridate::ymd("2019-12-18"), w.2)

  expect_message(y.1 <- clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = lubridate::ymd_hms("2019-01-01 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_message(y.2 <- clv.time.convert.user.input.to.timepoint(clv.t.years, user.timepoint = lubridate::ymd_hms("2019-12-18 03:19:54", tz = "UTC")), regexp = "ignored")
  expect_equal(lubridate::ymd("2019-01-01"), y.1)
  expect_equal(lubridate::ymd("2019-12-18"), y.2)
})




# number.timeunits.to.timeperiod ----------------------------------------------------------------------
context("Correctness - clv.time - number.timeunits.to.timeperiod")
test_that("Correct time period returned", {
  expect_equal(lubridate::period(4, "hours"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.hours, user.number.periods=4))
  expect_equal(lubridate::period(4, "hours"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.hours, user.number.periods=4.5))

  expect_equal(lubridate::period(4, "days"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.days, user.number.periods=4))
  expect_equal(lubridate::period(4, "days"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.days, user.number.periods=4.5))

  expect_equal(lubridate::period(4, "weeks"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.weeks, user.number.periods=4))
  expect_equal(lubridate::period(4, "weeks"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.weeks, user.number.periods=4.5))

  expect_equal(lubridate::period(4, "years"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.years, user.number.periods=4))
  expect_equal(lubridate::period(4, "years"), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t.years, user.number.periods=4.5))
})



# floor.date --------------------------------------------------------------------------------
context("Correctness - clv.time - floor.date")

test_that("floor date rounds down", {
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

test_that("floor date works when setting the estimation period",{
  tp.first <- as.Date("2018-01-01")
  tp.last  <- as.Date("2019-06-15")

  expect_silent(t.days <- clv.time.set.sample.periods(clv.t.days, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.days@timepoint.estimation.start, tp.first)

  expect_silent(t.weeks <- clv.time.set.sample.periods(clv.t.weeks, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                      tp.last.transaction = tp.last))
  expect_equal(t.weeks@timepoint.estimation.start, tp.first)

  expect_silent(t.years <- clv.time.set.sample.periods(clv.t.years, user.estimation.end = NULL, tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
  expect_equal(t.years@timepoint.estimation.start, tp.first)
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

# Is not allowed because never used. Also when predicting minum of 3 periods required.
# test_that("Correctly works for single period", {
#   # days
#   dt.cov.seq.d <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.t.days,
#                                                             tp.start = lubridate::ymd("2018-01-1"),
#                                                             tp.end = lubridate::ymd("2019-12-26"))
#   expect_true(all(dt.cov.seq.d$Cov.Date) == seq.Date(from=lubridate::ymd("2018-01-10"),
#                                                      to=lubridate::ymd("2019-01-26"),
#                                                      by="1 day"))
# })

