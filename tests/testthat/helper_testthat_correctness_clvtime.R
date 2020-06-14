fct.helper.clv.time.period.type <- function(clv.t){
  period.type <- switch(class(clv.t),
                        "clv.time.hours" = "hours",
                        "clv.time.days"  = "days",
                        "clv.time.weeks" = "weeks",
                        "clv.time.years" = "years",
                        NULL)
  stopifnot(!is.null(period.type))
  return(period.type)
}

fct.helper.clv.time.correct.datetype <- function(date.char, clv.t){
  d <- lubridate::ymd(date.char)
  if(is(clv.t, "clv.time.datetime")){
    d <- as.POSIXct.POSIXlt(as.POSIXlt.Date(d), tz = "UTC")
  }
  return(d)
}

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

# epsilon ---------------------------------------------------------------------------------------------

fct.testthat.correctness.clvtime.epsilon.correct.length.date <- function(clv.t){
  test_that("All date classes have epsilon of 1 day", {
    # returns correct
    expect_equal(as.numeric(days(1L), "days"), 1)
    expect_equal(as.numeric(clv.time.epsilon(clv.t),  units="days"), 1)

    # Operations correct
    # Plus
    expect_equal(lubridate::ymd("2020-01-01") + clv.time.epsilon(clv.t),  lubridate::ymd("2020-01-02"))
    # Minus
    expect_equal(lubridate::ymd("2020-01-01") - clv.time.epsilon(clv.t),  lubridate::ymd("2019-12-31"))

    # same as 1L
    expect_equal(lubridate::ymd("2020-01-01") + clv.time.epsilon(clv.t)  - 1L, lubridate::ymd("2020-01-01"))
  })
}

fct.testthat.correctness.clvtime.epsilon.correct.length.datetime <- function(clv.t){

  test_that("All datetime classes have epsilon of 1 second", {
    # returns correct
    expect_equal(as.numeric(clv.time.epsilon(clv.t),  units="seconds"), 1)

    # Operations correct
    expect_equal(lubridate::ymd_hms("2020-01-01 00:00:00", tz="UTC") + clv.time.epsilon(clv.t),
                 lubridate::ymd_hms("2020-01-01 00:00:01", tz="UTC"))

    expect_equal(lubridate::ymd_hms("2020-01-01 00:00:01", tz="UTC") - clv.time.epsilon(clv.t),
                 lubridate::ymd_hms("2020-01-01 00:00:00", tz="UTC"))

    # same as +1L
    expect_equal(lubridate::ymd_hms("2020-01-01 00:00:01", tz="UTC") + clv.time.epsilon(clv.t) - 1L,
                 lubridate::ymd_hms("2020-01-01 00:00:01", tz="UTC"))
  })

}


# set.sample.periods ---------------------------------------------------------------------------------------------
fct.testthat.correctness.clvtime.set.sample.periods.estimation.start <- function(clv.t){
  test_that("Estimation start is first transaction", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2019-06-15", clv.t)

    expect_silent(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = NULL,
                                                       tp.first.transaction = tp.first,
                                                       tp.last.transaction = tp.last))
    expect_equal(clv.t@timepoint.estimation.start, tp.first)

  })
}

fct.testthat.correctness.clvtime.set.sample.periods.no.estimation.end <- function(clv.t){

  # **last transaction or time period where last transaction is inside?
  test_that("No (NULL) estimation split results in last transaction = estimation end & no holdout", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2019-06-15", clv.t)

    # Dates
    expect_silent(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = NULL, tp.first.transaction = tp.first,
                                                       tp.last.transaction = tp.last))
    expect_equal(clv.t@timepoint.estimation.end, tp.last)
    expect_equal(clv.t@timepoint.holdout.start, tp.last)
    expect_equal(clv.t@timepoint.holdout.end, tp.last)
    expect_equal(clv.t@holdout.period.in.tu, 0)
  })
}
fct.testthat.correctness.clvtime.set.sample.periods.numeric.estimation.end <- function(clv.t){
  test_that("estimation split numeric results in estimation end = this many periods", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)

    period.type <- fct.helper.clv.time.period.type(clv.t)

    splitting.end <- switch(class(clv.t),
                            "clv.time.hours" = 41*7*24,
                            "clv.time.days"  = 41*7,
                            "clv.time.weeks" = 41,
                            "clv.time.years" = 1,
                            NULL)
    stopifnot(!is.null(splitting.end))

    expect_silent(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = splitting.end,
                                                       tp.first.transaction =tp.first,
                                                       tp.last.transaction = tp.last))
    expect_equal(clv.t@timepoint.estimation.end, tp.first+lubridate::period(splitting.end, period.type))
    expect_equal(clv.t@estimation.period.in.tu, splitting.end)
    expect_equal(clv.t@timepoint.holdout.start, clv.t@timepoint.estimation.end+1L)
    expect_equal(clv.t@timepoint.holdout.end, tp.last)
  })
}

fct.testthat.correctness.clvtime.set.sample.periods.warn.partial.period <- function(clv.t){
  test_that("Warn if numeric estimation implies partial periods", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)

    splitting.end <- switch(class(clv.t),
                            "clv.time.hours" = 6888.5,
                            "clv.time.days"  = 287.5,
                            "clv.time.weeks" = 41.5,
                            "clv.time.years" = 1.5,
                            NULL)
    stopifnot(!is.null(splitting.end))

    expect_warning(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = splitting.end,
                                                        tp.first.transaction =tp.first,
                                                        tp.last.transaction = tp.last),
                   regexp = "partial periods")
    expect_equal(clv.t@estimation.period.in.tu, floor(splitting.end))
  })
}


fct.testthat.correctness.clvtime.set.sample.periods.stop.estimation.period.less.1.period <- function(clv.t.hours, clv.t.days,
                                                                                                     clv.t.weeks, clv.t.years){

  for(clv.t in list(clv.t.hours, clv.t.days, clv.t.weeks, clv.t.years)){
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)
    # Numeric
    expect_error(clv.time.set.sample.periods(clv.t,
                                             user.estimation.end  = 0,
                                             tp.first.transaction = tp.first,
                                             tp.last.transaction  = tp.last),
                 regexp = "1 time.unit after")
    expect_error(clv.time.set.sample.periods(clv.t,
                                             user.estimation.end  = -3,
                                             tp.first.transaction = tp.first,
                                             tp.last.transaction  = tp.last),
                 regexp = "1 time.unit after")
    # Date
    expect_error(clv.time.set.sample.periods(clv.t,
                                             user.estimation.end  = tp.first,
                                             tp.first.transaction = tp.first,
                                             tp.last.transaction  = tp.last),
                 regexp = "1 time.unit after")
    expect_error(clv.time.set.sample.periods(clv.t,
                                             user.estimation.end  = tp.first-lubridate::days(1),
                                             tp.first.transaction = tp.first,
                                             tp.last.transaction  = tp.last),
                 regexp = "1 time.unit after")
  }

  expect_error(clv.time.set.sample.periods(clv.time = clv.t.hours,
                                           user.estimation.end  = "2018-01-01 00:35:49",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.hours),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.hours)),
              regexp = "1 time.unit after")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.days,
                                           user.estimation.end  = "2018-01-01",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.days),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.days)),
               regexp = "1 time.unit after")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.weeks,
                                           user.estimation.end  = "2018-01-03", # Wed
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.weeks), # Mon
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.weeks)),
               regexp = "1 time.unit after")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.years,
                                           user.estimation.end  = "2018-12-31",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.years),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.years)),
               regexp = "1 time.unit after")
}


fct.testthat.correctness.clvtime.set.sample.periods.stop.holdout.length.less.2.period <- function(clv.t.hours, clv.t.days,
                                                                                                     clv.t.weeks, clv.t.years){
  for(clv.t in list(clv.t.hours, clv.t.days, clv.t.weeks, clv.t.years)){
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)
    expect_error(clv.time.set.sample.periods(clv.t,
                                             user.estimation.end  = tp.last-lubridate::hours(1),
                                             tp.first.transaction = tp.first,
                                             tp.last.transaction  = tp.last),
                 regexp = "2 periods before")
  }

  expect_error(clv.time.set.sample.periods(clv.time = clv.t.hours,
                                           user.estimation.end  = "2025-06-14 22:40:11",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.hours),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.hours)),
               regexp = "2 periods before")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.days,
                                           user.estimation.end  = "2025-06-14",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.days),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.days)),
               regexp = "2 periods before")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.weeks,
                                           user.estimation.end  = "2025-06-11", # Wed
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.weeks),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-13", clv.t.weeks)),# Fr
               regexp = "2 periods before")
  expect_error(clv.time.set.sample.periods(clv.time = clv.t.years,
                                           user.estimation.end  = "2025-01-01",
                                           tp.first.transaction = fct.helper.clv.time.correct.datetype("2018-01-01", clv.t.years),
                                           tp.last.transaction  = fct.helper.clv.time.correct.datetype("2025-06-15", clv.t.years)),
               regexp = "2 periods before")
}


fct.testthat.correctness.clvtime.set.sample.periods.date.estimation.end <- function(clv.t){
  test_that("estimation split Date results in estimation end = on this date", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)
    tp.split <- as.Date("2019-07-19")

    period.type <- fct.helper.clv.time.period.type(clv.t)

    if(is(clv.t, "clv.time.datetime")){
      # POSIX dates in transactions - but split with Date (ymd by user)
      expect_silent(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = tp.split,
                                                         tp.first.transaction = tp.first,
                                                         tp.last.transaction = tp.last))
      # Split same Date but as posix
      expect_equal(clv.t@timepoint.estimation.end, as.POSIXct.POSIXlt(as.POSIXlt.Date(tp.split), tz = "UTC"))
    }else{
      expect_silent(clv.t <- clv.time.set.sample.periods(clv.t, user.estimation.end = tp.split,
                                                         tp.first.transaction = tp.first,
                                                         tp.last.transaction =  tp.last))
      expect_equal(clv.t@timepoint.estimation.end, tp.split)
    }

    expect_equal(clv.t@estimation.period.in.tu, time_length(interval(start = tp.first, end = tp.split), period.type))
    expect_equal(clv.t@timepoint.holdout.start, clv.t@timepoint.estimation.end+1L)
    expect_equal(clv.t@timepoint.holdout.end, tp.last)
  })
}




fct.testthat.correctness.clvtime.set.sample.periods.posixct.estimation.end <- function(clv.t){
  test_that("estimation split POSIXct results in estimation end = on this date", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)
    tp.split <- lubridate::ymd_hms("2019-07-19 15:36:19")

    period.type <- fct.helper.clv.time.period.type(clv.t)

    if(is(clv.t, "clv.time.datetime")){
      expect_silent(clv.t <- clv.time.set.sample.periods(clv.t,
                                                         user.estimation.end  = tp.split,
                                                         tp.first.transaction = tp.first,
                                                         tp.last.transaction  = tp.last))

      expect_equal(clv.t@timepoint.estimation.end, tp.split)
      expect_equal(clv.t@estimation.period.in.tu, time_length(interval(start = tp.first, end = tp.split), period.type))
    }else{
      # Date transactions - but split with POSIXct (given by user)
      expect_message(clv.t <- clv.time.set.sample.periods(clv.t,
                                                          user.estimation.end  = tp.split,
                                                          tp.first.transaction = tp.first,
                                                          tp.last.transaction  = tp.last),
                     regexp = "is ignored")
      # Split same but as Date
      expect_equal(clv.t@timepoint.estimation.end, as.Date(tp.split))
      expect_equal(clv.t@estimation.period.in.tu, time_length(interval(start = tp.first,
                                                                       end = as.Date(tp.split)), period.type))
    }

    expect_equal(clv.t@timepoint.holdout.start, clv.t@timepoint.estimation.end+1L)
    expect_equal(clv.t@timepoint.holdout.end, tp.last)
  })
}


fct.testthat.correctness.clvtime.set.sample.periods.char.estimation.end <- function(clv.t){
  test_that("estimation split char results in estimation end = on this date", {
    tp.first <- fct.helper.clv.time.correct.datetype("2018-01-01", clv.t)
    tp.last  <- fct.helper.clv.time.correct.datetype("2025-06-15", clv.t)
    period.type <- fct.helper.clv.time.period.type(clv.t)

    if(is(clv.t, "clv.time.datetime")){
      tp.split <- "2019-07-19 15:36:19"
      expect_silent(clv.t <- clv.time.set.sample.periods(clv.t,
                                                         user.estimation.end  = tp.split,
                                                         tp.first.transaction = tp.first,
                                                         tp.last.transaction  = tp.last))

      expect_equal(clv.t@timepoint.estimation.end, lubridate::ymd_hms(tp.split))
      expect_equal(clv.t@estimation.period.in.tu, time_length(interval(start = tp.first,
                                                                       end = lubridate::ymd_hms(tp.split)), period.type))
    }else{
      tp.split <- "2019-07-19"
      expect_silent(clv.t <- clv.time.set.sample.periods(clv.t,
                                                         user.estimation.end  = tp.split,
                                                         tp.first.transaction = tp.first,
                                                         tp.last.transaction  = tp.last))
      # Split same but as Date
      expect_equal(clv.t@timepoint.estimation.end, lubridate::ymd(tp.split))
      expect_equal(clv.t@estimation.period.in.tu, time_length(interval(start = tp.first,
                                                                       end = lubridate::ymd(tp.split)), period.type))
    }

    expect_equal(clv.t@timepoint.holdout.start, clv.t@timepoint.estimation.end+1L)
    expect_equal(clv.t@timepoint.holdout.end, tp.last)
  })
}


# convert.user.input.to.timepoint -------------------------------------------------------------------------------------
fct.testthat.correctness.clvtime.convert.user.input.chars.to.posixct <- function(clv.t.datetime){
  stopifnot(is(clv.t.datetime, "clv.time.datetime"))
  test_that("Chars convert to correct POSIX", {
    # Midnight
    expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-01-01 00:00:00"))
    # partial hours
    expect_equal(lubridate::ymd_hms("2019-12-18 05:00:01", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 05:00:01"))
    expect_equal(lubridate::ymd_hms("2019-12-18 05:32:03", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 05:32:03"))
    expect_equal(lubridate::ymd_hms("2019-12-18 14:59:59", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 14:59:59"))
    expect_equal(lubridate::ymd_hms("2019-12-18 23:59:59", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 23:59:59"))
    # exact hours
    expect_equal(lubridate::ymd_hms("2019-12-18 11:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 11:00:00"))
    expect_equal(lubridate::ymd_hms("2019-12-18 22:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 22:00:00"))
    expect_equal(lubridate::ymd_hms("2019-12-18 23:00:00", tz="UTC"), clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = "2019-12-18 23:00:00"))
  })
}

fct.testthat.correctness.clvtime.convert.user.input.chars.to.date <- function(clv.t.date){
  stopifnot(is(clv.t.date, "clv.time.date"))
  test_that("Chars convert to correct Dates", {
    expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = "2019-01-01"))
    expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = "2019-12-18"))
  })
}

fct.testthat.correctness.clvtime.convert.user.input.date.to.posixct <- function(clv.t.datetime){
  stopifnot(is(clv.t.datetime, "clv.time.datetime"))
  test_that("Dates convert to correct POSIX", {
    expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd("2019-01-01")))
    expect_equal(lubridate::ymd_hms("2019-12-18 00:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd("2019-12-18")))
  })
}

fct.testthat.correctness.clvtime.convert.user.input.date.to.date <- function(clv.t.date){
  stopifnot(is(clv.t.date, "clv.time.date"))
  test_that("Dates convert to correct Dates", {
    expect_equal(lubridate::ymd("2019-01-01"), clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = lubridate::ymd("2019-01-01")))
    expect_equal(lubridate::ymd("2019-12-18"), clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = lubridate::ymd("2019-12-18")))
  })
}

fct.testthat.correctness.clvtime.convert.user.input.posixct.to.posixct <- function(clv.t.datetime){
  stopifnot(is(clv.t.datetime, "clv.time.datetime"))
  test_that("POSIXct convert to correct POSIXct", {
    # Midnight
    expect_equal(lubridate::ymd_hms("2019-01-01 00:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-01-01 00:00:00",tz="UTC")))
    # partial hours
    expect_equal(lubridate::ymd_hms("2019-12-18 05:00:01", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 05:00:01",tz="UTC")))
    expect_equal(lubridate::ymd_hms("2019-12-18 05:32:03", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 05:32:03",tz="UTC")))
    expect_equal(lubridate::ymd_hms("2019-12-18 14:59:59", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 14:59:59",tz="UTC")))
    expect_equal(lubridate::ymd_hms("2019-12-18 23:59:59", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 23:59:59",tz="UTC")))
    # exact hours
    expect_equal(lubridate::ymd_hms("2019-12-18 11:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 11:00:00",tz="UTC")))
    expect_equal(lubridate::ymd_hms("2019-12-18 22:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 22:00:00",tz="UTC")))
    expect_equal(lubridate::ymd_hms("2019-12-18 23:00:00", tz="UTC"),
                 clv.time.convert.user.input.to.timepoint(clv.t.datetime, user.timepoint = lubridate::ymd_hms("2019-12-18 23:00:00",tz="UTC")))
  })
}


fct.testthat.correctness.clvtime.convert.user.input.posixct.to.date <- function(clv.t.date){
  stopifnot(is(clv.t.date, "clv.time.date"))
  test_that("POSIXct convert to correct Dates", {
    # outputs cuttoff
    expect_message(d.1 <- clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = lubridate::ymd_hms("2019-01-01 03:19:54", tz = "UTC")), regexp = "ignored")
    expect_message(d.2 <- clv.time.convert.user.input.to.timepoint(clv.t.date, user.timepoint = lubridate::ymd_hms("2019-12-18 03:19:54", tz = "UTC")), regexp = "ignored")
    expect_equal(lubridate::ymd("2019-01-01"), d.1)
    expect_equal(lubridate::ymd("2019-12-18"), d.2)
  })

}


# number.timeunits.to.timeperiod ---------------------------------------------------------------------------------------------
fct.testthat.correctness.clvtime.number.to.time.periods <- function(clv.t){
  test_that("Correct time period returned", {
    period.type <- fct.helper.clv.time.period.type(clv.t)
    expect_equal(lubridate::period(4, period.type), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t, user.number.periods=4))
    expect_equal(lubridate::period(4, period.type), clv.time.number.timeunits.to.timeperiod(clv.time=clv.t, user.number.periods=4.5))
  })
}


# floor.date -----------------------------------------------------------------------------------------------------
fct.testthat.correctness.clvtime.floor.date.rounds.down <- function(clv.t.hours, clv.t.days, clv.t.weeks, clv.t.years){
  test_that("floor date rounds down", {
    # Hours
    expect_equal(clv.time.floor.date(clv.t.hours, timepoint = lubridate::ymd_hms("2019-01-01 00:35:50")),
                 lubridate::ymd_hms("2019-01-01 00:00:00"))
    expect_equal(clv.time.floor.date(clv.t.hours, timepoint = lubridate::ymd_hms("2019-05-12 23:59:59")),
                 lubridate::ymd_hms("2019-05-12 23:00:00"))
    expect_equal(clv.time.floor.date(clv.t.hours, timepoint = lubridate::ymd_hms("2019-05-12 15:00:01")),
                 lubridate::ymd_hms("2019-05-12 15:00:00"))


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
}


fct.testthat.correctness.clvtime.floor.date.stays.when.correct <- function(clv.t.hours, clv.t.days, clv.t.years){

  test_that("floor date stays when already correct", {
    # Hours
    expect_equal(clv.time.floor.date(clv.t.hours, timepoint = lubridate::ymd_hms("2019-01-01 00:00:00")),
                 lubridate::ymd_hms("2019-01-01 00:00:00"))
    expect_equal(clv.time.floor.date(clv.t.hours, timepoint = lubridate::ymd_hms("2019-06-18 15:00:00")),
                 lubridate::ymd_hms("2019-06-18 15:00:00"))

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

}

# sequence.of.covariate.timepoints ---------------------------------------------------------------------------------------------
fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.off.end.off.period <- function(clv.t.days, clv.t.weeks, clv.t.years){
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
}


fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.on.end.off.period <- function(clv.t.days,
                                                                                                                clv.t.weeks, clv.t.years){
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
}

fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.off.end.on.period <- function(clv.t.days, clv.t.weeks,
                                                                                                                clv.t.years){
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
}

fct.testthat.correctness.clvtime.sequence.of.covariate.tp.start.end.correct.start.on.end.on.period <- function(clv.t.days,
                                                                                                               clv.t.weeks,
                                                                                                               clv.t.years){
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
}



# prediction.table ---------------------------------------------------------------------------------------------

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



