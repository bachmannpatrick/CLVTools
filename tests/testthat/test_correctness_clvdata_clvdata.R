data("cdnow")
data("apparelTrans") # for years

# context("Correctness - clvdata - clvdata")

# estimation.split ---------------------------------------------------------------------
context("Correctness - clvdata - estimation.split")


# **clv.time: predict(, 10). regexp 10 Weeks + regexp holdout.start
#     same as if with dates, also different tz / formats
#     prediction.end=14.4 all equal prediction.end=14. same with dates ("1997-12-26" vs "1997-12-31")

#
test_that("Different estimation.split formats result in same split - data in Dates", {
  #skip_on_cran()
  cdnow.date <- data.table::copy(cdnow)
  cdnow.date[, Date := as.Date(Date)]

  apparelTrans.date <- data.table::copy(apparelTrans)
  apparelTrans.date[, Date := as.Date(Date)]

  # fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37*7,
                                         estimation.char = as.character(cdnow.date[, min(Date)+(37*7)]),
                                         # estimation.char = as.character(cdnow[, clv.time.floor.date(clv.time.days("ymd"), min(Date))]+lubridate::period(37*7, "days")),
                                         time.unit = "days")

  fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         time.unit = "weeks")

  fct.helper.correctness.estimationsplit(data = apparelTrans.date, estimation.number = 1,
                                         estimation.char = as.character(apparelTrans.date[, min(Date)]+lubridate::period(1, "year")),
                                         time.unit = "years")
})


test_that("Different estimation.split formats result in same split - data in POSIX UTC", {
  #skip_on_cran()
  cdnow.posix <- data.table::copy(cdnow)
  cdnow.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "UTC")]

  apparelTrans.posix <- data.table::copy(apparelTrans)
  apparelTrans.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "UTC")]


  # fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7, estimation.char = "1997-09-17",
                                          time.unit = "days", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         # estimation.char = "1997-09-17",
                                          time.unit = "weeks", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = apparelTrans.posix, estimation.number = 1,
                                         estimation.char = as.character(apparelTrans.posix[, min(Date)]+lubridate::period(1, "year")),
                                          time.unit = "years", warn=TRUE)

})

test_that("Different estimation.split formats result in same split - data in POSIX ACST", {
  #skip_on_cran()
  cdnow.posix <- data.table::copy(cdnow)
  cdnow.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "Australia/Darwin")]

  apparelTrans.posix <- data.table::copy(apparelTrans)
  apparelTrans.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "Australia/Darwin")]

  # fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7, estimation.char = "1997-09-17",
                                          time.unit = "days", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                          time.unit = "weeks", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = apparelTrans.posix, estimation.number = 1,
                                         estimation.char = as.character(apparelTrans.posix[, min(Date)]+lubridate::period(1, "year")),
                                          time.unit = "years", warn=TRUE)

})


test_that("Different estimation.split formats result in same split - data in char", {
  #skip_on_cran()
  cdnow.char <- data.table::copy(cdnow)
  cdnow.char[, Date := as.character(Date)]

  apparelTrans.char <- data.table::copy(apparelTrans)
  apparelTrans.char[, Date := as.character(Date)]

  # fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37*7,
                                         estimation.char = "1997-09-17",
                                          time.unit = "days")

  fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37,
                                         # estimation.char = "1997-09-17",
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         time.unit = "weeks")

  fct.helper.correctness.estimationsplit(data = apparelTrans.char, estimation.number = 1,
                                         estimation.char = as.character(apparelTrans[, min(Date)]+lubridate::period(1, "years")),
                                         time.unit = "years")

})


test_that("No estimation.split ends on last transaction date", {
  #skip_on_cran()
  expect_silent(clv.data.cdnow <- clvdata(time.unit = "w", data.transactions = cdnow, date.format = "ymd"))
  expect_equal(clv.data.cdnow@clv.time@timepoint.estimation.start, cdnow[,min(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.estimation.end, cdnow[, max(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.holdout.start, cdnow[, max(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.holdout.end, cdnow[, max(Date)])
})

# time.unit ------------------------------------------------------------------------------
context("Correctness - clvdata - time.units")
test_that("Different units with same split results in same dates", {
  #skip_on_cran()

  # numeric breaks with floordate for estimation.start
  # expect_silent(data.days <-  clvdata(estimation.split = 37*7, time.unit = "days",
  #                                     data.transactions = cdnow, date.format = "ymd"))
  # expect_silent(data.weeks <- clvdata(estimation.split = 37, time.unit = "weeks",
  #                                     data.transactions = cdnow, date.format = "ymd"))
  # expect_silent(data.hours <- clvdata(estimation.split = 37*7*24, time.unit = "hours",
  #                                     data.transactions = cdnow, date.format = "ymd"))

  expect_silent(data.days <-  clvdata(estimation.split = "1997-09-17", time.unit = "days",
                                      data.transactions = cdnow, date.format = "ymd"))
  expect_silent(data.weeks <- clvdata(estimation.split = "1997-09-17", time.unit = "weeks",
                                      data.transactions = cdnow, date.format = "ymd"))

  expect_equal(data.weeks@clv.time@timepoint.estimation.end,   data.days@clv.time@timepoint.estimation.end)
  expect_equal(data.weeks@clv.time@timepoint.estimation.start,data.days@clv.time@timepoint.estimation.start)
  expect_equal(data.weeks@clv.time@timepoint.holdout.start,    data.days@clv.time@timepoint.holdout.start)
  expect_equal(data.weeks@clv.time@timepoint.holdout.end,      data.days@clv.time@timepoint.holdout.end)
})



# transaction.data ----------------------------------------------------------------------------
context("Correctness - clvdata - data.transactions")

test_that("Same results for different Id formats", {
  cdnow.char      <- data.table::copy(cdnow)
  cdnow.factor    <- data.table::copy(cdnow) # often when loading data
  cdnow.numeric   <- data.table::copy(cdnow)
  cdnow.char[,    Id := as.character(Id)]
  cdnow.numeric[, Id := as.numeric(Id)]
  cdnow.factor[,  Id := as.factor(as.character(Id))]

  expect_silent(data.char    <- clvdata(cdnow.char,    date.format = "ymd", time.unit = "w"))
  expect_silent(data.numeric <- clvdata(cdnow.numeric, date.format = "ymd", time.unit = "w"))
  expect_silent(data.factor  <- clvdata(cdnow.factor,  date.format = "ymd", time.unit = "w"))

  data.char@call    <- as.symbol("abc")
  data.numeric@call <- as.symbol("abc")
  data.factor@call  <- as.symbol("abc")

  expect_equal(data.char, data.factor)
  expect_equal(data.factor, data.numeric)
})

test_that("Same results for different Price formats", {
  cdnow.integer   <- data.table::copy(cdnow) # often when loading data
  cdnow.numeric   <- data.table::copy(cdnow)

  cdnow.numeric[, Price := as.numeric(as.integer(Price))] # make integer first to cutoff digits
  cdnow.integer[, Price := as.integer(Price)]

  expect_silent(data.numeric <- clvdata(cdnow.numeric, date.format = "ymd", time.unit = "w"))
  expect_silent(data.integer <- clvdata(cdnow.integer, date.format = "ymd", time.unit = "w"))

  data.numeric@call <- as.symbol("abc")
  data.integer@call <- as.symbol("abc")

  expect_equal(data.numeric, data.integer)
})

test_that("No price data gives correct object", {
  expect_silent(clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = NULL))
  expect_false(clv.cdnow@has.spending)
  expect_false("Price" %in% colnames(clv.cdnow@data.transactions))
  expect_false("Price" %in% colnames(clv.cdnow@data.repeat.trans))
  # also nothing in descriptives
  expect_true(nrow(clv.cdnow@descriptives.transactions[like(pattern = "Spending", vector = Name)])==0)
})

test_that("Price data gives correct object", {
  expect_silent(clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = "Price"))
  expect_true(clv.cdnow@has.spending)
  expect_true("Price" %in% colnames(clv.cdnow@data.transactions))
  expect_true("Price" %in% colnames(clv.cdnow@data.repeat.trans))
  # also Spending info in descriptives
  expect_true(nrow(clv.cdnow@descriptives.transactions[like(pattern = "Spending", vector = Name)]) > 0)
})

# test_that: same results for different Date formats is already checked very often in estimation.split

test_that("Transaction data was properly copied", {
  expect_silent(clv.data.cdnow <- clvdata(data.transactions = cdnow,time.unit = "weeks", date.format = "ymd"))
  expect_false(isTRUE(all.equal(data.table::address(clv.data.cdnow@data.transactions),
                                data.table::address(cdnow))))
})


