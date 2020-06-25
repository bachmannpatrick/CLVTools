
# Load required data -----------------------------------------------------------------------------------
data("apparelTrans")
data("cdnow")

context("Runability - clvdata - clvdata")


# data.transactions ------------------------------------------------------------------------------------
test_that("Works with data.frame / data.table input", {
  skip_on_cran()
  expect_silent(clvdata(data.transactions = as.data.frame(cdnow), time.unit = "w",date.format = "ymd"))
  expect_silent(clvdata(data.transactions = as.data.table(cdnow), time.unit = "w",date.format = "ymd"))
})

test_that("Works with unneeded extra column", {
  skip_on_cran()
  expect_silent(clvdata(data.transactions = cbind(cdnow, data.frame(abc=1:2)),
                        time.unit = "w",date.format = "ymd"))
  # of unallowed type
  expect_silent(clvdata(data.transactions = cbind(cdnow, data.frame(abc=as.factor(1:2))),
                        time.unit = "w",date.format = "ymd"))
})


test_that("Works with Id as character/factor/numeric", {
  skip_on_cran()

  cdnow.id <- data.table::copy(cdnow)

  cdnow.id[, Id := as.numeric(Id)]
  expect_silent(clvdata(data.transactions = cdnow.id, time.unit = "w",date.format = "ymd"))

  cdnow.id[, Id := as.character(Id)]
  expect_silent(clvdata(data.transactions = cdnow.id, time.unit = "w",date.format = "ymd"))

  # factor because often strings are loaded as factors instead of as strings
  cdnow.id[, Id := as.factor(Id)]
  expect_silent(clvdata(data.transactions = cdnow.id, time.unit = "w", date.format = "ymd"))
})



test_that("Works with transaction date as Date",{
  skip_on_cran()
  apparelTrans[, Date.date := lubridate::ymd(Date)]

  expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.date",
                        time.unit = "w", date.format = "ydm"))
})


test_that("Works with transaction date that is character and includes time",{
  # Posixct always has time, Date never
  skip_on_cran()

  apparelTrans[, Date.char := format(Date, "%Y-%m-%d %H:%M:%S")]
  expect_silent(clvdata(apparelTrans, date.format = "ymd HSM", name.date="Date.char", time.unit="h"))
  expect_silent(clvdata(apparelTrans, date.format = "ymd HSM", name.date="Date.char", time.unit="w"))

  apparelTrans[, Date.char := format(Date, "%H:%M:%S %Y-%m-%d ")]
  expect_silent(clvdata(apparelTrans, date.format = "HSM ymd", name.date="Date.char", time.unit="h"))
  expect_silent(clvdata(apparelTrans, date.format = "HSM ymd", name.date="Date.char", time.unit="w"))

})

test_that("Works with transaction date as POSIXct",{
  skip_on_cran()
  apparelTrans[, Date.posix := as.POSIXct(as.POSIXlt.Date(lubridate::ymd(Date)), tz="UTC")]

  # Datetime based tu
  expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.posix",
                        time.unit = "hours",date.format = "ydm"))

  # Date based tu
  expect_message(clvdata(data.transactions = apparelTrans, name.date = "Date.posix",
                        time.unit = "w",date.format = "ydm"), regexp = "ignored")
})

# Does not go into data.table
# test_that("Works with date as POSIXlt",{
#   skip_on_cran()
#   apparelTrans[, Date.posixlt := as.POSIXlt.Date(lubridate::ymd(Date))]
#
#   expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.posixlt",
#                         time.unit = "w",date.format = "ydm"))
# })

test_that("Works with Price as numeric and Integer", {
  skip_on_cran()

  cdnow.price <- data.table::copy(cdnow)

  cdnow.price[, Price := as.numeric(Price)]
  expect_silent(clvdata(data.transactions = cdnow.price, time.unit = "w",date.format = "ydm"))

  cdnow.price[, Price := as.integer(Price)]
  expect_silent(clvdata(data.transactions = cdnow.price, time.unit = "w",date.format = "ydm"))
})


test_that("Works without column Price", {
  skip_on_cran()

  cdnow.noprice <- data.table::copy(cdnow)
  cdnow.noprice[, Price := NULL]
  expect_silent(clvdata(data.transactions = cdnow.noprice,
                        time.unit = "w",date.format = "ymd",
                        name.price = NULL))
})


# date.format ------------------------------------------------------------------------------------------

test_that("Works with different character formats",{
  skip_on_cran()

  # Ensure proper date first
  apparelTrans[, Date := lubridate::ymd(Date)]

  apparelTrans[, Date.char := format(Date, "%Y-%m-%d")]
  expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.char",
                        time.unit = "w",date.format = "ymd"))

  apparelTrans[, Date.char := format(Date, "%m-%Y-%d")]
  expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.char",
                        time.unit = "w",date.format = "myd"))

  apparelTrans[, Date.char := format(Date, "%Y-%d-%m")]
  expect_silent(clvdata(data.transactions = apparelTrans, name.date = "Date.char",
                        time.unit = "w",date.format = "ydm"))
})



# test_that("Warns if unneded time is given in date.format", {
#   skip_on_cran()
#
#   # Ensure proper date first
#   apparelTrans[, Date := lubridate::ymd(Date)]
#   apparelTrans[, Date.char := format(Date, "%Y-%m-%d %H:%M:%S")]
#
#   expect_warning(do.call(clvdata, modifyList(l.std.args, alist(
#                   data.transactions = apparelTrans, name.date = "Date.char",
#                   time.unit = "w", date.format="ymdHMS"))))
#
#   expect_warning(do.call(clvdata, modifyList(l.std.args, alist(
#                   data.transactions = apparelTrans, name.date = "Date.char",
#                   time.unit = "d", date.format="ymdHMS"))))
#
#   expect_warning(do.call(clvdata, modifyList(l.std.args, alist(
#                   data.transactions = apparelTrans, name.date = "Date.char",
#                   time.unit = "y", date.format="ymdHMS"))))
# })

# *** What about estimation units < 1d? (ie hours)


# time.unit ------------------------------------------------------------------------------------------

test_that("Works with different time.units",{
  expect_silent(clvdata(time.unit = "h", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "d", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "w", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "y", data.transactions = cdnow, date.format = "ymd"))
})

test_that("Works with different spelling cases of time.unit ", {
  skip_on_cran()

  # Uppercase letters
  expect_silent(clvdata(time.unit = "D", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "W", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "Y", data.transactions = cdnow, date.format = "ymd"))

  # Lowercase
  #   tested just before
})

test_that("Works with full names in time.unit", {
  skip_on_cran()

  expect_silent(clvdata(time.unit = "day", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "week", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "year", data.transactions = cdnow, date.format = "ymd"))
})

test_that("Works with plurals in time.unit", {
  skip_on_cran()

  expect_silent(clvdata(time.unit = "days", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "weeks", data.transactions = cdnow, date.format = "ymd"))

  expect_silent(clvdata(time.unit = "years", data.transactions = cdnow, date.format = "ymd"))
})


# estimation.split ------------------------------------------------------------------------------------------
test_that("Works without estimation.split (=fit on full)",{
  skip_on_cran()
  expect_silent(clvdata(time.unit = "weeks", data.transactions = cdnow, date.format = "ymd"))
})

test_that("Works with estimation.split as numeric", {
  skip_on_cran()
  # integer
  expect_silent(clvdata(estimation.split = 37L, time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"))
  # real
  expect_silent(clvdata(estimation.split = 37.0, time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"))

  # non whole numbers throw warning
  expect_warning(clvdata(estimation.split = 37.75, time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"),
                 regexp = "partial periods")
})

test_that("Works with estimation.split as Date: char/date/posixct", {
  skip_on_cran()
  # char
  expect_silent(clvdata(estimation.split = "1997-10-01", time.unit = "hours",
                        data.transactions = cdnow, date.format = "ymd"))
  expect_silent(clvdata(estimation.split = "1997-10-01", time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"))
  # Date
  expect_silent(clvdata(estimation.split = as.Date("1997-10-01"), time.unit = "hours",
                        data.transactions = cdnow, date.format = "ymd"))
  expect_silent(clvdata(estimation.split = as.Date("1997-10-01"), time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"))
  # posixct
  expect_silent(clvdata(estimation.split = lubridate::ymd("1997-10-01", tz="UTC"), time.unit = "hours",
                        data.transactions = cdnow, date.format = "ymd"))
  # warns that time of day is ignored
  expect_message(clvdata(estimation.split = lubridate::ymd("1997-10-01", tz="UTC"), time.unit = "weeks",
                        data.transactions = cdnow, date.format = "ymd"), "ignored")
})

