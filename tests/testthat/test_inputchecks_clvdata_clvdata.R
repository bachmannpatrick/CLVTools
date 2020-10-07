
# Load required data ---------------------------------------------------------------------------------
data("apparelTrans")
data("cdnow")

# Parameter data.transactions ------------------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter data.transactions")

test_that("Fails if missing / NA / NULL", {
  expect_error(clvdata(data.transactions = , time.unit = "w",date.format = "ymd"))
  expect_error(clvdata(data.transactions = NULL, time.unit = "w",date.format = "ymd"))
  expect_error(clvdata(data.transactions = NA_real_, time.unit = "w",date.format = "ymd"))
})

test_that("Fails if not a data.frame", {
  expect_error(clvdata(data.transactions = as.list(cdnow), time.unit = "w",date.format = "ymd"))
  expect_error(clvdata(data.transactions = unlist(cdnow), time.unit = "w",date.format = "ymd"))
  expect_error(clvdata(data.transactions = c(Id=1:10, Date=seq(Sys.Date(), length.out = 10, by="day") ,
                                             Price=1:10), time.unit = "w",date.format = "ymd"))
})

test_that("Fails if no rows or cols", {
  expect_error(clvdata(data.transactions = data.frame(), time.unit = "w",date.format = "ymd"), regexp = "not be found")
  expect_error(clvdata(data.transactions = data.frame(Id=character(0), Date=character(0), Price=numeric(), stringsAsFactors = FALSE),
                       time.unit = "w",date.format = "ymd"), regexp = "empty")
})

test_that("Fails if any NA in any column", {
  cdnow.na <- data.table::copy(cdnow)

  cdnow.na[100, Price:= NA_real_]
  expect_error(clvdata(data.transactions = cdnow.na, time.unit = "w",date.format = "ymd"), regexp = "NAs")

  cdnow.na[100, Price:= 0]
  cdnow.na[100, Id := NA_character_]
  expect_error(clvdata(data.transactions = cdnow.na, time.unit = "w",date.format = "ymd"), regexp = "NAs")

  cdnow.na[100, Id := "1021"]
  cdnow.na[100, Date:= as.Date(NA_character_)]
  expect_error(clvdata(data.transactions = cdnow.na, time.unit = "w",date.format = "ymd"), regexp = "NAs")
})


test_that("Fails if Price is not numeric", {
  cdnow.notnum <- data.table::copy(cdnow)
  cdnow.notnum[, Price:=as.character(Price)]
  expect_error(clvdata(data.transactions = cdnow.notnum, time.unit = "w",date.format = "ymd"), regexp = "numeric")
})

test_that("Fails if does not have the Id column", {
  cdnow.noid <- data.table::copy(cdnow)
  cdnow.noid[, Id := NULL]
  expect_error(clvdata(data.transactions = cdnow.noid, time.unit = "w",date.format = "ymd"),
               regexp = "could not be found")
})

test_that("Fails if does not have the Date column", {
  cdnow.nodate <- data.table::copy(cdnow)
  cdnow.nodate[, Date := NULL]
  expect_error(clvdata(data.transactions = cdnow.nodate, time.unit = "w",date.format = "ymd"),
               regexp = "could not be found")
})

test_that("Fails if does not have the Price column", {
  cdnow.noprice <- data.table::copy(cdnow)
  cdnow.noprice[, Price := NULL]
  expect_error(clvdata(data.transactions = cdnow.noprice, time.unit = "w",date.format = "ymd"),
               regexp = "could not be found")
})

test_that("Has no default argument", {
  expect_true(is.symbol(formals(clvdata)$data.transactions))
})


# Parameter date.format ------------------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter date.format")

test_that("Fails if missing/NULL/NA", {
  expect_error(clvdata(data.transactions = cdnow, time.unit = "w"),
               regexp = "missing")
  expect_error(clvdata(date.format = NULL, data.transactions = cdnow, time.unit = "w"),
               regexp = "needs to be of type character")
  expect_error(clvdata(date.format = NA_character_, data.transactions = cdnow, time.unit = "w"),
               regexp = "any NA")
})

test_that("Fails if not character", {
  expect_error(clvdata(date.format = 1, data.transactions = cdnow, time.unit = "w"), regexp = "character")
  expect_error(clvdata(date.format = lubridate::ymd, data.transactions = cdnow, time.unit = "w"), regexp = "character")
  expect_error(clvdata(date.format = Sys.Date(), data.transactions = cdnow, time.unit = "w"), regexp = "character")

  expect_error(clvdata(date.format = character(0), data.transactions = cdnow, time.unit = "w"), regexp = "single")
  expect_error(clvdata(date.format = list("ymd"), data.transactions = cdnow, time.unit = "w"), regexp = "character")
  expect_error(clvdata(date.format = data.frame("ymd"), data.transactions = cdnow, time.unit = "w"), regexp = "character")
})

test_that("Fails if date.format wrong format", {
  cdnow.char <- data.table::copy(cdnow)
  cdnow.char[, Date := as.character(Date)]
  expect_error(clvdata(date.format = "ydm", data.transactions = cdnow.char, time.unit = "w"), regexp = "failed")
  expect_error(clvdata(date.format = "dmy", data.transactions = cdnow.char, time.unit = "w"), regexp = "failed")
  expect_error(clvdata(date.format = "dym", data.transactions = cdnow.char, time.unit = "w"), regexp = "failed")
  expect_error(clvdata(date.format = "myd", data.transactions = cdnow.char, time.unit = "w"), regexp = "failed")
})

test_that("Fails if has multiple", {
  expect_error(clvdata(date.format = c("ymd", "ymd"), data.transactions = cdnow, time.unit = "w"), regexp = "single")
  expect_error(clvdata(date.format = c("ymd", "dmy"), data.transactions = cdnow, time.unit = "w"), regexp = "single")
})


test_that("Has no default argument", {
  expect_true(is.symbol(formals(clvdata)$date.format))
})


# Parameter time.unit --------------------------------------------------------------------------------

context("Inputchecks - clvdata - Parameter time.unit")

test_that("Fails if missing/NULL/NA", {
  expect_error(clvdata(data.transactions = cdnow, date.format="ymd"),
               regexp = "missing")
  expect_error(clvdata(time.unit = , data.transactions = cdnow, date.format="ymd"),
               regexp = "missing")
  expect_error(clvdata(time.unit = NULL, data.transactions = cdnow, date.format="ymd"),
               regexp = "NULL")
  expect_error(clvdata(time.unit = NA_character_ , data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
})

test_that("Fails if not character", {
  expect_error(clvdata(time.unit = 1, data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(time.unit = list("week"), data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(time.unit = data.frame("week"), data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
})

test_that("Fails if wrong time.unit format", {
  expect_error(clvdata(time.unit = character(0), data.transactions = cdnow, date.format="ymd"),
               regexp = "one single element")
  expect_error(clvdata(time.unit = "beeks", data.transactions = cdnow, date.format="ymd"),
               regexp = "one of the following")
  expect_error(clvdata(time.unit = "decade", data.transactions = cdnow, date.format="ymd"),
               regexp = "one of the following")
  expect_error(clvdata(time.unit = "bour", data.transactions = cdnow, date.format="ymd"),
               regexp = "one of the following")
  expect_error(clvdata(time.unit = "minute", data.transactions = cdnow, date.format="ymd"),
               regexp = "one of the following")
})

test_that("Fails if has multiple", {
  expect_error(clvdata(time.unit = c("weeks", "years"), data.transactions = cdnow, date.format="ymd"),
               regexp = "one single element")
  expect_error(clvdata(time.unit = c("weeks", "weeks"), data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Has no default argument", {
  expect_true(is.symbol(formals(clvdata)$time.unit))
})



# Parameter estimation.split ---------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter estimation.split")

test_that("Fails if NA",{
  expect_error(clvdata(estimation.split = NA_real_,time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
})

test_that("Fails if not numeric, character, or date",{
  expect_error(clvdata(estimation.split = data.frame(37),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "of type")
  expect_error(clvdata(estimation.split = list(37),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "of type")
  expect_error(clvdata(estimation.split = "37",time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "can be converted")

  expect_error(clvdata(estimation.split = character(0),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "one single element")
  expect_error(clvdata(estimation.split = numeric(),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "one single element")
  expect_error(clvdata(estimation.split = as.Date(character(0)),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "one single element")
  expect_error(clvdata(estimation.split = as.Date(NA_character_),time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
})

test_that("Fails with character in wrong dateformat",{
  expect_error(clvdata(estimation.split = "2010-15-12",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "date.format")
  expect_error(clvdata(estimation.split = "12-15-2010",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "date.format")
})

test_that("Fails with split after last transaction",{
  expect_error(clvdata(estimation.split = "2010-01-01",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
  expect_error(clvdata(estimation.split = 200,time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
  expect_error(clvdata(estimation.split = 4,time.unit = "y", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
})

test_that("Fails with split in 2 periods before last transaction (ie in last period)",{
  expect_error(clvdata(estimation.split = "1998-06-30",time.unit = "d", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
  expect_error(clvdata(estimation.split = "1998-06-29",time.unit = "d", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
  expect_error(clvdata(estimation.split = "1998-06-30",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
  expect_error(clvdata(estimation.split = "1998-06-21",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "before the last transaction")
})

test_that("Fails if before all first transactions by customer", {
  expect_error(clvdata(estimation.split = 4,time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "Not all customers")
  expect_error(clvdata(estimation.split = "1997-02-01",time.unit = "w", data.transactions = cdnow,
                       date.format="ymd"), regexp = "Not all customers")
})

test_that("Has default argument NULL",{
  expect_true(is.null(formals(clvdata)$estimation.split))
})




# Parameter name.id ------------------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter name.id")

test_that("Fails if NA/NULL", {
  expect_error(clvdata(name.id = NULL, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "NULL")
  expect_error(clvdata(name.id = NA_character_, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
  expect_error(clvdata(name.id = character(0), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(clvdata(name.id = list("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.id = data.frame("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.id = 1, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(clvdata(name.id = c("Id", "Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
  expect_error(clvdata(name.id = c("Id", "Date"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(clvdata(name.id = "id", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.id = "ID", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.id = "di", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.id = "customer", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
})


test_that("Has default argument Id",{
  default.arg <- eval(formals(clvdata)[["name.id"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Id")
})


# Parameter name.date ------------------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter name.date")

test_that("Fails if NA/NULL", {
  expect_error(clvdata(name.date = NULL, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "NULL")
  expect_error(clvdata(name.date = NA_character_, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
  expect_error(clvdata(name.date = character(0), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(clvdata(name.date = list("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.date = data.frame("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.date = 1, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(clvdata(name.date = c("Id", "Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
  expect_error(clvdata(name.date = c("Id", "Date"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(clvdata(name.date = "dat", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.date = "date", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.date = "Datee", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  # expect_error(clvdata(name.date = "Price", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
  #              regexp = "not be found in the data")
  # expect_error(clvdata(name.date = "Id", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
  #              regexp = "not be found in the data")
})


test_that("Has default argument Date",{
  default.arg <- eval(formals(clvdata)[["name.date"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Date")
})




# Parameter name.price ------------------------------------------------------------------------------------
context("Inputchecks - clvdata - Parameter name.price")

test_that("Fails if NA/empty", {
  # Null is allowed to express no spending data
  expect_error(clvdata(name.price = NA_character_, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "any NA")
  expect_error(clvdata(name.price = character(0), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(clvdata(name.price = list("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.price = data.frame("Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
  expect_error(clvdata(name.price = 1, time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(clvdata(name.price = c("Id", "Id"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
  expect_error(clvdata(name.price = c("Id", "Date"), time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(clvdata(name.price = "price", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.price = "PRICE", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  expect_error(clvdata(name.price = "Pricee", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
               regexp = "not be found in the data")
  # expect_error(clvdata(name.price = "Id", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
  #              regexp = "not be found in the data")
  # expect_error(clvdata(name.price = "Date", time.unit = "w", data.transactions = cdnow, date.format="ymd"),
  #              regexp = "not be found in the data")
})


test_that("Has default argument Price",{
  default.arg <- eval(formals(clvdata)[["name.price"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Price")
})




# Parameter verbose ------------------------------------------------------------------------------------
# test_that("Has default argument verbose",{
#   default.arg <- eval(formals(clvdata)[["verbose"]])
#   expect_is(default.arg, "logical")
#   expect_true(default.arg)
# })


# Fails if Date is not convertible
# verbose has default True

