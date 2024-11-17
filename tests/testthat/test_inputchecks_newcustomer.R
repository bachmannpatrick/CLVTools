skip_on_cran()

p.cdnow <- fit.cdnow()
p.apparel.static <- fit.apparel.static()
p.apparel.dyn <- fit.apparel.dyncov.quick()


default.dyn.cov <- function(...){
  l.args <- list(...)
  l.default <- list(
    Cov.Date=c("2006-01-01", "2006-01-08"),
    cov=c(9.12, 0))
  l.default[names(l.args)] <- l.args
  l.default <- l.default[!sapply(l.default, is.null)]
  return(do.call(data.frame, l.default))
}

default.nc.static <- function(...){
  l.args <- list(...)
  l.default <- list(
    num.periods = 12.34,
    data.cov.life = data.frame(cov=1.23),
    data.cov.trans = data.frame(cov1=0.678, cov2=7.91)
  )
  l.default[names(l.args)] <- l.args
  return(do.call(newcustomer.static, l.default))
}


default.nc.dyn <- function(...){
  l.args <- list(...)
  l.default <- list(
    num.periods = 12.34,
    data.cov.life = default.dyn.cov(),
    data.cov.trans = default.dyn.cov(),
    first.transaction = '2000-01-01'
  )
  l.default[names(l.args)] <- l.args
  return(do.call(newcustomer.dynamic, l.default))
}


# No cov -------------------------------------------------------------------------------------------
test_that("num.periods not numeric", {
  expect_error(newcustomer(num.periods = "abc"), regexp = "numeric")
  expect_error(newcustomer(num.periods = data.frame(a=1)), regexp = "numeric")
})

test_that("num.periods not >= 0 ", {
  expect_error(newcustomer(num.periods = -1), regexp = "positive")
})


# Static cov ---------------------------------------------------------------------------------------
test_that("newcustomer.static cov data has right format ", {

  # no row
  expect_error(default.nc.static(data.cov.trans = data.frame()), regex = "exactly 1 row")
  expect_error(default.nc.static(data.cov.life = data.frame()), regex = "exactly 1 row")
  #
  # # more than 1 row
  expect_error(default.nc.static(data.cov.trans = data.frame(Id=c("a", "b"), cov=1:2)), regex = "exactly 1 row")
  expect_error(default.nc.static(data.cov.life = data.frame(Id=c("a", "b"), cov=1:2)), regex = "exactly 1 row")
  #
  # # no id column allowed
  expect_error(default.nc.static(data.cov.trans = data.frame(Id="a", cov=2)), regex = "'Id'")
  expect_error(default.nc.static(data.cov.life = data.frame(Id="a", cov=2)), regex = "'Id'")
  #
  # # no non-numeric column (factor/character/logical)
  expect_error(default.nc.static(data.cov.trans = data.frame(cov=as.factor("a"))), regex = "type numeric")
  expect_error(default.nc.static(data.cov.trans = data.frame(cov="a")), regex = "type numeric")
  expect_error(default.nc.static(data.cov.trans = data.frame(cov=TRUE)), regex = "type numeric")
  expect_error(default.nc.static(data.cov.trans = data.frame(cov1=1.12, cov2="abc")), regex = "type numeric")
})



# Dyn cov ---------------------------------------------------------------------------------------
test_that("newcustomer dyncov cov data has right format ", {
  # is empty
  df.empty <- default.dyn.cov(Cov.Date=as.Date(character(0)), cov=numeric(0))

  expect_error(default.nc.dyn(data.cov.life = df.empty), regex = "empty")
  expect_error(default.nc.dyn(data.cov.trans = df.empty), regex = "empty")

  # has id
  df.id <- default.dyn.cov(Id=c("a", "a"))
  expect_error(default.nc.dyn(data.cov.life = df.id), regex = "'Id'")
  expect_error(default.nc.dyn(data.cov.trans = df.id), regex = "'Id'")

  # only numeric no factor/characters/logical
  expect_error(default.nc.dyn(data.cov.life = default.dyn.cov(cov=c("m", "m"))), regex = "type numeric")
  expect_error(default.nc.dyn(data.cov.life = default.dyn.cov(cov=c(TRUE, FALSE))), regex = "type numeric")
  expect_error(default.nc.dyn(data.cov.life = default.dyn.cov(cov=factor(c("m", "m")))), regex = "type numeric")

  # no column named Cov.Date
  expect_error(default.nc.dyn(data.cov.life = default.dyn.cov(Cov.Date=NULL)), regex = "column named 'Cov.Date'")
  expect_error(default.nc.dyn(data.cov.trans = default.dyn.cov(Cov.Date=NULL)), regex = "column named 'Cov.Date'")

})

test_that("newcustomer dyncov cov data has right dates", {
  # Requires a fitted model object to convert dates
  # and matching cov names



  # default.correct.dyn.cov <- function(Cov.Date){
  #  return(data.frame(
  #    Cov.Date=Cov.Date,
  #    High.Season=c(0, 1),
  #    Gender=c(1, 1),
  #    Channel=c(1.23, 0.67)))
  # }
  fct.test.nc.dyn.content <- function(Cov.Date, first.trans, num.periods=2.23, regex){

    df.cov <- data.frame(
      Cov.Date=Cov.Date,
      High.Season=c(0, 1, 0),
      Gender=c(1, 1, 1),
      Channel=c(1.23, 0.67, 7.23))

    nc.dyn <- newcustomer.dynamic(
      num.periods = num.periods,
      data.cov.life = df.cov,
      data.cov.trans = df.cov,
      first.transaction = first.trans)

    expect_error(predict(p.apparel.dyn,newdata = nc.dyn),regexp = regex)
  }

  # Cov.Date is not unique
  fct.test.nc.dyn.content(Cov.Date=c("2006-01-01", "2006-01-01", "2006-01-08"), first.trans="2006-01-01", regex = 'has to be spaced')

  # Cov.Date has missing periods
  fct.test.nc.dyn.content(Cov.Date=c("2006-01-01", "2006-01-15", "2006-01-22"), first.trans="2006-01-05", regex = "has to be spaced")

  # Cov.date not on start of periods (w, d, y, h)
  fct.test.nc.dyn.content(Cov.Date=c("2000-01-01", "2000-01-08", "2000-01-15"), first.trans="2000-01-01", regex = "1999-12-26")

  # cov data does not cover prediction period t
  cov.dates.correct <- c("2006-01-01", "2006-01-08", "2006-01-15")
  fct.test.nc.dyn.content(Cov.Date=cov.dates.correct, first.trans="2006-01-01", num.periods=99, regex = 'end on 2007-11-25')
  # incl t is until next possible Cov.Date
  fct.test.nc.dyn.content(Cov.Date=cov.dates.correct, first.trans="2006-01-01", num.periods=3, regex = 'end on 2006-01-22')

  # error if first.trans outside cov data
  fct.test.nc.dyn.content(Cov.Date=cov.dates.correct, first.trans="2006-02-02", regex = 'start at 2006-01-29')

})

# predict ---------------------------------------------------------------------------------------
test_that("newcustomer fits the type of fitted model", {
  nc.nocov <- newcustomer(num.periods = 1.23)
  nc.static <- default.nc.static()
  nc.dyn <- default.nc.dyn()

  # nocov
  expect_error(predict(p.cdnow, newdata=nc.static), regexp = "output from")
  expect_error(predict(p.cdnow, newdata=nc.dyn), regexp = "output from")

  # static cov
  expect_error(predict(p.apparel.static, newdata=nc.nocov), regexp = "output from")
  expect_error(predict(p.apparel.static, newdata=nc.dyn), regexp = "output from")

  # dyncov
  expect_error(predict(p.apparel.dyn, newdata=nc.nocov), regexp = "output from")
  expect_error(predict(p.apparel.dyn, newdata=nc.static), regexp = "output from")
})


test_that("predict(): Error if other parameters are passed", {
  expect_error(predict(p.cdnow, newdata=newcustomer(12), prediction.end=12), regexp = "No other parameters")
  expect_error(predict(p.cdnow, newdata=newcustomer(12), continuous.discount.factor=0.1), regexp = "No other parameters")
  expect_error(predict(p.cdnow, newdata=newcustomer(12), predict.spending=TRUE), regexp = "No other parameters")
})

test_that("predict vs newcustomer: dyn/static cov data names are not the same as parameters", {
  # static cov
  # the default cov data generated does not match the cov names
  expect_error(
    predict(p.apparel.static, newdata=default.nc.static()),
    regexp = "exactly the following columns")

  # dyncov
  # the default cov data generated does not match the cov names
  expect_error(
    predict(p.apparel.dyn,newdata=default.nc.dyn()),
    regexp = "exactly the following columns")
})
