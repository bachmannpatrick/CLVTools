skip_on_cran()

fct.expect.silent.predict.newcustomer <- function(clv.fitted, nc){
  expect_silent(predict(clv.fitted, newdata=nc, verbose=FALSE))
}


# Load required data and fit all models --------------------------------------------------

# Fit all available models
optimx.args.fast <- list(method = "Nelder-Mead", itnmax=10, hessian=TRUE)

p.cdnow <- fit.cdnow(model = pnbd)
bg.cdnow <- fit.cdnow(model = bgnbd, optimx.args = optimx.args.fast)
ggom.cdnow <- fit.cdnow(model = ggomnbd, optimx.args = optimx.args.fast)

# Create cov models with differently named covariates to test that life/trans
# are not accidentially switched at some point
p.apparel.static <- fit.apparel.static(model = pnbd, names.cov.life = "Channel", names.cov.trans = c("Channel", "Gender"))
bg.apparel.static <- fit.apparel.static(model = bgnbd, names.cov.life = "Channel", names.cov.trans = c("Channel", "Gender"))
ggom.apparel.static <- fit.apparel.static(model = ggomnbd, names.cov.life = "Channel", names.cov.trans = c("Channel", "Gender"))

p.apparel.dyn <- fct.helper.dyncov.quickfit.apparel.data(
  names.cov.life = c("Marketing", "Gender"),
  names.cov.trans = c("Marketing", "Gender", "Channel")
)



# Tests no cov models ------------------------------------------------------------------
test_that("Works with num.periods less than 1, 2, 3 (nocov)", {
  for(m in list(p.cdnow, bg.cdnow, ggom.cdnow)){
    fct.expect.silent.predict.newcustomer(m, newcustomer(num.periods=0.123))
    fct.expect.silent.predict.newcustomer(m, newcustomer(num.periods=1.234))
    fct.expect.silent.predict.newcustomer(m, newcustomer(num.periods=2.345))
  }
})


# Tests static cov models -------------------------------------------------------------
df.cov.life.static <- data.frame(Channel=6.78)
df.cov.trans.static <- data.frame(Channel=6.78, Gender=-0.234)

default.nc.staticcov <- function(...){
  l.dots <- list(...)
  l.defaults <- list(
    num.periods=6.789,
    data.cov.life = df.cov.life.static,
    data.cov.trans = df.cov.trans.static
  )
  l.defaults[names(l.dots)] <- l.dots
  return(do.call(newcustomer.static, args = l.defaults))
}

test_that("Works with num.periods less than 1, 2, 3 (static cov)", {
  for(m in list(p.apparel.static, bg.apparel.static, ggom.apparel.static)){
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(num.periods=0.123))
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(num.periods=1.234))
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(num.periods=2.345))
  }
})

test_that("Works with data.frame and data.table (static cov)", {

  for(m in list(p.apparel.static, bg.apparel.static, ggom.apparel.static)){
    # only data.frame
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(
      data.cov.life=as.data.frame(df.cov.life.static),
      data.cov.trans=as.data.frame(df.cov.trans.static)))
    # only data.table
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(
      data.cov.life=as.data.table(df.cov.life.static),
      data.cov.trans=as.data.table(df.cov.trans.static)))
    # mixed
    fct.expect.silent.predict.newcustomer(m, default.nc.staticcov(
      data.cov.life=as.data.frame(df.cov.life.static),
      data.cov.trans=as.data.table(df.cov.trans.static)))
  }
})



# Tests dyncov models -----------------------------------------------------------------

# . Preparation -----------------------------------------------------------------------

# Define outside default.nc.dyncov because needed to build some of the tests
# Choose period outside fitting period
# have to be many dates to be able to run some of the tests
cov.dates.nc.dyncov <- as.Date(c("2051-02-12", "2051-02-19", "2051-02-26", "2051-03-05", "2051-03-12", "2051-03-19"))


default.dyn.cov.life <- function(cov.dates=cov.dates.nc.dyncov){
  return(data.table(
    Cov.Date=cov.dates,
    Gender=rep_len(0, length(cov.dates)),
    # Channel=rep_len(c(-12.2, 0, 2.4, 5.91, -0.3, -3.88), length(cov.dates)),
    Marketing=rep_len(c(4, 0, 7, 2, 9, 0), length(cov.dates))))
}

default.dyn.cov.trans <- function(cov.dates=cov.dates.nc.dyncov){
  return(data.table(
    Cov.Date=cov.dates,
    Gender=rep_len(0, length(cov.dates)),
    Channel=rep_len(c(0, 0, 2, 1.23, -1.23, -2), length(cov.dates)),
    Marketing=rep_len(c(4, 0, 7, 2, 9, 0), length(cov.dates))))
}

dt.cov.life.nc.dyncov <- default.dyn.cov.life()
dt.cov.trans.nc.dyncov <- default.dyn.cov.trans()

default.nc.dyncov <- function(...){
  l.dots <- list(...)

  if(!"cov.dates" %in% names(l.dots)){
    cov.dates <- cov.dates.nc.dyncov
  }else{
    # use the given ones
    cov.dates <- l.dots$cov.dates
    l.dots$cov.dates <- NULL
  }

  # Suppress warning because data.table produces a warning about
  # using POSIXlt
  l.defaults <- suppressWarnings(list(
    num.periods=4.567,
    data.cov.life=default.dyn.cov.life(cov.dates),
    data.cov.trans=default.dyn.cov.trans(cov.dates),
    first.transaction = "2051-02-16"
  ))
  l.defaults[names(l.dots)] <- l.dots
  return(do.call(newcustomer.dynamic, args = l.defaults))
}


# . Tests ------------------------------------------------------------------------------

test_that("Works with num.periods less than 1, 2, 3 (dyn cov)", {
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(num.periods=0.123))
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(num.periods=1.234))
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(num.periods=2.345))
})

test_that("Works with data.frame and data.table (dyn cov)", {
  # only data.frame
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    data.cov.life=as.data.frame(dt.cov.life.nc.dyncov),
    data.cov.trans=as.data.frame(dt.cov.trans.nc.dyncov)))
  # only data.table
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    data.cov.life=as.data.table(dt.cov.life.nc.dyncov),
    data.cov.trans=as.data.table(dt.cov.trans.nc.dyncov)))
  # mixed
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    data.cov.life=as.data.frame(dt.cov.life.nc.dyncov),
    data.cov.trans=as.data.table(dt.cov.trans.nc.dyncov)))
})

test_that("Works with cov data starting before first.transaction", {
  # move first.transaction further into cov data
  # (and shorten num.periods to still be in cov data)
  fct.expect.silent.predict.newcustomer(
    p.apparel.dyn,
    default.nc.dyncov(
      first.transaction = min(cov.dates.nc.dyncov) + days(16),
      num.periods = 1.234))
})

test_that("Works with cov data ending after num.periods", {
  # shorter num.periods to be within cov dates
  fct.expect.silent.predict.newcustomer(
    p.apparel.dyn,
    default.nc.dyncov(num.periods = 1.234))

})

test_that("Works with cov data from different period than fitting", {
  # by default is already after fitting period but set first.transaction to be
  # sure in case cov dates in default.nc.dyncov() is changed
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = "2051-02-22"))
})

test_that("Works with first.transaction on first Cov.Date", {
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = min(cov.dates.nc.dyncov)))
})

test_that("Works with num.periods < 1 and first.transaction & Cov.Date single period", {
  # < 1d
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = "2051-02-12",
    cov.dates =  "2051-02-12",
    num.periods = 0.123))
  # < 1week
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = "2051-02-12",
    cov.dates =  "2051-02-12",
    num.periods = 0.678))
})

test_that("Works with Cov.Date & first.transaction of type Date, character, POSIXct, POSIXlt", {
  char.first.trans <- as.character(min(cov.dates.nc.dyncov))
  chars.cov.dates.nc.dyncov <- as.character(cov.dates.nc.dyncov)

  # character
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = char.first.trans,
    cov.dates =  chars.cov.dates.nc.dyncov))
  # Date
  fct.expect.silent.predict.newcustomer(p.apparel.dyn, default.nc.dyncov(
    first.transaction = as.Date(char.first.trans),
    cov.dates =  as.Date(chars.cov.dates.nc.dyncov)))
  # POSIXct
  expect_message(
    predict(p.apparel.dyn, newdata=default.nc.dyncov(
      first.transaction = as.POSIXct(char.first.trans),
      cov.dates =  as.POSIXct(chars.cov.dates.nc.dyncov))),
    regex = "time of day stored"
  )
  # POSIXlt
  expect_message(
    predict(p.apparel.dyn, newdata=default.nc.dyncov(
      first.transaction = as.POSIXlt(char.first.trans),
      cov.dates =  as.POSIXlt(chars.cov.dates.nc.dyncov))),
    regex = "time of day stored"
  )
})


