fct.testthat.inputchecks.clvfitted.newdata.not.clvdata <- function(s3method, l.std.args){
  test_that("Fails if newdata not a clv.data object", {
    skip_on_cran()
    data.cdnow <- fct.helper.load.cdnow()
    expect_error(do.call(s3method, c(l.std.args, list(newdata = NA_character_))), regexp = "needs to be a clv data object")
    expect_error(do.call(s3method, c(l.std.args, list(newdata = character()))), regexp = "needs to be a clv data object")
    expect_error(do.call(s3method, c(l.std.args, list(newdata = "abc"))), regexp = "needs to be a clv data object")
    expect_error(do.call(s3method, c(l.std.args, list(newdata = 123))), regexp = "needs to be a clv data object")
    expect_error(do.call(s3method, c(l.std.args, list(newdata = data.cdnow))), regexp = "needs to be a clv data object")
    expect_error(do.call(s3method, c(l.std.args, list(newdata = unlist(data.cdnow)))), regexp = "needs to be a clv data object")
  })
}


fct.testthat.inputchecks.clvfitted.na.in.prediction.params.model <- function(s3method, clv.fitted, l.s3method.args){
  test_that("Fails if prediction.params.model are NA", {
    skip_on_cran()

    clv.fitted@prediction.params.model[2] <- NA_real_

    # remove clv.fitted from std args
    l.s3method.args <- l.s3method.args[!sapply(l.s3method.args, is, "clv.fitted")]
    expect_error(do.call(what = s3method, args = c(list(clv.fitted), l.s3method.args)), regexp = "NAs in the estimated model")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.newdata.has.different.covs <- function(s3method,
                                                                                      l.s3method.args,
                                                                                      clv.fitted.apparel.cov){
  test_that("Fails if newdata has different covariates (names)", {
    skip_on_cran()

    l.s3method.args <- l.s3method.args[!sapply(l.s3method.args, is, "clv.fitted")]

    # newdata should be exactly same except for the cov names
    clv.apparel.nocov <- as(clv.fitted.apparel.cov@clv.data, "clv.data")
    data.apparelStaticCov.additional <- data.table::copy(fct.helper.load.apparelStaticCov())
    data.apparelStaticCov.additional[, Haircolor := "red"]
    data.apparelStaticCov.additional[sample.int(.N, size = .N/4), Haircolor := "black"]

    # Other covs
    expect_silent(clv.apparel.static.other <- SetStaticCovariates(clv.data = clv.apparel.nocov,
                                                                  data.cov.life = data.apparelStaticCov.additional,
                                                                  data.cov.trans = data.apparelStaticCov.additional,
                                                                  names.cov.life = "Haircolor",
                                                                  names.cov.trans = "Haircolor"))
    expect_error(do.call(s3method, c(list(clv.fitted.apparel.cov, newdata = clv.apparel.static.other), l.s3method.args)),
                 regexp = "used for fitting are present in the")


    # More covs
    expect_silent(clv.apparel.static.more <- SetStaticCovariates(clv.data = clv.apparel.nocov,
                                                                 data.cov.life = data.apparelStaticCov.additional,
                                                                 data.cov.trans = data.apparelStaticCov.additional,
                                                                 names.cov.life = c("Gender","Channel", "Haircolor"),
                                                                 names.cov.trans = c("Gender", "Channel","Haircolor")))

    expect_error(do.call(s3method, c(list(clv.fitted.apparel.cov, newdata = clv.apparel.static.more), l.s3method.args)),
                 regexp = "used for fitting are present in the")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.newdata.is.different.class <- function(s3method,
                                                                                      l.s3method.args,
                                                                                      clv.fitted.transactions.nocov,
                                                                                      clv.fitted.transactions.staticcov,
                                                                                      clv.data.no.cov,
                                                                                      clv.data.static.cov){
  l.s3method.args <- l.s3method.args[!sapply(l.s3method.args, is, "clv.fitted")]
  test_that("Fails if newdata is of wrong clv.data", {
    skip_on_cran()
    # predicting nocov model with staticcov data
    expect_error(do.call(s3method, c(list(clv.fitted.transactions.nocov, newdata=clv.data.static.cov), l.s3method.args)), regexp = "of class clv.data")

    # predicting staticcov model with nocov data
    expect_error(do.call(s3method, c(list(clv.fitted.transactions.staticcov, newdata=clv.data.no.cov), l.s3method.args)), regexp ="of class clv.data.static.covariates")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.cov.na.in.prediction.params.cov <- function(s3method, clv.fitted.cov, l.s3method.args){
  test_that("Fails if prediction.params.life/trans are NA", {
    skip_on_cran()
    l.s3method.args <- l.s3method.args[!sapply(l.s3method.args, is, "clv.fitted")]
    clv.fitted.cov@prediction.params.life[1] <- NA_real_
    expect_error(do.call(s3method, c(list(clv.fitted.cov), l.s3method.args)), regexp = "NAs in the estimated covariate")
    clv.fitted.cov@prediction.params.life[1] <- 1 # remove NA

    clv.fitted.cov@prediction.params.trans[1] <- NA_real_
    expect_error(do.call(s3method, c(list(clv.fitted.cov), l.s3method.args)), regexp = "NAs in the estimated covariate")
    clv.fitted.cov@prediction.params.trans[1] <- 1 # remove NA
  })
}


fct.testthat.inputchecks.clvfittedtransactions.prediction.end.wrong.format <- function(fitted.transactions){

  test_that("Fails for prediction.end not char/date/posix", {
    skip_on_cran()
    expect_error(plot(fitted.transactions, prediction.end = list("2004-01-01")), regexp = "either of type")
    expect_error(plot(fitted.transactions, prediction.end = data.frame("2004-01-01")), regexp = "either of type")
  })

  test_that("Fails for prediction.end NA", {
    skip_on_cran()
    expect_error(plot(fitted.transactions, prediction.end = NA_real_), regexp = "any NA")
    expect_error(plot(fitted.transactions, prediction.end = NA_integer_), regexp = "any NA")
    expect_error(plot(fitted.transactions, prediction.end = NA_character_), regexp = "any NA")
  })

  test_that("Fails for multiple prediction.end", {
    skip_on_cran()
    expect_error(plot(fitted.transactions, prediction.end = c(1,2)), regexp = "of length 1")
    expect_error(plot(fitted.transactions, prediction.end = c(4,5)), regexp = "of length 1")
    expect_error(plot(fitted.transactions, prediction.end = 1:10), regexp = "of length 1")

    expect_error(plot(fitted.transactions, prediction.end = c("2004-01-01", "2003-01-01")), regexp = "of length 1")
    expect_error(plot(fitted.transactions, prediction.end = c("2004-01-01", "2004-01-01")), regexp = "of length 1")

    expect_error(plot(fitted.transactions, prediction.end = c(as.Date("2004-01-01"), as.Date("2003-01-01"))), regexp = "of length 1")
    expect_error(plot(fitted.transactions, prediction.end = c(as.Date("2004-01-01"), as.Date("2004-01-01"))), regexp = "of length 1")

    expect_error(plot(fitted.transactions, prediction.end = as.POSIXct(c(as.Date("2004-01-01"), as.Date("2003-01-01")))), regexp = "of length 1")
    expect_error(plot(fitted.transactions, prediction.end = as.POSIXct(c(as.Date("2004-01-01"), as.Date("2004-01-01")))), regexp = "of length 1")
  })

  test_that("Fails if prediciton.end is not in initial date.format", {
    skip_on_cran()
    expect_error(plot(fitted.transactions, prediction.end = format(fitted.transactions@clv.data@date.holdout.end+lubridate::weeks(6), "%m-%Y-%d")))
    expect_error(plot(fitted.transactions, prediction.end = format(fitted.transactions@clv.data@date.holdout.end+lubridate::weeks(6), "%m-%d-%Y")))
    expect_error(plot(fitted.transactions, prediction.end = format(fitted.transactions@clv.data@date.holdout.end+lubridate::weeks(6), "%d-%Y-%m")))
    expect_error(plot(fitted.transactions, prediction.end = format(fitted.transactions@clv.data@date.holdout.end+lubridate::weeks(6), "%d-%m-%Y")))
    expect_error(plot(fitted.transactions, prediction.end = format(fitted.transactions@clv.data@date.holdout.end+lubridate::weeks(6), "%Y-%d-%m")))
  })
}


fct.testthat.inputchecks.clvfittedtransactions.prediction.end.uses.newdata <- function(s3method, fitted.cdnow){

  test_that("prediction.end relates to newdata", {
    skip_on_cran()

    # can predict.end before cdnow[ min(date)] if in newdata they are earlier

    data.cdnow.earlier <- copy(fct.helper.load.cdnow())
    data.cdnow.earlier[, Date := as.Date(Date)]
    data.cdnow.earlier[, Date := Date - 1000] # 1000days back -> 1994-04-07 is first
    clv.newdata <- fct.helper.create.clvdata.cdnow(data.cdnow.earlier, estimation.split=37)

    # Cannot predict/plot with normal cdnow
    expect_error(do.call(s3method, list(fitted.cdnow, prediction.end = "1996-06-06", verbose=FALSE)))

    # But works if newdata is given
    expect_silent(do.call(s3method, list(fitted.cdnow, newdata = clv.newdata, prediction.end = "1996-06-06", verbose=FALSE)))
  })
}
