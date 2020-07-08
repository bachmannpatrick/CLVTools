# # **TODO: Prediction end not a date/numeric/char (= same tests as for plot)
# fct.helper.inputchecks.single.form.of.date <- function(){
#   test_that("Fails for NA",{
#
#   })
#   .fct.helper.inputchecks.fails.for.NA
#   .fct.helper.inputchecks.fails.for.NULL
# }

fct.testthat.inputchecks.clvfitted.predict.newdata.not.clvdata <- function(clv.fitted, data.cdnow){
  test_that("Fails if newdata not a clv.data object", {
    skip_on_cran()
    expect_error(predict(clv.fitted, newdata = NA_character_), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = character()), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = "abc"), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = 123), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = data.cdnow), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = unlist(data.cdnow)), regexp = "needs to be a clv data object")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.predict.discountfactor.out.of.range <- function(clv.fitted.transactions){
  test_that("Fails if discount factor out of [0,1)", {
    skip_on_cran()
    for(d in c(-0.01, -0.4, -1, 1, 1.01, 1.4, 10)){
      expect_error(predict(clv.fitted.transactions, prediction.end = 6, continuous.discount.factor = d), regexp = "needs to be in the interval")
    }
  })
}

fct.testthat.inputchecks.clvfittedtransactions.predict.prediction.end.fails.no.holdout <- function(clv.fitted.transactions.no.hold){
  test_that("Fails if prediction.end not given and has no holdout period", {
    skip_on_cran()
    if(!clv.data.has.holdout(clv.fitted.transactions.no.hold@clv.data)){
      expect_error(predict(clv.fitted.transactions.no.hold, verbose = FALSE), regexp = "if there is no holdout")
    }
  })
}

fct.testthat.inputchecks.clvfittedtransactions.predict.prediction.end.before.estimation.end <- function(clv.fitted){
  test_that("Fails if prediction.end before estimation end", {
    # Negative number
    expect_error(predict(clv.fitted, prediction.end = -1), regexp = "after the estimation period")
    expect_error(predict(clv.fitted, prediction.end = -10), regexp = "after the estimation period")
    expect_error(predict(clv.fitted, prediction.end = -5), regexp = "after the estimation period")

    # Date before
    expect_error(predict(clv.fitted, prediction.end = clv.fitted@clv.data@clv.time@timepoint.estimation.end - lubridate::days(1)), regexp = "after the estimation period")
    expect_error(predict(clv.fitted, prediction.end = clv.fitted@clv.data@clv.time@timepoint.estimation.end - lubridate::days(10)), regexp = "after the estimation period")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.predict.newdata.has.different.covs <- function(clv.fitted.apparel.cov,
                                                                                              data.apparelStaticCov){
  test_that("Fails if newdata has different covariates (names)", {
    skip_on_cran()

    # newdata should be exactly same except for the cov names
    clv.apparel.nocov <- as(clv.fitted.apparel.cov@clv.data, "clv.data")
    data.apparelStaticCov.additional <- data.table::copy(data.apparelStaticCov)
    data.apparelStaticCov.additional[, Haircolor := "red"]
    data.apparelStaticCov.additional[sample.int(.N, size = .N/4), Haircolor := "black"]

    # Other covs
    expect_silent(clv.apparel.static.other <- SetStaticCovariates(clv.data = clv.apparel.nocov,
                                                                  data.cov.life = data.apparelStaticCov.additional,
                                                                  data.cov.trans = data.apparelStaticCov.additional,
                                                                  names.cov.life = "Haircolor",
                                                                  names.cov.trans = "Haircolor"))
    expect_error(predict(clv.fitted.apparel.cov, newdata = clv.apparel.static.other),
                 regexp = "used for fitting are present in the")


    # More covs
    expect_silent(clv.apparel.static.more <- SetStaticCovariates(clv.data = clv.apparel.nocov,
                                                                 data.cov.life = data.apparelStaticCov.additional,
                                                                 data.cov.trans = data.apparelStaticCov.additional,
                                                                 names.cov.life = c("Gender","Channel", "Haircolor"),
                                                                 names.cov.trans = c("Gender", "Channel","Haircolor")))

    expect_error(predict(clv.fitted.apparel.cov, newdata = clv.apparel.static.more),
                 regexp = "used for fitting are present in the")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.predict.newdata.is.different.class <- function(clv.fitted.transactions.nocov,
                                                                                              clv.fitted.transactions.staticcov,
                                                                                              clv.data.no.cov,
                                                                                              clv.data.static.cov){
  test_that("Fails if newdata is of wrong clv.data", {
    skip_on_cran()
    # predicting nocov model with staticcov data
    expect_error(predict(clv.fitted.transactions.nocov, newdata = clv.data.static.cov), regexp = "of class clv.data")

    # predicting staticcov model with nocov data
    expect_error(predict(clv.fitted.transactions.staticcov, newdata=clv.data.no.cov), regexp ="of class clv.data.static.covariates")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.but.no.spending.data <- function(method, data.cdnow){
  test_that("Predict with spending fails if not spending data", {
    skip_on_cran()

    # Fit on data without spending, with holdout
    clv.data.no.spending   <- clvdata(data.cdnow, "ymd", "w", estimation.split = 37, name.price = NULL)
    clv.data.with.spending <- clvdata(data.cdnow, "ymd", "w", estimation.split = 37, name.price = "Price")
    expect_silent(fitted.no.spending   <- do.call(method, list(clv.data = clv.data.no.spending,   verbose=FALSE)))
    expect_silent(fitted.with.spending <- do.call(method, list(clv.data = clv.data.with.spending, verbose=FALSE)))
    expect_silent(fitted.spending.model <- gg(clv.data.with.spending, verbose=FALSE))

    expect_error(predict(fitted.no.spending, prediction.end = 6, predict.spending = TRUE), regexp = "no spending data")
    expect_error(predict(fitted.no.spending, prediction.end = 6, predict.spending = gg), regexp = "no spending data")
    expect_error(predict(fitted.no.spending, predict.spending = fitted.spending.model), regexp = "no spending data")

    # original data has spending, but newdata has not
    expect_error(predict(fitted.with.spending, prediction.end = 6, newdata = clv.data.no.spending, predict.spending = TRUE), regexp = "no spending data")
    expect_error(predict(fitted.with.spending, prediction.end = 6, newdata = clv.data.no.spending, predict.spending = gg), regexp = "no spending data")
    expect_error(predict(fitted.with.spending, prediction.end = 6, newdata = clv.data.no.spending, predict.spending = fitted.spending.model), regexp = "no spending data")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.wrong.type <- function(clv.fitted.transactions){
  test_that("Predict spending fails if wrong type for prediction.end", {
    skip_on_cran()
    # other than function, logical, or fitted spending object

    # illegal logical
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = NA), regexp = "cannot be NA")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = c(TRUE, TRUE)), regexp = "single")

    # other than logical
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = NULL), regexp = "has to be either")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = 123), regexp = "has to be either")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = list(TRUE)), regexp = "has to be either")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = data.frame(TRUE)), regexp = "has to be either")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = "TRUE"), regexp = "has to be either")

    # other fitted than spending model
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = clv.fitted.transactions), regexp = "fitted spending model")

    # other function than CLVTools spending model methods
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = sum), regexp = "spending model")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = pnbd), regexp = "spending model")
    expect_error(predict(clv.fitted.transactions, prediction.end = 6, predict.spending = bgnbd), regexp = "spending model")
  })
}



fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.has.NA <- function(clv.fitted.transactions, data.cdnow){
  test_that("Predict spending fails if prediction.end has NA coefs", {
    skip_on_cran()
    # fit spending model
    expect_silent(fitted.spending <- gg(clvdata(data.cdnow, "ymd", "w"), verbose=FALSE))
    # set 1 coef to NA
    coef(fitted.spending@optimx.estimation.output)[1] <- NA_real_
    expect_error(predict(clv.fitted.transactions, predict.spending = fitted.spending, prediction.end = 6), regexp = "contain NA")
  })
}







fct.testthat.inputchecks.clvfittedtransactions <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){
  context("Inputchecks - clv.fitted.transactions predict - newdata")
  clv.data.apparel.static.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                 estimation.split = 40)
  expect_silent(fitted.apparel.static <- pnbd(clv.data.apparel.static.cov, verbose = FALSE))
  expect_silent(clv.data.cdnow.nohold <- clvdata(data.cdnow, "ymd", "w", estimation.split = NULL))
  expect_silent(fitted.cdnow.nohold   <- pnbd(clv.data.cdnow.nohold, verbose = FALSE))


  fct.testthat.inputchecks.clvfitted.predict.newdata.not.clvdata(clv.fitted = fitted.cdnow.nohold, data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedtransactions.predict.newdata.has.different.covs(clv.fitted.apparel.cov = fitted.apparel.static,
                                                                                    data.apparelStaticCov = data.apparelStaticCov)
  fct.testthat.inputchecks.clvfittedtransactions.predict.newdata.is.different.class(clv.fitted.transactions.nocov = fitted.cdnow.nohold,
                                                                                    clv.fitted.transactions.staticcov = fitted.apparel.static,
                                                                                    clv.data.no.cov = clv.data.cdnow.nohold,
                                                                                    clv.data.static.cov = clv.data.apparel.static.cov)


  context("Inputchecks - clv.fitted.transactions predict - prediction.end")
  fct.testthat.inputchecks.clvfittedtransactions.predict.prediction.end.fails.no.holdout(clv.fitted.transactions.no.hold = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.predict.prediction.end.before.estimation.end(clv.fitted = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.predict.prediction.end.before.estimation.end(clv.fitted = fitted.apparel.static)


  context("Inputchecks - clv.fitted.transactions predict - predict.spending")
  fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.but.no.spending.data(method = pnbd,
                                                                                               data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.wrong.type(clv.fitted.transactions = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.predict.predict.spending.has.NA(clv.fitted.transactions = fitted.cdnow.nohold, data.cdnow = data.cdnow)


  context("Inputchecks - clv.fitted.transactions predict - continuous.discount.factor")
  # fct.helper.inputcheck.single.numeric(fct = predict, name.param="continuous.discount.factor",
  #                                      l.std.args=list(object = fitted.cdnow.nohold, prediction.end = 6))
  fct.testthat.inputchecks.clvfittedtransactions.predict.discountfactor.out.of.range(clv.fitted.transactions = fitted.cdnow.nohold)


  context("Inputchecks - clv.fitted.transactions predict - verbose")
  l.std.args <- list(object=fitted.cdnow.nohold, prediction.end=6)
  .fct.helper.inputchecks.single.logical(fct = predict, l.std.args = l.std.args,
                                         name.param = "verbose", null.allowed=FALSE)


  # context("Inputchecks - clv.fitted.transactions predict - ...")
  # fct.testthat.inputchecks.clvfitted.predict.ellipsis
}


data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
fct.testthat.inputchecks.clvfittedtransactions(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
