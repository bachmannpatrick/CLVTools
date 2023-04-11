
fct.testthat.inputchecks.clvfittedtransactions.plot.which <- function(clv.fitted){
  test_that("which - only allowed args", {
    skip_on_cran()
    expect_error(plot(clv.fitted, which=NULL), regexp = "cannot be NULL")
    expect_error(plot(clv.fitted, which=NA_character_), regexp = "may not contain")
    expect_error(plot(clv.fitted, which="trking"), regexp = "choose one of the following")
    expect_error(plot(clv.fitted, which="pmff"), regexp = "choose one of the following")
    expect_error(plot(clv.fitted, which=""), regexp = "choose one of the following")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.plot.prediction.end.before.estimation.start <- function(fitted.transactions){
  test_that("Fails if prediction.end before estimation.start", {
    expect_error(plot(fitted.transactions, prediction.end = -100000), regexp = "before the start of the estimation period")
    expect_error(plot(fitted.transactions, prediction.end = fitted.transactions@clv.data@clv.time@timepoint.estimation.start - lubridate::weeks(1)), regexp = "before the start of the estimation period")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.plot.ellipsis <- function(l.std.args){
  test_that("Stop if unnecessary inputs given in ellipsis", {
    skip_on_cran()
    expect_error(do.call(plot, c(l.std.args, list(abc = 123))), regexp = "further parameters")
    expect_error(do.call(plot, c(l.std.args, list(continuous.discount.factor = 0.2))), regexp = "further parameters")
    expect_error(do.call(plot, c(l.std.args, list(use.cor = TRUE))), regexp = "further parameters")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.othermodels.are.not.fitted.transaction.models <- function(clv.fitted, which){
  test_that("other.models is not list", {
    expect_error(plot(clv.fitted, which=which, other.models=NULL), regexp = "list of fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=123), regexp = "list of fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=clv.fitted), regexp = "list of fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=clv.fitted@clv.data), regexp = "list of fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=bgnbd), regexp = "list of fitted transaction models")
  })

  test_that("other.models contains element which are not clv.fitted.transactions", {
    expect_error(plot(clv.fitted, which=which, other.models=list(1, 2, 3)), regexp = "fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted, NA)), regexp = "fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted, 2, clv.fitted)), regexp = "fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted@clv.data)), regexp = "fitted transaction models")
    expect_error(plot(clv.fitted, which=which, other.models=list(pnbd, bgnbd)), regexp = "fitted transaction models")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.plot.label.with.othermodels <- function(clv.fitted, which, clv.fitted.other){

  test_that("label has required length", {
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other), label="other"), regexp = "contain exactly")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other, clv.fitted.other), label=c("this", "other")), regexp = "contain exactly")
  })


  test_that("label has no empty text elements", {
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other), label=c("this", "")), regexp = "empty")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other), label=c("", "other")), regexp = "empty")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other), label=c("", "")), regexp = "empty")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other, clv.fitted.other), label=c("this", "other", "")), regexp = "empty")
  })

  test_that("label has no duplicates", {
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other), label=c("other", "other")), regexp = "duplicate")
    expect_error(plot(clv.fitted, which=which, other.models=list(clv.fitted.other, clv.fitted.other), label=c("this", "other", "other")), regexp = "duplicate")
  })

}

fct.testthat.inputchecks.clvfittedtransactions.plot.pmf.trans.bins <- function(clv.fitted){
  test_that("trans.bins is valid input (integer vector)", {
    skip_on_cran()

    expect_error(plot(clv.fitted, which="pmf", trans.bins=NULL), regex="NULL")
    expect_error(plot(clv.fitted, which="pmf", trans.bins=NA_real_), regex="any NA")
    expect_error(plot(clv.fitted, which="pmf", trans.bins=c(1,2,NA_real_, 4)), regex="any NA")
    expect_error(plot(clv.fitted, which="pmf", trans.bins="1"), regex="vector of integer numbers")
    expect_error(plot(clv.fitted, which="pmf", trans.bins=c(1,2.2,3,4)), regex="all integer numbers")
    expect_error(plot(clv.fitted, which="pmf", trans.bins=c(-1,0,1,2)), regex="positive integer numbers")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.plot.pmf <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){
  expect_silent(fitted.cdnow.nohold <- pnbd(fct.helper.create.clvdata.cdnow(data.cdnow, estimation.split=NULL), verbose = FALSE))
  l.std.args <- list(x=fitted.cdnow.nohold, which="pmf")

  # Common
  fct.testthat.inputchecks.clvfittedtransactions.plot.common(which="pmf", l.std.args = l.std.args,
                                                             data.cdnow=data.cdnow, data.apparelTrans=data.apparelTrans,
                                                             data.apparelStaticCov=data.apparelStaticCov)


  fct.testthat.inputchecks.clvfittedtransactions.plot.pmf.trans.bins(clv.fitted = fitted.cdnow.nohold)

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "calculate.remaining")

  fct.helper.inputcheck.single.character(fct = plot, l.std.args = l.std.args, name.param = "label.remaining", null.allowed = FALSE)

}

fct.testthat.inputchecks.clvfittedtransactions.plot.tracking <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){

  expect_silent(fitted.cdnow.nohold <- pnbd(fct.helper.create.clvdata.cdnow(data.cdnow, estimation.split=NULL), verbose = FALSE))
  l.std.args <- list(x=fitted.cdnow.nohold, prediction.end=6, which="tracking")

  # Common
  fct.testthat.inputchecks.clvfittedtransactions.plot.common(which="tracking", l.std.args = l.std.args,
                                                             data.cdnow=data.cdnow, data.apparelTrans=data.apparelTrans,
                                                             data.apparelStaticCov=data.apparelStaticCov)

  fct.testthat.inputchecks.clvfittedtransactions.prediction.end.wrong.format(fitted.transactions = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.plot.prediction.end.before.estimation.start(fitted.transactions = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.prediction.end.uses.newdata(s3method = plot, fitted.cdnow = fitted.cdnow.nohold, data.cdnow = data.cdnow)

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "cumulative")
}



fct.testthat.inputchecks.clvfittedtransactions.plot.common <- function(which, l.std.args,
                                                                       data.cdnow, data.apparelTrans, data.apparelStaticCov){

  # Prepare
  clv.data.apparel.static.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans,
                                                                             data.apparelStaticCov = data.apparelStaticCov,
                                                                             estimation.split = 40)
  expect_silent(fitted.apparel.static <- pnbd(clv.data.apparel.static.cov, verbose = FALSE))
  clv.data.cdnow.nohold <- fct.helper.create.clvdata.cdnow(data.cdnow, estimation.split=NULL)
  expect_silent(fitted.cdnow.nohold   <- pnbd(clv.data.cdnow.nohold, verbose = FALSE))



  # Test common params
  fct.testthat.inputchecks.clvfittedtransactions.plot.which(clv.fitted = fitted.cdnow.nohold)


  fct.testthat.inputchecks.clvfitted.na.in.prediction.params.model(s3method = plot, clv.fitted = fitted.cdnow.nohold, l.s3method.args=l.std.args)
  fct.testthat.inputchecks.clvfittedtransactions.cov.na.in.prediction.params.cov(s3method = plot, clv.fitted.cov = fitted.apparel.static,
                                                                                 l.s3method.args=l.std.args)

  fct.testthat.inputchecks.clvfitted.newdata.not.clvdata(s3method = plot, l.std.args=l.std.args, data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedtransactions.newdata.has.different.covs(s3method = plot,
                                                                            l.s3method.args = l.std.args,
                                                                            clv.fitted.apparel.cov = fitted.apparel.static,
                                                                            data.apparelStaticCov = data.apparelStaticCov)
  fct.testthat.inputchecks.clvfittedtransactions.newdata.is.different.class(s3method = plot,
                                                                            l.s3method.args=l.std.args,
                                                                            clv.fitted.transactions.nocov = fitted.cdnow.nohold,
                                                                            clv.fitted.transactions.staticcov = fitted.apparel.static,
                                                                            clv.data.no.cov = clv.data.cdnow.nohold,
                                                                            clv.data.static.cov = clv.data.apparel.static.cov)

  context(paste0("Inputchecks - clv.fitted.transactions plot ",which," - other.models"))
  fct.testthat.inputchecks.clvfittedtransactions.othermodels.are.not.fitted.transaction.models(clv.fitted = fitted.cdnow.nohold, which = which)
  fct.testthat.inputchecks.clvfittedtransactions.othermodels.are.not.fitted.transaction.models(clv.fitted = fitted.apparel.static, which = which)

  # Label for single model and for when other.models is specified
  context(paste0("Inputchecks - clv.fitted.transactions plot ", which," - label, no other.models"))
  fct.helper.inputcheck.single.character(fct = plot, l.std.args = l.std.args, name.param = "label", null.allowed = TRUE)

  context(paste0("Inputchecks - clv.fitted.transactions plot ", which," - label, with other.models"))
  fct.testthat.inputchecks.clvfittedtransactions.plot.label.with.othermodels(clv.fitted=fitted.cdnow.nohold, which=which, clv.fitted.other=fitted.apparel.static)

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "transactions")

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "plot")

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose")

  fct.testthat.inputchecks.clvfittedtransactions.plot.ellipsis(l.std.args = l.std.args)
}




skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.inputchecks.clvfittedtransactions.plot.tracking(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
fct.testthat.inputchecks.clvfittedtransactions.plot.pmf(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
