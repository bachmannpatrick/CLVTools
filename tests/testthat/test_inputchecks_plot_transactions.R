fct.testthat.inputchecks.clvfittedtransactions.plot.prediction.end.before.estimation.start <- function(fitted.transactions){
  test_that("Fails if prediction.end before estimation.start", {
    expect_error(plot(fitted.transactions, prediction.end = -100000), regexp = "before the start of the estimation period")
    expect_error(plot(fitted.transactions, prediction.end = fitted.transactions@clv.data@clv.time@timepoint.estimation.start - lubridate::weeks(1)), regexp = "before the start of the estimation period")
  })
}

fct.testthat.inputchecks.clvfittedtransactions.plot.ellipsis <- function(fitted.transactions){
  test_that("Stop if unnecessary inputs given in ellipsis", {
    skip_on_cran()
    expect_error(plot(fitted.transactions, prediction.end = 6, abc = 123), regexp = "further parameters")
    expect_error(plot(fitted.transactions, prediction.end = 6, continuous.discount.factor = 0.2), regexp = "further parameters")
    expect_error(plot(fitted.transactions, prediction.end = 6, use.cor = TRUE), regexp = "further parameters")
  })
}


fct.testthat.inputchecks.clvfittedtransactions.plot <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){

  context("Inputchecks - clv.fitted.transactions plot - newdata")
  clv.data.apparel.static.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans,
                                                                             data.apparelStaticCov = data.apparelStaticCov,
                                                                             estimation.split = 40)
  expect_silent(fitted.apparel.static <- pnbd(clv.data.apparel.static.cov, verbose = FALSE))
  expect_silent(clv.data.cdnow.nohold <- clvdata(data.cdnow, "ymd", "w", estimation.split = NULL))
  expect_silent(fitted.cdnow.nohold   <- pnbd(clv.data.cdnow.nohold, verbose = FALSE))

  # expect_silent(pnbd.cdnow <- pnbd(clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37), verbose = FALSE))
  l.std.args <- list(fitted.cdnow.nohold, prediction.end=6)

  # General
  fct.testthat.inputchecks.clvfitted.na.in.prediction.params.model(s3method = plot, clv.fitted = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.cov.na.in.prediction.params.cov(s3method = plot, clv.fitted.cov = fitted.apparel.static)

  # newdata
  fct.testthat.inputchecks.clvfitted.newdata.not.clvdata(clv.fitted = fitted.cdnow.nohold, data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedtransactions.newdata.has.different.covs(s3method = plot,
                                                                            clv.fitted.apparel.cov = fitted.apparel.static,
                                                                            data.apparelStaticCov = data.apparelStaticCov)
  fct.testthat.inputchecks.clvfittedtransactions.newdata.is.different.class(s3method = plot,
                                                                            clv.fitted.transactions.nocov = fitted.cdnow.nohold,
                                                                            clv.fitted.transactions.staticcov = fitted.apparel.static,
                                                                            clv.data.no.cov = clv.data.cdnow.nohold,
                                                                            clv.data.static.cov = clv.data.apparel.static.cov)


  context("Inputchecks - clv.fitted.transactions plot - prediction.end")
  fct.testthat.inputchecks.clvfittedtransactions.prediction.end.wrong.format(fitted.transactions = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.plot.prediction.end.before.estimation.start(fitted.transactions = fitted.cdnow.nohold)
  fct.testthat.inputchecks.clvfittedtransactions.prediction.end.uses.newdata(s3method = plot, fitted.cdnow = fitted.cdnow.nohold, data.cdnow = data.cdnow)

  context("Inputchecks - clv.fitted.transactions plot - cumulative")
  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "cumulative")

  context("Inputchecks - clv.fitted.transactions plot - transactions")
  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "transactions")

  context("Inputchecks - clv.fitted.transactions plot - label")
  fct.helper.inputcheck.single.character(fct = plot, l.std.args = l.std.args, name.param = "label", null.allowed = TRUE)

  context("Inputchecks - clv.fitted.transactions plot - plot")
  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "plot")

  context("Inputchecks - clv.fitted.transactions plot - verbose")
  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose")

  context("Inputchecks - clv.fitted.transactions plot - ...")
  fct.testthat.inputchecks.clvfittedtransactions.plot.ellipsis(fitted.transactions = fitted.cdnow.nohold)

}




skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.inputchecks.clvfittedtransactions.plot(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
