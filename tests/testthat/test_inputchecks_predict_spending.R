
fct.testthat.inputchecks.clvfittedspending.predict.newdata.has.no.spending <- function(fitted.spending, data.cdnow){
  test_that("Fails if newdata has no spending data", {
    clv.newdata.nospending <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = NULL)
    expect_error(predict(fitted.spending, clv.newdata.nospending),regexp = "spending")
  })
}


fct.testthat.inputchecks.clvfittedspending.predict.ellipsis <- function(fitted.spending){

  test_that("Stop if unnecessary inputs given in ellipsis", {
    expect_error(predict(fitted.spending, abc = 123), "further parameters")
    expect_error(predict(fitted.spending, use.cor = TRUE), "further parameters")
    expect_error(predict(fitted.spending, prediction.end = 6), "further parameters")
  })

}


fct.testthat.inputchecks.clvfittedspending.predict <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){
  clv.data.apparel.static.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                             estimation.split = 52)
  expect_silent(fitted.spending <- gg(clv.data.apparel.static.cov, verbose = FALSE))

  # General
  fct.testthat.inputchecks.clvfitted.na.in.prediction.params.model(s3method = predict, clv.fitted = fitted.spending, l.s3method.args=list())

  # Newdata
  fct.testthat.inputchecks.clvfitted.newdata.not.clvdata(s3method=predict, l.std.args=list(fitted.spending), data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedspending.predict.newdata.has.no.spending(fitted.spending = fitted.spending, data.cdnow = data.cdnow)


  l.std.args <- list(object=fitted.spending)
  .fct.helper.inputchecks.single.logical(fct = predict, l.std.args = l.std.args,
                                         name.param = "verbose", null.allowed=FALSE)

  fct.testthat.inputchecks.clvfittedspending.predict.ellipsis(fitted.spending = fitted.spending)
}


data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
fct.testthat.inputchecks.clvfittedspending.predict(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
