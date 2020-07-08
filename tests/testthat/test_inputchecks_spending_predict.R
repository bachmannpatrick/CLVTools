
fct.testthat.inputchecks.clvfittedspending.predict.newdata.has.no.spending <- function(fitted.spending, data.cdnow){
  test_that("Fails if newdata has no spending data", {
    clv.newdata.nospending <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = NULL)
    expect_error(predict(fitted.spending, clv.newdata.nospending),regexp = "spending")
  })
}


fct.testthat.inputchecks.clvfittedspending <- function(data.cdnow, data.apparelTrans, data.apparelStaticCov){
  context("Inputchecks - clv.fitted.spending predict - newdata")
  clv.data.apparel.static.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                             estimation.split = 40)
  expect_silent(fitted.spending <- gg(clv.data.apparel.static.cov, verbose = FALSE))

  # fct.testthat.inputchecks.clvfitted.predict.newdata.not.clvdata(clv.fitted = fitted.spending, data.cdnow = data.cdnow)
  fct.testthat.inputchecks.clvfittedspending.predict.newdata.has.no.spending(fitted.spending = fitted.spending, data.cdnow = data.cdnow)


  context("Inputchecks - clv.fitted.spending predict - verbose")
  l.std.args <- list(object=fitted.spending)
  .fct.helper.inputchecks.single.logical(fct = predict, l.std.args = l.std.args,
                                         name.param = "verbose", null.allowed=FALSE)

  # context("Inputchecks - clv.fitted.spending predict - ...")
  # fct.testthat.inputchecks.clvfitted.predict.ellipsis
}


data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
fct.testthat.inputchecks.clvfittedspending(data.cdnow = cdnow, data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)
