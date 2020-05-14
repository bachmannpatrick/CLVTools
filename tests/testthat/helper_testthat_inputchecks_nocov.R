
fct.testthat.inputchecks.nocov.fails.for.start.params.subzero <- function(method, clv.data.no.holdout, clv.data.with.holdout, l.start.params.model)
test_that("Fails for start params <= 0", {
  lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.no.holdout)
  lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.with.holdout)
})

fct.testthat.inputchecks.helper.expect.error.for.params <- function(start.params.model, method, clv.data){
  l.args <- list(clv.data = clv.data, start.params.model = start.params.model)

  expect_error(do.call(what = method, args = l.args), regexp = "greater")
}

fct.testthat.inputchecks.nocov.cannot.predict.without.spending <- function(method, cdnow)
test_that("Spending fit cannot predict on newdata that has no spending", {
  l.args <- list(clv.data = clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37),
                 verbose = FALSE)
  # Spending fit
  expect_silent(clv.spending <- do.call(what = method, args = l.args))
  # Data without spending
  expect_silent(clv.cdnow.nospending <- clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_error(predict(clv.spending, newdata=clv.cdnow.nospending, verbose=FALSE, predict.spending=TRUE),
               regexp = "there is no spending data")

  # but works without spending
  expect_silent(dt.pred <- predict(clv.spending, newdata=clv.cdnow.nospending, predict.spending=FALSE, verbose=FALSE))
  expect_false(any(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})
