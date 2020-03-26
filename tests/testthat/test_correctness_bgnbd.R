# Load required data ---------------------------------------------------------------------------------
data("cdnow")

context("Correctness - BGNBD nocov - Recover parameters")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                   estimation.split = 38))

test_that("Cdnow nocov correct coefs and SE", {
  expect_silent(e.bgnbd.cdnow.nocov<-bgnbd(clv.data=clv.cdnow, start.params.model = c(r=1, alpha = 3, a = 1, b = 3), verbose=FALSE))
})


context("Correctness - PNBD nocov - predict")

test_that("Same when predicting as with fitting data", {
  # skip_on_cran()
  expect_silent(bgnbd.cdnow.fit <- bgnbd(clv.cdnow, verbose = FALSE))
  expect_true(isTRUE(all.equal(predict(bgnbd.cdnow.fit),
                               predict(bgnbd.cdnow.fit, newdata = clv.cdnow))))
})

test_that("fitting sample and predicting full data yields same results as predicting sample only", {
  skip_on_cran()
  # Sample only
  cdnow.id.sample <- unique(cdnow$Id)[1:100]
  cdnow.sample <- cdnow[Id %in% cdnow.id.sample]
  expect_silent(clv.cdnow.sample <- clvdata(data.transactions = cdnow.sample, date.format = "ymd",
                                            time.unit = "w", estimation.split = clv.cdnow@clv.time@timepoint.estimation.end))
  # Fit on sample only
  expect_silent(fitted.bgnbd.nocov.sample <- bgnbd(clv.cdnow.sample, verbose = FALSE))

  # Holdout has to start for both on the exact same timepoint (might differ because of different estimation start)
  stopifnot(clv.cdnow.sample@clv.time@timepoint.holdout.start == clv.cdnow@clv.time@timepoint.holdout.start)
  # Prediction.end
  # Predict sample only
  expect_silent(dt.predict.sample <- predict(fitted.bgnbd.nocov.sample, verbose=FALSE,
                                             predict.spending=FALSE, prediction.end="1998-07-06"))
  # Predict on full
  expect_silent(dt.predict.full <- predict(fitted.bgnbd.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))

  expect_true(nrow(dt.predict.sample) == 100)
  expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

  # The sample ones should be the exact same ones in the full
  expect_true(isTRUE(all.equal(dt.predict.sample,
                               dt.predict.full[Id %in% dt.predict.sample$Id])))

})
