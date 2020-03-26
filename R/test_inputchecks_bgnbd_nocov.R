# Load required data -----------------------------------------------------------------------------------
data("cdnow")

expect_silent(clv.data.cdnow.no.holdout   <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.cdnow.with.holdout <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args.noholdout <- list(clv.data=clv.data.cdnow.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)


context("Checkinputs - bgnbd nocov - Model specific")
test_that("Fails for start params <= 0", {
  expect_error(bgnbd(clv.data.cdnow.no.holdout, start.params.model = c(r=0, alpha=1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.cdnow.no.holdout, start.params.model = c(r=-1, alpha=1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.cdnow.no.holdout, start.params.model = c(r=1, alpha=1, a=0, b=1)),
               regexp = "greater")

  expect_error(bgnbd(clv.data.cdnow.with.holdout, start.params.model = c(r=0, alpha=1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.cdnow.with.holdout, start.params.model = c(r=-1, alpha=1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.cdnow.with.holdout, start.params.model = c(r=1, alpha=1, a=0, b=1)),
               regexp = "greater")
})

test_that("Spending fit cannot predict on newdata that has no spending", {
  # Spending fit
  expect_silent(clv.bgnbd.spending <- bgnbd(clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37),
                                          verbose = FALSE))
  # Data without spending
  expect_silent(clv.cdnow.nospending <- clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_error(predict(clv.bgnbd.spending, newdata=clv.cdnow.nospending, verbose=FALSE, predict.spending=TRUE),
               regexp = "there is no spending data")

  # but works without spending
  expect_silent(dt.pred <- predict(clv.bgnbd.spending, newdata=clv.cdnow.nospending, predict.spending=FALSE, verbose=FALSE))
  expect_false(any(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})
