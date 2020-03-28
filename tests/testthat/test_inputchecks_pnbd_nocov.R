# Load required data -----------------------------------------------------------------------------------
data("cdnow")

expect_silent(clv.data.cdnow.no.holdout   <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.cdnow.with.holdout <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args.noholdout <- list(clv.data=clv.data.cdnow.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)


# Parameter clv.data ------------------------------------------------------------------------------------
fct.helper.inputchecks.check.all.no.cov.model(fct.model = pnbd, l.std.args = l.std.args.noholdout,   name.model="pnbd nocov")
fct.helper.inputchecks.check.all.no.cov.model(fct.model = pnbd, l.std.args = l.std.args.withholdout, name.model="pnbd nocov")

context("Inputchecks - pnbd nocov - Model specific")
test_that("Fails for start params <= 0", {
  expect_error(pnbd(clv.data.cdnow.no.holdout, start.params.model = c(alpha=0, beta=1, r=1, s=1)),
               regexp = "greater")
  expect_error(pnbd(clv.data.cdnow.no.holdout, start.params.model = c(alpha=-1, beta=1, r=1, s=1)),
               regexp = "greater")
  expect_error(pnbd(clv.data.cdnow.no.holdout, start.params.model = c(alpha=1, beta=1, r=0, s=1)),
               regexp = "greater")

  expect_error(pnbd(clv.data.cdnow.with.holdout, start.params.model = c(alpha=0, beta=1, r=1, s=1)),
               regexp = "greater")
  expect_error(pnbd(clv.data.cdnow.with.holdout, start.params.model = c(alpha=-1, beta=1, r=1, s=1)),
               regexp = "greater")
  expect_error(pnbd(clv.data.cdnow.with.holdout, start.params.model = c(alpha=1, beta=1, r=0, s=1)),
               regexp = "greater")
})


test_that("Spending fit cannot predict on newdata that has no spending", {
  # Spending fit
  expect_silent(clv.pnbd.spending <- pnbd(clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37),
                                          verbose = FALSE))
  # Data without spending
  expect_silent(clv.cdnow.nospending <- clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_error(predict(clv.pnbd.spending, newdata=clv.cdnow.nospending, verbose=FALSE, predict.spending=TRUE),
               regexp = "there is no spending data")

  # but works without spending
  expect_silent(dt.pred <- predict(clv.pnbd.spending, newdata=clv.cdnow.nospending, predict.spending=FALSE, verbose=FALSE))
  expect_false(any(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})

