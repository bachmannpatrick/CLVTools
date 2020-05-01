# Tests that GGompertz / NBD models are consistent among themselves
skip_on_cran()
skip_on_ci()
skip_on_covr()


# gamma=0 ------------------------------------------------------------------------------------------------
context("Consistency - GGompertz / NBD - nocov vs staticcov gamma=0 ")

# . Data to play with ------------------------------------------------------------------------------------
data("cdnow")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd",
                                   time.unit = "w", estimation.split = 37))

dt.cov.static <- data.table::data.table(Id = unique(cdnow$Id), Gender=c(1, rep(c(1,0), 1178)))
expect_silent(clv.cdnow.static <- SetStaticCovariates(clv.cdnow,
                                                      data.cov.life = dt.cov.static, data.cov.trans = dt.cov.static,
                                                      names.cov.life = "Gender", names.cov.trans = "Gender"))

# Fit nocov model --------------------------------------------------------------------------------------
expect_silent(p.nocov <- ggomnbd(clv.cdnow, verbose=FALSE))

# Fit staticcov model ----------------------------------------------------------------------------------
#   Replace coefs with the ones from dyncov, set coefs for covs to 0
expect_silent(p.staticcov <- ggomnbd(clv.cdnow.static, verbose=FALSE))
expect_silent(p.staticcov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.beta", "log.b", "log.s",  "life.Gender", "trans.Gender")] <-
                c(p.nocov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.beta", "log.b", "log.s")], 0,0))


test_that("Predict yields same results for all models with gamma=0", {
  skip_on_cran()

  # DERT unequal to DECT because only predict short period!

  # Standard
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))

  # With prediction.end
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE, prediction.end = 6))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))

  # with discount rates
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))
})


test_that("plot yields same results for all models with gamma=0", {
  skip_on_cran()

  # Prediction end for faster calcs. Should not affect results
  expect_warning(dt.plot.no        <- plot(p.nocov, verbose=FALSE, plot=FALSE, prediction.end = 10),
                 regexp = "full holdout")
  expect_warning(dt.plot.staticcov <- plot(p.staticcov, verbose=FALSE, plot=FALSE, prediction.end = 10),
                 regexp = "full holdout")
  # Rename to random names because have different colnames by model
  data.table::setnames(dt.plot.no, c("A", "B", "C"))
  data.table::setnames(dt.plot.staticcov, c("A", "B", "C"))
  expect_true(isTRUE(all.equal(dt.plot.no, dt.plot.staticcov)))
})
