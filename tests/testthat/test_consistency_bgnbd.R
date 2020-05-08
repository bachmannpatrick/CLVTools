#
# Tests that BG/NBD models are consistent among themselves
skip_on_cran()


# Consistency
# nocov vs static cov:
#   same fit with all covs = 0
#   same predict with gamma=0




context("Nocov/cov Consistency - BG/NBD - all cov data = 0")
data("apparelTrans")
expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "w",
                                     estimation.split = 38))

data("apparelStaticCov")
# Cannot set all to 0 as requires at least 2 distinct values per cov
expect_silent(apparelStaticCov.0 <- apparelStaticCov)
expect_silent(apparelStaticCov.0[, Gender := 0])
expect_silent(apparelStaticCov.0[1, Gender := 1])
expect_silent(apparelStaticCov.0[, Channel := 0])
expect_silent(apparelStaticCov.0[1, Channel := 1])
expect_silent(clv.apparel.static <- SetStaticCovariates(clv.apparel,
                                                        data.cov.life = apparelStaticCov.0, data.cov.trans = apparelStaticCov.0,
                                                        names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))


# Fit models
expect_silent(b.nocov  <- bgnbd(clv.apparel, verbose = FALSE))
expect_silent(b.static <- bgnbd(clv.apparel.static, verbose = FALSE))

# test_that("Cov params are insignificant", {
#   expect_true(all(coef(summary(b.static))[c("life.Gender", "life.Channel", "trans.Gender", "trans.Channel"), 4] > 0.1))
# })

test_that("Model parameters are nearly the same", {
  expect_true(all.equal(coef(b.nocov), coef(b.static)[c("r", "alpha", "a", "b")], tolerance = 0.05))
})



context("Nocov/cov Consistency - BG/NBD - cov params = 0")

# Set parameters ------------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov

# static cov
expect_silent(b.static@prediction.params.model[c("r", "alpha", "a", "b")] <-
                b.nocov@prediction.params.model[c("r", "alpha", "a", "b")])
expect_silent(b.static@prediction.params.life[c("Gender", "Channel")] <- 0)
expect_silent(b.static@prediction.params.trans[c("Gender", "Channel")] <- 0)

# Actual tests ---------------------------------------------------------------------------------

test_that("Predict yields same results for all models with gamma=0", {

  # DERT unequal to DECT because only predict short period!

  # Standard
  expect_silent(dt.pred.nocov    <- predict(b.nocov, verbose=FALSE))
  expect_silent(dt.pred.static   <- predict(b.static, verbose=FALSE))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))

  # With prediction.end
  expect_silent(dt.pred.nocov     <- predict(b.nocov,  verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.static    <- predict(b.static, verbose=FALSE, prediction.end = 6))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))

  # with discount rates
  expect_silent(dt.pred.nocov     <- predict(b.nocov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.static    <- predict(b.static, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))
})


test_that("plot yields same results for all models with gamma=0", {
  # Prediction end for faster calcs. Should not affect results
  expect_warning(dt.plot.nocov     <- plot(b.nocov, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")
  expect_warning(dt.plot.static    <- plot(b.static, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")

  # Rename to random names because have different colnames by model
  data.table::setnames(dt.plot.nocov, c("A", "B", "C"))
  data.table::setnames(dt.plot.static , c("A", "B", "C"))
  expect_true(isTRUE(all.equal(dt.plot.nocov, dt.plot.static)))
})
