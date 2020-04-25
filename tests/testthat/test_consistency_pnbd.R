# Tests that PNBD models are consistent among themselves
skip_on_cran()


# Consistency
# nocov vs static cov:
#   same fit with all covs = 0
#   same predict with gamma=0

# nocov vs dyncov:
#   same fit with all covs = 0
#   same predict with gamma=0


context("Nocov/cov Consistency - PNBD - all cov data = 0")
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
# Cannot fully fit dyncov as takes way too long
expect_silent(p.nocov  <- pnbd(clv.apparel, verbose = FALSE))
expect_silent(p.static <- pnbd(clv.apparel.static, verbose = FALSE))

test_that("Cov params are insignificant", {
  expect_true(all(coef(summary(p.static))[c("life.Gender", "life.Channel", "trans.Gender", "trans.Channel"), 4] > 0.1))
})

test_that("Model parameters are nearly the same", {
  expect_true(all.equal(coef(p.nocov), coef(p.static)[c("r", "alpha", "s","beta")], tolerance = 0.05))
})



context("Nocov/cov Consistency - PNBD - prediction with cov params = 0")
# Also possible for dyncov

# Create dyncov model, quickly ------------------------------------------------------------------
data("apparelDynCov")
expect_message(clv.apparel.dyn  <- SetDynamicCovariates(clv.apparel,name.id = "Id",name.date = "Cov.Date",
                                                        data.cov.life  = apparelDynCov,names.cov.life = c("Marketing", "Gender", "Channel"),
                                                        data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel")),
               regexp = "cut off")

expect_warning(p.dyncov <- pnbd(clv.apparel.dyn, start.params.model = c(r=1, alpha=3, s=1, beta=3),
                                optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                                   hessian=FALSE, # no hessian
                                                   control=list(kkt=FALSE, # kkt takes forever
                                                                reltol = 1000)),
                                verbose = FALSE),
               regexp = "Hessian")


# Set parameters ------------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov

# static cov
expect_silent(p.static@prediction.params.model[c("r", "alpha", "s", "beta")] <-
                p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])
expect_silent(p.static@prediction.params.life[c("Gender", "Channel")] <- 0)
expect_silent(p.static@prediction.params.trans[c("Gender", "Channel")] <- 0)

# dyncov
expect_silent(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")] <-
                p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])
expect_silent(p.dyncov@prediction.params.life[c("Marketing", "Gender", "Channel")] <- 0)
expect_silent(p.dyncov@prediction.params.trans[c("Marketing", "Gender", "Channel")] <- 0)

# Recalculate the LL data for these fake params
expect_silent(log.params <- setNames(log(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")]),
                                     c("log.r", "log.alpha", "log.s", "log.beta")))
expect_silent(log.params[c("trans.Marketing", "trans.Gender", "trans.Channel", "life.Marketing", "life.Gender", "life.Channel")] <- 0)
expect_silent(p.dyncov@LL.data <- CLVTools:::pnbd_dyncov_LL(params=log.params, clv.fitted = p.dyncov))


# Actual tests ---------------------------------------------------------------------------------

test_that("Predict yields same results for all models with gamma=0", {

  # DERT unequal to DECT because only predict short period!

  # Standard
  expect_silent(dt.pred.nocov    <- predict(p.nocov, verbose=FALSE))
  expect_silent(dt.pred.static   <- predict(p.static, verbose=FALSE))
  expect_silent(dt.pred.dyncov   <- predict(p.dyncov, verbose=FALSE))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))
  expect_true(isTRUE(all.equal(dt.pred.nocov[,  !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))

  # With prediction.end
  expect_silent(dt.pred.nocov     <- predict(p.nocov,  verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.static    <- predict(p.static, verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.dyncov    <- predict(p.dyncov, verbose=FALSE, prediction.end = 6))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))
  expect_true(isTRUE(all.equal(dt.pred.nocov[,  !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))


  # with discount rates
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.static    <- predict(p.static, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.dyncov    <- predict(p.dyncov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.static)))
  expect_true(isTRUE(all.equal(dt.pred.nocov[,  !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))
})


test_that("plot yields same results for all models with gamma=0", {
  # Prediction end for faster calcs. Should not affect results
  expect_warning(dt.plot.nocov     <- plot(p.nocov, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")
  expect_warning(dt.plot.static    <- plot(p.static, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")
  expect_warning(dt.plot.dyncov    <- plot(p.dyncov, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")

  # Rename to random names because have different colnames by model
  data.table::setnames(dt.plot.nocov, c("A", "B", "C"))
  data.table::setnames(dt.plot.static , c("A", "B", "C"))
  data.table::setnames(dt.plot.dyncov, c("A", "B", "C"))
  expect_true(isTRUE(all.equal(dt.plot.nocov, dt.plot.static)))
  expect_true(isTRUE(all.equal(dt.plot.nocov, dt.plot.dyncov)))
})
