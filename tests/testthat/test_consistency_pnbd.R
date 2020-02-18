# Tests that PNBD models are consistent among themselves
skip_on_cran()
skip_on_ci()
skip_on_covr()

# gamma=0 ------------------------------------------------------------------------------------------------
context("Consistency - PNBD - gamma=0")

# . Data to play with ------------------------------------------------------------------------------------
data("cdnow")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd",
                                   time.unit = "w", estimation.split = 37))

dt.cov.static <- data.table::data.table(Id = unique(cdnow$Id), Gender=c(1, rep(c(1,0), 1178)))
expect_silent(clv.cdnow.static <- SetStaticCovariates(clv.cdnow,
                                                      data.cov.life = dt.cov.static, data.cov.trans = dt.cov.static,
                                                      names.cov.life = "Gender", names.cov.trans = "Gender"))

dt.cov.dyn <- data.table::data.table(expand.grid(Id = unique(cdnow$Id),
                                                 Date = seq(lubridate::ymd("1996-12-29"),
                                                            lubridate::ymd("1998-06-28"),
                                                            by = "weeks")))
dt.cov.dyn[, Haircolor := c(1, rep(c(1,2), nrow(dt.cov.dyn)/2))]
expect_silent(clv.cdnow.dyn <- SetDynamicCovariates(clv.data = clv.cdnow,
                                                    data.cov.life = dt.cov.dyn, data.cov.trans = dt.cov.dyn,
                                                    names.cov.life = "Haircolor", names.cov.trans = "Haircolor"))

# Fit nocov model --------------------------------------------------------------------------------------
#   Replace coefs with the ones from dyncov
expect_silent(p.nocov <- pnbd(clv.cdnow, verbose=FALSE))

# Fit staticcov model ----------------------------------------------------------------------------------
#   Replace coefs with the ones from dyncov, set coefs for covs to 0
expect_silent(p.staticcov <- pnbd(clv.cdnow.static, verbose=FALSE))
expect_silent(p.staticcov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.s", "log.beta", "life.Gender", "trans.Gender")] <-
                c(p.nocov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.s", "log.beta")], 0,0))


# Fit dyncov model --------------------------------------------------------------------------------------
expect_warning(p.dyncov <- pnbd(clv.cdnow.dyn, start.params.model = c(r=1, alpha=3, s=1, beta=3),
                 optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                    hessian=FALSE, # no hessian
                                    control=list(kkt=FALSE, # kkt takes forever
                                                 reltol = 1000)),
                 verbose = FALSE),
               regexp = "Hessian")

# Set params to nocov params cov gammas=0
expect_silent(p.dyncov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.s", "log.beta")] <-
                p.nocov@optimx.estimation.output[1, c("log.r", "log.alpha", "log.s", "log.beta")])
expect_silent(p.dyncov@optimx.estimation.output[1, c("life.Haircolor","trans.Haircolor")] <- c(0,0))

# Recalculate the LL data for these fake params
expect_silent(log.params <- coef(p.dyncov))
expect_silent(log.params[c("log.r", "log.alpha", "log.s", "log.beta")] <- log(log.params[c("r", "alpha", "s", "beta")]))
expect_silent(log.params <-log.params[c("log.r", "log.alpha", "log.s", "log.beta","trans.Haircolor", "life.Haircolor")])
expect_silent(p.dyncov@LL.data <- CLVTools2:::pnbd_dyncov_LL(params=log.params, obj = p.dyncov))


# gamma=0 ------------------------------------------------------------------------------------------------
context("Consistency - PNBD - gamma=0")

test_that("Predict yields same results for all models with gamma=0", {
  skip_on_cran()

  # DERT unequal to DECT because only predict short period!

  # Standard
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE))
  expect_silent(dt.pred.dyncov    <- predict(p.dyncov, verbose=FALSE))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))
  expect_true(isTRUE(all.equal(dt.pred.staticcov[, !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))

  # With prediction.end
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE, prediction.end = 6))
  expect_silent(dt.pred.dyncov    <- predict(p.dyncov, verbose=FALSE, prediction.end = 6))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))
  expect_true(isTRUE(all.equal(dt.pred.staticcov[, !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))


  # with discount rates
  expect_silent(dt.pred.nocov     <- predict(p.nocov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.staticcov <- predict(p.staticcov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(dt.pred.dyncov    <- predict(p.dyncov, verbose=FALSE, continuous.discount.factor = 0.25))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.staticcov)))
  expect_true(isTRUE(all.equal(dt.pred.staticcov[, !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))
})


test_that("plot yields same results for all models with gamma=0", {
  skip_on_cran()

  # Prediction end for faster calcs. Should not affect results
  expect_warning(dt.plot.no        <- plot(p.nocov, verbose=FALSE, plot=FALSE, prediction.end = 10),
                 regexp = "full holdout")
  expect_warning(dt.plot.staticcov <- plot(p.staticcov, verbose=FALSE, plot=FALSE, prediction.end = 10),
                 regexp = "full holdout")
  expect_warning(dt.plot.dyncov    <- plot(p.dyncov, verbose=FALSE, plot=FALSE, prediction.end = 10),
                 regexp = "full holdout")

  # Rename to random names because have different colnames by model
  data.table::setnames(dt.plot.no, c("A", "B", "C"))
  data.table::setnames(dt.plot.staticcov, c("A", "B", "C"))
  data.table::setnames(dt.plot.dyncov, c("A", "B", "C"))
  expect_true(isTRUE(all.equal(dt.plot.no, dt.plot.staticcov)))
  expect_true(isTRUE(all.equal(dt.plot.staticcov, dt.plot.dyncov)))
})


# static cov data ------------------------------------------------------------------------------------------------
context("Consistency - PNBD - static cov data")
# Same covs for static and dyncov

# . Data preparation ---------------------------------------------------------------------------------------------
# Dyncov corresponds to static cov data
expect_silent(dt.cov.dyn[dt.cov.static, Gender := i.Gender, on="Id"])
expect_silent(clv.static.dyn <- SetDynamicCovariates(clv.data =clv.cdnow,
                                                     data.cov.life = dt.cov.dyn, data.cov.trans = dt.cov.dyn,
                                                     names.cov.life = "Gender", names.cov.trans = "Gender"))

# set dyncov params to same as static cov
expect_warning(p.static.dyncov <-
                 pnbd(clv.static.dyn, start.params.model = c(r=1, alpha=3, s=1, beta=3),
                      optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                         hessian=FALSE, # no hessian
                                         control=list(kkt=FALSE, # kkt takes forever
                                                      reltol = 1000)),
                      verbose = FALSE),
               regexp = "Hessian")

# Set params to staticcov params
expect_silent(p.static.dyncov@optimx.estimation.output[1, c("log.r","log.alpha","log.s","log.beta", "life.Gender","trans.Gender")] <-
                p.staticcov@optimx.estimation.output[1, c("log.r","log.alpha","log.s","log.beta", "life.Gender","trans.Gender")])

# Recalculate the LL data for these fake params
expect_silent(log.params <- coef(p.static.dyncov))
expect_silent(log.params[c("log.r", "log.alpha", "log.s", "log.beta")] <- log(log.params[c("r", "alpha", "s", "beta")]))
expect_silent(log.params <-log.params[c("log.r", "log.alpha", "log.s", "log.beta","trans.Gender", "life.Gender")])
expect_silent(p.static.dyncov@LL.data <- CLVTools:::pnbd_dyncov_LL(params=log.params, obj = p.static.dyncov))

# . Run plot and predict
test_that("Predict yields same result as static cov", {
  expect_silent(dt.pred.static <- predict(p.staticcov, verbose=FALSE))
  expect_silent(dt.pred.dyncov <- predict(p.static.dyncov, verbose=FALSE))
  expect_silent(data.table::setnames(dt.pred.dyncov, old="DECT",new = "DERT"))
  expect_true(isTRUE(all.equal(dt.pred.static[, !c("DERT", "predicted.CLV")],
                               dt.pred.dyncov[, !c("DERT", "predicted.CLV")])))
})

test_that("Plot yields same result as static cov", {
  expect_warning(dt.plot.static <- plot(p.staticcov, verbose=FALSE, prediction.end = 10, plot=FALSE),
                 regexp = "full holdout")
  expect_warning(dt.plot.dyncov <- plot(p.static.dyncov, verbose=FALSE, prediction.end = 10, plot=FALSE),
                 regexp = "full holdout")
  data.table::setnames(dt.plot.static, c("A", "B", "C"))
  data.table::setnames(dt.plot.dyncov, c("A", "B", "C"))
  expect_true(isTRUE(all.equal(dt.plot.static, dt.plot.dyncov)))
})




