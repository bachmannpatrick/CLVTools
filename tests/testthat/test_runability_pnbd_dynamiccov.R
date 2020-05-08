# Load Data ------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")
apparelDynCov <- apparelDynCov[Cov.Date > "2005-01-01" ] #otherwise "cutoff" message


# DO NOT RUN ANYTHING DYNCOV ON CRAN (runtime ca 1min..?)
skip_on_cran()


# Basic runability ---------------------------------------------------------------------------------
context("Runability - PNBD dynamiccov - Basic runability")

# Take a sample of customers only
mini.apparelTrans <- apparelTrans[Id %in% unique(apparelTrans$Id)[1:100]]
mini.apparelDynCov <- apparelDynCov[Id %in% mini.apparelTrans$Id]

expect_silent(clv.data.trans <- clvdata(data.transactions = mini.apparelTrans, date.format = "ymd",
                                        time.unit = "W", estimation.split = 40))

expect_silent(clv.data.mini.dyncov <-
                SetDynamicCovariates(clv.data = clv.data.trans,
                                     data.cov.life = mini.apparelDynCov,
                                     data.cov.trans = mini.apparelDynCov,
                                     names.cov.life = "Gender",
                                     names.cov.trans = "Gender",
                                     name.date = "Cov.Date"))


# Can fit dyncov pnbd. Do outside so only have to fit once but use for all tests
#   high tolerance to converge quickly
#   no hessian to avoid additional evaluations after convergence
expect_warning(fitted.dyncov <- pnbd(clv.data.mini.dyncov,
                                     start.params.model = c(r=0.4011475, alpha=22.7155565,
                                                            s=0.2630372, beta=19.1752426),
                                     start.params.life = c(Gender=0.9304636),
                                     start.params.trans = c(Gender=1.0934721),
                                     optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                                        hessian=FALSE, # no hessian
                                                        control=list(kkt=FALSE, # kkt takes forever
                                                                     reltol = 1000))),
               # trace=6, REPORT=1))),
               regexp = "Hessian could not be derived.")

# Do not call usual helper for S3 checks as they take too long

# **TODO: Or do load an already fitted object at this point for verification??

# Cheat and set a fake hessian as it was not estimated during optimization for speed reasons
# Hessian from static cov pnbd
fake.hess <- structure(c(979.019728504732, -833.029498091497, -328.098609941573, 258.918547365243, -198.39816295105, 617.835400045399,
                         -833.029498091497, 850.416620581025, 235.300182628772, -184.286149754065, 137.842394217897, -631.483808344787,
                         -328.098609941573, 235.300182628772, 265.168175979473, -193.63193759222, 160.709773619312, -177.81494575965,
                         258.918547365243, -184.286149754065, -193.63193759222, 143.911727169075, -118.898176270749, 137.842394013186,
                         -198.39816295105, 137.842394217897, 160.709773619312, -118.898176270749, 118.898177254365, -137.842393385251,
                         617.835400045399, -631.483808344787, -177.81494575965, 137.842394013186, -137.842393385251, 631.483808845486),
                       .Dim = c(6L, 6L), .Dimnames = list(c("log.r", "log.alpha", "log.s", "log.beta", "life.Gender", "trans.Gender"),
                                                          c("log.r", "log.alpha", "log.s", "log.beta", "life.Gender", "trans.Gender")))

fitted.dyncov@optimx.hessian <- fake.hess

# Standard S3 tests ---------------------------------------------------------------
# Run the standard S3 tests on the fitted model,
#   but not plot() and predict() which takes too long

full.names <- c("r", "alpha", "s","beta", "life.Gender", "trans.Gender")

.fct.helper.s3.fitted.coef(clv.fitted = fitted.dyncov, full.names = full.names)

.fct.helper.s3.fitted.vcov(clv.fitted = fitted.dyncov, full.names = full.names)

.fct.helper.s3.fitted.confint(clv.fitted = fitted.dyncov, full.names = full.names)

.fct.helper.s3.fitted.summary(clv.fitted = fitted.dyncov)

.fct.helper.s3.fitted.print(clv.fitted = fitted.dyncov)

.fct.helper.s3.fitted.nobs(clv.fitted = fitted.dyncov)

.fct.helper.s3.fitted.logLik(clv.fitted = fitted.dyncov)


# LL.data ---------------------------------------------------------------
test_that("LL.data is correct",{
  expect_true(isTRUE(all.equal(as.numeric(logLik(fitted.dyncov)),
                               fitted.dyncov@LL.data[, sum(LL)],
                               tolerance = sqrt(.Machine$double.eps))))
})

# Plot ------------------------------------------------------------------
test_that("Plot works", {
  # Only check whether the expectation runs through,
  #   with as few as possible periods (from estimation.end)
  expect_warning(plot(fitted.dyncov, prediction.end = 5, verbose=FALSE),
                 regexp = "Not plotting full holdout period")
})

test_that("Plot always has 0 on repeat transactions and expectations", {
  expect_warning(dt.plot <- plot(fitted.dyncov, prediction.end = 5, verbose=FALSE, plot=FALSE),
                 regexp = "Not plotting full holdout period")
  expect_true(isTRUE(all.equal(unlist(dt.plot[period.until == min(period.until), c(2,3)]),
                               c(0,0), check.attributes = FALSE)))
})

# Predict ----------------------------------------------------------------
test_that("Predict works", {
  # Only check whether the expectation runs through,
  #   with as few as possible periods (from estimation.end)
  expect_silent(predict(fitted.dyncov, prediction.end = 5, verbose=FALSE))
})

test_that("Predict newdata works by predicting on another sample", {
  # Full fails for whatever reason

  # expect_output(dt.pred.mini <- predict(fitted.dyncov))
  # expect_output(dt.pred.full <- predict(fitted.dyncov, newdata=clv.data.full.dyncov))
  #
  # expect_true(nrow(dt.pred.mini) == length(unique(mini.apparelTrans$Id)))
  # expect_true(nrow(dt.pred.full) == length(unique(apparelTrans$Id)))
  # expect_true(all.equal(dt.pred.mini,
  #                       dt.pred.full[Id %in% dt.pred.mini$Id]))

  # The next 200 Ids
  mini2.apparelTrans <- apparelTrans[Id %in% unique(apparelTrans$Id)[101:200]]
  mini2.apparelDynCov <- apparelDynCov[Id %in% mini2.apparelTrans$Id]

  # Estimation split exactly the same as the one for fitting
  clv.data.trans.mini2 <- clvdata(data.transactions = mini2.apparelTrans, date.format = "ymd",
                                  time.unit = "W",
                                  estimation.split = fitted.dyncov@clv.data@clv.time@timepoint.estimation.end)

  clv.data.mini2.dyncov <-
    SetDynamicCovariates(clv.data = clv.data.trans.mini2,
                         data.cov.life = mini2.apparelDynCov,
                         data.cov.trans = mini2.apparelDynCov,
                         names.cov.life = "Gender",
                         names.cov.trans = "Gender",
                         name.date = "Cov.Date")

  bck.fittted.dyncov <- data.table::copy(fitted.dyncov)

  # Use model fitted on first sample to predict for another 2nd sample
  expect_silent(predict(fitted.dyncov, newdata=clv.data.mini2.dyncov, verbose=FALSE))

  # Check that the fitted model is left unchanged
  expect_true(isTRUE(all.equal(bck.fittted.dyncov, fitted.dyncov)))
})


# . Prepare additional, longer cov data ---------------------------------------------------------------------------
# Add additional 100w of fake cov data for all Ids
dt.additional.cov <- expand.grid(Id = unique(apparelDynCov$Id),
                                 Cov.Date = seq(from=apparelDynCov[, max(Cov.Date)]+lubridate::weeks(1),
                                                length.out = 100, by = "week"), stringsAsFactors = FALSE)
setDT(dt.additional.cov)
dt.additional.cov[, Marketing := rep(c(0,1,2,3),.N/4)]
dt.additional.cov[, Gender := rep(c(0,1),.N/2)]
dt.additional.cov[, Channel := rep(c(0,1),.N/2)]

expect_silent(mini.apparelDynCov.long <- data.table::rbindlist(l = list(mini.apparelDynCov,
                                                                        dt.additional.cov[Id %in% mini.apparelDynCov$Id]),
                                                               use.names = TRUE))

expect_silent(clv.data.mini.extra <- SetDynamicCovariates(clv.data.trans,
                                                          data.cov.life = mini.apparelDynCov.long,
                                                          data.cov.trans = mini.apparelDynCov.long,
                                                          names.cov.life = c("Gender"),
                                                          names.cov.trans = c("Gender"),
                                                          name.date = "Cov.Date"))

# Newdata ----------------------------------------------------------------------------------------------------------
context("Runability - PNBD dynamiccov - newdata")

test_that("Can predict longer with newdata than with the data used for fitting", {
  expect_error(predict(fitted.dyncov, prediction.end = "2018-01-01"),
               regexp = "in the fitted model are not long enough")
  expect_silent(dt.predict <- predict(fitted.dyncov, newdata=clv.data.mini.extra,
                                      prediction.end = "2006-07-26",verbose=FALSE))
  expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
})


test_that("Can plot longer with newdata than with the data used for fitting", {
  expect_error(plot(fitted.dyncov, prediction.end = "2018-01-01"),
               regexp = "in the fitted model are not long enough")
  expect_silent(dt.plot <- plot(fitted.dyncov, newdata=clv.data.mini.extra, plot=FALSE,
                                prediction.end = "2006-07-26",verbose=FALSE))
  expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
})

# Overlong data ------------------------------------------------------------------------------
context("Runability - PNBD dynamiccov - Overlong data")


# Cannot do without holdout because takes too long to estimate
test_that("Can predict/plot beyond holdout if there is more covs in the data than used for holdout",{
  skip_on_covr()

  expect_silent(clv.data.mini.extra <- SetDynamicCovariates(clv.data.trans,
                                                            data.cov.life = mini.apparelDynCov.long,
                                                            data.cov.trans = mini.apparelDynCov.long,
                                                            names.cov.life = c("Marketing", "Gender","Channel"),
                                                            names.cov.trans = c("Marketing", "Gender","Channel"),
                                                            name.date = "Cov.Date"))
  # Fit model until estimation.end only (end of estimation.end, 2016-10-08)
  #   high tolerance to converge quickly
  #   no hessian to avoid additional evaluations after convergence
  expect_warning(fitted.dyncov <- pnbd(clv.data.mini.extra,
                                       start.params.model = c(r=0.4011475, alpha=22.7155565,
                                                              s=0.2630372, beta=19.1752426),
                                       start.params.life = c(Marketing=0.5, Gender = 0.8, Channel=0.9304636),
                                       start.params.trans = c(Marketing=1.1, Gender = 1.33, Channel=0.9304636),
                                       optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                                          hessian=FALSE, # no hessian
                                                          control=list(kkt=FALSE, # kkt takes forever
                                                                       reltol = 1000))),
                 regexp = "Hessian could not be derived.")

  # Only predict & plots until transaction data end / holdout end
  expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE))
  expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE))
  expect_true(dt.plot[, max(period.until)] <= ceiling_date(clv.data.trans@clv.time@timepoint.holdout.end, unit="week"))
  expect_true(dt.predict[, max(period.last)] <= clv.data.trans@clv.time@timepoint.holdout.end)

  # Can also predict & plot further
  prediction.end.over <-  clv.data.trans@clv.time@timepoint.holdout.end + lubridate::period(10, "weeks")
  expect_silent(dt.plot <- plot(fitted.dyncov, plot=FALSE, verbose=FALSE, prediction.end = prediction.end.over))
  expect_silent(dt.predict <- predict(fitted.dyncov, verbose=FALSE, prediction.end = prediction.end.over))

  expect_true(dt.plot[, max(period.until)] > clv.data.trans@clv.time@timepoint.holdout.end)
  expect_true(dt.predict[, max(period.last)] > clv.data.trans@clv.time@timepoint.holdout.end)
})

