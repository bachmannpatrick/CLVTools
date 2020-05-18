# Load Data ------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")
apparelDynCov <- apparelDynCov[Cov.Date > "2005-01-01" ] #otherwise "cutoff" message


# DO NOT RUN ANYTHING DYNCOV ON CRAN
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
fct.testthat.runability.dynamiccov.LL.is.correct(clv.fitted = fitted.dyncov)

# Plot ------------------------------------------------------------------
fct.testthat.runability.dynamiccov.plot.works(clv.fitted = fitted.dyncov)

fct.testthat.runability.dynamiccov.plot.has.0.repeat.transactions.expectations(clv.fitted = fitted.dyncov)

# Predict ----------------------------------------------------------------
fct.testthat.runability.dynamiccov.predict.works(clv.fitted = fitted.dyncov)

fct.testthat.runability.dynamiccov.predict.newdata.works(clv.fitted = fitted.dyncov,
                                                         apparelTrans = apparelTrans,
                                                         apparelDynCov = apparelDynCov)


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

fct.testthat.runability.dynamiccov.predict.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.mini.extra = clv.data.mini.extra, clv.data.trans = clv.data.trans)

fct.testthat.runability.dynamiccov.plot.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.mini.extra = clv.data.mini.extra, clv.data.trans = clv.data.trans)

# Overlong data ------------------------------------------------------------------------------
context("Runability - PNBD dynamiccov - Overlong data")

# Cannot do without holdout because takes too long to estimate
fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout(method = pnbd,
                                                                   clv.data.trans = clv.data.trans,
                                                                   mini.apparelDynCov.long = mini.apparelDynCov.long,
                                                                   start.params.model = c(r=0.4011475, alpha=22.7155565,
                                                                                          s=0.2630372, beta=19.1752426))

