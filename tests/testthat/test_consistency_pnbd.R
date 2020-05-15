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



fct.testthat.consistency.cov.data.0.cov.params.insignificant(m.static = p.static)

fct.testthat.consistency.cov.data.0.model.params.nearly.same(m.nocov = p.nocov,
                                                             m.static = p.static,
                                                             param.names = c("r", "alpha", "s","beta"))

fct.testthat.consistency.cov.data.0.same.LL(fct.LL.ind.nocov = pnbd_nocov_LL_ind,
                                            fct.LL.ind.static.cov = pnbd_staticcov_LL_ind,
                                            fitted.nocov = p.nocov, fitted.static.cov = p.static)








context("Nocov/cov Consistency - PNBD - cov params = 0")
# Also possible for dyncov

# Create staticcov model with ordinary cov data
expect_silent(clv.apparel.static <- SetStaticCovariates(clv.apparel,
                                                        data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov,
                                                        names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))
expect_silent(p.static.g0 <- pnbd(clv.apparel.static, verbose=FALSE))


# Create dyncov model, quickly ------------------------------------------------------------------
data("apparelDynCov")
expect_message(clv.apparel.dyn  <- SetDynamicCovariates(clv.apparel,name.id = "Id",name.date = "Cov.Date",
                                                        data.cov.life  = apparelDynCov,names.cov.life = c("Marketing", "Gender", "Channel"),
                                                        data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel")),
               regexp = "cut off")

expect_warning(p.dyncov.g0 <- pnbd(clv.apparel.dyn, start.params.model = c(r=1, alpha=3, s=1, beta=3),
                                   optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                                      hessian=FALSE, # no hessian
                                                      control=list(kkt=FALSE, # kkt takes forever
                                                                   reltol = 1000)),
                                   verbose = FALSE),
               regexp = "Hessian")


# Set parameters ------------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov

expect_silent(p.static.g0@prediction.params.model[c("r", "alpha", "s", "beta")] <-
                p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])
expect_silent(p.static.g0@prediction.params.life[c("Gender", "Channel")] <- 0)
expect_silent(p.static.g0@prediction.params.trans[c("Gender", "Channel")] <- 0)

# dyncov
expect_silent(p.dyncov.g0@prediction.params.model[c("r", "alpha", "s", "beta")] <-
                p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])
expect_silent(p.dyncov.g0@prediction.params.life[c("Marketing", "Gender", "Channel")] <- 0)
expect_silent(p.dyncov.g0@prediction.params.trans[c("Marketing", "Gender", "Channel")] <- 0)

# Recalculate the LL data for these fake params
expect_silent(log.params <- setNames(log(p.dyncov.g0@prediction.params.model[c("r", "alpha", "s", "beta")]),
                                     c("log.r", "log.alpha", "log.s", "log.beta")))
expect_silent(log.params[c("trans.Marketing", "trans.Gender", "trans.Channel", "life.Marketing", "life.Gender", "life.Channel")] <- 0)
expect_silent(p.dyncov.g0@LL.data <- CLVTools:::pnbd_dyncov_LL(params=log.params, clv.fitted = p.dyncov.g0))


fct.testthat.consistency.cov.params.0.same.LL(fct.LL.ind.nocov = pnbd_nocov_LL_ind,
                                              fct.LL.ind.static.cov = pnbd_staticcov_LL_ind,
                                              fitted.nocov = p.nocov, fitted.static.cov = p.static.g0)

fct.testthat.consistency.cov.params.0.predict.same(m.nocov = p.nocov,
                                                   m.static = p.static.g0,
                                                   m.dyncov = p.dyncov.g0)

fct.testthat.consistency.cov.params.0.plot.same(m.nocov = p.nocov,
                                                m.static = p.static.g0,
                                                m.dyncov = p.dyncov.g0)
