skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

fct.testthat.consistency(name.model = "PNBD", method = pnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         param.names = c("r", "alpha", "beta", "s"),
                         fct.LL.ind.nocov = pnbd_nocov_LL_ind, fct.LL.ind.static.cov = pnbd_staticcov_LL_ind)

# nocov vs dyncov:
#   same fit with all covs=0 - cannot do because fitting dyncov takes too long
#   same predict with gamma=0

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd",
                                     time.unit = "w",estimation.split = 38))
expect_silent(p.nocov     <- pnbd(clv.apparel, verbose = FALSE))



fct.helper.quickfit.dyncov <- function(data.apparelTrans, data.apparelDynCov){
  # Create dyncov model, quickly
  expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd",
                                       time.unit = "w",estimation.split = 38))
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
  return(p.dyncov)
}

p.dyncov.g0 <- fct.helper.quickfit.dyncov(data.apparelTrans = apparelTrans, data.apparelDynCov = apparelDynCov)

# Dyncov: Set parameters ------------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov

expect_silent(p.dyncov.g0@prediction.params.model[c("r", "alpha", "s", "beta")] <-
                p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])
expect_silent(p.dyncov.g0@prediction.params.life[c("Marketing", "Gender", "Channel")] <- 0)
expect_silent(p.dyncov.g0@prediction.params.trans[c("Marketing", "Gender", "Channel")] <- 0)

# Recalculate the LL data for these fake params
expect_silent(log.params <- setNames(log(p.dyncov.g0@prediction.params.model[c("r", "alpha", "s", "beta")]),
                                     c("log.r", "log.alpha", "log.s", "log.beta")))
expect_silent(log.params[c("trans.Marketing", "trans.Gender", "trans.Channel", "life.Marketing", "life.Gender", "life.Channel")] <- 0)
expect_silent(p.dyncov.g0@LL.data <- CLVTools:::pnbd_dyncov_LL(params=log.params, clv.fitted = p.dyncov.g0))



# Dyncov: Compare nocov vsdyncov LL, prediction & plot -------------------------------------------------------
# compare LL

# nocov LL
expect_silent(log.params.nocov <- setNames(log(coef(p.nocov)[p.nocov@clv.model@names.original.params.model]),
                                           p.nocov@clv.model@names.prefixed.params.model))
expect_silent(l.args.nocov <- list(vLogparams = log.params.nocov,
                                   vX = p.nocov@cbs$x, vT_x = p.nocov@cbs$t.x, vT_cal = p.nocov@cbs$T.cal))
expect_silent(LL.ind.nocov <- do.call(pnbd_nocov_LL_ind, l.args.nocov))


# Compare vs dyncov LL
expect_true(isTRUE(all.equal(p.dyncov.g0@LL.data[order(Id)]$LL, LL.ind.nocov[, 1])))


fct.testthat.consistency.cov.params.0.predict.same(fitted.nocov = p.nocov,
                                                   fitted.cov.g0 = p.dyncov.g0,
                                                   is.dyncov = TRUE)

fct.testthat.consistency.cov.params.0.plot.same(fitted.nocov = p.nocov,
                                                fitted.cov.g0 = p.dyncov.g0)

