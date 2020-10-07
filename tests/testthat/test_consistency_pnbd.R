skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

fct.testthat.consistency(name.model = "PNBD", method = pnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         param.names = c("r", "alpha", "beta", "s"),
                         fct.LL.ind.nocov = pnbd_nocov_LL_ind, fct.LL.ind.static.cov = pnbd_staticcov_LL_ind)

# nocov vs dyncov --------------------------------------------------------------------------------------
#   same fit with all covs=0 - cannot do because fitting dyncov takes too long
#   same predict with gamma=0

expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd",
                                     time.unit = "w",estimation.split = 38))
expect_silent(p.nocov     <- pnbd(clv.apparel, verbose = FALSE))


# . Prepare dyncov: Set parameters -------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov
fct.helper.dyncov.g0.with.predition.params.model <- function(p.dyncov, prediction.params.model){
  # fct.helper.dyncov.load.fitted()#
  expect_silent(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")] <- prediction.params.model)

  expect_silent(p.dyncov@prediction.params.life[c("Marketing", "Gender", "Channel")] <- 0)
  expect_silent(p.dyncov@prediction.params.trans[c("Marketing", "Gender", "Channel")] <- 0)

  # Recalculate the LL data for these fake params
  expect_silent(log.params <- setNames(log(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")]),
                                       c("log.r", "log.alpha", "log.s", "log.beta")))
  expect_silent(log.params[c("trans.Marketing", "trans.Gender", "trans.Channel", "life.Marketing", "life.Gender", "life.Channel")] <- 0)
  expect_silent(p.dyncov@LL.data <- CLVTools:::pnbd_dyncov_LL(params=log.params, clv.fitted = p.dyncov))
  return(p.dyncov)
}

p.dyncov.g0 <- fct.helper.dyncov.quickfit(data.apparelTrans = apparelTrans, data.apparelDynCov = apparelDynCov)
p.dyncov.g0 <- fct.helper.dyncov.g0.with.predition.params.model(p.dyncov = p.dyncov.g0, prediction.params.model = p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])

# . Dyncov: Compare nocov vs dyncov LL, prediction & plot -------------------------------------------------------
fct.testthat.consistency.cov.params.0.predict.same(fitted.nocov = p.nocov,
                                                   fitted.cov.g0 = p.dyncov.g0,
                                                   is.dyncov = TRUE)

fct.testthat.consistency.cov.params.0.plot.same(fitted.nocov = p.nocov,
                                                fitted.cov.g0 = p.dyncov.g0)


# compare LL
test_that("Dyncov LL same as nocov for alpha==beta and alpha!=beta", {
  skip_on_cran()

  .fct.nocov.LL.for.params <- function(params.model){
    expect_silent(log.params.nocov <- setNames(log(params.model[p.nocov@clv.model@names.original.params.model]),
                                               p.nocov@clv.model@names.prefixed.params.model))
    expect_silent(l.args.nocov <- list(vLogparams = log.params.nocov,
                                       vX = p.nocov@cbs$x, vT_x = p.nocov@cbs$t.x, vT_cal = p.nocov@cbs$T.cal))
    expect_silent(LL.ind.nocov <- do.call(pnbd_nocov_LL_ind, l.args.nocov))
    return(drop(LL.ind.nocov))
  }

  # Alpha != beta
  stopifnot(coef(p.nocov)["beta"] != coef(p.nocov)["alpha"])
  LL.ind.nocov.alpha.neq.beta <- .fct.nocov.LL.for.params(params.model = coef(p.nocov))
  expect_true(isTRUE(all.equal(p.dyncov.g0@LL.data[order(Id)]$LL, LL.ind.nocov.alpha.neq.beta)))

  # Alpha == beta
  model.params.a.eq.b <- coef(p.nocov)
  model.params.a.eq.b[c("alpha", "beta")] <- 1.234
  LL.ind.nocov.alpha.eq.beta <- .fct.nocov.LL.for.params(params.model = model.params.a.eq.b)
  p.dyncov.g0.a.eq.b <- fct.helper.dyncov.g0.with.predition.params.model(p.dyncov = p.dyncov.g0, prediction.params.model = model.params.a.eq.b)
  expect_true(isTRUE(all.equal(p.dyncov.g0.a.eq.b@LL.data[order(Id)]$LL, LL.ind.nocov.alpha.eq.beta)))
})




