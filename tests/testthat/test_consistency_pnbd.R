skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# PNBD nocov vs static cov consistency
fct.testthat.consistency(name.model = "PNBD", method = pnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         fct.LL.ind.nocov = pnbd_nocov_LL_ind, fct.LL.ind.static.cov = pnbd_staticcov_LL_ind)


# Dyncov vs nocov consistency ---------------------------------------------------------------------------
#   same predict with gamma=0

p.nocov <- fit.apparel.nocov()


# . Prepare dyncov: Set parameters -------------------------------------------------------------------

.fn.helper.dyncov.recalculate.LLdata.with.prediction.params.model <- function(p.dyncov, prediction.params.model){
  names.params.model <- p.dyncov@clv.model@names.original.params.model
  names.params.all <- colnames(coef(p.dyncov@optimx.estimation.output))
  names.params.covs <- names.params.all[!names.params.all %in% names.params.model]

  p.dyncov@prediction.params.model[names.params.model] <- prediction.params.model[names.params.model]

  # Recalculate the LL data for these fake params
  log.params <- setNames(
    log(p.dyncov@prediction.params.model[names.params.model]),
    paste0('log.', names.params.model)
  )
  log.params[names.params.covs] <- 0

  p.dyncov@LL.data <- pnbd_dyncov_getLLdata(clv.fitted=p.dyncov, params=log.params)

  return(p.dyncov)
}

# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov
fct.helper.dyncov.g0.with.predition.params.model <- function(p.dyncov, prediction.params.model){
  p.dyncov <- copy(p.dyncov)

  expect_silent(p.dyncov@prediction.params.life[] <- 0)
  expect_silent(p.dyncov@prediction.params.trans[] <- 0)

  return(.fn.helper.dyncov.recalculate.LLdata.with.prediction.params.model(
    p.dyncov = p.dyncov, prediction.params.model = prediction.params.model
  ))
}

fn.helper.dyncov.cov0.with.prediction.params.model <- function(p.dyncov, prediction.params.model){
  p.dyncov <- copy(p.dyncov)

  names.covs.life <- p.dyncov@clv.data@names.cov.data.life
  names.covs.trans <- p.dyncov@clv.data@names.cov.data.trans

  # Set covs itself to 0
  p.dyncov@clv.data@data.cov.life[, (names.covs.life) := 0]
  p.dyncov@clv.data@data.cov.trans[, (names.covs.trans) := 0]

  # Set cov data in walks to 0
  p.dyncov@data.walks.life.aux[, (names.covs.life) := 0]
  p.dyncov@data.walks.life.real[, (names.covs.life) := 0]
  p.dyncov@data.walks.trans.aux[, (names.covs.trans) := 0]
  p.dyncov@data.walks.trans.real[, (names.covs.trans) := 0]

  return(.fn.helper.dyncov.recalculate.LLdata.with.prediction.params.model(
    p.dyncov = p.dyncov, prediction.params.model = prediction.params.model
  ))
}

p.dyncov.g0 <- fct.helper.dyncov.quickfit.apparel.data()

# dont fit dyncov again for cov0 model but use p.dyncov.g0
# have to use it before setting gamma=0
p.dyncov.cov0 <- fn.helper.dyncov.cov0.with.prediction.params.model(
  p.dyncov = p.dyncov.g0,
  prediction.params.model = p.nocov@prediction.params.model
)

p.dyncov.g0 <- fct.helper.dyncov.g0.with.predition.params.model(
  p.dyncov = p.dyncov.g0,
  prediction.params.model = p.nocov@prediction.params.model
)



# . Dyncov: Compare nocov vs dyncov LL ---------------------------------------------------------------
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

# . predict(newdata=newcustomer): collapses into nocov for gamma=0--------------

test_that("predict newcustomer dyncov same results for all models with gamma=0", {
  # requires newcustomer.dynamic() while `fct.testthat.consistency.cov.params.0.predict.newcustomer.same()`
  # uses newcustomer.static()
  df.cov <- fct.helper.default.newcustomer.covdata.dyncov()

  expect_silent(nc.pred.nocov <- predict(
    p.nocov,
    newdata=newcustomer(num.periods = 7.89),
    verbose=FALSE))

  expect_silent(nc.pred.g0 <- predict(
    p.dyncov.g0,
    newdata=newcustomer.dynamic(
      num.periods = 7.89,
      data.cov.life = df.cov,
      data.cov.trans = df.cov,
      first.transaction = '2000-01-13'),
    verbose=FALSE))

  expect_equal(nc.pred.nocov, nc.pred.g0)
})



# . PMF: dyncov with gamma=0 and/or cov=0 same as nocov ------------------------

# Calculate only once for all tests and only 0:2 because dyncov pmf very slow
dt.pmf.dyn.g0 <- pmf(p.dyncov.g0, x=0:2)
dt.pmf.dyn.cov0 <- pmf(p.dyncov.cov0, x=0:2)
dt.pmf.nocov <- pmf(p.nocov, x=0:2)

test_that("All customers have the same pmf if cov=0 and/or gamma=0", {
  expect_true(dt.pmf.dyn.g0[, !"Id"][, lapply(.SD, uniqueN)][, all(.SD == 1)])
  expect_true(dt.pmf.dyn.cov0[, !"Id"][, lapply(.SD, uniqueN)][, all(.SD == 1)])
})

test_that("Dyncov PMF collapses into nocov PMF if cov=0 and/or gamma=0", {
  expect_equal(dt.pmf.nocov, dt.pmf.dyn.g0)
  expect_equal(dt.pmf.nocov, dt.pmf.dyn.cov0)
})

test_that("Dyncov PMF same for cov=0 as for gamma=0", {
  expect_equal(dt.pmf.dyn.g0, dt.pmf.dyn.cov0)
})

