fct.testthat.consistency.cov.data.0.cov.params.insignificant <- function(fitted.static.cov0){
  test_that("Cov params are insignificant", {
    expect_true(all(coef(summary(fitted.static.cov0))[c("life.Gender", "life.Channel", "trans.Gender", "trans.Channel"), 4] > 0.1))
  })
}

fct.testthat.consistency.cov.data.0.model.params.nearly.same <- function(fitted.nocov, fitted.static.cov0, param.names){
  test_that("Model parameters are nearly the same", {
    expect_true(all.equal(coef(fitted.nocov), coef(fitted.static.cov0)[param.names], tolerance = 0.05))
  })
}

fct.testthat.consistency.cov.data.0.same.LL <- function(fct.LL.ind.nocov, fct.LL.ind.static.cov,
                                                        fitted.nocov, fitted.static.cov0){
  test_that("Same LL for cov data = 0", {
    log.params.nocov <- setNames(log(coef(fitted.nocov)[fitted.nocov@clv.model@names.original.params.model]),
                                 fitted.nocov@clv.model@names.prefixed.params.model)
    l.args.nocov <- list(vLogparams = log.params.nocov,
                         vX = fitted.nocov@cbs$x, vT_x = fitted.nocov@cbs$t.x, vT_cal = fitted.nocov@cbs$T.cal)
    expect_silent(LL.ind.nocov <- do.call(fct.LL.ind.nocov, l.args.nocov))

    n.cov.life  <- length(fitted.static.cov0@prediction.params.life)
    n.cov.trans <- length(fitted.static.cov0@prediction.params.trans)

    params.static.cov <- c(log.params.nocov, rnorm(n=n.cov.life), rnorm(n=n.cov.trans))
    l.args.static.cov <- list(vParams = params.static.cov,
                              vX = fitted.static.cov0@cbs$x, vT_x = fitted.static.cov0@cbs$t.x, vT_cal = fitted.static.cov0@cbs$T.cal,
                              mCov_life  = matrix(data = 0, nrow= nobs(fitted.static.cov0), ncol=n.cov.life),
                              mCov_trans = matrix(data = 0, nrow= nobs(fitted.static.cov0), ncol=n.cov.trans))

    expect_silent(LL.ind.staticcov <- do.call(fct.LL.ind.static.cov, l.args.static.cov))

    expect_true(isTRUE(all.equal(LL.ind.nocov, LL.ind.staticcov)))
    expect_true(isTRUE(all.equal(sum(LL.ind.nocov), sum(LL.ind.staticcov))))
  })
}


fct.testthat.consistency.cov.params.0.same.LL <- function(fct.LL.ind.nocov, fct.LL.ind.static.cov,
                                                          fitted.nocov, fitted.static.g0){
  test_that("Same LL for cov params gamma=0",{
    log.params.nocov <- setNames(log(coef(fitted.nocov)[fitted.nocov@clv.model@names.original.params.model]),
                                 fitted.nocov@clv.model@names.prefixed.params.model)
    l.args.nocov <- list(vLogparams = log.params.nocov,
                         vX = fitted.nocov@cbs$x, vT_x = fitted.nocov@cbs$t.x, vT_cal = fitted.nocov@cbs$T.cal)
    expect_silent(LL.ind.nocov <- do.call(fct.LL.ind.nocov, l.args.nocov))


    params.static.cov <- c(log.params.nocov,
                           fitted.static.g0@prediction.params.life,
                           fitted.static.g0@prediction.params.trans)

    l.args.static.cov <- list(vParams = params.static.cov,
                              vX = fitted.static.g0@cbs$x, vT_x = fitted.static.g0@cbs$t.x, vT_cal = fitted.static.g0@cbs$T.cal,
                              mCov_life  = CLVTools:::clv.data.get.matrix.data.cov.life(fitted.static.g0@clv.data,
                                                                                        correct.col.names = names(fitted.static.g0@prediction.params.life),
                                                                                        correct.row.names = fitted.static.g0@cbs$Id),
                              mCov_trans = CLVTools:::clv.data.get.matrix.data.cov.trans(fitted.static.g0@clv.data,
                                                                                         correct.col.names = names(fitted.static.g0@prediction.params.trans),
                                                                                         correct.row.names = fitted.static.g0@cbs$Id))
    expect_silent(LL.ind.staticcov <- do.call(fct.LL.ind.static.cov, l.args.static.cov))

    expect_true(isTRUE(all.equal(LL.ind.nocov, LL.ind.staticcov)))
    expect_true(isTRUE(all.equal(sum(LL.ind.nocov), sum(LL.ind.staticcov))))
  })
}

fct.testthat.consistency.cov.params.0.predict.same <- function(fitted.nocov, fitted.cov.g0, is.dyncov){
  test_that("Predict yields same results for all models with gamma=0", {

    fct.compare.prediction.result <- function(dt.pred.nocov, dt.pred.cov){
      if(is.dyncov == TRUE){
        # DERT unequal to DECT because only predict short period!
        expect_silent(data.table::setnames(dt.pred.cov, old="DECT",new = "DERT"))
        expect_true(isTRUE(all.equal(dt.pred.nocov[,  !c("DERT", "predicted.CLV")],
                                     dt.pred.cov[,    !c("DERT", "predicted.CLV")])))
      }else{
        # If dont have DERT, still can compare nocov vs staticcov because DERT=0 for both
        expect_true(isTRUE(all.equal(dt.pred.nocov, dt.pred.cov)))
      }
    }


    # Standard
    expect_silent(dt.pred.nocov    <- predict(fitted.nocov,  verbose=FALSE))
    expect_silent(dt.pred.cov      <- predict(fitted.cov.g0, verbose=FALSE))

    fct.compare.prediction.result(dt.pred.nocov = dt.pred.nocov, dt.pred.cov = dt.pred.cov)


    # With prediction.end
    expect_silent(dt.pred.nocov     <- predict(fitted.nocov,  verbose=FALSE, prediction.end = 6))
    expect_silent(dt.pred.cov       <- predict(fitted.cov.g0, verbose=FALSE, prediction.end = 6))

    fct.compare.prediction.result(dt.pred.nocov = dt.pred.nocov, dt.pred.cov = dt.pred.cov)


    # with discount rates
    expect_silent(dt.pred.nocov     <- predict(fitted.nocov,  verbose=FALSE,  continuous.discount.factor = 0.25))
    expect_silent(dt.pred.cov       <- predict(fitted.cov.g0, verbose=FALSE,  continuous.discount.factor = 0.25))

    fct.compare.prediction.result(dt.pred.nocov = dt.pred.nocov, dt.pred.cov = dt.pred.cov)
  })
}

fct.testthat.consistency.cov.params.0.plot.same <- function(fitted.nocov, fitted.cov.g0){
  test_that("plot yields same results for all models with gamma=0", {
    # Prediction end for faster calcs. Should not affect results
    expect_warning(dt.plot.nocov     <- plot(fitted.nocov,  verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")
    expect_warning(dt.plot.cov       <- plot(fitted.cov.g0, verbose=FALSE, plot=FALSE, prediction.end = 10), regexp = "full holdout")

    # Rename to random names because have different colnames by model
    data.table::setnames(dt.plot.nocov,  c("A", "B", "C"))
    data.table::setnames(dt.plot.cov ,   c("A", "B", "C"))

    expect_true(isTRUE(all.equal(dt.plot.nocov, dt.plot.cov)))
  })
}


# Tests that models are consistent among themselves
# Consistency = nocov vs static cov:
#   same fit with all covs = 0
#   same predict with gamma = 0
fct.testthat.consistency <- function(name.model, method, has.dyncov, data.apparelTrans, data.apparelStaticCov, param.names,
                                     fct.LL.ind.nocov, fct.LL.ind.static.cov){

  # Fit object on cov data with all 0
  #   Cannot set all to 0 as requires at least 2 distinct values per cov
  expect_silent(clv.apparel <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd",
                                       time.unit = "w", estimation.split = 38))

  context(paste0("Nocov/cov Consistency - ",name.model," - all cov data = 0"))

  expect_silent(apparelStaticCov.0 <- data.apparelStaticCov)
  expect_silent(apparelStaticCov.0[,  Gender  := 0])
  expect_silent(apparelStaticCov.0[1, Gender  := 1])
  expect_silent(apparelStaticCov.0[,  Channel := 0])
  expect_silent(apparelStaticCov.0[1, Channel := 1])
  expect_silent(clv.apparel.static.cov0 <-
                  SetStaticCovariates(clv.apparel,
                                      data.cov.life = apparelStaticCov.0, data.cov.trans = apparelStaticCov.0,
                                      names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))

  expect_silent(fitted.nocov       <- do.call(method, list(clv.data = clv.apparel, verbose = FALSE)))
  expect_silent(fitted.static.cov0 <- do.call(method, list(clv.data = clv.apparel.static.cov0, verbose = FALSE)))

  # **TODO: remove or enable?
  # fct.testthat.consistency.cov.data.0.cov.params.insignificant(fitted.static.cov0 = fitted.static.cov0)
  # fct.testthat.consistency.cov.data.0.model.params.nearly.same(fitted.nocov = fitted.nocov, fitted.static.cov0 = fitted.static.cov0,
  #                                                              param.names = param.names)

  fct.testthat.consistency.cov.data.0.same.LL(fitted.nocov = fitted.nocov, fitted.static.cov0 = fitted.static.cov0,
                                              fct.LL.ind.nocov = fct.LL.ind.nocov, fct.LL.ind.static.cov = fct.LL.ind.static.cov)

  context(paste0("Nocov/cov Consistency - ",name.model," - cov params = 0"))

  # Fake the parameters to be exactly the same and 0 for covariates
  #   Replace model coefs with that from nocov
  expect_silent(fitted.static.g0 <- do.call(method, list(clv.data = SetStaticCovariates(clv.apparel,
                                                                                     data.cov.life = data.apparelStaticCov, data.cov.trans = data.apparelStaticCov,
                                                                                     names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")),
                                                         verbose = FALSE)))
  expect_silent(fitted.static.g0@prediction.params.model[param.names] <-
                  fitted.nocov@prediction.params.model[param.names])
  expect_silent(fitted.static.g0@prediction.params.life[c("Gender", "Channel")]  <- 0)
  expect_silent(fitted.static.g0@prediction.params.trans[c("Gender", "Channel")] <- 0)


  fct.testthat.consistency.cov.params.0.same.LL(fct.LL.ind.nocov = fct.LL.ind.nocov, fct.LL.ind.static.cov=fct.LL.ind.static.cov,
                                                fitted.nocov = fitted.nocov, fitted.static.g0 = fitted.static.g0)
  fct.testthat.consistency.cov.params.0.predict.same(fitted.nocov = fitted.nocov, fitted.cov.g0 = fitted.static.g0,
                                                     is.dyncov = FALSE)
  fct.testthat.consistency.cov.params.0.plot.same(fitted.nocov = fitted.nocov, fitted.cov.g0 = fitted.static.g0)
}



