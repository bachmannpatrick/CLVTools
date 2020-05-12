fct.testthat.consistency.cov.data.0.cov.params.insignificant <- function(p.static){
  test_that("Cov params are insignificant", {
    expect_true(all(coef(summary(p.static))[c("life.Gender", "life.Channel", "trans.Gender", "trans.Channel"), 4] > 0.1))
  })
}

fct.testthat.consistency.cov.data.0.model.params.nearly.same <- function(p.nocov, p.static, param.names){
  test_that("Model parameters are nearly the same", {
    expect_true(all.equal(coef(p.nocov), coef(p.static)[param.names], tolerance = 0.05))
  })
}

fct.testthat.consistency.cov.data.0.same.LL <- function(fct.LL.ind.nocov, fct.LL.ind.static.cov,
                                                          fitted.nocov, fitted.static.cov){
  test_that("Same LL for cov data = 0", {
    log.params.nocov <- setNames(log(coef(fitted.nocov)[fitted.nocov@clv.model@names.original.params.model]),
                                 fitted.nocov@clv.model@names.prefixed.params.model)
    l.args.nocov <- list(vLogparams = log.params.nocov,
                         vX = fitted.nocov@cbs$x, vT_x = fitted.nocov@cbs$t.x, vT_cal = fitted.nocov@cbs$T.cal)
    expect_silent(LL.ind.nocov <- do.call(fct.LL.ind.nocov, l.args.nocov))

    n.cov.life  <- length(fitted.static.cov@prediction.params.life)
    n.cov.trans <- length(fitted.static.cov@prediction.params.trans)

    params.static.cov <- c(log.params.nocov, rnorm(n=n.cov.life), rnorm(n=n.cov.trans))
    l.args.static.cov <- list(vParams = params.static.cov,
                              vX = fitted.static.cov@cbs$x, vT_x = fitted.static.cov@cbs$t.x, vT_cal = fitted.static.cov@cbs$T.cal,
                              mCov_life  = matrix(data = 0, nrow= nobs(fitted.static.cov), ncol=n.cov.life),
                              mCov_trans = matrix(data = 0, nrow= nobs(fitted.static.cov), ncol=n.cov.trans))

    expect_silent(LL.ind.staticcov <- do.call(fct.LL.ind.static.cov, l.args.static.cov))

    expect_true(isTRUE(all.equal(LL.ind.nocov, LL.ind.staticcov)))
    expect_true(isTRUE(all.equal(sum(LL.ind.nocov), sum(LL.ind.staticcov))))
  })
}


fct.testthat.consistency.cov.params.0.same.LL <- function(fct.LL.ind.nocov, fct.LL.ind.static.cov,
                                                          fitted.nocov, fitted.static.cov.g0){
  test_that("Same LL for cov params gamma=0",{
    log.params.nocov <- setNames(log(coef(fitted.nocov)[fitted.nocov@clv.model@names.original.params.model]),
                                 fitted.nocov@clv.model@names.prefixed.params.model)
    l.args.nocov <- list(vLogparams = log.params.nocov,
                         vX = fitted.nocov@cbs$x, vT_x = fitted.nocov@cbs$t.x, vT_cal = fitted.nocov@cbs$T.cal)
    expect_silent(LL.ind.nocov <- do.call(fct.LL.ind.nocov, l.args.nocov))


    params.static.cov <- c(log.params.nocov,
                           fitted.static.cov.g0@prediction.params.life,
                           fitted.static.cov.g0@prediction.params.trans)

    l.args.static.cov <- list(vParams = params.static.cov,
                              vX = fitted.static.cov.g0@cbs$x, vT_x = fitted.static.cov.g0@cbs$t.x, vT_cal = fitted.static.cov.g0@cbs$T.cal,
                              mCov_life  = CLVTools:::clv.data.get.matrix.data.cov.life(fitted.static.cov.g0@clv.data),
                              mCov_trans = CLVTools:::clv.data.get.matrix.data.cov.trans(fitted.static.cov.g0@clv.data))
    expect_silent(LL.ind.staticcov <- do.call(fct.LL.ind.static.cov, l.args.static.cov))

    expect_true(isTRUE(all.equal(LL.ind.nocov, LL.ind.staticcov)))
    expect_true(isTRUE(all.equal(sum(LL.ind.nocov), sum(LL.ind.staticcov))))
  })
}

fct.testthat.consistency.cov.params.0.predict.same <- function(p.nocov, p.static, p.dyncov){
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
}

fct.testthat.consistency.cov.params.0.plot.same <- function(p.nocov, p.static, p.dyncov){
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

}
