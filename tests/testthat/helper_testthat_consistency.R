fct.testthat.consistency.staticcov.cov.insignificant <- function(p.static){
  test_that("Cov params are insignificant", {
    expect_true(all(coef(summary(p.static))[c("life.Gender", "life.Channel", "trans.Gender", "trans.Channel"), 4] > 0.1))
  })
}

fct.testthat.consistency.nocov.model.params.nearly.same <- function(p.nocov, p.static, param.names){
  test_that("Model parameters are nearly the same", {
    expect_true(all.equal(coef(p.nocov), coef(p.static)[param.names], tolerance = 0.05))
  })
}

fct.testthat.consistency.predict.same.as.gamma.zero <- function(p.nocov, p.static, p.dyncov){
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

fct.testthat.consistency.plot.same.as.gamma.zero <- function(p.nocov, p.static, p.dyncov){
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
