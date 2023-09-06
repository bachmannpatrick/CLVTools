fct.testthat.correctness.clvfittedspending.remove.first.transaction <- function(method, clv.data){

  test_that("Remove first transaction works correctly", {
    expect_silent(fitted.remove     <- do.call(method, list(remove.first.transaction = TRUE,  clv.data = clv.data, verbose = FALSE)))
    expect_silent(fitted.not.remove <- do.call(method, list(remove.first.transaction = FALSE, clv.data = clv.data, verbose = FALSE)))

    # All coefs have to be different
    expect_false(any(coef(fitted.remove) == coef(fitted.not.remove)))
    # cbs has all different values, but still same Ids, all Ids
    expect_false(any(fitted.remove@cbs[order(Id), "x"]        == fitted.not.remove@cbs[order(Id), "x"]))
    # Spending may be the same for customers with Spending=0 or same spending in all transactions in original data
    # expect_false(any(fitted.remove@cbs[order(Id), "Spending"] == fitted.not.remove@cbs[order(Id), "Spending"]))
    expect_setequal(fitted.remove@cbs$Id,                        fitted.not.remove@cbs$Id)
    expect_setequal(fitted.remove@cbs$Id, clv.data@data.transactions[, unique(Id)])
  })
}


fct.testthat.correctness.clvfittedspending.cbs.same.as.pnbd <- function(method, clv.data){
  test_that("Spending model cbs same as pnbd", {
    skip_on_cran()
    expect_silent(fitted <- do.call(method, args = list(clv.data=clv.data, remove.first.transaction = TRUE, verbose = FALSE)))
    expect_silent(dt.cbs.pnbd     <- pnbd_cbs(clv.data))

    expect_equal(dt.cbs.pnbd[order(Id), c("Id", "x")],
                 fitted@cbs[order(Id), c("Id", "x")])
  })
}



fct.testthat.correctness.clvfittedspending <- function(name.model, method,
                                                       data.cdnow, data.apparelTrans, data.apparelStaticCov,
                                                       correct.start.params.model, correct.params.coef, correct.LL){

  clv.cdnow.hold   <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=37)
  clv.cdnow.nohold <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=NULL)
  clv.apparel.staticcov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                       estimation.split = 52)
  clv.apparel.nocov <- as(clv.apparel.staticcov, "clv.data")
  expect_silent(obj.fitted <- do.call(method, list(clv.data = clv.cdnow.hold, verbose=FALSE)))

  fct.testthat.correctness.clvfittedspending.remove.first.transaction(method = method, clv.data = clv.cdnow.hold)
  fct.testthat.correctness.clvfittedspending.remove.first.transaction(method = method, clv.data = clv.cdnow.nohold)


  fct.testthat.correctness.clvfittedspending.cbs.same.as.pnbd(method = method, clv.data = clv.cdnow.hold)
  fct.testthat.correctness.clvfittedspending.cbs.same.as.pnbd(method = method, clv.data = clv.cdnow.nohold)


  fct.testthat.correctness.clvfitted.correct.coefs(method = method, cdnow=data.cdnow,
                                                   start.params.model = correct.start.params.model,
                                                   params.nocov.coef = correct.params.coef,
                                                   LL.nocov = correct.LL)


  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.cdnow.hold, kkt2.true = TRUE)
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.nocov, kkt2.true = TRUE)
  fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box(method = method, clv.data = clv.apparel.staticcov, kkt2.true = TRUE)


  fct.testthat.correctness.clvfitted.newdata.same.predicting.fitting(clv.fitted = obj.fitted)

}
