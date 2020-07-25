fct.testthat.correctness.clvfitted.flawless.results.out.of.the.box <- function(method, clv.data, kkt2.true){
  test_that("Flawless results out of the box", {
    skip_on_cran()
    expect_silent(fitted <- do.call(what = method, args = list(clv.data, verbose = FALSE)))
    expect_silent(res.sum <- summary(fitted))
    # No NAs anywhere
    expect_false(any(!is.finite(coef(res.sum)))) # vcov and coef together
    fct.DT.any.non.finite <- function(DT){
      return(DT[, any(sapply(.SD, function(x){any(!is.finite(x))})), .SDcols = DT[, sapply(.SD, is.numeric)]])
    }

    expect_false(fct.DT.any.non.finite(predict(fitted, verbose = FALSE)))
    if(is(fitted, "clv.fitted.transactions")){
      expect_false(fct.DT.any.non.finite(plot(fitted, plot = FALSE, verbose = FALSE)[, !"Actual Number of Repeat Transactions"]))
    }
    # KKTs both true
    expect_true(res.sum$kkt1)
    if(kkt2.true)
      expect_true(res.sum$kkt2)
  })
}

fct.testthat.correctness.clvfitted.nocov.correct.se <- function(method, cdnow, start.params.model, params.nocov.se){
  test_that("Cdnow nocov correct SE", {
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))

    l.args <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = method, args = l.args))

    # From previous fit
    expect_equal(sqrt(diag(vcov(p.cdnow))), params.nocov.se, tolerance = 0.001)
  })
}

fct.testthat.correctness.clvfitted.correct.coefs <- function(method, cdnow, start.params.model, params.nocov.coef, LL.nocov){
  test_that("Cdnow nocov correct coefs", {
    skip_on_cran()
    expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "w", estimation.split = "1997-09-30"))

    l.args <- list(clv.data=clv.cdnow, start.params.model = start.params.model, verbose=FALSE)
    expect_silent(p.cdnow <- do.call(what = method, args = l.args))

    # From previous fit
    expect_equal(coef(p.cdnow), params.nocov.coef, tolerance = 0.001)
    expect_equal(as.numeric(logLik(p.cdnow)), LL.nocov, tolerance = 0.001)
  })
}

