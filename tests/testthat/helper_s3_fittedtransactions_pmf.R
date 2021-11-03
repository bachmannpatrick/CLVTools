fct.testthat.runability.clvfittedtransactions.pmf <- function(fitted.transactions){
  test_that("Works with integers", {
    skip_on_cran()
    expect_silent(pmf(fitted.transactions, x=0:5))
  })

  test_that("Works with numeric", {
    skip_on_cran()
    expect_silent(pmf(fitted.transactions, x=c(0,1,2,3.0)))
  })

  test_that("Works with single, x=0", {
    skip_on_cran()
    expect_silent(pmf(fitted.transactions, x=0))
  })

  test_that("Correct format", {
    expect_silent(dt.pmf <- pmf(fitted.transactions, x=0:4))
    expect_setequal(colnames(dt.pmf), c("Id", paste0("pmf.x.", 0:4)))
    expect_true(length(colnames(dt.pmf)) == 6)

    expect_false(anyNA(dt.pmf))
    expect_true(all(dt.pmf[, !"Id"] >= 0 & dt.pmf[, !"Id"] <= 1))
  })
}
