skip_on_cran()
data("cdnow")


test_that("x is valid input (integer vector)", {
  skip_on_cran()

  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split = NULL)
  expect_silent(p.cdnow <- pnbd(clv.cdnow, verbose=FALSE))

  expect_error(pmf(p.cdnow, x=NULL), regex="NULL")
  expect_error(pmf(p.cdnow, x=NA_real_), regex="any NA")
  expect_error(pmf(p.cdnow, x=c(1,2,NA_real_, 4)), regex="any NA")
  expect_error(pmf(p.cdnow, x="1"), regex="vector of integer numbers")
  expect_error(pmf(p.cdnow, x=c(1,2.2,3,4)), regex="all integer numbers")
  expect_error(pmf(p.cdnow, x=c(-1,0,1,2)), regex="positive integer numbers")
})


