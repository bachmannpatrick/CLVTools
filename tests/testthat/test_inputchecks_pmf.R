skip_on_cran()


test_that("x is valid input (integer vector)", {
  skip_on_cran()

  p.cdnow <- fit.cdnow()

  expect_error(pmf(p.cdnow, x=NULL), regex="NULL")
  expect_error(pmf(p.cdnow, x=NA_real_), regex="any NA")
  expect_error(pmf(p.cdnow, x=c(1,2,NA_real_, 4)), regex="any NA")
  expect_error(pmf(p.cdnow, x="1"), regex="vector of integer numbers")
  expect_error(pmf(p.cdnow, x=c(1,2.2,3,4)), regex="all integer numbers")
  expect_error(pmf(p.cdnow, x=c(-1,0,1,2)), regex="positive integer numbers")
})


