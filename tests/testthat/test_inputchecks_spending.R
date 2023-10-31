skip_on_cran()
data("cdnow")

clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

# data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data", {
  skip_on_cran()
  expect_error(spending(family=gg, data=), "object of class clv.data, data.frame, or data.table")
  expect_error(spending(family=gg, data=NULL), "object of class clv.data, data.frame, or data.table")
  expect_error(spending(family=gg, data=123), "object of class clv.data, data.frame, or data.table")
  expect_error(spending(family=gg, data=cdnow), "object of class clv.data, data.frame, or data.table")
})

# family --------------------------------------------------------------------------------------------

test_that("Fails if family is missing", {
  expect_error(latentAttrition(formula = , family=, data=clv.cdnow), "of the following families")
})

test_that("Fails if family it no allowed method", {
  skip_on_cran()
  expect_error(spending(family=pnbd, data=clv.cdnow), "of the following families")
  expect_error(spending(family=ggg, data=clv.cdnow), "of the following families")
})

# formula: there is no formula to arg ----------------------------------------------------------------

