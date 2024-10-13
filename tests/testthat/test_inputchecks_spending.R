skip_on_cran()
data("cdnow")

clv.cdnow <- fct.helper.create.clvdata.cdnow()

# data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data", {
  skip_on_cran()
  expect_error(spending(family=gg, data=), "provide a 'clv.data' object")
  expect_error(spending(family=gg, data=NULL), "provide a 'clv.data' object")
  expect_error(spending(family=gg, data=123), "provide a 'clv.data' object")
  expect_error(spending(family=gg, data=cdnow), "provide a 'clv.data' object")
})

# family --------------------------------------------------------------------------------------------

test_that("Fails if family is missing", {
  expect_error(latentAttrition(formula = , family=, data=clv.cdnow), "of the following inputs")
})

test_that("Fails if family it no allowed method", {
  skip_on_cran()
  expect_error(spending(family=pnbd, data=clv.cdnow), "of the following inputs")
  expect_error(spending(family=cdnow, data=clv.cdnow), "of the following inputs")
  expect_error(spending(family=bgnbd, data=clv.cdnow), "of the following inputs")
})


# dots -----------------------------------------------------------------------------------------------
test_that("Fails if additional args in dots that are not allowed for gg", {
  expect_error(spending(family=gg, data=clv.cdnow, verboes = TRUE), "may not be passed")
  expect_error(spending(family=gg, data=clv.cdnow, remove.first=FALSE), "may not be passed")
  expect_error(spending(family=gg, data=clv.cdnow, start.parameters=cdnow), "may not be passed")
})

# formula: there is no formula to arg ----------------------------------------------------------------

