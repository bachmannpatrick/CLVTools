skip_on_cran()
data("cdnow")

# nocov, clv.data -----------------------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow()

# .data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data", {
  expect_error(latentAttrition(formula =, family=pnbd, data=), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=NULL), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=123), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=cdnow), "clv.data")
})

# .formula -----------------------------------------------------------------------------------------------
test_that("Fails if formula given (but not required because nocov)", {
  expect_error(latentAttrition(formula = ~pnbd, family=pnbd, data=clv.cdnow), "may not be specified")
  expect_error(latentAttrition(formula = ~pnbd(), family=pnbd, data=clv.cdnow), "may not be specified")
  expect_error(latentAttrition(formula = ~data, family=pnbd, data=clv.cdnow), "may not be specified")
  expect_error(latentAttrition(formula = ~x, family=pnbd, data=clv.cdnow), "may not be specified")
  expect_error(latentAttrition(formula = ~1, family=pnbd, data=clv.cdnow), "may not be specified")
  expect_error(latentAttrition(formula = data()~pnbd(), family=pnbd, data=clv.cdnow), "may not be specified")
})


# .family -----------------------------------------------------------------------------------------------

test_that("Fails if family is missing", {
  expect_error(latentAttrition(formula = , family=, data=clv.cdnow), "one of the following")
})

test_that("Fails if family is not allowed method", {
  expect_error(latentAttrition(formula = , family='pnbd', data=clv.cdnow), "one of the following")
  expect_error(latentAttrition(formula = , family=cdnow, data=clv.cdnow), "one of the following")
})


# .dots -----------------------------------------------------------------------------------------------
test_that("Fails if additional args in dots are not allowed for family", {
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, clv.data=clv.cdnow), "may not be passed")
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, names.cov.life="Gender"), "may not be passed")
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, start.params.trans=c(Channel=2)), "may not be passed")
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, reg.weights=c(trans=10, life=20)), "may not be passed")
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, abc=3), "may not be passed")
  expect_error(latentAttrition(formula = , family=pnbd, data=clv.cdnow, vverbose=TRUE), "may not be passed")
})


# static cov, clv.data -----------------------------------------------------------------------------------------------
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(estimation.split = NULL)
# .formula -----------------------------------------------------------------------------------------------
test_that("Fails if no formula",{
  expect_error(latentAttrition(formula=, family=pnbd, data=clv.apparel.cov), "provide a valid formula object")
})
test_that("Fails if no RHS2",{
  expect_error(latentAttrition(formula=~High.Season, family=pnbd, data=clv.apparel.cov), "using a two-part notation")
  expect_error(latentAttrition(formula=~., family=pnbd, data=clv.apparel.cov), "using a two-part notation")
  expect_error(latentAttrition(formula=~1, family=pnbd, data=clv.apparel.cov), "using a two-part notation")
  expect_error(latentAttrition(formula=~-1, family=pnbd, data=clv.apparel.cov), "using a two-part notation")
  expect_error(latentAttrition(formula=~pnbd(), family=pnbd, data=clv.apparel.cov), "using a two-part notation")
})

test_that("Fails if more than 2 RHS",{
  expect_error(latentAttrition(formula=~.|.|., family=pnbd, data=clv.apparel.cov), "two-part notation")
  expect_error(latentAttrition(formula=~.|.|.|., family=pnbd, data=clv.apparel.cov), "two-part notation")
  expect_error(latentAttrition(formula=~Channel|Gender|., family=pnbd, data=clv.apparel.cov), "two-part notation")
  expect_error(latentAttrition(formula=~Channel|Gender|Gender, family=pnbd, data=clv.apparel.cov), "two-part notation")
})

test_that("Fails if has LHS",{
  expect_error(latentAttrition(data()~1, family=pnbd, data=clv.apparel.cov), "no dependent variable")
  expect_error(latentAttrition(pnbd()~1, family=pnbd, data=clv.apparel.cov), "no dependent variable")
  expect_error(latentAttrition(1~-1, family=pnbd, data=clv.apparel.cov), "no dependent variable")
  expect_error(latentAttrition(Gender~-1, family=pnbd, data=clv.apparel.cov), "no dependent variable")
  expect_error(latentAttrition(Gender|.~High.Season, family=pnbd, data=clv.apparel.cov), "no dependent variable")
  expect_error(latentAttrition(Gender~High.Season, family=pnbd, data=clv.apparel.cov), "no dependent variable")
})

test_that("Fails if RHS2/3 not in cov data",{
  skip_on_cran()
  expect_error(latentAttrition(formula=~gender|., family=pnbd, data=clv.apparel.cov), "could be found in the data")
  expect_error(latentAttrition(formula=~gender|gender, family=pnbd, data=clv.apparel.cov), "could be found in the data")
  expect_error(latentAttrition(formula=~.|gender, family=pnbd, data=clv.apparel.cov), "could be found in the data")
  expect_error(latentAttrition(formula=~.|.-gender, family=pnbd, data=clv.apparel.cov), "could be found in the data")
  expect_error(latentAttrition(formula=~.-family|., family=pnbd, data=clv.apparel.cov), "could be found in the data")
})

test_that("Fails if additional args in dots are not allowed for family + cov data", {
  # may not give names which are to be specified in formula
  expect_error(latentAttrition(formula = ~.|., family=pnbd, data=clv.apparel.cov, names.cov.life="Gender"), "may not be passed")
  expect_error(latentAttrition(formula = ~.|., family=pnbd, data=clv.apparel.cov, names.cov.trans="Gender"), "may not be passed")
  expect_error(latentAttrition(formula = ~.|., family=pnbd, data=clv.apparel.cov, clv.data=clv.apparel.cov), "may not be passed")

})




# dyncov cov -------------------------------------------------------------------------------------------------
# Same input checks are conducted as for static covs, therefore do not need additional checks
