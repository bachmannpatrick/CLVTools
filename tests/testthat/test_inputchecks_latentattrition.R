skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

# nocov, clv.data -----------------------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

# .data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data", {
  expect_error(latentAttrition(formula =, family=pnbd, data=), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=NULL), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=123), "clv.data")
  expect_error(latentAttrition(formula =, family=pnbd, data=cdnow), "clv.data")
})

# .formula -----------------------------------------------------------------------------------------------
test_that("Fails if formula given (but not required because nocov)", {
  expect_error(latentAttrition(formula = ~pnbd, family=pnbd, data=clv.cdnow), "formula not required")
  expect_error(latentAttrition(formula = ~pnbd(), family=pnbd, data=clv.cdnow), "formula not required")
  expect_error(latentAttrition(formula = ~data, family=pnbd, data=clv.cdnow), "formula not required")
  expect_error(latentAttrition(formula = ~x, family=pnbd, data=clv.cdnow), "formula not required")
  expect_error(latentAttrition(formula = ~1, family=pnbd, data=clv.cdnow), "formula not required")
  expect_error(latentAttrition(formula = data()~pnbd(), family=pnbd, data=clv.cdnow), "formula not required")
})


# .family -----------------------------------------------------------------------------------------------

test_that("Fails if family is missing", {
  expect_error(latentAttrition(formula = , family=, data=clv.cdnow), "of the following families")
})

test_that("Fails if family is not allowed method", {
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow), "of the following families")
  expect_error(latentAttrition(formula = , family=paretonbd, data=clv.cdnow), "of the following families")
  expect_error(latentAttrition(formula = , family=bgndb, data=clv.cdnow), "of the following families")
})


# .dots -----------------------------------------------------------------------------------------------
test_that("Fails if additional args in dots are not allowed for family", {
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow, names.cov.life="Gender"), "not required for family")
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow, start.params.trans=c(Channel=2)), "not required for family")
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow, reg.lambdas=c(trans=10, life=20)), "not required for family")
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow, abc=3), "not required for family")
  expect_error(latentAttrition(formula = , family=pareto, data=clv.cdnow, vverbose=TRUE), "not required for family")
})


# static cov, clv.data -----------------------------------------------------------------------------------------------
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                                               estimation.split = NULL)
# .formula -----------------------------------------------------------------------------------------------
test_that("Fails if no formula",{
  expect_error(latentAttrition(formula=, family=pnbd, data=clv.apparel.cov), "two formula has to be specified")
})
test_that("Fails if no RHS2",{
  expect_error(latentAttrition(formula=~Marketing, family=pnbd, data=clv.apparel.cov), "two part formula")
  expect_error(latentAttrition(formula=~., family=pnbd, data=clv.apparel.cov), "two part formula")
  expect_error(latentAttrition(formula=~1, family=pnbd, data=clv.apparel.cov), "two part formula")
  expect_error(latentAttrition(formula=~-1, family=pnbd, data=clv.apparel.cov), "two part formula")
  expect_error(latentAttrition(formula=~pnbd(), family=pnbd, data=clv.apparel.cov), "two part formula")
})

test_that("Fails if more than 2 RHS",{
  expect_error(latentAttrition(formula=~.|.|., family=pnbd, data=clv.apparel.cov), "no more than two parts")
  expect_error(latentAttrition(formula=~.|.|.|., family=pnbd, data=clv.apparel.cov), "no more than two parts")
  expect_error(latentAttrition(formula=~Channel|Gender|., family=pnbd, data=clv.apparel.cov), "no more than two parts")
  expect_error(latentAttrition(formula=~Channel|Gender|Gender, family=pnbd, data=clv.apparel.cov), "no more than two parts")
})

test_that("Fails if has LHS",{
  expect_error(latentAttrition(data()~1, family=pnbd, data=clv.apparel.cov), "no left part")
  expect_error(latentAttrition(pnbd()~1, family=pnbd, data=clv.apparel.cov), "no left part")
  expect_error(latentAttrition(~-1, family=pnbd, data=clv.apparel.cov), "no left part")
})

test_that("Fails if RHS2/3 not in cov data",{
  skip_on_cran()
  expect_error(latentAttrition(formula=~gender|., family=pnbd, data=clv.apparel.cov), "could not be found in the data")
  expect_error(latentAttrition(formula=~gender|gender, family=pnbd, data=clv.apparel.cov), "could not be found in the data")
  expect_error(latentAttrition(formula=~.|gender, family=pnbd, data=clv.apparel.cov), "could not be found in the data")
  expect_error(latentAttrition(formula=~.|.-gender, family=pnbd, data=clv.apparel.cov), "could not be found in the data")
  expect_error(latentAttrition(formula=~.-family|., family=pnbd, data=clv.apparel.cov), "could not be found in the data")
})



# dyncov cov -------------------------------------------------------------------------------------------------
# Same input checks are conducted as for static covs, therefore do not need additional checks
