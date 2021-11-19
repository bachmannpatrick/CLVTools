skip_on_cran()
data("cdnow")

context("Inputchecks - latentAttrition - nocov")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

test_that("Fails if no model in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~., data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~Id, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~Id+Price, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~abc, data = clv.cdnow), "of the following models")
})

test_that("Fails if wrong model in RHS1", {
  # not as function
  expect_error(latentAttrition(~pnbd, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggomnbd, data = clv.cdnow), "of the following models")

  # inexistent function
  expect_error(latentAttrition(~gg(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~pndb(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bnbd(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggmnbd(), data = clv.cdnow), "of the following models")
})

test_that("Fails if anything else but model in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()+., data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~pnbd()+id, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd()+xyz, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd()+a:b, data = clv.cdnow), "of the following models")
})

test_that("Fails if multiple models in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()+bgnbd(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggomnbd()+bgnbd(), data = clv.cdnow), "of the following models")
})


test_that("Fails if unparsable given to model", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd(Id)), "can be parsed")
  expect_error(latentAttrition(~pnbd(Id, Price)), "can be parsed")
  expect_error(latentAttrition(~pnbd(clv.cdnow)), "can be parsed")
  expect_error(latentAttrition(~pnbd(clv.cdnow), data = clv.cdnow), "can be parsed")
})

test_that("Fails if RHS2/3/4 but no covariate data", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()|.|., data = clv.cdnow), "only contain 1 part")
  expect_error(latentAttrition(~pnbd()|Id|Price, data = clv.cdnow), "only contain 1 part")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=1, trans=2), data = clv.cdnow), "only contain 1 part")
})


test_that("Fails if non-parsable input to model", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd(use.cor=True), data = clv.cdnow), "parse")
  expect_error(latentAttrition(~pnbd(use.cor=abc), data = clv.cdnow), "parse")
  expect_error(latentAttrition(~pnbd(start.params.model = abc), data = clv.cdnow), "parse")
})

# context("Inputchecks - latentAttrition - static cov")
