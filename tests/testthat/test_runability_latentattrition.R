skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

context("Runability - latentAttrition - nocov")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)


test_that("Works out of the box with all models", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd(), data=clv.cdnow, verbose=FALSE))
  expect_silent(latentAttrition(~bgnbd(), data=clv.cdnow, verbose=FALSE))
  expect_silent(latentAttrition(~ggomnbd(), data=clv.cdnow, verbose=FALSE))
})

test_that("Works with optimx args", {
  skip_on_cran()
  expect_silent(p.cdnow <- latentAttrition(~pnbd(), data=clv.cdnow, verbose=FALSE, optimx.args=list(method="Nelder-Mead")))
  expect_setequal("Nelder-Mead", rownames(p.cdnow@optimx.estimation.output))
})

test_that("Works with verbose", {
  skip_on_cran()
  # verbose=FALSE is tested in every other place
  expect_message(latentAttrition(~pnbd(), data=clv.cdnow, verbose=TRUE))
})

test_that("Works with args to model", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd(start.params.model = c(r=1, alpha=0.2, s=2.3, beta=3.45)), data=clv.cdnow, verbose=FALSE))
  expect_warning(latentAttrition(~pnbd(use.cor = TRUE, start.param.cor=0.01), data=clv.cdnow, verbose=FALSE,
                                 optimx.args=list(itnmax=20,hessian=FALSE,control=list(kkt=F))),
                 "Hessian")
  expect_silent(latentAttrition(~bgnbd(start.params.model = c(r=1, alpha=0.2, a=2.3, b=3.45)), data=clv.cdnow, verbose=FALSE))
})



context("Runability - latentAttrition - static cov")

clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)
# test_that("Works without RHS2/3", {
#   expect_silent(latentAttrition(~pnbd(), clv.apparel.cov, verbose=FALSE))
# })

test_that("Every model works with regularization", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd()|.|.|regularization(life=8, trans=10), clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~bgnbd()|.|.|regularization(life=8, trans=10), clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~ggomnbd()|.|.|regularization(life=8, trans=10), clv.apparel.cov, verbose=FALSE))
})

test_that("Every model works with constraint", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd()|.|.|constraint(Gender), clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~bgnbd()|.|.|constraint(Gender), clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~ggomnbd()|.|.|constraint(Gender), clv.apparel.cov, verbose=FALSE))
})



context("Runability - latentAttrition - data.frame")

test_that("Works out-of-the-box", {
  skip_on_cran()
  expect_silent(latentAttrition(data() ~ pnbd(), cdnow, verbose=FALSE))
})

test_that("Works with explicit parameters", {
  skip_on_cran()
  expect_silent(latentAttrition(data(unit=w) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=NULL, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=37, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
})

test_that("Works with static covariates", {
  skip_on_cran()
  # outofthebox
  expect_silent(latentAttrition(data() ~ pnbd()|Gender|Gender+Channel, data=apparelTrans, cov = apparelStaticCov, verbose=FALSE))
  expect_silent(latentAttrition(data() ~ pnbd()|.|., data=apparelTrans, cov=apparelStaticCov, verbose=FALSE))
  # explicit parameters
  expect_silent(latentAttrition(data(unit=w, split=37, format=ymd) ~ pnbd()|.|., data=apparelTrans, cov = apparelStaticCov, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=37, format=ymd) ~ pnbd()|Gender|Gender+Channel, data=apparelTrans, cov = apparelStaticCov, verbose=FALSE))
})




