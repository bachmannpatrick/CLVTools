skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# nocov ---------------------------------------------------------------------------------
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




# static cov ------------------------------------------------------------------------------------
context("Runability - latentAttrition - static cov")
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)

test_that("Every model works without specials", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd()|.|., clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~bgnbd()|.|., clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~ggomnbd()|.|., clv.apparel.cov, verbose=FALSE))
})

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

test_that("Works with transformations in RHS2/3 and named in constraint()", {
  skip_on_cran()
  # with and without space in transformation (x+y vs x + y)
  expect_silent(latentAttrition(~pnbd()|log(Gender+2)|log(Gender+2)|constraint(log(Gender+2)), clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(~pnbd()|log(Gender+2)|log(Gender  +  2)|constraint(log( Gender + 2 )), clv.apparel.cov, verbose=FALSE))
})


test_that("Works with . excluding covs", {
  skip_on_cran()
  expect_silent(latentAttrition(~pnbd()|.|.-Gender, clv.apparel.cov, verbose=FALSE))
})


# dyn cov ------------------------------------------------------------------------------------
# Do one single test to verify formula interface works also with dyncov
context("Runability - latentAttrition - dyn cov")
clv.apparel.dyn.cov <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,  data.apparelDynCov=apparelDynCov, estimation.split=40)

test_that("Works with . and excluding covs", {
  skip_on_cran()
  expect_warning(latentAttrition(~pnbd()|.-Gender|., clv.apparel.dyn.cov, verbose=FALSE, optimx.args = fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
  expect_warning(latentAttrition(~pnbd()|.|.-Gender, clv.apparel.dyn.cov, verbose=FALSE, optimx.args = fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
})


# data.frame ------------------------------------------------------------------------------------------------------------------------
context("Runability - latentAttrition - data.frame")

test_that("Works out-of-the-box, nocov", {
  skip_on_cran()
  expect_silent(latentAttrition(data() ~ pnbd(), cdnow, verbose=FALSE))
})

test_that("Works with explicit parameters, nocov", {
  skip_on_cran()
  expect_silent(latentAttrition(data(unit=w) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=NULL, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=37, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
  expect_silent(latentAttrition(data(unit=w, split=, format=ymd) ~ pnbd(), data=cdnow, verbose=FALSE))
})

test_that("Works out-of-the-box, static covariates", {
  skip_on_cran()
  # outofthebox
  expect_silent(latentAttrition(data() ~ pnbd()|Gender|Gender+Channel, data=apparelTrans, cov = apparelStaticCov, verbose=FALSE))
  expect_silent(latentAttrition(data() ~ pnbd()|.|., data=apparelTrans, cov=apparelStaticCov, verbose=FALSE))
})


test_that("Works out-of-the-box, dynamic covariates", {
  skip_on_cran()
  expect_warning(latentAttrition(data() ~ pnbd()|Marketing+Gender|Gender+Channel, data=apparelTrans, cov = apparelDynCov,
                                verbose=FALSE, optimx.args = fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
  expect_warning(latentAttrition(data() ~ pnbd()|.|., data=apparelTrans, cov = apparelDynCov, verbose=FALSE,
                                optimx.args = fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
})

