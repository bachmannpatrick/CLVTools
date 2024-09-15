skip_on_cran()

# nocov ---------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow()


test_that("Works out of the box with all models (family unevaluated)", {
  skip_on_cran()
  expect_silent(latentAttrition(formula = , family=pnbd, data=clv.cdnow, verbose=FALSE))
  expect_silent(latentAttrition(formula = , family=bgnbd, data=clv.cdnow, verbose=FALSE))
  expect_silent(latentAttrition(formula = , family=ggomnbd, data=clv.cdnow, verbose=FALSE))
})


# test_that("Works out of the box with all models (family evaluated)", {
#   skip_on_cran()
#   expect_silent(latentAttrition(formula = , family=pnbd(), data=clv.cdnow, verbose=FALSE))
#   expect_silent(latentAttrition(formula = , family=bgnbd(), data=clv.cdnow, verbose=FALSE))
#   expect_silent(latentAttrition(formula = , family=ggomnbd(), data=clv.cdnow, verbose=FALSE))
# })

test_that("Works with optimx args", {
  skip_on_cran()
  expect_silent(p.cdnow <- latentAttrition(formula = , family=pnbd, data=clv.cdnow, verbose=FALSE, optimx.args=list(method="Nelder-Mead")))
  expect_setequal("Nelder-Mead", rownames(p.cdnow@optimx.estimation.output))
})

test_that("Works with verbose", {
  skip_on_cran()
  # verbose=FALSE is tested in every other place
  expect_message(latentAttrition(formula = , family=pnbd, data=clv.cdnow, verbose=TRUE))
})

test_that("Works with extra args (start.params)", {
  skip_on_cran()
  expect_silent(latentAttrition(formula =, family=bgnbd, start.params.model = c(r=1, alpha=0.2, a=2.3, b=3.45), data=clv.cdnow, verbose=FALSE))
})

test_that("Works with correlation", {
  skip_on_cran()
  expect_warning(latentAttrition(formula = , family = pnbd, data=clv.cdnow, verbose=FALSE,
                                 use.cor = TRUE, start.param.cor=0.01,
                                 optimx.args=list(itnmax=20,hessian=FALSE,control=list(kkt=F))),
                 "Hessian")
})



# static cov ------------------------------------------------------------------------------------
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(estimation.split = NULL)

test_that("Every model works without specials", {
  skip_on_cran()
  expect_silent(latentAttrition(formula =~ .|., family=pnbd, data = clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(formula =~ .|., family=bgnbd, data = clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(formula =~ .|., family=ggomnbd, data = clv.apparel.cov, verbose=FALSE))
})


test_that("Works with regularization", {
  skip_on_cran()
  expect_silent(latentAttrition(formula =~ .|., family=pnbd, data = clv.apparel.cov, verbose=FALSE, reg.lambdas = c(life=8, trans=10)))
})

test_that("Works with selecting single covs", {
  skip_on_cran()
  expect_silent(latentAttrition(formula = ~Gender|Gender+Channel, family=pnbd, data=clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(formula = ~Gender|Channel, family=pnbd, data=clv.apparel.cov, verbose=FALSE))
})

test_that("Works with transformations in RHS2/3 and named in constraint()", {
  skip_on_cran()
  # with and without space in transformation (x+y vs x + y)
  expect_silent(latentAttrition(formula = ~ log(Gender+2)|log(Gender+2), family=pnbd, data = clv.apparel.cov, verbose=FALSE, names.cov.constr="log.Gender...2."))
  expect_silent(latentAttrition(formula = ~ log(Gender+2)|log(Gender  +  2), family=pnbd, data=clv.apparel.cov, verbose=FALSE, names.cov.constr="log.Gender...2."))
})

test_that("Works with . and excluding covs", {
  skip_on_cran()
  expect_silent(latentAttrition(formula = ~.|., family=pnbd, data=clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(formula = ~Gender-Channel|Channel-Gender, family=pnbd, data=clv.apparel.cov, verbose=FALSE))
  expect_silent(latentAttrition(formula = ~.-Channel|.-Gender, family=pnbd, data=clv.apparel.cov, verbose=FALSE))
})


# dyn cov ------------------------------------------------------------------------------------
clv.apparel.dyn.cov <- fct.helper.create.clvdata.apparel.dyncov()

test_that("Works with . and excluding covs", {
  skip_on_cran()
  expect_warning(latentAttrition(formula=~.-Gender|., family=pnbd, data=clv.apparel.dyn.cov, verbose=FALSE, optimx.args = fct.helper.dyncov.get.optimxargs.quickfit(hessian=FALSE)), "Hessian")
  expect_warning(latentAttrition(formula=~Channel|.-Gender, family=pnbd, data=clv.apparel.dyn.cov, verbose=FALSE, optimx.args = fct.helper.dyncov.get.optimxargs.quickfit(hessian=FALSE)), "Hessian")
})
