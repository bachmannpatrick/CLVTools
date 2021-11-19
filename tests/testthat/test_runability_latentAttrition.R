skip_on_cran()
data("cdnow")

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
                                 optimx.args=list(itnmax=20,hessian=F,control=list(kkt=F))),
                 "Hessian")
  expect_silent(latentAttrition(~bgnbd(start.params.model = c(r=1, alpha=0.2, a=2.3, b=3.45)), data=clv.cdnow, verbose=FALSE))
})


