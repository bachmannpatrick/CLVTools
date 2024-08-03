skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# nocov ------------------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

test_that("Works out of the box", {
  skip_on_cran()
  expect_silent(spending(family=gg, data=clv.cdnow, verbose=FALSE))
})

test_that("Works out of the box", {
  skip_on_cran()
  expect_silent(spending(family=gg, start.params.model=c(p=1, q=10, gamma=33), data=clv.cdnow, verbose=FALSE))
})

test_that("Works with args in ...", {
  skip_on_cran()
  expect_silent(spending(family=gg, remove.first.transaction=TRUE, data=clv.cdnow, verbose=FALSE))
  expect_silent(spending(family=gg, start.params.model=NULL, data=clv.cdnow, verbose=FALSE))
})

test_that("Works with optimx args", {
  skip_on_cran()
  expect_silent(gg.cdnow <- spending(family=gg, remove.first.transaction=TRUE, data=clv.cdnow, verbose=FALSE, optimx.args=list(method="Nelder-Mead")))
  expect_setequal("Nelder-Mead", rownames(gg.cdnow@optimx.estimation.output))
})


# static cov --------------------------------------------------------------------------------------
clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)

test_that("Works with clv.data static cov", {
  skip_on_cran()
  expect_silent(spending(family=gg, data=clv.apparel.static, verbose=FALSE))
})


# dyn cov -----------------------------------------------------------------------------------------
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans, apparelDynCov, estimation.split = NULL)

test_that("Works with clv.data dyn cov", {
  skip_on_cran()
  expect_silent(spending(family=gg, data=clv.apparel.dyn, verbose=FALSE))
})


