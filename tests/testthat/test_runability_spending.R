skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# nocov ------------------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

test_that("Works out of the box with clv.data", {
  skip_on_cran()
  expect_silent(spending(~gg(), clv.cdnow, verbose=FALSE))
})

test_that("Works with data=data.frame", {
  skip_on_cran()
  # out of the box
  expect_silent(spending(data()~gg(), cdnow, verbose=FALSE))
  # explicit params
  expect_silent(spending(data(split=37)~gg(), cdnow, verbose=FALSE))
})

test_that("Works with start params", {
  skip_on_cran()
  expect_silent(spending(~gg(start.params.model=c(p=1, q=10, gamma=33)), clv.cdnow, verbose=FALSE))
})

test_that("Works with remove.first", {
  skip_on_cran()
  expect_silent(spending(~gg(remove=T), clv.cdnow, verbose=FALSE))
  expect_silent(spending(~gg(remove=FALSE), clv.cdnow, verbose=FALSE))
})

test_that("Works with optimx args", {
  skip_on_cran()
  expect_silent(gg.cdnow <- spending(~gg(), clv.cdnow, verbose=FALSE, optimx.args=list(method="Nelder-Mead")))
  expect_setequal("Nelder-Mead", rownames(gg.cdnow@optimx.estimation.output))
})

# static cov --------------------------------------------------------------------------------------
clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)

test_that("Works with clv.data static cov", {
  skip_on_cran()
  expect_silent(spending(~gg(), clv.apparel.static, verbose=FALSE))
})


# dyn cov -----------------------------------------------------------------------------------------
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans, apparelDynCov, estimation.split = NULL)

test_that("Works with clv.data dyn cov", {
  skip_on_cran()
  expect_silent(spending(~gg(), clv.apparel.dyn, verbose=FALSE))
})


