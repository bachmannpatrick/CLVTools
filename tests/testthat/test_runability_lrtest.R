skip_on_cran()

data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

expect_silent(clv.apparel <- clvdata(
  apparelTrans,
  date.format = "ymd",
  time.unit = "w",
  estimation.split = 104
))

clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(
  data.apparelTrans = apparelTrans,
  data.apparelStaticCov = apparelStaticCov,
  estimation.split = 104
)

expect_silent(p.apparel.nocov <- pnbd(clv.apparel, verbose=FALSE))
expect_silent(p.apparel.static <- pnbd(clv.apparel.static, verbose=FALSE))

test_that("Runs with all models", {
  skip_on_cran()

  expect_silent(df.res <- lrtest(
    p.apparel.nocov,
    p.apparel.static,
    fct.helper.dyncov.quickfit.apparel.data(
      data.apparelTrans=apparelTrans,
      data.apparelDynCov=apparelDynCov,
      hessian=FALSE)
  ))
  expect_true(is.data.frame(df.res))
  expect_s3_class(df.res, "anova")
  expect_true(nrow(df.res) == 3)
  expect_true(all(colnames(df.res) == c("#Df", "LogLik", "Df", "Chisq", "Pr(>Chisq)")))

  expect_silent(lrtest(
    bgnbd(clv.apparel, verbose=FALSE),
    bgnbd(clv.apparel.static, verbose=FALSE)))

  expect_silent(lrtest(
    ggomnbd(clv.apparel, verbose=FALSE),
    ggomnbd(clv.apparel.static, verbose=FALSE)))
})

test_that("Runs when called from lmtest::lrtest()", {
    expect_silent(lmtest::lrtest(
      p.apparel.nocov,
      p.apparel.static
    ))
})

test_that("Runs when the `lmtest` package is attached", {
  if(requireNamespace("lmtest", quietly = TRUE)){
    expect_silent(lmtest::lrtest(
      p.apparel.nocov,
      p.apparel.static
    ))
  }
})
