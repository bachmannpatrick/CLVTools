skip_on_cran()

clv.apparel <- fct.helper.create.clvdata.apparel.nocov()
clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov()

expect_silent(p.apparel.nocov <- pnbd(clv.apparel, verbose=FALSE))
expect_silent(p.apparel.static <- pnbd(clv.apparel.static, verbose=FALSE))

test_that("Runs with all models", {
  skip_on_cran()

  # all pnbds
  expect_silent(df.res <- lrtest(
    p.apparel.nocov,
    p.apparel.static,
    fct.helper.dyncov.quickfit.apparel.data()
  ))
  expect_true(is.data.frame(df.res))
  expect_s3_class(df.res, "anova")
  expect_true(nrow(df.res) == 3)
  expect_true(all(colnames(df.res) == c("#Df", "LogLik", "Df", "Chisq", "Pr(>Chisq)")))

  # bgnbds
  expect_silent(lrtest(
    bgnbd(clv.apparel, verbose=FALSE),
    bgnbd(clv.apparel.static, verbose=FALSE)))

  # ggomnbd
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
