skip_on_cran()

optimx.args <- list(itnmax=100)

fn.compare.hessian <- function(clv.fitted){
  expect_equal(
    hessian(clv.fitted),
    clv.fitted@optimx.hessian
    )
}


test_that("hessian() produces same result - no cov", {
  skip_on_cran()

  for(m in list(pnbd, bgnbd, ggomnbd, gg)){
    fn.compare.hessian(
      fit.cdnow(model=m, optimx.args = optimx.args)
    )
  }

  # With cor
  fn.compare.hessian(
    fit.cdnow(model = pnbd, use.cor=TRUE, optimx.args = optimx.args)
  )
})



test_that("hessian() produces same result - static cov", {
  skip_on_cran()

  for(m in list(pnbd, bgnbd, ggomnbd)){
    # Default specification
    fn.compare.hessian(
      fit.apparel.static(model=m, optimx.args = optimx.args)
    )

    # With constrained covs
    fn.compare.hessian(
      fit.apparel.static(
        model=m,
        names.cov.constr = "Gender",
        optimx.args = optimx.args)
    )

    # With regularization
    fn.compare.hessian(
      fit.apparel.static(
        model=m,
        reg.lambdas = c(life = 10, trans=5),
        optimx.args = optimx.args)
    )

    # With constrained covs & regularization
    fn.compare.hessian(
      fit.apparel.static(
        model=m,
        names.cov.constr = "Channel",
        reg.lambdas = c(life = 10, trans=5),
        optimx.args = optimx.args)
    )
  }


  # PNBD only: With cor
  fn.compare.hessian(
    fit.apparel.static(model = pnbd, use.cor=TRUE, optimx.args = optimx.args)
  )
})


test_that("hessian() produces same result - dyn cov", {
  skip_on_cran()

  # Default
  fn.compare.hessian(
    fit.apparel.dyncov(model = pnbd, optimx.args = optimx.args)
  )

  # With cor
  fn.compare.hessian(
    fit.apparel.dyncov(model = pnbd, use.cor=TRUE, optimx.args = optimx.args)
  )

  # With constrained covs
  fn.compare.hessian(
    fit.apparel.dyncov(model = pnbd, names.cov.constr = "Gender", optimx.args = optimx.args)
  )

  # With regularization
  fn.compare.hessian(
    fit.apparel.dyncov(model = pnbd, reg.lambdas = c(trans=10, life=5), optimx.args = optimx.args)
  )

})



test_that("hessian() fails if parameters are non-finite",{
  skip_on_cran()

  p.cdnow <- fit.cdnow(optimx.args=optimx.args)
  p.cdnow@optimx.estimation.output[1, "log.r"] <- NA_real_

  expect_error(hessian(p.cdnow), regexp = "Cannot proceed")
})

test_that("Internal clv.fitted.get.LL: Params position and order checked", {
  # Indirectly tested for correctness by being used in hessian()
  skip_on_cran()

  p.reg.constr <- fit.apparel.static(
    model = pnbd,
    reg.lambdas = c(trans = 4, life = 9),
    names.cov.constr = "Gender",
    optimx.args= optimx.args)

  LL.reg.constr <- clv.fitted.get.LL(p.reg.constr)
  final.coefs <- drop(coef(p.reg.constr@optimx.estimation.output))

  # Have to be named
  expect_error(LL.reg.constr(setNames(final.coefs, NULL)), regexp = "has to be named")

  # Does not work with extra coefs
  expect_error(LL.reg.constr(coef(p.reg.constr)), regexp = "has to be named")

  # Results are independent of order
  expect_identical(
    LL.reg.constr(sort(final.coefs, decreasing = FALSE)),
    LL.reg.constr(sort(final.coefs, decreasing = TRUE))
  )

})
