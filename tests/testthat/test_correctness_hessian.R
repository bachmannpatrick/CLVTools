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

  for(m in list(pnbd, bgnbd, ggomnbd, gg)){
    fn.compare.hessian(
      fit.apparel.static(model=m, optimx.args = optimx.args)
    )

    # With constrained covs
    if(!identical(m, gg)){
      fn.compare.hessian(
        fit.apparel.static(
          model=m,
          names.cov.constr = "Gender",
          optimx.args = optimx.args)
      )
    }
  }

  # With cor
  fn.compare.hessian(
    fit.apparel.static(model = pnbd, use.cor=TRUE, optimx.args = optimx.args)
  )
})


test_that("hessian() produces same result - dyn cov", {
  skip_on_cran()

  # No cor
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
})



test_that("hessian() fails if parameters are non-finite",{
  p.cdnow <- fit.cdnow(optimx.args=optimx.args)
  p.cdnow@optimx.estimation.output[1, "log.r"] <- NA_real_

  expect_error(hessian(p.cdnow), regexp = "Cannot proceed")
})
